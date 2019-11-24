#lang racket/base

(require
 racket/list
 racket/dict
 racket/struct
 racket/match
 syntax/parse
 )

(provide
 (struct-out grammar-refiner)
 grammar-refiners->transformers)

#|
Here we define sets as hashes where the values are always #t. These are used for
easier management of uniqueness properties without having to deal with values.
|#
(define (set . ks)
  (make-immutable-hash
   (map (λ (k) (cons k #t))
        ks)))
(define (set-set s k)
  (hash-set s k #t))
(define (set-append s ks)
  (for/fold ([new-s s])
            ([k ks])
    (set-set new-s k)))
(define (set-has-key? s k)
  (hash-has-key? s k))

#|
Given a dictionary detailing dependencies among elements, produce a list of
lists layering those elements.

Consider a dependency mapping such as the following (where '->' is pronounced
'depends upon'):
  a -> []         b -> [a, c]     c -> [d]        d -> [a]
  e -> [a]        f -> [c]        g -> []         h -> [g]

When viewed as a dependency graph, the layers become evident:

        ___          (there is no layer here)
       /   \
      a     g     -> [a, g]
     / \    |
    d   e   h     -> [d, e, h]
    |
    c             -> [c]
   / \
  b   f           -> [b, f]

So, for this example, `stratify' produces the following list:
  '((a g) (e d h) (c) (f b))

(No guarantees are made about the order of elements within each sub-list.)

The dictionary passed in should map hashable elements to lists of hashable
elements. The above example can be represented via symbols as:
  (define deps (hash 'a '()
                     'b '(a c)
                     'c '(d)
                     'd '(a)
                     'e '(a)
                     'f '(c)
                     'g '()
                     'h '(g)))
|#
(define (stratify dep-hash)
  (define (stratify layers added elements dependencies)
    #|
    layers       - the accumulated layers of dependencies (a list of lists of elements)
    added        - a set denoting which elements have already been added to the layers
    elements     - the list of elements remaining to add
    dependencies - a map from each element to a list of elements it depends on
    |#
    #;(display (format "stratify\n  layers: ~a\n  added: ~a\n  elements: ~a\n  dependencies: ~a\n\n"
                     layers added elements dependencies))
    (if (empty? elements)
        (reverse layers)  ; Put the layer without dependencies first.
        (let ([queue  ; Create a list of elements that can be added with the current layers.
               (filter (λ (e) (andmap values
                                      (map (λ (d) (set-has-key? added d))
                                           (hash-ref dependencies e))))
                             elements)])
          (if (empty? queue)
              (raise-arguments-error 'stratify
                                     "received an empty queue"
                                     "elements" elements
                                     "added" added
                                     "dependencies" dependencies
                                     "layers" layers)
              (stratify
               (cons queue layers)
               (set-append added queue)
               (remove* queue elements)
               dependencies)))))
  (stratify '() (set) (dict-keys dep-hash) dep-hash))

#|
Users can specify a #:follows on grammar-refiners. This functions normalizes
inputs to reduce errors. Users can specify #:follows as any of the following:

  #:follows foo
  #:follows 'foo
  #:follows (foo)
  #:follows '(foo)

When multiple arguments are to be given, a list form *must* be used:

  #:follows (foo bar)
  #:follows '(foo bar)
|#
(define (fix-follows follows)
  (match follows
    [(list quote (list quote real-follows)) real-follows]
    [(list quote real-follows) real-follows]
    [(list real-follows) (list real-follows)]
    [(list) (list)]
    [real-follows (list real-follows)]))

#|
Given a list of grammar-refiners, return a list of refiner names ordered based
on their indicated #:follows dependencies.
|#
(define (sort-refiners refs)
  ; Extract dependencies for stratification.
  (define refiners-dependencies-hash
    (for/hash ([ref refs])
      (values
       (grammar-refiner-name ref)
       (fix-follows (grammar-refiner-follows ref)))))
  ; Separate the refiners into layers based on dependency, then flatten them
  ; into a single correctly-ordered list.
  (flatten (stratify refiners-dependencies-hash)))

(define (grammar-refiners->transformers refs-hash)
  ; Produce a predicate to test a node's type.
  (define (type->pred type)
    #'(λ (n) (eq? type (node-type n))))
  ; Merge multiple predicates into a single predicate. All predicates must be
  ; satisfied by the input to pass, hence the conjunction.
  (define (preds->pred preds)
    #'(λ (n)
        (for/fold ([res #t])
                  ([p preds])
          (and res (p n)))))
  ; Extract a pair of syntax functions from the syntax-list of functions that
  ; correspond to a specific node type.
  ;
  ; The syntax-list of functions should either have one or two functions in it.
  ; When there is only one, it is a transformer function that should be applied
  ; to a node type in all cases. When there are two, the first function is a
  ; predicate to test whether a transformer should be run, and the second is the
  ; transformer function.
  (define (funcs->func-pair stx)
    (match (syntax->list stx)
      [(list pred func)
       (cons pred func)]
      [(list func)
       (cons #f func)]
      [_ (cons #f #f)]))
  ; Given a predicate and a transformer function, produce a final transformer.
  (define (pred+func->trans pred func)
    (with-syntax ([pred pred]
                  [func func])
      #'(λ (n) (and (pred n)
                    (func n)))))
  ; Given a map from types to syntax-lists of functions associated with each
  ; type, produce a list of transformers. Each transformer will correspond to a
  ; single type.
  (define (func-hash->transformers funcs-by-type)
    (for/list ([(type funcs) (in-dict funcs-by-type)])
      (match-let* ([type-pred (type->pred type)]
                   [(cons pred func)
                    (match (funcs->func-pair funcs)
                      [(cons _ #f)
                       (raise-argument-error 'grammar-refiner-transform
                                             "(syntax? (-> ast-node? ast-node?)"
                                             #f)]
                      [(cons #f func)
                       (cons type-pred func)]
                      [(cons pred func)
                       (cons (preds->pred (list type-pred pred)) func)])])
        (pred+func->trans pred func))))
  ; Map refiner names to maps of functions (for easier lookup).
  (define func-hashes-by-refiner
    (for/hash ([(refiner funcs) (in-hash refs-hash)])
      (values
       (grammar-refiner-name refiner)
       funcs)))
  ; A map from refiner names to their associated transformers (as syntax
  ; objects).
  (define transformers-by-refiner
    (for/hash ([(ref funcs-by-type) (in-dict func-hashes-by-refiner)])
      (values ref
              (func-hash->transformers funcs-by-type))))
  ; Sort the refiners based on their #:follows declarations.
  (define sorted-refiners
    (sort-refiners (dict-keys refs-hash)))
  (define sorted-transformers
    (flatten
     (for/list ([ref sorted-refiners])
       (dict-ref transformers-by-refiner ref))))
  sorted-transformers)

#|
The grammar-refiner struct groups refiner names with a #:follows declaration
that allows for specifying the sequence in which refiners should be applied. The
actual definition of the refiner (the transformation function which will be
applied to a node) is specified separately.
|#
(struct grammar-refiner
  (name follows)
  #:property prop:procedure (λ (stx)
                              (raise-syntax-error
                               'grammar-refiner
                               "Can't be used directly as a macro."
                               stx))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (gr) 'grammar-refiner)
      (λ (gr) (list (grammar-refiner-name gr) (grammar-refiner-follows gr)))))])
