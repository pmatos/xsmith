#lang racket/base

(require
 "grammar-properties.rkt"
 racket/list
 racket/dict
 racket/struct
 racket/match
 racket/syntax
 syntax/parse
 )

(provide
 (struct-out grammar-refiner)
 sort-refiners
 refiner-stx->att-rule-name
 grammar-refiner-transform
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
Given a list of grammar-refiners, return a list of those refiners ordered based
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
  (define sorted-names (flatten (stratify refiners-dependencies-hash)))
  ; Convert names back into the grammar-refiner objects we received initially.
  (map (λ (name)
         (findf (λ (ref)
                  (eq? name
                       (grammar-refiner-name ref)))
                refs))
       sorted-names))

(define (refiner-stx->att-rule-name ref-stx)
  (let ([ref-name (grammar-refiner-name ref-stx)])
    (with-syntax
      ([att-rule-name
        (format-id #'ref-name #:source #'ref-name "_xsmith_auto-ref_~a" ref-name)])
      #'(att-rule
         att-rule-name))))

(define (grammar-refiner-transform grammar-ref-name-stx
                                   infos-hash)
  (define (infos->section infos-hash pa)
    (syntax-parse pa
      [p:property-arg-refiner (hash-ref (hash-ref infos-hash 'refs-info)
                                        (syntax-local-value #'p.name)
                                        (hash))]
      [p:property-arg-att-rule (hash-ref (hash-ref infos-hash 'ag-info)
                                         (syntax->datum #'p.name)
                                         (hash))]))
  (define (section->infos ref-arg new-hash infos-hash)
    (syntax-parse ref-arg
      [r:property-arg-refiner
       (define refs-hash (hash-ref infos-hash 'refs-info))
       (define this-ref-hash
         (hash-ref refs-hash (syntax-local-value #'r.name) (hash)))
       (hash-set infos-hash 'refs-info
                 (hash-set refs-hash
                           (syntax-local-value #'r.name)
                           (for/fold ([combined this-ref-hash])
                                     ([k (dict-keys new-hash)])
                             (define old-val (dict-ref combined k '()))
                             (define new-val
                               (syntax-parse (dict-ref new-hash k)
                                 [(nv ...) (syntax->list #'(nv ...))]
                                 [bad-stx (raise-syntax-error
                                           (syntax->datum #'r.name)
                                           "bad return from refiner transformer"
                                           #'bad-stx
                                           #'r.name)]))
                             (hash-set combined k (append old-val
                                                          new-val)))
                           new-hash))]
      [p:property-arg-att-rule
       (define rules-hash (hash-ref infos-hash 'ag-info))
       (define this-rule-hash
         (hash-ref rules-hash (syntax->datum #'p.name) (hash)))
       (hash-set infos-hash #'ag-info
                 (hash-set rules-hash
                           (syntax->datum #'p.name)
                           (for/fold ([combined this-rule-hash])
                                     ([k (dict-keys new-hash)])
                             (when (dict-ref combined k #f)
                               (raise-syntax-error 'grammar-refiner-transform
                                                   "duplicate rule"
                                                   #'p.name))
                             (define new-val (dict-ref new-hash k))
                             (hash-set combined k new-val))))]))
  (syntax-parse grammar-ref-name-stx
    [gr:grammar-refiner-stx
     (let* ([slv (syntax-local-value #'gr)]
            [append (refiner-stx->att-rule-name slv)])
       (define ret (infos->section infos-hash #'(refiner gr)))
      (section->infos append ret infos-hash))]))




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
          (and re
               s (p n)))))
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
     (for/list ([ref (map grammar-refiner-name sorted-refiners)])
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

(define-syntax-class grammar-refiner-stx
  (pattern gr:id
           #:when (grammar-refiner? (syntax-local-value #'gr (λ () #f)))))
(define-syntax-class property-arg-refiner
  (pattern ((~datum refiner) name:id)))
