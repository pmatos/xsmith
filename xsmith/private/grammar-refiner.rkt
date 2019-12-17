#lang racket/base

(require
 "grammar-properties.rkt"
 racket/list
 racket/dict
 racket/struct
 racket/match
 racket/syntax
 syntax/parse
 (for-template
  racr
  racket/base)  ;; This is required for providing λ at runtime.
 )

(provide
 (struct-out grammar-refiner)
 sort-refiners
 refiner-stx->att-rule-name
 grammar-refiner-transform)

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

#|
Given a refiner (as a syntax object), produce a name for the att-rule that will
correspond to this refiner. For a refiner named "my-refiner", this will produce:

    #'(att-rule _xsmith_auto-ref_my-refiner)
|#
(define (refiner-stx->att-rule-name ref-stx)
  (let ([ref-name (grammar-refiner-name ref-stx)])
    (with-syntax
      ([att-rule-name
        (format-id #'ref-name #:source #'ref-name "_xsmith_auto-ref_~a" ref-name)])
      #'att-rule-name)))


#|
Refiner values consist of a list of (optional) predicate functions followed by
a single transformation function. This function will combine these into a single
large function to be used with RACR's `perform-rewrites` function.
|#
(define (ref-funcs->refiner funcs-stx)
  (define funcs (syntax->list funcs-stx))
  (match funcs
    ;; If there is only one function in the list, return it as-is.
    [(list func)
     func]
    ;; Otherwise, merge the functions into a single function where each other
    ;; function is and-ed together.
    [_ (with-syntax ([(func ...) funcs])
         #'(λ (n)
             (and (func n) ...)))]))

#|
Given a grammar refiner and an infos hash, transforms the refiner into an
att-rule and puts it in the infos-hash in the appropriate position.

The infos hash is a hash containing (at least) two keys ('refs-info and
'ag-info), each of which themselves contain hashes mapping names of properties
to whatever is needed.
|#
(define (grammar-refiner-transform grammar-ref-name-stx
                                   infos-hash)
  (syntax-parse grammar-ref-name-stx
    [gr:grammar-refiner-stx
     (let* ([slv (syntax-local-value #'gr)]
            [ref-name (syntax->datum (refiner-stx->att-rule-name slv))])
       (define ret (hash-ref (hash-ref infos-hash 'refs-info)
                             slv
                             (hash)))
       (define att-rules-hash (hash-ref infos-hash 'ag-info))
       (define this-rules-hash
         (hash-ref att-rules-hash ref-name (hash)))
       (hash-set infos-hash 'ag-info
                 (hash-set att-rules-hash
                           ref-name
                           (for/fold ([combined this-rules-hash])
                                     ([k (dict-keys ret)])
                             (when (dict-ref combined k #f)
                               (raise-syntax-error 'grammar-refiner-transform
                                                   "duplicate rule"
                                                   #'ref-name))
                             (define refiner
                               (ref-funcs->refiner (dict-ref ret k)))
                             (hash-set combined k refiner)))))]))

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
