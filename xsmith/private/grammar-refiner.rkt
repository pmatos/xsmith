#lang racket/base

(require racket/list
         racket/dict
         racket/struct)

(provide
 (struct-out grammar-refiner)
 (all-defined-out))

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
    (if (empty? elements)
        (reverse layers)  ; Put the layer without dependencies first.
        #;(let ([queue (filter values
                             (for/list ([element elements])
                               (let* ([deps (hash-ref dependencies element)]
                                      [satisfied-deps (map (λ (dep) (set-has-key? added dep))
                                                           deps)]
                                      [all-deps-satisfied? (andmap values satisfied-deps)])
                                 (if all-deps-satisfied?
                                     element
                                     #f))))])
          ...)
        (let ([queue (filter (λ (e) (andmap values
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

(define (sort-refiners refs-hash)
  (define refiners-dependencies-hash
    (for/hash ([ref (dict-keys refs-hash)])
      (values
       (grammar-refiner-name ref)
       (grammar-refiner-follows ref))))
  (display (format "refiners-dependencies-hash\n~a\n\n" refiners-dependencies-hash))
  (define sorted-refs (stratify refiners-dependencies-hash))
  (display (format "sorted-refs\n~a\n\n" sorted-refs))
  #;(dict-for-each refs-hash
                 (λ (k v)
                   (display (format "~a :: ~a\n" k v))))
  (dict-keys refs-hash))

(define (grammar-refiner-transform infos-hash
                                   refs-hash)
  (define (refs-hash->ref name)
    (hash-ref refs-hash (syntax-local-value name)))
  (define sorted-refs
    (sort-refiners refs-hash))
  ; TODO - return something else
  infos-hash)

(struct grammar-refiner
  (name follows)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-refiner
                                      "Can't be used directly as a macro."
                                      stx))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (gr) 'grammar-refiner)
      (λ (gr) (list (grammar-refiner-name gr) (grammar-refiner-follows gr)))))])
