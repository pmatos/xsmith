#lang xsmith/private/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require
 "grammar-properties.rkt"
 racket/list
 racket/dict
 racket/struct
 racket/match
 racket/syntax
 syntax/parse
 (for-template
  xsmith/private/base
  racr)  ;; This is required for providing λ at runtime.
 )

(provide
 (struct-out grammar-refiner)
 sort-refiners
 refiner-stx->att-rule-name
 refiner-stx->ref-pred-func
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
Provide a wrapper to get the refiner-predicate out of a refiner syntax object.
|#
(define (refiner-stx->ref-pred-func ref-stx)
  (grammar-refiner-refiner-predicate ref-stx))


#|
Refiner values consist of a list of (optional) predicate functions followed by
a single transformation function. This function will combine these into a single
large function to be used with RACR's `perform-rewrites` function.
|#
(define (ref-funcs->refiner global-predicate funcs-stx)
  (define funcs (syntax->list funcs-stx))
  ;; If the global-predicate is not #f and is a syntax object that does not
  ;; contain #f, it should be consed onto the front of the function list.
  (when (and global-predicate
             (syntax->datum global-predicate))
    (set! funcs (cons global-predicate funcs)))
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
     (let* ([ref-stx (syntax-local-value #'gr)]
            [ref-name (syntax->datum (refiner-stx->att-rule-name ref-stx))]
            [global-pred (grammar-refiner-global-predicate ref-stx)]
            [ref-hash (hash-ref (hash-ref infos-hash 'refs-info)
                                ref-stx
                                (hash))]
            [att-rules-hash (hash-ref infos-hash 'ag-info)]
            [this-rules-hash (hash-ref att-rules-hash ref-name (hash))])
       ;; Provide a default clause that has no effect.
       ;; This is important because refiners are applied to all nodes in an AST,
       ;; regardless of whether they have a clause defined. The implementation
       ;; of the default clause can be specified by the user by using the key
       ;; #f, but the refiner's global-predicate (if there is one) will not be
       ;; applied to the #f clause in any case.
       (when (not (dict-has-key? ref-hash #f))
         (set! ref-hash (dict-set ref-hash #f #'[(λ (n) #f)])))
       (hash-set infos-hash 'ag-info
                 (hash-set att-rules-hash
                           ref-name
                           (for/fold ([combined this-rules-hash])
                                     ([k (dict-keys ref-hash)])
                             (when (dict-ref combined k #f)
                               (raise-syntax-error 'grammar-refiner-transform
                                                   "duplicate rule"
                                                   #'ref-name))
                             (define refiner
                               (ref-funcs->refiner
                                (and k global-pred)  ;; Don't use the global-pred for a #f key.
                                (dict-ref ref-hash k)))
                             (hash-set combined k refiner)))))]))

#|
The grammar-refiner struct groups refiner names with a #:follows declaration
that allows for specifying the sequence in which refiners should be applied. The
actual definition of the refiner (the transformation function which will be
applied to a node) is specified separately.

Additionally, there are two predicates that can be optionally supplied:
 - refiner-predicate is a predicate function that determines whether the refiner
   itself will be run after AST generation
 - global-predicate is a predicate function that will be composed in front of
   all the other refiner functions
|#
(struct grammar-refiner
  (name follows refiner-predicate global-predicate)
  #:property prop:procedure (λ (stx)
                              (raise-syntax-error
                               'grammar-refiner
                               "Can't be used directly as a macro."
                               stx))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (gr) 'grammar-refiner)
      (λ (gr) (list (grammar-refiner-name gr)
                    (grammar-refiner-follows gr)
                    (grammar-refiner-refiner-predicate gr)
                    (grammar-refiner-global-predicate gr)))))])

(define-syntax-class grammar-refiner-stx
  (pattern gr:id
           #:when (grammar-refiner? (syntax-local-value #'gr (λ () #f)))))
(define-syntax-class property-arg-refiner
  (pattern ((~datum refiner) name:id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
