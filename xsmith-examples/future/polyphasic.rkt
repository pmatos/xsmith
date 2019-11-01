#lang racket/base

(require
 xsmith
 racr
 pprint)

;; stree = Search Tree (it will be sorted later)
(define-spec-component stree-grammar)

;; Grammar specification.
(add-to-grammar
 stree-grammar
 [Node #f ()
       #:prop may-be-generated #f]
 [Branch Node ([l : Node]
               [r : Node]
               Val)]
 [Leaf Node (Val)
       #:prop wont-over-deepen #t]
 [Empty Node ()
        #:prop wont-over-deepen #t]
 [Val #f ([v = (random 100)])]
 )

;; Trivial type system.
(define int (base-type 'int))
(add-prop
 stree-grammar
 type-info
 [Branch [int (λ (n t) (hash 'l t
                             'r t
                             'Val int))]]
 [Leaf [int (λ (n t) (hash 'Val t))]]
 [Empty [int (λ (n t) (hash))]]
 [Val [int (λ (n t) (hash))]]
 )

;; Rendering (using pprint).
(add-prop
 stree-grammar
 render-node-info
 [Branch (λ (n) (v-append
                 (text "(branch")
                 (h-append
                  (indent 2 (v-append
                             (render-node (ast-child 'Val n))
                             (render-node (ast-child 'l n))
                             (render-node (ast-child 'r n))))
                  (text ")"))))]
 [Leaf (λ (n) (h-append
               (text "(leaf")
               (render-node (ast-child 'Val n))
               (text ")")))]
 [Empty (λ (n) (text "(empty)"))]
 [Val (λ (n) (text (number->string (ast-child 'v n))))]
 )

(add-prop
 stree-grammar
 render-hole-info
 [#f (λ (h) (h-append (text "<")
                      (text (symbol->string (ast-node-type h)))
                      (text ">")))])

;; Post-generation transformations.
(add-transformer
 stree-grammar
 straighten
 #:transformer
 (λ (n)
   (case n
     [Branch (let ([lt (straighten (ast-child 'l n))]
                   [rt (straighten (ast-child 'r n))])
               (case lt
                 [Empty (set! lt rt)]
                 [else (...)]))]
     [Leaf (Leaf (ast-child 'v n))]
     [Empty Empty])))

(add-transformer
  stree-grammar
  sort
  #:follows straighten  ; this provides sequencing (can also be a list)
  #:transformer
  (λ (n)
    ...))

(define (height n)
  (match (ast-node-type n)
    [Empty 0]
    [Leaf 1]
    [Branch (+ 1 (max (height (ast-child 'l n)) (height (ast-child 'r n))))]))

(define (unbalanced? n)
  (and (eq? (ast-node-type n) Branch)
       (or (> (height (ast-child 'l n)) (+ 1 (height (ast-child 'r n))))
           (> (height (ast-child 'r n)) (+ 1 (height (ast-child 'l n)))))))

#|
- no separate predicate
- internal match
- explicit recursion

- mutation/production unspecified
|#
(add-transformer
 stree-grammar
 balance
 (λ (n) (match (ast-node-type n)
          [Empty ()]
          [Leaf ()]
          [Branch (begin
                    ((get-transformer balance) (ast-child 'l n))
                    ((get-transformer balance) (ast-child 'r n))
                    (when (unbalanced? n)
                      ...))])))

#|
- no separate predicate
- top-level match
- explicit recursion

- mutation/production unspecified
|#
(add-transformer
 stree-grammar
 balance
 [Empty ()]
 [Leaf ()]
 [Branch (begin
           ((get-transformer balance) (ast-child 'l n))
           ((get-transformer balance) (ast-child 'r n))
           (when (unbalanced? n)
             ...))])

#|
- uses predicate
- internal match
|#
(add-transformer
 stree-grammar
 balance
 #:predicate unbalanced?
 #:transformer (λ (n) ...))  ;; immediately perform the modifications

;; Assemble!
(assemble-spec-components stree stree-grammar)
  ; transformers are automatically applied

(define (stree-generate)
  (stree-generate-ast 'Branch))

(xsmith-command-line
 stree-generate
 #:fuzzer-name "stree"
 #:default-max-depth 20
 #:format-render (λ (d) (pretty-format d 120)))
