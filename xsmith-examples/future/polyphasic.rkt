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
;; TODO

;; Assemble!
(assemble-spec-components stree stree-grammar)

(define (stree-generate)
  (stree-generate-ast 'Branch))

(xsmith-command-line
 stree-generate
 #:fuzzer-name "stree"
 #:default-max-depth 20
 #:format-render (λ (d) (pretty-format d 120)))
