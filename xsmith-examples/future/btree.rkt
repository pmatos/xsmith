#lang racket/base

(require
 xsmith
 racr
 pprint)

(define-spec-component btree-grammar)

(add-to-grammar
 btree-grammar
 [Node #f ()
       #:prop may-be-generated #f]
 [Branch Node ([l : Node]
               [r : Node]
               Val)]
 [Leaf Node (Val)
       #:prop wont-over-deepen #t]
 [Val #f ([v = (random 100)])]
 )

(define int (base-type 'int))

(add-prop
 btree-grammar
 type-info
 [Branch [int
          (λ (n t)
            (hash 'l t
                  'r t
                  'Val int))]]
 [Leaf [int
        (λ (n t)
          (hash 'Val t))]]
 [Val [int (λ (n t) (hash))]]
 )

(add-prop
 btree-grammar
 render-node-info
 [Branch (λ (n)
           (v-append
            (text "(branch")
            (indent 2
                    (v-append
                     (render-node (ast-child 'Val n))
                     (render-node (ast-child 'l n))
                     (h-append
                      (render-node (ast-child 'r n))
                      (text ")"))))))]
 [Leaf (λ (n)
         (h-append
          (text "(leaf ")
          (render-node (ast-child 'Val n))
          (text ")")))]
 [Val (λ (n)
        (text (number->string (ast-child 'v n))))]
 )

(add-prop
 btree-grammar
 render-hole-info
 [#f (λ (h) (h-append
             (text "<")
             (text (symbol->string (ast-node-type h)))
             (text ">")))])

(assemble-spec-components btree btree-grammar)

(define (btree-generate)
  (btree-generate-ast 'Branch))

(xsmith-command-line
 btree-generate
 #:fuzzer-name "btree"
 #:default-max-depth 20
 #:format-render (λ (d) (pretty-format d 120)))
