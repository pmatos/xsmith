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
 [Val #f ([v = (random 100)])
      #:prop wont-over-deepen #t]
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

(add-att-rule
 btree-grammar
 pretty-print
 [Branch (λ (n i)
           (v-append
            (text "(branch")
            (indent 2
                    (v-append
                     (att-value 'pretty-print (ast-child 'Val n) i)
                     (att-value 'pretty-print (ast-child 'l n) i)
                     (h-append
                      (att-value 'pretty-print (ast-child 'r n) i)
                      (text ")"))))))]
 [Leaf (λ (n i)
         (h-append
          (text "(leaf ")
          (att-value 'pretty-print (ast-child 'Val n) i)
          (text ")")))]
 [Val (λ (n i)
        (text (number->string (ast-child 'v n))))]
 )

(assemble-spec-components btree btree-grammar)

(define (btree-generate-and-print)
  (let ([ast (btree-generate-ast 'Branch)])
    (pretty-print (att-value 'pretty-print ast 0)
                  (current-output-port)
                  120)))

(xsmith-command-line
 btree-generate-and-print
 #:fuzzer-name "btree"
 #:default-max-depth 20)
