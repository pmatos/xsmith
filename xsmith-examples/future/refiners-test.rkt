#lang racket/base

(require
 xsmith
 racr
 )

(define-spec-component tg)

(add-to-grammar
 tg
 [Prog #f ([val : Val])]
 [Val #f ([v = (random 100)])
      #:prop wont-over-deepen #t]
 )

(define int (base-type 'int))
(add-prop
 tg
 type-info
 [Prog [(fresh-type-variable)
        (λ (n t) (hash 'val t))]]
 [Val [int (λ (n t) (hash))]]
 )

(add-prop
 tg
 render-node-info
 [Prog (λ (n) (render-node (ast-child 'val n)))]
 [Val (λ (n) (number->string (ast-child 'v n)))]
 )

(define-refiner
  tg
  evens-only
  [#f [(λ (n) #f)]]
  [Val [(λ (n) (odd? (ast-child 'v n)))
        (λ (n) (begin
                 (eprintf (format "odd value encountered: ~a\n" (ast-child 'v n)))
                 (make-fresh-node 'Val (hash 'v (+ 1 (ast-child 'v n))))))]])

#;(define-refiner
  tg
  times-two
  #:follows evens-only
  [Val [(λ (n) (begin
                 (display "will multiply by two")
                 (make-fresh-node )))]])

(assemble-spec-components t tg)

(xsmith-command-line
 (λ () (t-generate-ast 'Prog))
 #:fuzzer-name "trans-test-fuzzer")
