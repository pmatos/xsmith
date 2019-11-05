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

(define-transformer
 tg
 evens-only
 #:predicate
 (λ (n) (when (eq? (ast-node-type n) Val)
          (let ([v (ast-child 'v n)])
            (odd? v))))
 #:transformer
 (λ (n) (display "odd value encountered")))

(assemble-spec-components t tg)

(xsmith-command-line
 (λ () (t-generate-ast 'Prog))
 #:fuzzer-name "trans-test-fuzzer")
