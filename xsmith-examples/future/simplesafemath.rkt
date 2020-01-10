#lang racket/base

(require
 xsmith
 racr)

(define-spec-component sm)

(define lower-bound -100)
(define upper-bound 100)

(add-to-grammar
 sm
 [Prog #f ([arith : ArithOp])]
 [ArithOrVal #f ()]
 [Val ArithOrVal ([v = (random lower-bound upper-bound)])
      #:prop wont-over-deepen #t]
 [ArithOp ArithOrVal ([lhs : ArithOrVal]
                      [rhs : ArithOrVal])]
 ;; Safe arithmetic operations.
 [SafeArithOp ArithOp ()]
 [SafePlusOp SafeArithOp ()]
 [SafeMinusOp SafeArithOp ()]
 [SafeTimesOp SafeArithOp ()]
 [SafeDivideOp SafeArithOp ()]
 ;; Unsafe arithmetic operations.
 [UnsafePlusOp SafePlusOp ()]
 [UnsafeMinusOp SafeMinusOp ()]
 [UnsafeTimesOp SafeTimesOp ()]
 [UnsafeDivideOp SafeDivideOp ()]
 )

(add-prop
 sm
 may-be-generated
 ;; Abstract nodes.
 [ArithOrVal #f]
 [ArithOp #f]
 [SafeArithOp #f]
 ;; Unsafe operations cannot be directly generated.
 [UnsafePlusOp #f]
 [UnsafeMinusOP #f]
 [UnsafeTimesOp #f]
 [UnsafeDivideOp #f])


(define (render-arith-op op n)
  `(,op
    ,(render-node (ast-child 'lhs n))
    ,(render-node (ast-child 'rhs n))))

(add-prop
 sm
 render-node-info
 [Prog (λ (n) (render-node (ast-child 'arith n)))]
 [Val (λ (n) (number->string (ast-child 'v n)))]
 ;; Safe arithmetic operations.
 [SafePlusOp (λ (n) (render-arith-op "safe-+" n))]
 [SafeMinusOp (λ (n) (render-arith-op "safe--" n))]
 [SafeTimesOp (λ (n) (render-arith-op "safe-*" n))]
 [SafeDivideOp (λ (n) (render-arith-op "safe-/" n))]
 ;; Unsafe arithmetic operations.
 [UnsafePlusOp (λ (n) (render-arith-op "+" n))]
 [UnsafeMinusOp (λ (n) (render-arith-op "-" n))]
 [UnsafeTimesOp (λ (n) (render-arith-op "*" n))]
 [UnsafeDivideOp (λ (n) (render-arith-op "/" n))]
 )

(assemble-spec-components m sm)

(xsmith-command-line
 (λ () (m-generate-ast 'Prog))
 #:fuzzer-name "safe-math-test-fuzzer"
 #:default-max-depth 5)
