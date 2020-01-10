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
 [Val #f ([v = (random lower-bound upper-bound)])
      #:prop wont-over-deepen #t]
 [ArithOp #f ([lhs : Val]
              [rhs : Val])]
 ;; Safe arithmetic operations.
 [SafeArithOp ArithOp ()]
 [SafePlusOp SafeArithOp ()]
 [SafeMinusOp SafeArithOp ()]
 [SafeTimesOp SafeArithOp ()]
 [SafeDivideOp SafeArithOp ()]
 ;; Unsafe arithmetic operations.
 [UnsafeArithOp ArithOp ()]
 [UnsafePlusOp UnsafeArithOp ()]
 [UnsafeMinusOp UnsafeArithOp ()]
 [UnsafeTimesOp UnsafeArithOp ()]
 [UnsafeDivideOp UnsafeArithOp ()]
 )

(add-prop
 sm
 may-be-generated
 [ArithOp #f]
 [UnsafeArithOp #f]
 [UnsafePlusOp #f]
 [UnsafeMinusOP #f]
 [UnsafeTimesOp #f]
 [UnsafeDivideOp #f])


(define (render-arith-op op n)
  (format
   "~a ~a ~a"
   (render-node (ast-child 'lhs n))
   op
   (render-node (ast-child 'rhs n))))

(add-prop
 sm
 render-node-info
 [Prog (λ (n) (render-node (ast-child 'arith n)))]
 [Val (λ (n) (number->string (ast-child 'v n)))]
 ;; Safe arithmetic operations.
 [SafePlusOp (λ (n) (render-arith-op "++" n))]
 [SafeMinusOp (λ (n) (render-arith-op "--" n))]
 [SafeTimesOp (λ (n) (render-arith-op "**" n))]
 [SafeDivideOp (λ (n) (render-arith-op "//" n))]
 ;; Unsafe arithmetic operations.
 [UnsafePlusOp (λ (n) (render-arith-op "+" n))]
 [UnsafeMinusOp (λ (n) (render-arith-op "-" n))]
 [UnsafeTimesOp (λ (n) (render-arith-op "*" n))]
 [UnsafeDivideOp (λ (n) (render-arith-op "/" n))]
 )

(assemble-spec-components m sm)

(xsmith-command-line
 (λ () (m-generate-ast 'Prog))
 #:fuzzer-name "safe-math-test-fuzzer")
