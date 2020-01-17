#lang racket/base

(require
 xsmith
 xsmith/racr-convenience
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
 [UnsafeArithOp ArithOp ()]
 [UnsafePlusOp UnsafeArithOp ()]
 [UnsafeMinusOp UnsafeArithOp ()]
 [UnsafeTimesOp UnsafeArithOp ()]
 [UnsafeDivideOp UnsafeArithOp ()]
 )

(add-prop
 sm
 may-be-generated
 ;; Abstract nodes.
 [ArithOrVal #f]
 [ArithOp #f]
 [SafeArithOp #f]
 [UnsafeArithOp #f]
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
 [SafePlusOp (λ (n) (render-arith-op "+" n))]
 [SafeMinusOp (λ (n) (render-arith-op "-" n))]
 [SafeTimesOp (λ (n) (render-arith-op "*" n))]
 [SafeDivideOp (λ (n) (render-arith-op "/" n))]
 ;; Unsafe arithmetic operations.
 [UnsafePlusOp (λ (n) (render-arith-op "UNSAFE-+" n))]
 [UnsafeMinusOp (λ (n) (render-arith-op "UNSAFE--" n))]
 [UnsafeTimesOp (λ (n) (render-arith-op "UNSAFE-*" n))]
 [UnsafeDivideOp (λ (n) (render-arith-op "UNSAFE-/" n))]
 )


(define (subtree-is-safe? n)
  #;(eprintf (format "subtree-is-safe? ~a\n" n))
  (or
   (eq? (node-type n) 'Val)
   (and
    (ormap (λ (t) (eq? (node-type n) t))
           '('SafePlusOp
             'SafeMinusOp
             'SafeTimesOp
             'SafeDivideOp))
    (subtree-is-safe? (ast-child 'lhs n))
    (subtree-is-safe? (ast-child 'rhs n)))))


(define-refiner
  sm
  make-math-unsafe
  [#f [(λ (n) #f)]]
  [SafePlusOp [(λ (n) (subtree-is-safe? (ast-child 'lhs n)))
               (λ (n) (subtree-is-safe? (ast-child 'rhs n)))
               (λ (n) (make-replacement-node 'UnsafePlusOp n (hash 'lhs (make-fresh-node 'Val (hash 'v 42)))))]]
  )

(assemble-spec-components m sm)

(xsmith-command-line
 (λ () (m-generate-ast 'Prog))
 #:fuzzer-name "safe-math-test-fuzzer"
 #:default-max-depth 5)
