#lang racket/base

(require
 xsmith
 xsmith/racr-convenience
 racr
 racket/pretty)

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
 [UnsafeMinusOp #f]
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
 [Val (λ (n) (ast-child 'v n))]
 ;; Safe arithmetic operations.
 [SafePlusOp (λ (n) (render-arith-op '+ n))]
 [SafeMinusOp (λ (n) (render-arith-op '- n))]
 [SafeTimesOp (λ (n) (render-arith-op '* n))]
 [SafeDivideOp (λ (n) (render-arith-op '/ n))]
 ;; Unsafe arithmetic operations.
 [UnsafePlusOp (λ (n) (render-arith-op '!+ n))]
 [UnsafeMinusOp (λ (n) (render-arith-op '!- n))]
 [UnsafeTimesOp (λ (n) (render-arith-op '!* n))]
 [UnsafeDivideOp (λ (n) (render-arith-op '!/ n))]
 )

(add-prop
 sm
 render-hole-info
 [#f (λ (h) `(HOLE ,(node-type h)))])


(define safe-arith-types
  '('SafePlusOp
    'SafeMinusOp
    'SafeTimesOp
    'SafeDivideOp))

(define (safe? n)
  (let ([t (node-type n)])
    (or
     (eq? t 'Val)
     (and
      (ormap (λ (at) (eq? t at))
             safe-arith-types)
      (safe? (ast-child 'lhs n))
      (safe? (ast-child 'rhs n))))))

(define (make-unsafe? n)
  (let* ([lhs (ast-child 'lhs n)]
         [rhs (ast-child 'rhs n)]
         [lhs-type (node-type lhs)]
         [rhs-type (node-type rhs)])
    (or
     (and
      (eq? lhs-type 'Val)
      (eq? rhs-type 'Val)
      (odd? (ast-child 'v lhs))
      (odd? (ast-child 'v rhs)))
     (not (safe? lhs))
     (not (safe? rhs)))))


(define-refiner
  sm
  make-math-unsafe
  [#f [(λ (n) #f)]]
  [SafePlusOp [(λ (n) (make-unsafe? n))
               (λ (n) (make-replacement-node 'UnsafePlusOp n))]]
  [SafeMinusOp [(λ (n) (make-unsafe? n))
                (λ (n) (make-replacement-node 'UnsafeMinusOp n))]]
  [SafeTimesOp [(λ (n) (make-unsafe? n))
                (λ (n) (make-replacement-node 'UnsafeTimesOp n))]]
  [SafeDivideOp [(λ (n) (make-unsafe? n))
                 (λ (n) (make-replacement-node 'UnsafeDivideOp n))]]
  )

(assemble-spec-components m sm)

(xsmith-command-line
 (λ () (m-generate-ast 'Prog))
 #:fuzzer-name "safe-math-test-fuzzer"
 #:default-max-depth 5
 #:format-render (λ (s) (parameterize ([pretty-print-columns 0])
                          (pretty-format s))))
