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


;;;;;;;;
;; The initial AST is built with guaranteed-safe arithmetic operations. After
;; initial generation is complete, some analysis is done so that some operations
;; can be replaced with their unsafe counterparts.
;;
;; In this specific example, arithmetic done on two odd values is allowed to be
;; made unsafe. (This is an arbitrary rule.) Unsafety is then propagated
;; upwards, requiring that any operation whose operand is unsafe must itself be
;; unsafe.

(add-att-rule
 sm
 is-unsafe-type?
 [#f (λ (n) #f)]
 [UnsafeArithOp (λ (n) #t)])

(define (safe? n)
  (and
   (not (att-value 'is-unsafe-type? n))
   (if (ast-subtype? n 'SafeArithOp)
       (and (safe? (ast-child 'lhs n))
            (safe? (ast-child 'rhs n)))
       #t)))

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
  #:refiner-predicate (λ () (xsmith-feature-enabled? 'unsafe-math))
  #:global-predicate (λ (n) (make-unsafe? n))
  [SafePlusOp [(λ (n) (make-replacement-node 'UnsafePlusOp n))]]
  [SafeMinusOp [(λ (n) (make-replacement-node 'UnsafeMinusOp n))]]
  [SafeTimesOp [(λ (n) (make-replacement-node 'UnsafeTimesOp n))]]
  [SafeDivideOp [(λ (n) (make-replacement-node 'UnsafeDivideOp n))]]
  )

(define-refiner
  sm
  make-vals-even
  #:follows make-math-unsafe
  #:refiner-predicate (λ () (xsmith-feature-enabled? 'even-math))
  [Val [(λ (n) (odd? (ast-child 'v n)))
        (λ (n) (make-replacement-node 'Val n (hash 'v (+ 1 (ast-child 'v n)))))]])

;;;;;;;;

(assemble-spec-components m sm)

(xsmith-command-line
 (λ () (m-generate-ast 'Prog))
 #:fuzzer-name "safe-math-test-fuzzer"
 #:default-max-depth 5
 #:format-render (λ (s) (parameterize ([pretty-print-columns 0])
                          (pretty-format s)))
 #:features '([unsafe-math #t]
              [even-math #f]))
