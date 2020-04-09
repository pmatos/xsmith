#lang racket/base

(require
 xsmith
 racr
 xsmith/racr-convenience
 "core.rkt"
 racket/string
 racket/list
 racket/pretty
 )

(define-spec-component racket-comp)

(add-basic-expressions racket-comp
                       #:ProgramWithSequence #t
                       #:VoidExpression #t
                       #:AssignmentExpression #t
                       #:IfExpression #t
                       #:ExpressionSequence #t
                       #:LetSequential #t
                       #:LambdaWithExpression #t
                       #:Booleans #t
                       #:Strings #t
                       #:ImmutableList #t
                       #:MutableArray #t
                       #:MutableArrayAssignmentExpression #t
                       #:ImmutableArray #t
                       #:MutableStructuralRecord #t
                       #:MutableStructuralRecordAssignmentExpression #t
                       #:ImmutableStructuralRecord #t
                       )



(add-prop
 racket-comp
 render-hole-info
 [#f (λ (h) '_HOLE_)])


(define (render-child sym n)
  (render-node (ast-child sym n)))
(define (render-children sym n)
  (map render-node (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))

(add-prop
 racket-comp
 render-node-info

 [ProgramWithSequence
  (λ (n)
    `((define (safe-divide numerator divisor)
        (if (equal? 0 divisor)
            0
            (/ numerator divisor)))
      (define (safe-car list fallback)
        (if (null? list)
            fallback
            (car list)))
      (define (safe-cdr list fallback)
        (if (null? list)
            fallback
            (cdr list)))
      (define (immutable-vector-set vec index-raw val)
        (define index (modulo index-raw (vector-length vec)))
        (define new-val ,(render-child 'newvalue n))
        (vector->immutable-vector
         (build-vector (vector-length vec)
                       (λ (i) (if (equal? index)
                                  val
                                  (vector-ref vec i))))))
      ,@(render-children 'definitions n)
      ,(render-child 'ExpressionSequence n)
      (begin
        ,@(for/list ([c (ast-children (ast-child 'definitions n))])
            (if (base-type? (concretize-type (att-value 'xsmith_type c)))
                `(printf "Variable ~a value: ~v\n"
                         ',(string->symbol (ast-child 'name c))
                         ,(string->symbol (ast-child 'name c)))
                '(void))))))]

 [Definition (λ (n)
               `(define ,(string->symbol (ast-child 'name n))
                  ,(render-child 'Expression n)))]
 [AssignmentExpression
  (λ (n) `(set! ,(string->symbol (ast-child 'name n))
                ,(render-child 'newvalue n)))]
 [ExpressionSequence
  (λ (n)
    `(begin
       ,@(render-children 'effectexpressions n)
       ,(render-child 'finalexpression n)))]
 [LetSequential
  (λ (n)
    `(let* (,@(map (λ (dn) `[,(string->symbol (ast-child 'name dn))
                             ,(render-child 'Expression dn)])
                   (ast-children (ast-child 'definitions n))))
       ,(render-child 'body n)))]

 [IfExpression
  (λ (n)
    `(if ,(render-child 'test n)
         ,(render-child 'then n)
         ,(render-child 'else n)))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]

 [ProcedureApplication
  (λ (n)
    `(#%app ,(render-child 'procedure n)
            ,@(render-children 'arguments n)))]
 [FormalParameter (λ (n) (string->symbol (ast-child 'name n)))]
 [LambdaWithExpression
  (λ (n)
    `(lambda (,@(render-children 'parameters n))
       ,(render-child 'body n)))]

 [BoolLiteral (λ (n) (not (not (ast-child 'v n))))]
 [Not (λ (n) `(not ,(render-child 'Expression n)))]
 [And (binary-op-renderer 'and)]
 [Or (binary-op-renderer 'or)]

 [IntLiteral (λ (n) (ast-child 'v n))]
 [Plus (binary-op-renderer '+)]
 [Minus (binary-op-renderer '-)]
 [Times (binary-op-renderer '*)]
 [LessThan (binary-op-renderer '<)]
 [GreaterThan (binary-op-renderer '>)]

 [SafeDivide (binary-op-renderer 'safe-divide)]

 [StringLiteral (λ (n) (ast-child 'v n))]
 [StringAppend (binary-op-renderer 'string-append)]
 [StringLength (λ (n) `(string-length ,(render-child 'Expression n)))]

 [ImmutableListLiteral
  (λ (n) `(list ,@(render-children 'expressions n)))]
 [ImmutableListSafeCar
  (λ (n) `(safe-car ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListSafeCdr
  (λ (n) `(safe-cdr ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListCons
  (λ (n) `(cons ,(render-child 'newvalue n)
                ,(render-child 'list n)))]
 [MutableArrayLiteral
  (λ (n) `(vector ,@(render-children 'expressions n)))]
 [ImmutableArrayLiteral
  (λ (n) `(vector-immutable ,@(render-children 'expressions n)))]
 [MutableArrayReference
  (λ (n)
    (define array-rendered (render-child 'array n))
    `(vector-ref ,array-rendered
                 (modulo ,(render-child 'index n)
                         (vector-length ,array-rendered))))]
 [ImmutableArrayReference
  (λ (n)
    `(let ([vec ,(render-child 'array n)])
       (vector-ref vec
                   (modulo ,(render-child 'index n)
                           (vector-length vec)))))]
 [MutableArrayAssignmentExpression
  (λ (n)
    (define array-rendered (render-child 'array n))
    `(vector-set! ,array-rendered
                  (modulo ,(render-child 'index n)
                          (vector-length ,array-rendered))
                  ,(render-child 'newvalue n)))]
 [ImmutableArraySet
  (λ (n)
    `(immutable-vector-set ,(render-child 'array n)
                           ,(render-child 'index n)
                           ,(render-child 'newvalue n)))]

 [MutableStructuralRecordLiteral
  (λ (n)
    `(make-hash (list ,@(map (λ (name val)
                               `(cons ',name
                                      ,(render-node val)))
                             (ast-child 'fieldnames n)
                             (ast-children (ast-child 'expressions n))))))]
 [ImmutableStructuralRecordLiteral
  (λ (n)
    `(hash ,@(apply append
                    (map (λ (name val)
                           `(',name
                             ,(render-node val)))
                         (ast-child 'fieldnames n)
                         (ast-children (ast-child 'expressions n))))))]
 [MutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [ImmutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [MutableStructuralRecordAssignmentExpression
  (λ (n)
    `(hash-set! ,(render-child 'record n)
                ',(ast-child 'fieldname n)
                ,(render-child 'newvalue n)))]
 [ImmutableStructuralRecordSet
  (λ (n)
    `(hash-set ,(render-child 'record n)
               ',(ast-child 'fieldname n)
               ,(render-child 'newvalue n)))]
 [VoidExpression (λ (n) '(void))]
 )



(assemble-spec-components
 racket
 racket-comp)


(define (racket-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (racket-generate-ast 'ProgramWithSequence)))

(define (racket-format-render s-exps)
  (define out (open-output-string))
  (for ([symex s-exps])
    (pretty-print symex out 1))
  (format "#lang racket/base\n~a"
          (get-output-string out)))

(module+ main
  (xsmith-command-line
   racket-generate
   #:format-render racket-format-render
   #:comment-wrap (λ (lines) (string-join (map (λ (l) (format ";; ~a" l)) lines)
                                          "\n"))
   ))
