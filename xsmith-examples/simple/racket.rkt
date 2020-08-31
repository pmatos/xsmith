#lang clotho

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 racket/string
 racket/list
 racket/pretty
 )

(define-basic-spec-component racket-comp)

(add-basic-expressions racket-comp
                       #:ProgramWithSequence #t
                       #:VoidExpression #t
                       #:AssignmentExpression #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:IfExpression #t
                       #:ExpressionSequence #t
                       #:LetSequential #t
                       #:LambdaWithExpression #t
                       #:Numbers #t
                       #:Booleans #t
                       #:Strings #t
                       #:ImmutableList #t
                       #:MutableArray #t
                       #:MutableArraySafeAssignmentExpression #t
                       #:ImmutableArray #t
                       #:MutableStructuralRecord #t
                       #:MutableStructuralRecordAssignmentExpression #t
                       #:ImmutableStructuralRecord #t
                       )

(define (build-any-collection-type inner)
  ;; We can only have one of each generic type in a type variable at the moment,
  ;; so to get around that we'll put the inner decision inside another type variable.
  (fresh-type-variable (immutable (fresh-type-variable (array-type inner)
                                                       (list-type inner)))
                       (mutable (array-type inner))))

(define (make-for-with-outer-def-printer for-name)
  (λ (n)
    (define cd (ast-child 'collection n))
    (define cd-name (ast-child 'name cd))
    `(let ([,(string->symbol cd-name) ,(render-child 'Expression cd)])
       (,for-name ([,(string->symbol (ast-child 'name (ast-child 'elemname n)))
                    ,(string->symbol cd-name)])
                  ,(render-child 'body n)))))

(add-loop-over-container
 racket-comp
 #:name ForList
 #:collection-type-constructor build-any-collection-type
 #:loop-type-constructor (λ (inner) (immutable (list-type inner)))
 #:bind-whole-collection? #t
 )
(add-property racket-comp
          render-node-info
          [ForList (make-for-with-outer-def-printer 'for/list)])
(add-loop-over-container
 racket-comp
 #:name ForVector
 #:collection-type-constructor build-any-collection-type
 #:loop-type-constructor (λ (inner) (mutable (array-type inner)))
 #:bind-whole-collection? #t
 )
(add-property racket-comp
          render-node-info
          [ForVector (make-for-with-outer-def-printer 'for/vector)])
(add-loop-over-container
 racket-comp
 #:name ForVoid
 #:collection-type-constructor build-any-collection-type
 #:loop-type-constructor (λ (inner) void-type)
 #:body-type-constructor (λ (loop-type elem-type) void-type)
 )
(add-property racket-comp
          render-node-info
          [ForVoid
           (λ (n)
             `(for ([,(string->symbol (ast-child 'name (ast-child 'elemname n)))
                     ,(render-child 'collection n)])
                ,(render-child 'body n)))])


(add-property
 racket-comp
 render-hole-info
 [#f (λ (h) '_HOLE_)])


(define (render-child sym n)
  (att-value 'xsmith_render-node (ast-child sym n)))
(define (render-children sym n)
  (map (λ (cn) (att-value 'xsmith_render-node cn)) (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))

(add-property
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
        (vector->immutable-vector
         (build-vector (vector-length vec)
                       (λ (i) (if (equal? i index)
                                  val
                                  (vector-ref vec i))))))
      ,@(render-children 'definitions n)
      (define program-result ,(render-child 'ExpressionSequence n))
      (begin
        ,(if (base-type? (att-value 'xsmith_type
                                    (ast-child 'ExpressionSequence n)))
             '(printf "Program body result: ~v\n" program-result)
             '(void))
        ,@(for/list ([c (ast-children (ast-child 'definitions n))]
                     #:when (base-type? (concretize-type
                                         (att-value 'xsmith_type c))))
            `(printf "Variable ~a value: ~v\n"
                     ',(string->symbol (ast-child 'name c))
                     ,(string->symbol (ast-child 'name c)))))))]

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
 [MutableArraySafeReference
  (λ (n)
    (define array-rendered (render-child 'array n))
    `(vector-ref ,array-rendered
                 (modulo ,(render-child 'index n)
                         (vector-length ,array-rendered))))]
 [ImmutableArraySafeReference
  (λ (n)
    `(let ([vec ,(render-child 'array n)])
       (vector-ref vec
                   (modulo ,(render-child 'index n)
                           (vector-length vec)))))]
 [MutableArraySafeAssignmentExpression
  (λ (n)
    (define array-rendered (render-child 'array n))
    `(vector-set! ,array-rendered
                  (modulo ,(render-child 'index n)
                          (vector-length ,array-rendered))
                  ,(render-child 'newvalue n)))]
 [ImmutableArraySafeSet
  (λ (n)
    `(immutable-vector-set ,(render-child 'array n)
                           ,(render-child 'index n)
                           ,(render-child 'newvalue n)))]

 [MutableStructuralRecordLiteral
  (λ (n)
    `(make-hash (list ,@(map (λ (name val)
                               `(cons ',name
                                      ,(att-value 'xsmith_render-node val)))
                             (ast-child 'fieldnames n)
                             (ast-children (ast-child 'expressions n))))))]
 [ImmutableStructuralRecordLiteral
  (λ (n)
    `(hash ,@(apply append
                    (map (λ (name val)
                           `(',name
                             ,(att-value 'xsmith_render-node val)))
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




(define (type-thunks-for-concretization)
  (list
   (λ()int-type)
   (λ()bool-type)
   (λ()string-type)
   (λ()(immutable (list-type (fresh-type-variable))))
   (λ()(mutable (array-type (fresh-type-variable))))
   (λ()(immutable (array-type (fresh-type-variable))))
   (λ()(mutable (fresh-structural-record-type)))
   (λ()(immutable (fresh-structural-record-type)))
   ))


(define (racket-format-render s-exps)
  (define out (open-output-string))
  (for ([symex s-exps])
    (pretty-print symex out 1))
  (format "#lang racket/base\n~a"
          (get-output-string out)))

(define-xsmith-interface-functions
  [racket-comp]
  #:fuzzer-name racket
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithSequence
  #:format-render racket-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format ";; ~a" l)) lines)
                                         "\n")))

(module+ main (racket-command-line))
