#lang racket/base

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 racket/string
 racket/list
 racket/pretty
 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  ))

;; Likely I'll want to fuzz both with and without exceptions.  Let's make it toggleable.
;; TODO - this should be backed by a parameter or something so I can set it as a command-line option.
(define NE? #t)

(define random-max 4294967087)

(define-spec-component racket-comp)

(add-basic-expressions racket-comp
                       #:ProgramWithSequence #t
                       #:VoidExpression #t
                       #:AssignmentExpression #t
                       #:IfExpression #t
                       #:ExpressionSequence #t
                       #:LetSequential #t
                       #:LambdaWithExpression #t
                       ;#:Numbers #t
                       ;#:Booleans #t
                       ;#:Strings #t
                       #:ImmutableList #t
                       #:MutableArray #t
                       #:MutableArraySafeAssignmentExpression #t
                       #:ImmutableArray #t
                       #:MutableStructuralRecord #t
                       #:MutableStructuralRecordAssignmentExpression #t
                       #:ImmutableStructuralRecord #t
                       )


(add-prop
 racket-comp
 render-hole-info
 [#f (λ (h) '_HOLE_)])


(define-generic-type box-type ([type covariant]))
;; canned-components.rkt provides some of these types, etc, but let's override them so they can be subtypes of bool.  But only override ones that aren't used in the canned components that we use! (eg. number is used.)
;; TODO - make canned-components have a keyword for what number type to use?
(define char (base-type 'char bool))
(define string (base-type 'string bool))
(define symbol (base-type 'symbol bool))
(define keyword (base-type 'keyword bool))

(define (render-child sym n)
  (att-value 'xsmith_render-node (ast-child sym n)))
(define (render-children sym n)
  (map (λ (cn) (att-value 'xsmith_render-node cn)) (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))

(define ((render-variadic variadic-function-name) n)
  `(,variadic-function-name ,@(render-children 'minargs n)
                           ,@(render-children 'moreargs n)))
(define (moreargs-default)
  (random 5))
(define no-child-types (λ (n t) (hash)))

(define-syntax-parser ag [(_ arg ...) #'(add-to-grammar racket-comp arg ...)])
(define-syntax-parser ap [(_ arg ...) #'(add-prop racket-comp arg ...)])


(ag
 [EmptyListLiteral Expression ()
                   #:prop choice-weight 1
                   #:prop type-info [(immutable (list-type (fresh-type-variable)))
                                     no-child-types]
                   #:prop render-node-info (λ (n) 'null)]
 [VariadicExpression Expression ([minargs : Expression *]
                                 [moreargs : Expression * = (random 5)])
                     #:prop may-be-generated #f])


(define-syntax-parser ag/atomic-literal
  [(_ name:id type:expr fresh-expr:expr)
   #'(ag [name Expression ([v = fresh-expr])
               #:prop choice-weight 1
               #:prop type-info [type no-child-types]
               #:prop render-node-info (λ (n) (ast-child 'v n))])])
(ag/atomic-literal IntLiteral int (random-int))
(ag/atomic-literal CharLiteral char (random-char))
(ag/atomic-literal BoolLiteral bool (random-bool))
(ag/atomic-literal StringLiteral string (random-string))
(ag/atomic-literal SymbolLiteral symbol (string->symbol (random-string)))
(ag/atomic-literal KeywordLiteral keyword (string->keyword (random-string)))

(define-syntax-parser ag/variadic
  [(_ name:id symbol:expr min-args:expr
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)])))
      ...)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs min-args)
               #:prop type-info [type (λ (n t) (hash 'minargs t 'moreargs t))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/variadic Times '* 0)
(ag/variadic Plus '+ 0)
(ag/variadic Minus '- 1)
(ag/variadic Divide (if NE? 'NE/ '/) 1)
(ag/variadic BitwiseAnd 'bitwise-and 0 #:type int)
(ag/variadic BitwiseIor 'bitwise-ior 0 #:type int)
(ag/variadic BitwiseXor 'bitwise-xor 0 #:type int)
(ag/variadic Append 'append 0 #:type (immutable (list-type (fresh-type-variable))))

 ;; The numerical comparison operators require at least 1 argument.  I'm not sure why they don't accept 0 args -- eg. as a predicate that an empty list is sorted.
(define-syntax-parser ag/number-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs number 'moreargs number))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/number-compare LessThan '<)
(ag/number-compare LessThanEqual '<=)
(ag/number-compare NumericEqual '=)
(ag/number-compare GreaterThan '>)
(ag/number-compare GreaterThanEqual '>=)
(define-syntax-parser ag/char-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs char 'moreargs char))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/char-compare CharLessThan 'char<?)
(ag/char-compare CharLessThanEqual 'char<=?)
(ag/char-compare CharEqual 'char=?)
(ag/char-compare CharGreaterThan 'char>?)
(ag/char-compare CharGreaterThanEqual 'char>=?)
(ag/char-compare CharCILessThan 'char-ci<?)
(ag/char-compare CharCILessThanEqual 'char-ci<=?)
(ag/char-compare CharCIEqual 'char-ci=?)
(ag/char-compare CharCIGreaterThan 'char-ci>?)
(ag/char-compare CharCIGreaterThanEqual 'char-ci>=?)


(define-syntax-parser ag/single-arg
  [(_ name:id symbol:expr
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'Expression t))])))
      ...)
   #'(ag [name Expression (Expression)
               #:prop type-info [type ctype]
               #:prop render-node-info
               (λ (n) `(,symbol ,(render-child 'Expression n)))])])
(define-syntax-parser Ectype
  [(_ etype:expr)
   #'(λ (n t) (hash 'Expression etype))])
(ag/single-arg Abs 'abs)
(ag/single-arg Acos 'acos)
(ag/single-arg Asin 'asin)
(ag/single-arg AddOne 'add1)
(ag/single-arg SubOne 'sub1)
(ag/single-arg Angle 'angle)
(ag/single-arg Ceiling 'ceiling)
(ag/single-arg CharToInteger 'char->integer #:type int
               #:ctype (λ (n t) (hash 'Expression char)))
(ag/single-arg CharDowncase 'char-downcase #:type char)
(ag/single-arg CharFoldcase 'char-foldcase #:type char)
(ag/single-arg CharTitlecase 'char-titlecase #:type char)
(ag/single-arg CharUpcase 'char-upcase #:type char)
(ag/single-arg CharUTFELength 'char-utf-8-length #:type int #:ctype (Ectype char))
(ag/single-arg CharGeneralCategory 'char-general-category #:type symbol
               #:ctype (Ectype char))
(define-syntax-parser ag/char-pred
  [(_ name:id sym:expr)
   #'(ag/single-arg name sym #:type bool #:ctype (Ectype char))])
(ag/char-pred CharAlphabeticP 'char-alphabetic?)
(ag/char-pred CharBlankP 'char-blank?)
(ag/char-pred CharGraphicP 'char-graphic?)
(ag/char-pred CharIsoControlP 'char-iso-control?)
(ag/char-pred CharLowerCaseP 'char-lower-case?)
(ag/char-pred CharNumericP 'char-numeric?)
(ag/char-pred CharPunctuationP 'char-punctuation?)
(ag/char-pred CharSymbolicP 'char-symbolic?)
(ag/char-pred CharTitleCaseP 'char-title-case?)
(ag/char-pred CharUpperCaseP 'char-upper-case?)
(ag/char-pred CharWhitespaceP 'char-whitespace?)

(ag/single-arg Truncate 'truncate)
(ag/single-arg Not 'not #:type bool)
(ag/single-arg AtanOne (if NE? 'NE/atan 'atan))
(ag/single-arg BitwiseNot 'bitwise-not #:type int)
(ag/single-arg ZeroP 'zero? #:type bool #:ctype (Ectype number))
(ag/single-arg NullP 'null? #:type bool
               #:ctype (Ectype (immutable (list-type (fresh-type-variable)))))

(define-syntax-parser ag/type-predicate
  [(_ name:id symbol:expr)
   #'(ag/single-arg name symbol
                    #:type bool
                    #:ctype (Ectype (fresh-type-variable)))])
(ag/type-predicate BooleanP 'boolean?)
(ag/type-predicate BoxP 'box?)
(ag/type-predicate ByteP 'byte?)
(ag/type-predicate BytesP 'string?)
(ag/type-predicate CharP 'char?)
(ag/type-predicate ComplexP 'complex?)
(ag/type-predicate ExnP 'exn?)
(ag/type-predicate HashP 'hash?)
(ag/type-predicate ImmutableP 'immutable?)
(ag/type-predicate KeywordP 'keyword?)
(ag/type-predicate ListP 'list?)
(ag/type-predicate ListPairP 'list-pair?)
(ag/type-predicate MPairP 'mpair?)
(ag/type-predicate NumberP 'number?)
(ag/type-predicate PairP 'pair?)
(ag/type-predicate ParameterP 'parameter?)
(ag/type-predicate ParameterizationP 'parameterization?)
(ag/type-predicate PathP 'path?)
(ag/type-predicate PregexpP 'pregexp?)
(ag/type-predicate ProcedureP 'procedure?)
(ag/type-predicate RationalP 'rational?)
(ag/type-predicate RealP 'real?)
(ag/type-predicate RegexpP 'regexp?)
(ag/type-predicate StringP 'string?)
(ag/type-predicate StructTypeP 'struct-type?)
(ag/type-predicate StructTypePropertyP 'struct-type-property?)
(ag/type-predicate StructP 'struct?)
(ag/type-predicate SymbolP 'symbol?)
(ag/type-predicate SyntaxP 'syntax?)
(ag/type-predicate TrueObjectP 'true-object?)
(ag/type-predicate VectorP 'Vector?)
(ag/type-predicate VoidP 'void?)

(ag/single-arg MutableBoxLiteral 'box
               #:type (mutable (box-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! (mutable (box-type inner-type)) t)
                         (hash 'Expression inner-type)))
(ap wont-over-deepen [MutableBoxLiteral #t])
(ag/single-arg ImmutableBoxLiteral 'box
               #:type (immutable (box-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! (immutable (box-type inner-type)) t)
                         (hash 'Expression inner-type)))
(ap wont-over-deepen [ImmutableBoxLiteral #t])
(ag/single-arg Unbox 'unbox
               #:type (fresh-type-variable)
               #:ctype (λ (n t) (hash 'Expression (fresh-type-variable
                                                   (immutable (box-type t))
                                                   (mutable (box-type t))))))
(ap mutable-container-access [Unbox (read 'box)])
(ag [SetBox Expression ([box : Expression] [newval : Expression])
            #:prop mutable-container-access (write 'box)
            #:prop type-info [void-type
                              (λ (n t)
                                (define inner-type (fresh-type-variable))
                                (hash 'box (mutable (box-type inner-type))
                                      'newval inner-type))]
            #:prop render-node-info (λ (n) `(set-box! ,(render-child 'box n)
                                                      ,(render-child 'newval n)))])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rendering for basic expressions (IE add-basic-expression)
;;;;; Mostly this is from the simple/racket fuzzer.

(add-prop
 racket-comp
 render-node-info

 [ProgramWithSequence
  (λ (n)
    `((define (NE/ arg1 . args)
        (if (null? args)
            (if (eq? arg1 0)
                0
                (/ arg1))
            (apply / arg1 (map (λ (x) (if (eq? 0 x) 1 x)) args))))
      ;; TODO - add safe/NoException wrappers
      ;; TODO - NE/atan
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
        ,(if #;(base-type? (att-value 'xsmith_type
                                      (ast-child 'ExpressionSequence n)))
             #t
             '(printf "Program body result: ~v\n" program-result)
             '(void))
        ,@(for/list ([c (ast-children (ast-child 'definitions n))]
                     #:when #t #;(base-type? (concretize-type
                                              (att-value 'xsmith_type c)))
                     )
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

 ;[BoolLiteral (λ (n) (not (not (ast-child 'v n))))]
 ;[Not (λ (n) `(not ,(render-child 'Expression n)))]
 ;[And (binary-op-renderer 'and)]
 ;[Or (binary-op-renderer 'or)]

 ;[IntLiteral (λ (n) (ast-child 'v n))]
 ;[Plus (binary-op-renderer '+)]
 ;[Minus (binary-op-renderer '-)]
 ;[Times (binary-op-renderer '*)]
 ;[LessThan (binary-op-renderer '<)]
 ;[GreaterThan (binary-op-renderer '>)]
 ;[SafeDivide (binary-op-renderer 'safe-divide)]

 ;[StringLiteral (λ (n) (ast-child 'v n))]
 ;[StringAppend (binary-op-renderer 'string-append)]
 ;[StringLength (λ (n) `(string-length ,(render-child 'Expression n)))]

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



(assemble-spec-components
 racket
 racket-comp)

(define (type-thunks-for-concretization)
  (list #;(λ()float) #;(λ()number) (λ()int) #;(λ()bool) #;(λ()string)))

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
