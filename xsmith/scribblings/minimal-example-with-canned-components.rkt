#lang clotho

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 racket/string
 racket/list
 racket/pretty)

;; We first define a basic component and add a bunch of expressions.

(define-basic-spec-component somelisp-comp)

(add-basic-expressions somelisp-comp
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
                       #:ImmutableList #t)

(add-loop-over-container
 somelisp-comp
 #:name Map
 #:collection-type-constructor (λ (inner) (immutable (list-type inner)))
 #:loop-type-constructor (λ (inner) (immutable (list-type inner))))


;; Now we basically just add the render property unless we want to manually
;; define more.


(define (render-child sym n)
  (att-value 'xsmith_render-node (ast-child sym n)))
(define (render-children sym n)
  (map (λ (cn) (att-value 'xsmith_render-node cn))
       (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))


(add-property
 somelisp-comp
 render-node-info

 [ProgramWithSequence
  (λ (n)
    ;; Unless our language has a well-defined exception interface that
    ;; we are trying to fuzz, we need to provide “safe” versions of
    ;; functions that aren't total.
    ;; Canned components uses safe functions and requires us to provide
    ;; definitions.
    ;; Here we provide some before the generated program.
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
      ,@(render-children 'definitions n)
      (define program-result ,(render-child 'ExpressionSequence n))

      ;; Let's add code to print the values of generated global variables.
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
    `(,(render-child 'procedure n)
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
 [VoidExpression (λ (n) '(void))]
 [Map (λ (n) `(map (λ (,(ast-child 'name (ast-child 'elemname n)))
                     ,(render-child 'body n))
                   ,(render-child 'collection n)))])



(assemble-spec-components
 somelisp
 somelisp-comp)

;; Sometimes we have a type variable that is not concrete but needs to be.
;; Here we provide a list of options we can pick for an unconstrained
;; type variable.
(define (type-thunks-for-concretization)
  (list
   (λ()int-type)
   (λ()bool-type)
   (λ()string-type)
   (λ()(immutable (list-type (fresh-type-variable))))))

(define (somelisp-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (somelisp-generate-ast 'ProgramWithSequence)))

(define (somelisp-format-render s-exps)
  (define out (open-output-string))
  (for ([symex s-exps])
    (pretty-print symex out 1))
  (get-output-string out))

(module+ main
  (xsmith-command-line
   somelisp-generate
   #:format-render somelisp-format-render
   #:comment-wrap (λ (lines)
                    (string-join
                     (map (λ (l) (format ";; ~a" l)) lines)
                     "\n"))))
