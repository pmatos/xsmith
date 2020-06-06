#lang racket/base

(provide
 add-basic-expressions
 add-basic-statements
 ;; Type system stuff
 return-type
 no-return-type
 fresh-maybe-return-type
 void-type
 number
 int
 float
 bool
 string
 mutable
 immutable
 array-type
 list-type
 )

(require
 xsmith
 racr
 xsmith/racr-convenience
 racket/dict
 racket/list
 racket/port
 (for-syntax
  racket/base
  syntax/parse
  (for-syntax
   racket/base
   syntax/parse
   )))




;; TODO - all of these magic constants should be parameterizable somehow.  Eg. maybe they should be racket parameters.

;; Long arrays can sometimes create really long generation times, eg. for arrays of arrays of arrays of functions...
(define array-max-length 4)
(define max-effect-expressions 3)
(define fieldname-options
  '(a b c d e f g))
(define (random-field-name)
  (random-ref fieldname-options))
(define (arg-length)
  (random 6))
(define (random-string-literal)
  (random-ref (list "foo" "bar" "baz" "quux")))



(define (immutable-structural-record-type-constraint single?)
  (λ (n)
    (if (att-value 'xsmith_is-hole? n)
        (immutable
         (fresh-structural-record-type (hash)))
        (immutable
         (fresh-structural-record-type
          #:finalized? #t
          (for/hash ([k (if single?
                            (list (ast-child 'fieldname n))
                            (ast-child 'fieldnames n))])
            (values k (fresh-type-variable))))))))
(define mutable-structural-record-assignment-type-rhs
  (λ (n t)
    (define newtype (fresh-type-variable))
    (hash 'record (mutable
                   (fresh-structural-record-type
                    (hash (ast-child 'fieldname n) newtype)))
          'newvalue newtype)))
(define mutable-array-assignment-type-rhs
  (λ (n t)
    (define inner (fresh-type-variable))
    (hash 'array (mutable (array-type inner))
          'index int
          'newvalue inner)))

(define (lambda-fresh-implementation cur-hole make-fresh-node-func)
  (let* ([type (att-value 'xsmith_type cur-hole)]
         [ftype (function-type
                 (product-type #f)
                 (fresh-type-variable))]
         [unification-dumb-return-value (unify! ftype type)]
         [force-exploration-return
          (force-type-exploration-for-node! cur-hole)]
         [parameters
          (map (λ (t)
                 (make-fresh-node-func 'FormalParameter
                                       (hash 'type t)))
               (or (product-type-inner-type-list
                    (function-type-arg-type ftype))
                   (map (λ (x) (fresh-type-variable))
                        (make-list (arg-length) #f))))])
    (unify! (product-type (map (λ (x) (ast-child 'type x))
                               parameters))
            (function-type-arg-type ftype))
    (hash
     'type type
     'parameters parameters)))

(define (make-lambda-type-rhs finalize-return-type-function)
  (λ (n t)
    (define args-type (product-type
                       (map (λ(x)(fresh-type-variable))
                            (ast-children (ast-child 'parameters n)))))
    (define return-type (fresh-type-variable))
    (unify! (function-type args-type return-type)
            t)
    (define args-list (product-type-inner-type-list args-type))
    (hash-set
     (for/hash ([c (ast-children (ast-child 'parameters n))]
                [at args-list])
       (values c at))
     (ast-child 'body n)
     (finalize-return-type-function return-type))))


(begin-for-syntax
  (define-syntax (use? stx)
    (syntax-parse stx
      [(_ name (~optional default:boolean))
       #'(let ([att (attribute name)])
           (if att
               (syntax-parse att
                 [#t #t]
                 [#f #t])
               (~? default #f)))])))

(define-syntax (add-basic-expressions stx)
  (syntax-parse stx
    [(_ component
        (~or
         (~optional (~seq #:ProgramWithSequence use-program-with-sequence:boolean))
         (~optional (~seq #:VoidExpression use-void-expression:boolean))
         (~optional (~seq #:AssignmentExpression use-assignment-expression:boolean))
         (~optional (~seq #:IfExpression use-if-expression:boolean))
         (~optional (~seq #:LambdaWithExpression use-LWE:boolean))
         (~optional (~seq #:LambdaWithBlock use-LWB:boolean))
         (~optional (~seq #:LetSequential use-let-sequential:boolean))
         (~optional (~seq #:ExpressionSequence use-expression-sequence:boolean))
         (~optional (~seq #:Numbers use-numbers:boolean))
         (~optional (~seq #:Booleans use-booleans:boolean))
         (~optional (~seq #:Strings use-strings:boolean))
         (~optional (~seq #:MutableArray use-mutable-array:boolean))
         (~optional (~seq #:MutableArraySafeAssignmentExpression
                          use-mutable-array-safe-assignment-expression:boolean))
         (~optional (~seq #:ImmutableArray use-immutable-array:boolean))
         (~optional (~seq #:ImmutableList use-immutable-list:boolean))
         (~optional (~seq #:MutableStructuralRecord
                          use-mutable-structural-record:boolean))
         (~optional (~seq #:MutableStructuralRecordAssignmentExpression
                          use-mutable-structural-record-assignment-expression:boolean))
         (~optional (~seq #:ImmutableStructuralRecord
                          use-immutable-structural-record:boolean))
         )
        ...
        )
     #`(begin
         ;; Core grammar

         (add-to-grammar
          component
          ;; TODO - where to put Definition and FormalParameter?
          [Definition #f ([type]
                          [name]
                          Expression)
            #:prop binder-info (name type definition)]
          [FormalParameter #f (type [name = (fresh-var-name "arg_")])
                           #:prop binder-info (name type parameter)]


          [Expression #f ()
                      #:prop may-be-generated #f]
          [VariableReference Expression (name)
                             #:prop reference-info (read name)]

          ;; TODO - procedure application should require at least one kind of lambda...
          [ProcedureApplication Expression
                                ([procedure : Expression]
                                 [arguments : Expression * = (arg-length)])]


          )

         (add-prop
          component
          type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [FormalParameter [(fresh-type-variable) no-child-types]]

          ;; TODO - this error message is dumb, because it doesn't say WHICH node is falling back like this.  It should be able to, but I would need to be able to access the current choice object, which is not available here.
          [Expression [(error 'type-info "Trying to type check as an expression without a specialized implementation.  You probably forgot to add a type-info property for a subtype of Expression.")
                       no-child-types]]
          [VariableReference [(fresh-type-variable) no-child-types]]

          [ProcedureApplication
           [(fresh-type-variable)
            (λ (n t)
              (define proc (ast-child 'procedure n))
              (define args (ast-children (ast-child 'arguments n)))
              (define args-type (product-type
                                 (map (λ(x)(fresh-type-variable))
                                      args)))
              (define args-type-list (product-type-inner-type-list args-type))
              (hash-set
               (for/hash ([arg args]
                          [arg-type args-type-list])
                 (values arg arg-type))
               'procedure
               (function-type args-type t)))]]
          )


         ;;; Optional components

         #,@(if (use? use-numbers)
                #'((add-to-grammar
                    component
                    [NumberLiteral Expression (v)
                                   #:prop may-be-generated #f
                                   #:prop choice-weight 1]
                    [IntLiteral NumberLiteral ()
                                ;; TODO - better random integer
                                #:prop fresh (hash 'v (random 200000))]
                    [Plus Expression ([l : Expression] [r : Expression])]
                    [Minus Expression ([l : Expression] [r : Expression])]
                    [Times Expression ([l : Expression] [r : Expression])]
                    [SafeDivide Expression ([l : Expression] [r : Expression])]
                    [LessThan Expression ([l : Expression] [r : Expression])]
                    [GreaterThan Expression ([l : Expression] [r : Expression])])
                   (add-prop
                    component type-info
                    [IntLiteral [int no-child-types]]
                    [Plus [number numeric-bin-op-subtype]]
                    [Minus [number numeric-bin-op-subtype]]
                    [Times [number numeric-bin-op-subtype]]
                    [SafeDivide [number numeric-bin-op-subtype]]
                    [LessThan [bool numeric-bin-op/no-relation-to-return]]
                    [GreaterThan [bool numeric-bin-op/no-relation-to-return]]))
                #'())

         #,@(if (use? use-void-expression)
                #'((add-to-grammar
                    component
                    [VoidExpression Expression ()
                                    #:prop type-info [void-type no-child-types]]))
                #'())

         #,@(if (use? use-program-with-sequence)
                #'((add-to-grammar
                    component
                    [ProgramWithSequence
                     #f ([definitions : Definition *]
                         [ExpressionSequence])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'ExpressionSequence t))]]))
                #'())

         #,@(if (use? use-assignment-expression)
                #'((add-to-grammar
                    component
                    [AssignmentExpression
                     Expression (name [newvalue : Expression])
                     #:prop reference-info (write name #:unifies newvalue)
                     #:prop type-info
                     [void-type
                      (λ (n t) (hash 'newvalue (fresh-type-variable)))]]))
                #'())

         #,@(if (use? use-if-expression)
                #'((add-to-grammar
                    component
                    [IfExpression
                     Expression ([test : Expression]
                                 [then : Expression]
                                 [else : Expression])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'test bool
                                     'then t
                                     'else t))]]))
                #'())

         #,@(if (use? use-booleans)
                #'((add-to-grammar
                    component
                    [BoolLiteral Expression ([v = (even? (random 2))])
                                 #:prop choice-weight 1]
                    [Not Expression ([Expression])]
                    [And Expression ([l : Expression] [r : Expression])]
                    [Or Expression ([l : Expression] [r : Expression])])
                   (add-prop
                    component
                    type-info
                    [BoolLiteral [bool no-child-types]]
                    [Not [bool (λ (n t) (hash 'Expression bool))]]
                    [And [bool (λ (n t) (hash 'l bool 'r bool))]]
                    [Or [bool (λ (n t) (hash 'l bool 'r bool))]]))
                #'())


         #,@(if (use? use-strings)
                #'((add-to-grammar
                    component
                    [StringLiteral
                     Expression
                     ;; TODO - better fresh string literals
                     ([v = (random-string-literal)])
                     #:prop choice-weight 1]
                    [StringAppend Expression ([l : Expression] [r : Expression])]
                    [StringLength Expression (Expression)])
                   (add-prop
                    component
                    type-info
                    [StringLiteral [string (no-child-types)]]
                    [StringAppend [string (λ (n t) (hash 'l string 'r string))]]
                    [StringLength [int (λ (n t) (hash 'Expression string))]]))
                #'())


         #,@(if (use? use-LWE)
                #'((add-to-grammar
                    component
                    [LambdaWithExpression
                     Expression ([parameters : FormalParameter * = (arg-length)]
                                 [body : Expression])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (lambda-fresh-implementation current-hole make-fresh-node)
                     #:prop type-info
                     [(function-type (product-type #f) (fresh-type-variable))
                      (make-lambda-type-rhs (λ (rt) rt))]]))
                #'())
         #,@(if (use? use-LWB)
                #'((add-to-grammar
                    component
                    [LambdaWithBlock
                     Expression ([parameters : FormalParameter * = (arg-length)]
                                 [body : Block])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (lambda-fresh-implementation current-hole make-fresh-node)
                     #:prop type-info
                     [(function-type (product-type #f) (fresh-type-variable))
                      (make-lambda-type-rhs (λ (rt) (return-type rt)))]]))
                #'())

         #,@(if (use? use-let-sequential)
                #'((add-to-grammar
                    component
                    [LetSequential
                     Expression ([definitions : Definition *]
                                 [body : Expression])
                     #:prop strict-child-order? #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'body t))]]))
                #'())

         #,@(if (use? use-expression-sequence)
                #'((add-to-grammar
                    component
                    [ExpressionSequence
                     Expression
                     ([effectexpressions : Expression *
                                         = (add1 (random max-effect-expressions))]
                      [finalexpression : Expression])
                     #:prop strict-child-order? #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        ;; TODO - the effect type could be any, but using void-type will force it to use a side-effectful expression
                        (hash 'effectexpressions (λ (c) void-type)
                              'finalexpression t))]]))
                #'())

         #,@(if (use? use-immutable-array)
                #'((add-to-grammar
                    component
                    [ImmutableArrayLiteral
                     Expression
                     ;; Be sure arrays are never empty.
                     ([expressions : Expression * = (add1 (random array-max-length))])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1]
                    [ImmutableArraySafeReference
                     Expression ([array : Expression]
                                 [index : Expression])]
                    [ImmutableArraySafeSet
                     Expression ([array : Expression]
                                 [index : Expression]
                                 [newvalue : Expression])])
                   (add-prop
                    component
                    type-info
                    [ImmutableArrayLiteral [(immutable (fresh-array-type))
                                            (λ (n t)
                                              (define et (fresh-type-variable))
                                              (define at (immutable (array-type et)))
                                              (subtype-unify! at t)
                                              (hash 'expressions et))]]
                    [ImmutableArraySafeReference
                     [(fresh-type-variable)
                      (λ (n t) (hash 'index int
                                     'array (immutable
                                             (array-type t))))]]
                    [ImmutableArraySafeSet
                     [(immutable (fresh-array-type))
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (unify! (immutable
                                 (array-type inner-t))
                                t)
                        (hash 'index int
                              'array t
                              'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-immutable-list)
                #'((add-to-grammar
                    component
                    [ImmutableListLiteral
                     Expression
                     ([expressions : Expression * = (random array-max-length)])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop type-info
                     [(immutable (list-type (fresh-type-variable)))
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (unify! t (immutable (list-type inner-t)))
                        (hash 'expressions inner-t))]]
                    [ImmutableListSafeCar Expression ([list : Expression]
                                                      [fallback : Expression])
                                          #:prop type-info
                                          [(fresh-type-variable)
                                           (λ (n t)
                                             (hash 'list (immutable (list-type t))
                                                   'fallback t))]]
                    [ImmutableListSafeCdr Expression ([list : Expression]
                                                      [fallback : Expression])
                                          #:prop type-info
                                          [(immutable
                                            (list-type (fresh-type-variable)))
                                           (λ (n t)
                                             (hash 'list t
                                                   'fallback t))]]
                    [ImmutableListCons Expression ([list : Expression]
                                                   [newvalue : Expression])
                                       #:prop type-info
                                       [(immutable (list-type (fresh-type-variable)))
                                        (λ (n t)
                                          (define inner-t (fresh-type-variable))
                                          (unify! (immutable (list-type inner-t)) t)
                                          (hash 'list t
                                                'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-mutable-array)
                #'((add-to-grammar
                    component
                    [MutableArrayLiteral
                     Expression
                     ;; Be sure arrays are never empty.
                     ([expressions : Expression * = (add1 (random array-max-length))])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop type-info
                     [(mutable (fresh-array-type))
                      (λ (n t)
                        (define et (fresh-type-variable))
                        (define at (mutable (array-type et)))
                        (subtype-unify! at t)
                        (hash 'expressions et))]]
                    [MutableArraySafeReference
                     Expression ([array : Expression]
                                 [index : Expression])
                     #:prop mutable-container-access (read 'MutableArray)
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'index int
                                     'array (mutable
                                             (array-type t))))]]))
                #'())
         #,@(if (use? use-mutable-array-safe-assignment-expression)
                #'((add-to-grammar
                    component
                    [MutableArraySafeAssignmentExpression
                     Expression
                     ([array : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableArray)
                     #:prop type-info
                     [void-type mutable-array-assignment-type-rhs]]))
                #'())


         #,@(if (use? use-immutable-structural-record)
                #'((add-to-grammar
                    component
                    [ImmutableStructuralRecordLiteral
                     Expression
                     (fieldnames [expressions : Expression *])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (let* ([t (begin (force-type-exploration-for-node!
                                       (current-hole))
                                      (att-value 'xsmith_type (current-hole)))]
                            [srt (fresh-structural-record-type)]
                            [msrt (immutable srt)]
                            [_side-effect (unify! t msrt)]
                            [fd (structural-record-type-known-field-dict srt)]
                            [necessary-fields (dict-keys fd)]
                            ;; Let's inject extra fields.
                            [new-fields (map (λ(_) (random-field-name))
                                             (make-list (add1 (random 2)) #f))]
                            [all-fields (remove-duplicates
                                         (append necessary-fields new-fields))])
                       (hash 'fieldnames all-fields
                             'expressions (length all-fields)))
                     #:prop type-info
                     [(immutable-structural-record-type-constraint #f)
                      (λ (n t)
                        (define fsrt (fresh-structural-record-type))
                        (define mfsrt (immutable fsrt))
                        (unify! mfsrt t)
                        (define td (structural-record-type-known-field-dict fsrt))
                        (for/hash ([c (ast-children (ast-child 'expressions n))]
                                   [f (ast-child 'fieldnames n)])
                          (values c
                                  (dict-ref td f))))]]
                    [ImmutableStructuralRecordReference
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'record
                                     (immutable
                                      (fresh-structural-record-type
                                       (hash (ast-child 'fieldname n) t)))))]]
                    [ImmutableStructuralRecordSet
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression]
                      [newvalue : Expression])
                     #:prop type-info
                     [(immutable-structural-record-type-constraint #t)
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (unify! (immutable (fresh-structural-record-type
                                            (hash (ast-child 'fieldname n)
                                                  inner-t)))
                                t)
                        (hash 'record t
                              'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-mutable-structural-record)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordLiteral
                     Expression
                     (fieldnames [expressions : Expression *])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (let* ([t (begin (force-type-exploration-for-node!
                                       (current-hole))
                                      (att-value 'xsmith_type (current-hole)))]
                            [srt (fresh-structural-record-type)]
                            [msrt (mutable srt)]
                            [_side-effect (unify! t msrt)]
                            [fd (structural-record-type-known-field-dict srt)]
                            [necessary-fields (dict-keys fd)]
                            ;; Let's inject extra fields.
                            [new-fields (map (λ(_) (random-field-name))
                                             (make-list (add1 (random 2)) #f))]
                            [all-fields (remove-duplicates
                                         (append necessary-fields new-fields))])
                       (hash 'fieldnames all-fields
                             'expressions (length all-fields)))
                     #:prop type-info
                     [(λ (n)
                        (if (att-value 'xsmith_is-hole? n)
                            (mutable
                             (fresh-structural-record-type (hash)))
                            (mutable
                             (fresh-structural-record-type
                              #:finalized? #t
                              (for/hash ([k (ast-child 'fieldnames n)])
                                (values k (fresh-type-variable)))))))
                      (λ (n t)
                        (define fsrt (fresh-structural-record-type))
                        (define mfsrt (mutable fsrt))
                        (unify! mfsrt t)
                        (define td (structural-record-type-known-field-dict fsrt))
                        (for/hash ([c (ast-children (ast-child 'expressions n))]
                                   [f (ast-child 'fieldnames n)])
                          (values c
                                  (dict-ref td f))))]]
                    [MutableStructuralRecordReference
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression])
                     #:prop mutable-container-access (read 'MutableStructuralRecord)
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'record
                                     (mutable
                                      (fresh-structural-record-type
                                       (hash (ast-child 'fieldname n) t)))))]]))
                #'())
         #,@(if (use? use-mutable-structural-record-assignment-expression)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordAssignmentExpression
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : VariableReference]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableStructuralRecord)
                     #:prop type-info
                     [void-type mutable-structural-record-assignment-type-rhs]]))
                #'())


         ;; end begin
         )]))


(define-syntax (add-basic-statements stx)
  (syntax-parse stx
    [(_ component
        (~or
         (~optional (~seq #:ProgramWithBlock use-program-with-block:boolean))
         (~optional (~seq #:AssignmentStatement use-assignment-statement:boolean))
         (~optional (~seq #:ExpressionStatement use-expression-statement:boolean))
         (~optional (~seq #:MutableArraySafeAssignmentStatement
                          use-mutable-array-safe-assignment-statement:boolean))
         (~optional (~seq #:MutableStructuralRecordAssignmentStatement
                          use-mutable-structural-record-assignment-statement:boolean))
         )
        ...
        )
     #`(begin
         (add-to-grammar
          component
          [Statement #f ()
                     #:prop may-be-generated #f]
          [ReturnStatement Statement (Expression)
                           #:prop wont-over-deepen #t]
          [Block Statement ([definitions : Definition *]
                            [statements : Statement * = (add1 (random 5))])
                 #:prop strict-child-order? #t]
          ;; TODO - these languages all have some kind of loop.  What kind of loop should I model here?  Maybe just “loop over an array”?
          [IfElseStatement Statement
                           ([test : Expression] [then : Block] [else : Block])])
         (add-prop
          component
          type-info
          ;;; Statements

          ;; TODO - this error message is dumb, because it doesn't say WHICH node is falling back like this.  It should be able to, but I would need to be able to access the current choice object, which is not available here.
          [Statement [(error 'type-info "Trying to type check as a statement without a specialized implementation.  You probably forgot to add a type-info property for a subtype of Statement.")
                      no-child-types]]
          [ReturnStatement [(return-type (fresh-type-variable))
                            (λ (n t)
                              (define inner (fresh-type-variable))
                              (unify! (return-type inner) t)
                              (hash 'Expression inner))]]
          [Block [(fresh-maybe-return-type)
                  (λ (n t)
                    (define statements (ast-children (ast-child 'statements n)))
                    (define last-statement (car (reverse statements)))
                    (define statement-dict
                      (for/hash ([s (ast-children (ast-child 'statements n))])
                        (values s
                                (if (eq? s last-statement)
                                    t
                                    no-return-type))))
                    (for/fold ([dict statement-dict])
                              ([d (ast-children (ast-child 'definitions n))])
                      (dict-set dict d (fresh-type-variable))))]]
          [IfElseStatement [(fresh-maybe-return-type)
                            (λ (n t) (hash 'test bool
                                           'then t
                                           'else t))]])

         #,@(if (use? use-program-with-block)
                #'((add-to-grammar
                    component
                    [ProgramWithBlock
                     #f ([definitions : Definition *]
                         [Block])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'Block (λ (c) no-return-type)))]]))
                #'())

         #,@(if (use? use-expression-statement)
                #'((add-to-grammar
                    component
                    [ExpressionStatement
                     Statement (Expression)
                     #:prop wont-over-deepen #t
                     #:prop type-info
                     [no-return-type
                      (λ (n t) (hash 'Expression (fresh-type-variable)))]]))
                #'())

         #,@(if (use? use-assignment-statement)
                #'((add-to-grammar
                    component
                    [AssignmentStatement
                     Statement (name Expression)
                     #:prop reference-info (write name #:unifies Expression)
                     #:prop wont-over-deepen #t
                     #:prop type-info
                     [no-return-type
                      (λ (n t) (hash 'Expression (fresh-type-variable)))]]))
                #'())
         #,@(if (use? use-mutable-array-safe-assignment-statement)
                #'((add-to-grammar
                    component
                    [MutableArraySafeAssignmentStatement
                     Statement
                     ([array : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableArray)
                     #:prop type-info
                     [no-return-type
                      (λ (n t)
                        (define inner (fresh-type-variable))
                        (hash 'array (mutable (array-type inner))
                              'index int
                              'newvalue inner))]]))
                #'())
         #,@(if (use? use-mutable-structural-record-assignment-statement)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordAssignmentStatement
                     Statement
                     ([fieldname = (random-field-name)]
                      [record : VariableReference]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableStructuralRecord)
                     #:prop type-info
                     [no-return-type
                      mutable-structural-record-assignment-type-rhs]]))
                #'())


         )]))




;;;;;; Types

;; Statement types
(define-generic-type return-type (type))
(define no-return-type (base-type 'no-return-type))

(define (fresh-maybe-return-type)
  (fresh-type-variable (return-type (fresh-type-variable))
                       no-return-type))

;; Expression types
(type-variable-subtype-default #t)
(define void-type (base-type 'void))
(define number (base-type 'number))
(define int (base-type 'int number))
(define float (base-type 'float number))
(define bool (base-type 'bool))
(define string (base-type 'string))

(define-generic-type mutable ([type covariant]))
(define-generic-type immutable ([type covariant]))

(define-generic-type array-type ([type covariant]))
(define (fresh-array-type) (array-type (fresh-type-variable)))
(define-generic-type list-type ([type covariant]))
(define (fresh-list-type) (list-type (fresh-type-variable)))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))


(define numeric-bin-op/no-relation-to-return
  (λ (n t) (hash 'l number 'r number)))
(define numeric-bin-op-subtype
  (λ (n t)
    (hash 'l t 'r t)))
