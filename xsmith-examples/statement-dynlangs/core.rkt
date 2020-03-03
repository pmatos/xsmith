#lang racket/base

(require
 xsmith
 racr
 xsmith/racr-convenience
 racket/dict
 racket/list
 ;; TODO - replace with xsmith/private/random
 racket/random
 racket/port
 )

(provide
 (all-defined-out)
 )

(define-spec-component statement-dynlangs-core)

(add-to-grammar
 statement-dynlangs-core

 [Program #f ([definitions : Definition *]
              ;; TODO - what do I want the structure of these programs to be?  Just definitions?  Definitions then some statements?
              [Block])]

 [Definition #f ([type]
                 [name]
                 Expression)
   #:prop binder-info (name type definition)]
 [FormalParameter #f (type [name = (fresh-var-name "arg_")])
                  #:prop binder-info (name type parameter)]

 [Statement #f ()
            #:prop may-be-generated #f]
 [Block Statement ([definitions : Definition *]
                   [statements : Statement * = (add1 (random 5))])
        #:prop strict-child-order? #t]
 [ExpressionStatement Statement (Expression)
                      #:prop wont-over-deepen #t]
 [AssignmentStatement Statement (name Expression)
                      #:prop reference-info (write name #:unifies Expression)]
 [IfElseStatement Statement ([test : Expression] [then : Block] [else : Block])]
 ;; TODO - these languages all have some kind of loop.  What kind of loop should I model here?  Maybe just “loop over an array”?

 ;; TODO - return statement


 [Expression #f ()
             #:prop may-be-generated #f]
 [VariableReference Expression (name)
                    #:prop reference-info (read name)]
 [ProcedureApplication Expression
                       ([procedure : Expression]
                        [arguments : Expression * = (arg-length)])]
 [LambdaWithExpression Expression ([parameters : FormalParameter * = (arg-length)]
                                   [body : Expression])
                       #:prop wont-over-deepen #t]


 [LiteralBool Expression ([v = (even? (random 2))])]
 [Not Expression ([Expression])]
 [And Expression ([l : Expression] [r : Expression])]
 [Or Expression ([l : Expression] [r : Expression])]

 [LiteralNumber Expression (v)
                #:prop may-be-generated #f]
 [LiteralInt LiteralNumber ()
             ;; TODO - better random integer
             #:prop fresh (hash 'v (random 200000))]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]
 [Times Expression ([l : Expression] [r : Expression])]
 [SafeDivide Expression ([l : Expression] [r : Expression])]
 [LessThan Expression ([l : Expression] [r : Expression])]
 [GreaterThan Expression ([l : Expression] [r : Expression])]

 [LiteralString Expression ([v = (random-ref (list "foo" "bar" "baz" "quux"))])]
 [StringAppend Expression ([l : Expression] [r : Expression])]
 [StringLength Expression (Expression)]

 ;; Be sure arrays are never empty.
 [LiteralArray Expression ([expressions : Expression * = (add1 (random 10))])
               #:prop wont-over-deepen #t]
 [ArrayReference Expression ([array : VariableReference] [index : Expression])]
 [ArrayAssignmentStatement Statement ([array : VariableReference]
                                      [index : Expression]
                                      [newvalue : Expression])]

 [LiteralStructuralRecord Expression (fieldnames [expressions : Expression *])
                          #:prop wont-over-deepen #t]
 [StructuralRecordReference Expression ([fieldname = (random-field-name)]
                                        [record : VariableReference])]
 [StructuralRecordAssignmentStatement Statement ([fieldname = (random-field-name)]
                                                 [record : VariableReference]
                                                 [newvalue : Expression])]


 ;; TODO - equality

 ;; TODO - array
 ;; TODO - structural record


 ;; TODO - what types do I want to assume in this?
 ;; int, bool, string
 ;; list/array -- what interface do I want to support?  Probably mutable array of fixed length.
 ;; dictionary (maybe both as a generic and as a structural record type)
 ;; higher order functions?  lambdas? (eg. if I include python I have to limit lambdas more than for other languages)

 ;; TODO - what operators do I want to support?
 ;; equality for all base types.  I'm not sure whether the set I want to model will consistently have reasonable notions of equality for compound types...  But I could assume I have a decent notion of equality and write my own in a header of the pretty printer.
 ;; gt, lt, etc for ints or other numbers
 ;; string length, string append, ...?
 ;; array reference, array set, array copy, copying array merge
 ;; structural record get, set, copy, maybe some notion of merging?
 ;; dictionary get, set, copy, maybe some notion of merging?
 ;; maybe an array for-each loop?  Probably not a loop over dictionaries due to ordering issues.

 )


(define fieldname-options
  '(a b c d e f g))
(define (random-field-name)
  (random-ref fieldname-options))
(define (arg-length)
  (random 6))


(add-prop
 statement-dynlangs-core
 fresh
 [LiteralStructuralRecord
  (let* ([t (begin (force-type-exploration-for-node!
                    (current-hole))
                   (att-value 'xsmith_type (current-hole)))]
         [srt (fresh-structural-record-type)]
         [_side-effect (unify! t srt)]
         [fd (structural-record-type-known-field-dict srt)]
         [necessary-fields (dict-keys fd)]
         ;; Let's inject extra fields.
         [new-fields (map (λ(_) (random-field-name))
                          (make-list (add1 (random 2)) #f))]
         [all-fields (remove-duplicates
                      (append necessary-fields new-fields))])
    (hash 'fieldnames all-fields
          'expressions (length all-fields)))]
 [LambdaWithExpression
  (let* ([type (att-value 'xsmith_type current-hole)]
         [ftype (function-type
                 (product-type #f)
                 (fresh-type-variable))]
         [unification-dumb-return-value (unify! ftype type)]
         [force-exploration-return (force-type-exploration-for-node! current-hole)]
         [parameters
          (map (λ (t)
                 #;(xd-printf "making fresh FormalParameter with type: ~v\n" t)
                 (make-fresh-node 'FormalParameter
                                  (hash 'type t)))
               (or (product-type-inner-type-list
                    (function-type-arg-type ftype))
                   (map (λ (x) (fresh-type-variable))
                        (make-list (arg-length) #f))))])
    #;(xd-printf "lambda type: ~v, FormalParameters types: ~v\n"
                 type
                 (map (λ (x) (ast-child 'type x))
                      parameters))
    (unify! (product-type (map (λ (x) (ast-child 'type x))
                               parameters))
            (function-type-arg-type ftype))
    (hash
     'type type
     'parameters parameters))]

 )

;;;;;; Types

;; Statement types
(define-generic-type return-type (type))
(define-generic-type no-return-type (type))

(define (fresh-maybe-return)
  (fresh-type-variable (return-type (fresh-type-variable))
                       (no-return-type (fresh-type-variable))))
(define (fresh-no-return) (no-return-type (fresh-type-variable)))

;; Expression types
(type-variable-subtype-default #t)
(define number (base-type 'number))
(define int (base-type 'int number))
;(define float (base-type 'float number))
(define bool (base-type 'bool))
(define string (base-type 'string))

(define (type-thunks-for-concretization)
  (list #;(λ()float) #;(λ()number) (λ()int) (λ()bool) (λ()string)))

(define-generic-type array-type ([type covariant]))
(define (fresh-array-type) (array-type (fresh-type-variable)))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))


(define numeric-bin-op/no-relation-to-return
  (λ (n t) (hash 'l number 'r number)))
(define numeric-bin-op-subtype
  (λ (n t)
    (hash 'l t 'r t)))


(add-prop
 statement-dynlangs-core
 type-info
 [Program [(fresh-type-variable)
           (λ (n t)
             (hash 'definitions (λ (c) (fresh-type-variable))
                   'Block (λ (c) (fresh-type-variable))))]]


 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [FormalParameter [(fresh-type-variable) no-child-types]]

 ;;; Statements
 [Statement [(error 'typing-statement) no-child-types]]
 [Block [(fresh-maybe-return)
         (λ (n t)
           (define statements (ast-children (ast-child 'statements n)))
           (define last-statement (car (reverse statements)))
           (define statement-dict
             (for/hash ([s (ast-children (ast-child 'statements n))])
               (values s
                       (if (eq? s last-statement)
                           t
                           (fresh-no-return)))))
           (for/fold ([dict statement-dict])
                     ([d (ast-children (ast-child 'definitions n))])
             (dict-set dict d (fresh-type-variable))))]]
 [ExpressionStatement [(fresh-no-return)
                       (λ (n t) (hash 'Expression (fresh-type-variable)))]]
 [AssignmentStatement [(fresh-no-return)
                       (λ (n t) (hash 'Expression (fresh-type-variable)))]]
 [IfElseStatement [(fresh-maybe-return)
                   (λ (n t) (hash 'test bool
                                  'then t
                                  'else t))]]

 ;;; Expressions
 [Expression [(error 'typing-expression) no-child-types]]
 [VariableReference [(fresh-type-variable) (λ (n t) no-child-types)]]

 [LambdaWithExpression
  [(function-type (product-type #f) (fresh-type-variable))
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
      return-type))]]

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

 [LiteralBool [bool no-child-types]]
 [Not [bool (λ (n t) (hash 'Expression bool))]]
 [And [bool (λ (n t) (hash 'l bool 'r bool))]]
 [Or [bool (λ (n t) (hash 'l bool 'r bool))]]

 [LiteralInt [int no-child-types]]
 [Plus [number numeric-bin-op-subtype]]
 [Minus [number numeric-bin-op-subtype]]
 [Times [number numeric-bin-op-subtype]]
 [SafeDivide [number numeric-bin-op-subtype]]
 [LessThan [bool numeric-bin-op/no-relation-to-return]]
 [GreaterThan [bool numeric-bin-op/no-relation-to-return]]


 [LiteralString [string (no-child-types)]]
 [StringAppend [string (λ (n t) (hash 'l string 'r string))]]
 [StringLength [int (λ (n t) (hash 'Expression string))]]

 [LiteralArray [(fresh-array-type)
                (λ (n t)
                  (define et (fresh-type-variable))
                  (define at (array-type et))
                  (subtype-unify! at t)
                  (hash 'expressions et))]]
 [ArrayReference [(fresh-type-variable)
                  (λ (n t) (hash 'index int
                                 'array (array-type t)))]]

 [ArrayAssignmentStatement [(fresh-no-return)
                            (λ (n t)
                              (define inner (fresh-type-variable))
                              (hash 'array (array-type inner)
                                    'index int
                                    'newvalue inner))]]


 [LiteralStructuralRecord
  [(λ (n)
     (if (att-value 'xsmith_is-hole? n)
         (fresh-subtype-of (fresh-structural-record-type (hash)))
         (fresh-structural-record-type
          #:finalized? #t
          (for/hash ([k (ast-child 'fieldnames n)])
            (values k (fresh-type-variable))))))
   (λ (n t)
     (define fsrt (fresh-structural-record-type))
     (unify! fsrt t)
     (define td (structural-record-type-known-field-dict fsrt))
     (for/hash ([c (ast-children (ast-child 'expressions n))]
                [f (ast-child 'fieldnames n)])
       (values c
               (dict-ref td f))))]]

 [StructuralRecordReference
  [(fresh-type-variable)
   (λ (n t) (hash 'record
                  (fresh-structural-record-type
                   (hash (ast-child 'fieldname n) t))))]]
 [StructuralRecordAssignmentStatement
  [(fresh-no-return)
   (λ (n t)
     (define newtype (fresh-type-variable))
     (hash 'record (fresh-structural-record-type
                    (hash (ast-child 'fieldname n) newtype))
           'newvalue newtype))]]

 )



