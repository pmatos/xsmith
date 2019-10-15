#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2019 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide cish-grammar)

(require
 "cish-utils.rkt"
 xsmith
 (only-in pprint [empty empty-doc])
 racr
 racket/random
 racket/class
 racket/dict

 (for-syntax
  racket/base
  syntax/parse
  ))


(define-spec-component cish-grammar)

(add-to-grammar
 cish-grammar
 [Node #f ([precomment = empty-doc]
           [postcomment = empty-doc])]
 [Program Node ([structdefinitions : StructDefinition * =
                                   (if (xsmith-feature-enabled? 'structs)
                                       (random 3)
                                       0)]
                [globalvariables : VariableDeclaration * = (random 5)]
                [functions : FunctionDefinition * = (random 3)]
                [main : FunctionDefinition])]

 [Declaration Node ([type]
                    [name])]
 [VariableDeclaration Declaration (Expression)]
 [StructDefinition Declaration ()]
 [FunctionDefinition Declaration ([params : FormalParam *]
                                  Block)]
 [FormalParam Node ([type]
                    [name = (fresh-var-name "arg_")])]


 [Statement Node ()]
 [NullStatement Statement ()]
 [Block Statement ([declarations : VariableDeclaration * = (random 2)]
                   [statements : Statement * = (add1 (random 4))])]
 [ExpressionStatement Statement (Expression)]
 [IfStatement Statement ([test : Expression]
                         [then : Statement])]
 [IfElseStatement IfStatement ([else : Statement])]
 [ReturnStatement Statement ()]
 ;[VoidReturnStatement ReturnStatement ()]
 [ValueReturnStatement ReturnStatement (Expression)]

 [LoopStatement Statement ([test : Expression]
                           [body : Statement])]
 [WhileStatement LoopStatement ()]
 [DoWhileStatement LoopStatement ()]
 [ForStatement LoopStatement ([init : VariableDeclaration]
                              [update : Expression])]

 [Expression Node ()]

 [AssignmentExpression Expression ([name]
                                   Expression)]
 [FunctionApplicationExpression Expression ([function : VariableReference]
                                            [args : Expression *])]
 [BinaryExpression Expression ([l : Expression]
                               [r : Expression])]
 [AdditionExpression BinaryExpression ()]
 [UnsafeAdditionExpression AdditionExpression ()]
 [SubtractionExpression BinaryExpression ()]
 [UnsafeSubtractionExpression SubtractionExpression ()]
 [MultiplicationExpression BinaryExpression ()]
 [UnsafeMultiplicationExpression MultiplicationExpression ()]
 [DivisionExpression BinaryExpression ()]
 [UnsafeDivisionExpression DivisionExpression ()]

 [IntOnlyBinaryExpression BinaryExpression ()]
 [ModulusExpression IntOnlyBinaryExpression ()]
 [UnsafeModulusExpression ModulusExpression ()]

 [ComparisonExpression BinaryExpression ()]
 [EqualityExpression ComparisonExpression ()]
 [GreaterThanExpression ComparisonExpression ()]
 [LessThanExpression ComparisonExpression ()]
 [LessOrEqualExpression ComparisonExpression ()]
 [GreaterOrEqualExpression ComparisonExpression ()]

 [IfExpression Expression ([test : Expression]
                           [then : Expression]
                           [else : Expression])]
 [LiteralInt Expression (val)]
 [LiteralFloat Expression (val)]
 [VariableReference Expression (name)]
 [VolatileVariableReference Expression (VariableReference)]
 ;; to massage types.
 [VolatileInitializer Expression (Expression)]
 [LiteralStruct Expression ([structdefref : VariableReference]
                            [vals : Expression *])]
 [StructReference Expression (fieldname
                              [structdefref : VariableReference]
                              [structval : Expression])]
 [StructSetField Expression (fieldname
                             [structdefref : VariableReference]
                             [structval : VariableReference]
                             [updateval : Expression])]

 )

(add-prop cish-grammar
          may-be-generated
          ;; abstract nodes
          [Declaration #f]
          [Statement #f]
          [ReturnStatement #f]
          [LoopStatement #f]
          [Expression #f]
          [BinaryExpression #f]
          [IntOnlyBinaryExpression #f]
          [ComparisonExpression #f]

          ;; Unsafe nodes should be generated by analysis transformations,
          ;; not random choice.
          [UnsafeAdditionExpression #f]
          [UnsafeSubtractionExpression #f]
          [UnsafeMultiplicationExpression #f]
          [UnsafeDivisionExpression #f]
          [UnsafeModulusExpression #f]
          )

(add-prop cish-grammar
          depth-increase
          [Block (λ (n) (if (member (node-type (parent-node n))
                                    '(IfStatement
                                      IfElseStatement
                                      FunctionDefinition
                                      ForStatement
                                      WhileStatement
                                      DoWhileStatement))
                            (att-value 'xsmith_ast-depth (parent-node n))
                            (add1 (att-value 'xsmith_ast-depth (parent-node n)))))]
          ;; some nodes should never increase depth
          [ExpressionStatement (λ(n)0)]
          [AssignmentExpression (λ(n)0)]
          [VolatileVariableReference (λ(n)0)]
          [VolatileInitializer (λ(n)0)]
          [Declaration (λ(n)0)])

(add-prop cish-grammar
          wont-over-deepen
          [LiteralStruct #t]
          [VolatileVariableReference #t]
          [VolatileInitializer #t]
          )


(add-prop cish-grammar
          fresh
          [IfElseStatement (hash
                            'then
                            (make-hole 'Block)
                            'else
                            (make-hole 'Block))]
          [FunctionApplicationExpression
           (hash 'function (make-hole 'VariableReference)
                 ;; Make an empty args list which will be rewritten
                 ;; by fresh for VariableReference.
                 'args 0)]
          ;[StructReference (hash)]
          ;[StructSetField (hash)]
          ;[LiteralStruct (hash)]
          [VariableReference
           (hash 'name
                 (λ ()
                   (define (struct-def-ref? x)
                     (define parent (parent-node x))
                     (and (or (ast-subtype? parent 'StructReference)
                              (ast-subtype? parent 'StructSetField)
                              (ast-subtype? parent 'LiteralStruct))
                          (eq? (current-hole) (ast-child 'structdefref parent))))
                   (let* ([choice (send this xsmith_get-reference!)]
                          [parent (parent-node (current-hole))])
                     (when (and (ast-subtype? parent 'FunctionApplicationExpression)
                                (eq? (current-hole) (ast-child 'function parent)))
                       ;; Rewrite the function argument list to match the
                       ;; length required by the type we've chosen.
                       (let ([arg-children (create-ast-list
                                            (map (λ (x) (make-hole 'Expression))
                                                 (product-type-inner-type-list
                                                  (function-type-arg-type
                                                   (binding-type choice)))))])
                         (enqueue-inter-choice-transform
                          (λ ()
                            (rewrite-subtree
                             (ast-child 'args parent)
                             arg-children)))))
                     (when (and (struct-def-ref? (current-hole))
                                (not (ast-subtype? parent 'LiteralStruct)))
                       (enqueue-inter-choice-transform
                        (λ ()
                          (define inner-names
                            (nominal-record-type-inners
                             (nominal-record-definition-type-type
                              (binding-type choice))))
                          (define parent-type (att-value 'xsmith_type parent))
                          (define choices
                            (filter (λ (k) (can-unify? parent-type
                                                       (dict-ref inner-names k)))
                                    (dict-keys inner-names)))
                          (when (null? choices)
                            (error
                             'cish-struct-ref
                             "No fields in struct with appropriate type.  This should not have happened. Type needed: ~v, types available: ~v"
                             parent-type
                             (dict-values inner-names)))
                          (define fieldname-choice (random-ref choices))
                          (rewrite-terminal 'fieldname parent fieldname-choice))))
                     (when (and (struct-def-ref? (current-hole))
                                (ast-subtype? parent 'LiteralStruct))
                       (enqueue-inter-choice-transform
                        (λ ()
                          (define vals-children
                            (create-ast-list
                             (map (λ (x) (make-hole 'Expression))
                                  (dict-keys (nominal-record-type-inners
                                              (nominal-record-definition-type-type
                                               (binding-type choice)))))))
                          (rewrite-subtree
                           (ast-child 'vals parent)
                           vals-children))))
                     (binding-name choice))))]
          [VariableDeclaration
           (hash 'name (if (equal? (top-ancestor-node (current-hole))
                                   (parent-node (current-hole)))
                           (fresh-var-name "global_")
                           (fresh-var-name "local_"))
                 'type (fresh-concrete-var-type))]
          [StructDefinition
           (let* ([type (nominal-record-definition-type
                         (concretize-type (any-nominal-record-type)))]
                  [name (nominal-record-type-name
                         (nominal-record-definition-type-type type))])
             (hash 'name name
                   'type type))]
          [FunctionDefinition
           (λ (lift-fields)
             (let* ([parent (parent-node (current-hole))]
                    [main? (and (eq? (node-type parent) 'Program)
                                (eq? (ast-child 'main parent) current-hole))]
                    [name (or (dict-ref lift-fields 'name #f)
                              (if main?
                                  "main_inner"
                                  (fresh-var-name "func_")))]
                    [type (or (dict-ref lift-fields 'type #f)
                              (if main?
                                  (function-type (product-type '()) int)
                                  (fresh-concrete-function-type)))])
               (hash 'name name
                     'type type
                     'params (map (λ (t) (make-fresh-node 'FormalParam
                                                          (hash 'type t)))
                                  (product-type-inner-type-list
                                   (function-type-arg-type
                                    type))))))]
          [LiteralInt (hash 'val (* (random 100)
                                    (if (equal? 0 (random 2))
                                        1
                                        -1)))]
          [LiteralFloat (hash 'val (* (random)
                                      (random 10)
                                      (if (equal? 0 (random 2))
                                          1
                                          -1)))]
          )

(add-prop cish-grammar
          binder-info
          [Declaration (name type definition)]
          [VariableDeclaration (name type definition)]
          [FunctionDefinition (name type definition)]
          [StructDefinition (name type definition)]
          [FormalParam (name type parameter)])
(add-prop cish-grammar
          reference-info
          [VariableReference (read name)]
          [AssignmentExpression (write name)]
          )
(add-prop cish-grammar
          io
          [StructSetField #t]
          [VolatileVariableReference #t]
          )
(add-prop cish-grammar
          strict-child-order?
          [Program #t]
          [Block #t]
          [IfStatement #t]
          [IfExpression #t]
          [LoopStatement #t])
(add-prop cish-grammar
          lift-predicate
          [FunctionDefinition #f]
          ;[FunctionDefinition (λ (n type) #f)]
          [Program (λ (n type) #t)]
          ;; TODO - not function types AND not struct types
          [Block (λ (n type) (and (not (function-type? type))
                                  (not (nominal-record-definition-type? type))))])
(add-prop cish-grammar
          lift-type->ast-binder-type
          [#f (λ (type) (cond [(function-type? type)
                               'FunctionDefinition]
                              [(nominal-record-definition-type? type)
                               'StructDefinition]
                              [else 'VariableDeclaration]))])


#|
Type definitions are in cish-utils.rkt
|#

;; TODO - this property is wonky because it needs more info than I originally anticipated due to shortsightedness in wanting to design something easy to write.  It needs to take the node as well as its type, or maybe have an option to take its node.
(add-prop
 cish-grammar
 type-info
 [#f [(error 'typing-base-node) (no-child-types)]]
 ;[Node [(error 'typing-node) (no-child-types)]]
 [Program [(fresh-type-variable)
           (λ (n t) (hash 'main (function-type (product-type '())
                                               int)
                          'structdefinitions (λ (c) (nominal-record-definition-type
                                                     (fresh-type-variable)))
                          'globalvariables (λ (c) (fresh-type-variable))
                          'functions (λ (c) (function-type
                                             (product-type #f)
                                             (fresh-type-variable)))))]]

 ;[Declaration [(error 'typing-declaration) (no-child-types)]]
 [VariableDeclaration [(fresh-type-variable)
                       (λ (n t)
                         (let ([declaration-type-annotation (ast-child 'type n)])
                           (unify! t declaration-type-annotation)
                           (hash 'Expression
                                 declaration-type-annotation)))]]
 [StructDefinition [(nominal-record-definition-type (fresh-type-variable))
                    (λ (n t)
                      (unify! t (ast-child 'type n))
                      (hash))]]
 [FunctionDefinition [(function-type (product-type #f) (fresh-type-variable))
                      (λ (n t)
                        (let ([definition-type-annotation (ast-child 'type n)]
                              [f-type (function-type
                                       (product-type #f)
                                       (fresh-type-variable))])
                          (unify! t definition-type-annotation)
                          (unify! t f-type)
                          (define arg-types (product-type-inner-type-list
                                             (function-type-arg-type f-type)))
                          (hash-set
                           (for/hash ([arg (ast-children (ast-child 'params n))]
                                      [arg-type arg-types])
                             (values arg arg-type))
                           'Block (return-type (function-type-return-type f-type)))))]]
 [FormalParam [(fresh-type-variable) (no-child-types)]]

 [Statement [(error 'typing-statement) (no-child-types)]]
 ;[Statement [(fresh-type-variable (fresh-no-return) (return-type (fresh-type-variable))) (no-child-types)]]

 [NullStatement [(fresh-no-return) (no-child-types)]]
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
                     ([d (ast-children (ast-child 'declarations n))])
             (dict-set dict d (fresh-type-variable))))]]
 [ExpressionStatement [(fresh-no-return)
                       (λ (n t) (hash 'Expression (fresh-type-variable)))]]
 [IfStatement [(fresh-no-return)
               (λ (n t) (hash 'test bool
                              'then t))]]
 [IfElseStatement [(fresh-maybe-return)
                   (λ (n t) (hash 'test bool
                                  'then t
                                  'else t))]]
 [ReturnStatement [(error 'typing-non-value-return-statement) (no-child-types)]]
 [ValueReturnStatement [(return-type (fresh-type-variable))
                        (λ (n t)
                          (define rt (return-type (fresh-type-variable)))
                          (unify! t rt)
                          (hash 'Expression (return-type-type rt)))]]


 [LoopStatement [(fresh-maybe-return)
                 (λ (n t) (hash 'test bool
                                'body t))]]
 ;; WhileStatement
 ;; DoWhileStatement
 [ForStatement [(fresh-maybe-return)
                (λ (n t)
                  (let ([loop-var-type (fresh-type-variable)])
                    (hash 'test bool
                          'body t
                          'init loop-var-type
                          'update loop-var-type)))]]

 ;[Expression [(fresh-type-variable) (no-child-types)]]
 [Expression [(error 'typing-expression) (no-child-types)]]

 [AssignmentExpression [(fresh-type-variable)
                        (λ (n t) (hash 'Expression t))]]
 [FunctionApplicationExpression
  [(fresh-type-variable)
   (λ (n t)
     (define args-type (product-type #f))
     (define arg-nodes (ast-children (ast-child 'args n)))
     (define func-node (ast-child 'function n))
     (define arg-types (map (λ (c) (fresh-type-variable)) arg-nodes))
     (when (not (att-value 'xsmith_is-hole? func-node))
       (unify! args-type (product-type arg-types)))
     (for/fold ([dict (hash 'function (function-type args-type t))])
               ([a arg-nodes]
                [t arg-types])
       (dict-set dict a t)))]]
 [AdditionExpression [(fresh-type-variable int float)
                      (λ (n t) (hash 'l t 'r t))]]
 [SubtractionExpression [(fresh-type-variable int float)
                         (λ (n t) (hash 'l t 'r t))]]
 [MultiplicationExpression [(fresh-type-variable int float)
                            (λ (n t) (hash 'l t 'r t))]]
 [DivisionExpression [(fresh-type-variable int float)
                      (λ (n t) (hash 'l t 'r t))]]

 [IntOnlyBinaryExpression [int
                           (λ (n t) (hash 'l int 'r int))]]

 [ComparisonExpression [(fresh-type-variable bool)
                        (λ (n t) (let ([arg-t (fresh-type-variable int float)])
                                   (hash 'l arg-t 'r arg-t)))]]

 [IfExpression [(fresh-type-variable)
                (λ (n t) (hash 'test bool 'then t 'else t))]]
 [LiteralInt [(fresh-type-variable int bool) (no-child-types)]]
 [LiteralFloat [float (no-child-types)]]
 [VariableReference [(fresh-type-variable) (no-child-types)]]
 [VolatileVariableReference [(fresh-type-variable int bool float)
                             (λ (n t) (hash 'VariableReference (volatile-type t)))]]
 [VolatileInitializer [(volatile-type (fresh-type-variable))
                       (λ (n t)
                         (define vt (volatile-type (fresh-type-variable)))
                         (unify! vt t)
                         (hash 'Expression (volatile-type-type vt)))]]
 [StructReference [(fresh-type-variable)
                   (λ (n t)
                     (define type-with-field
                       (nominal-record-type-with (ast-child 'fieldname n) t))
                     (hash 'structval type-with-field
                           'structdefref (nominal-record-definition-type
                                          type-with-field)))]]
 [StructSetField [(fresh-type-variable)
                  (λ (n t)
                    (define type-with-field
                      (nominal-record-type-with (ast-child 'fieldname n) t))
                    (hash 'structval type-with-field
                          'updateval t
                          'structdefref (nominal-record-definition-type
                                         type-with-field)))]]
 [LiteralStruct
  [(any-nominal-record-type)
   (λ (n t)
     (if (att-value 'xsmith_is-hole? (ast-child 'structdefref n))
         (hash 'structdefref (nominal-record-definition-type t))
         (let* ([vals (ast-children (ast-child 'vals n))]
                [struct-ref (ast-child 'structdefref n)]
                [struct-name-bind (att-value 'xsmith_binding struct-ref)]
                [inners (nominal-record-type-inners
                         (nominal-record-definition-type-type
                          (binding-type struct-name-bind)))])
           (hash-set
            (for/hash ([v vals]
                       [field-name (dict-keys inners)])
              (values v (dict-ref inners field-name)))
            'structdefref
            (nominal-record-definition-type t)))))]]
 )

(add-prop
 cish-grammar
 choice-filters-to-apply
 [#f (
      features-enabled
      misc-constraints
      )])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
