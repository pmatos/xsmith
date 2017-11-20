#lang racket
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2016, 2017 The University of Utah
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

(require racr)
(require racr/testing) ;; racr/testing is needed for print-ast
(require pprint)
(require "random.rkt")
(require "choice.rkt")
(require "scope-graph.rkt")
(require racket/random)
(require "xsmith-options.rkt")
(require "xsmith-version.rkt")
(provide do-it)

(define spec (create-specification))

(define page-width      80)
(define nest-step       4)
(define lbrace          (char #\{))
(define rbrace          (char #\}))
(define lparen          (char #\())
(define rparen          (char #\)))
(define semi            (char #\;))
(define plus            (char #\+))
(define minus           (char #\-))
(define star            (char #\*))
(define slash           (char #\/))
(define percent         (char #\%))
(define eqsign          (char #\=))
(define comment-start   (text "/*"))
(define comment-end     (text "*/"))

(define return          (text "return"))

(define (comment d)
  (if (eq? d empty)
      empty
      (hs-append comment-start d comment-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The mutable state of the code generator.
;; XXX --- should encapsulaye the RNG?  Currently, the RNG is a separate
;;   parameter, automatically manged by Racket.
;; XXX --- should this reference the options, too?  Right now, the options are
;;   are separate parameter.

(define xsmith-state (make-parameter #f))

(struct generator-state
  ((fresh-name-counter #:mutable))
  )

(define (make-generator-state)
  (generator-state 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-specification spec
  (ast-rule 'Node->precomment-postcomment)
  (ast-rule 'Program:Node->Declaration*-FunctionDefinition<main)

  (ast-rule 'Declaration:Node->name)
  (ast-rule 'DeclarationHole:Declaration->)
  (ast-rule 'VariableDeclaration:Declaration->typename-Expression)
  ;; TODO - the block in a function definition should get some constraints from its parent - eg. it should have a return of the appropriate type in each branch
  (ast-rule 'FunctionDefinition:Declaration->typename-FormalParam*-Block)
  (ast-rule 'FormalParam:Node->typename-name)


  (ast-rule 'Statement:Node->)
  (ast-rule 'NullStatement:Statement->)
  (ast-rule 'Block:Statement->Declaration*-Statement*)
  (ast-rule 'ExpressionStatement:Statement->Expression)
  (ast-rule 'ReturnStatement:Statement->)
  (ast-rule 'VoidReturnStatement:ReturnStatement->)
  (ast-rule 'ValueReturnStatement:ReturnStatement->Expression)
  (ast-rule 'StatementHole:Statement->)

  (ast-rule 'Expression:Node->)
  (ast-rule 'ExpressionHole:Expression->)
  ;; TODO LValues?
  (ast-rule 'AssignmentExpression:Expression->name-Expression)
  (ast-rule 'FunctionApplicationExpression:Expression->name-Expression*)
  (ast-rule 'BinaryExpression:Expression->Expression<l-Expression<r)
  (ast-rule 'AdditionExpression:BinaryExpression->)
  (ast-rule 'SubtractionExpression:BinaryExpression->)
  (ast-rule 'MultiplicationExpression:BinaryExpression->)
  (ast-rule 'DivisionExpression:BinaryExpression->)
  (ast-rule 'ModulusExpression:BinaryExpression->)
  (ast-rule 'LiteralInt:Expression->val)
  (ast-rule 'LiteralFloat:Expression->val)
  (ast-rule 'VariableReference:Expression->name)
  (ast-rule 'FunctionCall:Expression->name-ArgumentList)

  (ast-rule 'ArgumentList:Node->)
  (ast-rule 'ArgumentListEmpty:ArgumentList->)
  (ast-rule 'ArgumentListNode:ArgumentList->Expression-ArgumentList)

  (compile-ast-specifications 'Node) ; Program


  (ag-rule ast-depth
           [Program (λ (n) 0)]
           [Node (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))])

  ;; This is probably not necessary, but I'm not sure offhand right now how to test
  ;; the type of a node.
  (ag-rule is-program?
           [Program (λ (n) #t)]
           [Node (λ (n) #f)])
  ;; IE are declarations here global?
  (ag-rule at-top-level?
           [Program (λ (n) #t)]
           [Node (λ (n) (let ([p (ast-parent n)])
                          (or (att-value 'is-program? (ast-parent n))
                              (and (ast-list-node? p)
                                   (att-value 'is-program? (ast-parent p))))))])

  (ag-rule
   pretty-print
   [Program (λ (n)
              (vb-append
               (comment (ast-child 'precomment n))
               (apply vb-append (map (λ (cn) (att-value 'pretty-print cn))
                                      (append (ast-children (ast-child 'Declaration* n))
                                              (list (ast-child 'main n)))))
               (comment (ast-child 'postcomment n))))]
   [FunctionDefinition
    (λ (n)
      (vb-append
       (comment (ast-child 'precomment n))
       (group (h-append
               (text (ast-child 'typename n))
               space
               (text (ast-child 'name n))
               lparen
               (text (string-join (map (λ (fp) (string-append (ast-child 'typename fp)
                                                              " "
                                                              (ast-child 'name fp)))
                                       (ast-children (ast-child 'FormalParam* n)))
                                  ", "))
               rparen))
       (att-value 'pretty-print (ast-child 'Block n))
       (comment (ast-child 'postcomment n))))]
   [Block (λ (n)
            (v-append
             (comment (ast-child 'precomment n))
             (h-append
              lbrace
              (nest
               nest-step
               (apply v-append
                      ;; add an extra text node so linebreaks are added...
                      (text "")
                      (append
                       (map (λ (cn) (att-value 'pretty-print cn))
                            (ast-children (ast-child 'Declaration* n)))
                       (map (λ (cn) (att-value 'pretty-print cn))
                            (ast-children (ast-child 'Statement* n))))))
              line
              rbrace)
             (comment (ast-child 'postcomment n))))]
   [ExpressionStatement
    (λ (n) (h-append (comment (ast-child 'precomment n))
                     (att-value 'pretty-print (ast-child 3 n))
                     semi
                     (comment (ast-child 'postcomment n))))]
   [ValueReturnStatement
    (λ (n) (h-append (comment (ast-child 'precomment n))
                     return
                     space
                     (att-value 'pretty-print (ast-child 3 n))
                     semi
                     (comment (ast-child 'postcomment n))))]
   [NullStatement (λ (n)
                    (h-append (comment (ast-child 'precomment n))
                              semi
                              (comment (ast-child 'postcomment n))))]
   [VariableDeclaration
    (λ (n) (h-append (comment (ast-child 'precomment n))
                     (hs-append
                      (text (ast-child 'typename n))
                      (text (ast-child 'name n))
                      eqsign
                      (att-value 'pretty-print (ast-child 'Expression n)))
                     semi
                     (comment (ast-child 'postcomment n))))]
   [LiteralInt (λ (n) (h-append
                       (comment (ast-child 'precomment n))
                       (text (number->string (ast-child 'val n)))
                       (comment (ast-child 'postcomment n))))]
   [LiteralFloat (λ (n) (h-append
                         (comment (ast-child 'precomment n))
                         (text (number->string (ast-child 'val n)))
                         (comment (ast-child 'postcomment n))))]
   [VariableReference (λ (n) (h-append
                              (comment (ast-child 'precomment n))
                              (text (ast-child 'name n))
                              (comment (ast-child 'postcomment n))))]
   [FunctionApplicationExpression
    (λ (n) (h-append (text (ast-child 'name n))
                     lparen
                     (apply h-append
                            (add-between (map (λ (a) (att-value 'pretty-print a))
                                              (ast-children (ast-child 'Expression* n)))
                                         (text ", ")))
                     rparen))]
   [BinaryExpression
    (λ (n) (h-append (comment (ast-child 'precomment n))
                     lparen
                     (hs-append (att-value 'pretty-print (ast-child 'l n))
                                (att-value 'pretty-print-op n)
                                (att-value 'pretty-print (ast-child 'r n)))
                     rparen
                     (comment (ast-child 'postcomment n))))]
   )

  (ag-rule
   pretty-print-op
   [AdditionExpression (λ (n) plus)]
   [SubtractionExpression (λ (n) minus)]
   [MultiplicationExpression (λ (n) star)]
   [DivisionExpression (λ (n) slash)]
   [ModulusExpression (λ (n) percent)]
   )

  (ag-rule
   scope-graph-binding
   [Node (λ (n) (error 'scope-graph-binding "no default ag-rule"))]
   [FunctionDefinition
    (λ (n) (binding (ast-child 'name n)
                    ;; TODO - decide what should really go here
                    (hash 'type (append (list '->)
                                        (map (λ (fp) (ast-child 'typename fp))
                                             (ast-children (ast-child 'FormalParam* n)))
                                        (list (ast-child 'typename n))))))]
   [VariableDeclaration
    (λ (n) (binding (ast-child 'name n)
                    ;; TODO - decide what should really go here
                    (hash 'type (ast-child 'typename n))))]
   [FormalParam
    (λ (n) (binding (ast-child 'name n)
                    (hash 'type (ast-child 'typename n))))]
   [DeclarationHole
    (λ (n) #f)])

  (ag-rule
   scope-graph-scope
   [Program
    (λ (n) (scope #f
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'Declaration* n))))
                  '()))]
   [FunctionDefinition
    (λ (n) (scope (att-value 'scope-graph-scope (ast-parent n))
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'FormalParam* n))))
                  '()))]
   [Block
    (λ (n) (scope (att-value 'scope-graph-scope (ast-parent n))
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'Declaration* n))))
                  '()))]
   [Node
    (λ (n) (att-value 'scope-graph-scope (ast-parent n)))])

  (ag-rule
   visible-bindings
   [Node (λ (n) (visible-bindings (att-value 'scope-graph-scope n)))])
  (ag-rule
   illegal-variable-names
   [Node (λ (n) '())]
   [Program (λ (n) (map (λ (cn) (ast-child 'name cn))
                        (ast-children (ast-child 'Declaration* n))))]
   [Block (λ (n) (map (λ (cn) (ast-child 'name cn))
                      (ast-children (ast-child 'Declaration* n))))]
   [Declaration (λ (n) (att-value 'illegal-variable-names (ast-parent n)))]
   [Expression (λ (n) (att-value 'illegal-variable-names (ast-parent n)))]
   )

  (ag-rule
   current-function-return-type
   [Node (λ (n) (error 'current-function-return-type "no default ag-rule"))]
   [Statement (λ (n) (att-value 'current-function-return-type (ast-parent n)))]
   [FunctionDefinition (λ (n) (ast-child 'typename n))])

  (ag-rule
   children-type-dict
   ;; For eg. functions to associate a child node with the type it must be
   [Node (λ (n) (error 'children-type-dict "no default ag-rule"))]
   [ExpressionStatement (λ (n) (hasheq (ast-child 'Expression n) #f))]
   [ValueReturnStatement (λ (n) (hasheq (ast-child 'Expression n)
                                        (att-value 'current-function-return-type n)))]
   [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n)
                                       (ast-child 'typename n)))]
   [FunctionApplicationExpression
    (λ (n)
      (let ([f-bind (resolve-reference (reference (ast-child 'name n)
                                                  (att-value 'scope-graph-scope n)))])
        (for/fold ([h (hasheq)])
                  ([cn (ast-children (ast-child 'Expression* n))]
                   [t (reverse (cdr (reverse (cdr (hash-ref (binding-bound f-bind)
                                                            'type)))))])
          (hash-set h cn t))))]
   [BinaryExpression (λ (n) (let ([t (att-value 'type-context n)])
                              (hasheq (ast-child 'l n) t
                                      (ast-child 'r n) t)))]
   ;; TODO - function call, anything with child expressions...
   )
  (ag-rule
   type-context
   [Node (λ (n) (error 'type-context "no default ag-rule"))]
   [Expression (λ (n)
                 (dict-ref (att-value 'children-type-dict (ast-parent n))
                                n))]
   [BinaryExpression
    (λ (n) (let ([parent-context (dict-ref
                                  (att-value 'children-type-dict (ast-parent n))
                                  n)])
             ;; #f is not a valid type context -- if there is no type
             ;; dictated by the parent, choose one of the number types.
             (or parent-context (fresh-var-type))))]
   )


  (compile-ag-specifications)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

(define cish-ast-choice%
  (class ast-choice%
    (define/override (choice-weight) 10)
    (define/public (features-enabled)
      (let ((disabled (xsmith-option 'features-disabled)))
        (if (ormap (λ (f) (dict-ref disabled f #f))
                   (send this features))
            #f
            this)))
    (define/public (wont-over-deepen holenode)
      (if (<= (att-value 'ast-depth holenode) (xsmith-option 'max-depth))
          this
          #f))
    (define/public (constrain-type holenode)
      this)
    (define/public (top-level-declaration-at-top-level holenode)
      this)
    (super-new)))

(define StatementChoice
  (class cish-ast-choice%
    (super-new)))
(define NullStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'NullStatement))
    (define/override (features) '(null))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (choice-weight) 2)
    (super-new)))
(define ExpressionStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'ExpressionStatement (fresh-node 'ExpressionHole)))
    (define/override (wont-over-deepen holenode)
      this)
    (super-new)))
(define BlockChoice
  (class StatementChoice
    (define/override (choice-weight) 15)
    (define/override (fresh)
      (fresh-node 'Block
              ;; declarations
              (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                       "standin-name"))
                                    (make-list (random 3) #f)))
              ;; statements
              (create-ast-list (map (λ (x) (fresh-node 'StatementHole))
                                    (make-list (random 5) #f)))))
    (super-new)))
(define ReturnStatementChoice
  (class StatementChoice
    (define/override (wont-over-deepen holenode)
      this)
    (super-new)))
(define ValueReturnStatementChoice
  (class ReturnStatementChoice
    (define/override (fresh)
      (fresh-node 'ValueReturnStatement (fresh-node 'ExpressionHole)))
    (super-new)))

(define ExpressionChoice
  (class cish-ast-choice%
    (super-new)))
(define LiteralIntChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'LiteralInt (random 100)))
    (define/override (features) '(int))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (equal? t "int")) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define LiteralFloatChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'LiteralFloat (* (random) (random 10))))
    (define/override (features) '(float))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (equal? t "float")) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define VariableReferenceChoice
  (class ExpressionChoice
    (define/override (choice-weight) 15)
    (define ref-choices-filtered #f)
    (define/override (fresh)
      (fresh-node 'VariableReference (binding-name (random-ref ref-choices-filtered))))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (constrain-type holenode)
      (define visibles (att-value 'visible-bindings holenode))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names holenode))))
                visibles))
      (define type-needed (att-value 'type-context holenode))
      (define legal-with-type
        (if type-needed
            (filter (λ (b) (equal? type-needed
                                   (dict-ref (binding-bound b) 'type)))
                    legal-refs)
            legal-refs))
      (set! ref-choices-filtered legal-with-type)
      (and (not (null? legal-with-type)) this))
    (super-new)))
(define FunctionApplicationExpressionChoice
  (class ExpressionChoice
    (define ref-choices-filtered #f)
    (define/override (fresh)
      (define chosen-func (random-ref ref-choices-filtered))
      (fresh-node 'FunctionApplicationExpression
                  (binding-name chosen-func)
                  (create-ast-list
                   (map (λ (x) (fresh-node 'ExpressionHole))
                        (make-list (- (length (dict-ref (binding-bound chosen-func)
                                                        'type))
                                      2)
                                   #f)))))
    (define/override (constrain-type holenode)
      (define visibles (filter (λ (b) (function-type?
                                       (dict-ref (binding-bound b) 'type)))
                               (att-value 'visible-bindings holenode)))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names holenode))))
                visibles))
      (define type-needed (att-value 'type-context holenode))
      (define legal-with-type
        (if type-needed
            (filter (λ (b) (equal? type-needed
                                   (car (reverse (dict-ref (binding-bound b) 'type)))))
                    legal-refs)
            legal-refs))
      (set! ref-choices-filtered legal-with-type)
      (and (not (null? legal-with-type)) this))
    (super-new)))
(define AdditionExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'AdditionExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(addition))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (member t '("int" "float"))) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define SubtractionExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'SubtractionExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(subtraction))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (member t '("int" "float"))) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define MultiplicationExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'MultiplicationExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(multiplication))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (member t '("int" "float"))) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define DivisionExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'DivisionExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(division))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (member t '("int" "float"))) this]
              [(not t) this]
              [else #f])))
    (super-new)))
(define ModulusExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'ModulusExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(modulus))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (member t '("int" "float"))) this]
              [(not t) this]
              [else #f])))
    (super-new)))

(define DeclarationChoice
  (class cish-ast-choice%
    (define/override (choice-weight) 1)
    (super-new)))
(define VariableDeclarationChoice
  (class DeclarationChoice
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (fresh)
      (fresh-node 'VariableDeclaration
                  (fresh-var-name)
                  (fresh-var-type)
                  (fresh-node 'ExpressionHole)))
    (super-new)))
(define FunctionDefinitionChoice
  (class DeclarationChoice
    (define/override (top-level-declaration-at-top-level holenode)
      (if (att-value 'at-top-level? holenode)
          this
          #f))
    (define/override (fresh)
      (fresh-node 'FunctionDefinition
                  (fresh-var-name "func")
                  (fresh-var-type)
                  ;; parameters
                  (create-ast-list (map (λ (x) (fresh-node 'FormalParam
                                                           (fresh-var-type)
                                                           (fresh-var-name)))
                                        (make-list (random 5) #f)))
                  ;; TODO -- it would be better to just have a hole and make sure
                  ;;         the generator code picks up all the requirements from
                  ;;         the hole node's place in the tree.
                  (fresh-node 'Block
                              (create-ast-list (list))
                              (create-ast-list
                               (append
                                (map (λ (x) (fresh-node 'StatementHole))
                                     (make-list (random 5) #f))
                                (list
                                 (fresh-node
                                  'ValueReturnStatement
                                  (fresh-node 'ExpressionHole))))))))
    (super-new)))

(define (fresh-var-name [base "var"])
  (let ((n (generator-state-fresh-name-counter (xsmith-state))))
    (set-generator-state-fresh-name-counter! (xsmith-state) (add1 n))
    (format "~a~a" base n)))
(define (fresh-var-type)
  (let ((disabled (xsmith-option 'features-disabled)))
    ;; XXX Obviously, the code below is not quite right.
    ;; What is both float and int are disabled?
    (cond [(dict-ref disabled 'float #f)
           "int"]
          [(dict-ref disabled 'int #f)
           "float"]
          [else
           (random-ref '("int" "float"))])))
(define (function-type? t)
  (and (list? t)
       (not (null? t))
       (eq? (car t) '->)))

(define (statement-choices)
  (list (new NullStatementChoice)
        (new ExpressionStatementChoice)
        (new BlockChoice)
        (new ValueReturnStatementChoice)))

(define (expression-choices)
  (list (new LiteralIntChoice)
        (new LiteralFloatChoice)
        (new FunctionApplicationExpressionChoice)
        (new AdditionExpressionChoice)
        (new SubtractionExpressionChoice)
        (new MultiplicationExpressionChoice)
        (new DivisionExpressionChoice)
        (new ModulusExpressionChoice)
        (new VariableReferenceChoice)))

(define (declaration-choices)
  (list (new VariableDeclarationChoice)
        (new FunctionDefinitionChoice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (fresh-node type attr-val ...)
  (create-ast spec type (list empty empty attr-val ...)))
; (create-ast spec type (list (text "/*foo*/") (text "/*bar*/") attr-val ...)))

(define-syntax-rule (maybe-send obj method arg ...)
  (and obj (send obj method arg ...)))
(define-syntax-rule (maybe-send+ obj (method arg ...) ...)
  (let* ([tmp obj]
         [tmp (maybe-send tmp method arg ...)] ...)
    tmp))

(define (apply-choice-filters choice-list hole-node)
  (filter (λ (choice) (maybe-send+ choice
                                   (features-enabled)
                                   (top-level-declaration-at-top-level hole-node)
                                   (wont-over-deepen hole-node)
                                   (constrain-type hole-node)))
          choice-list))

(define (replace-with-expression n)
  (let ([o (choose-ast (apply-choice-filters (expression-choices) n))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-statement n)
  (let ([o (choose-ast (apply-choice-filters (statement-choices) n))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-declaration n)
  (let ([o (choose-ast (apply-choice-filters (declaration-choices) n))])
    (rewrite-subtree n (send o fresh))))

(define (generate-random-prog n)
  (let ([fill-in
         (λ (n)
           (if (ast-list-node? n)
               #f
               (case (ast-node-type n)
                 ((ExpressionHole)
                  (replace-with-expression n)
                  #t)
                 ((StatementHole)
                  (replace-with-statement n)
                  #t)
                 ((DeclarationHole)
                  (replace-with-declaration n)
                  #t)
                 (else #f))))])
    (perform-rewrites n 'top-down fill-in))
  n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-Prog)
  (define p
    (fresh-node 'Program
                (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                         "standin-name"))
                                      (make-list (random 5) #f)))
                (fresh-node 'FunctionDefinition
                            "main"
                            "int"
                            ;; parameters
                            (create-ast-list '())
                            (fresh-node 'Block
                                        (create-ast-list (list))
                                        (create-ast-list
                                         (append
                                          (map (λ (x) (fresh-node 'StatementHole))
                                               (make-list (random 5) #f))
                                          (list
                                           (fresh-node
                                            'ValueReturnStatement
                                            (fresh-node 'ExpressionHole)))))))))
  (rewrite-terminal 'precomment p
                    (h-append
                     line
                     (vb-append
                      (text "This is a RANDOMLY GENERATED PROGRAM.")
                      (hs-append
                       (fill 10 (text "Generator:"))
                       (text xsmith-version-string))
                      (hs-append
                       (fill 10 (text "Options:"))
                       (apply hs-append
                              (map text
                                   (vector->list
                                    (xsmith-option 'command-line)))))
                      (hs-append
                       (fill 10 (text "Seed:"))
                       (text (number->string (xsmith-option 'random-seed))))
                      soft-break)))
  p)

(define (do-it options)
  (let ((state (make-generator-state)))
    ;; Initialize the state from the options.
    ;; Pretty lame to use `parameterize` just for this.  XXX Fix options API.
    (parameterize ((xsmith-options options))
      (random-seed (xsmith-option 'random-seed)))
    (do-one state options)))

(define (do-one state options)
  (parameterize ((xsmith-state state)
                 (xsmith-options options))
    (let ((ast (generate-random-prog (fresh-Prog))))
      (if (dict-has-key? (xsmith-options) 'output-filename)
          (call-with-output-file (xsmith-option 'output-filename)
            #:exists 'replace
            (lambda (out)
              (pretty-print (att-value 'pretty-print ast)
                            out
                            page-width)))
          (pretty-print (att-value 'pretty-print ast)
                        (current-output-port)
                        page-width))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
