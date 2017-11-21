#lang racket/base
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

(require
 racr
 racr/testing ;; racr/testing is needed for print-ast
 pprint
 racket/random
 racket/string
 racket/dict
 racket/class
 (except-in racket/list empty)
 "random.rkt"
 "choice.rkt"
 "scope-graph.rkt"
 "xsmith-options.rkt"
 "xsmith-version.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(provide do-it make-do-it)

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
(define greater         (char #\>))
(define less            (char #\<))
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
  (ast-rule 'FunctionDefinitionHole:FunctionDefinition->)
  (ast-rule 'FormalParam:Node->typename-name)


  (ast-rule 'Statement:Node->)
  (ast-rule 'NullStatement:Statement->)
  (ast-rule 'Block:Statement->Declaration*-Statement*)
  (ast-rule 'ExpressionStatement:Statement->Expression)
  (ast-rule 'IfStatement:Statement->Expression<test-Statement<then)
  (ast-rule 'IfElseStatement:IfStatement->Statement<else)
  (ast-rule 'ReturnStatement:Statement->)
  (ast-rule 'VoidReturnStatement:ReturnStatement->)
  (ast-rule 'ValueReturnStatement:ReturnStatement->Expression)
  (ast-rule 'StatementHole:Statement->)
  (ast-rule 'BlockHole:Block->)

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

  (ast-rule 'ComparisonExpression:BinaryExpression->)
  (ast-rule 'EqualityExpression:ComparisonExpression->)
  (ast-rule 'GreaterThanExpression:ComparisonExpression->)
  (ast-rule 'LessThanExpression:ComparisonExpression->)
  (ast-rule 'LessOrEqualExpression:ComparisonExpression->)
  (ast-rule 'GreaterOrEqualExpression:ComparisonExpression->)

  (ast-rule 'IfExpression:Expression->Expression<test-Expression<then-Expression<else)
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
           [Node (λ (n) (add1 (att-value 'ast-depth (parent-node n))))])

  ;; IE are declarations here global?
  (ag-rule at-top-level?
           [Program (λ (n) #t)]
           [Node (λ (n) (let ([p (parent-node n)])
                          (or (equal? (node-type p) 'Program)
                              (and (ast-list-node? p)
                                   (equal? (node-type (parent-node p)) 'Program)))))])

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
   [IfStatement
    (λ (n) (v-append
            (h-append (text "if ")
                      lparen
                      (att-value 'pretty-print (ast-child 'test n))
                      rparen)
            (att-value 'pretty-print (ast-child 'then n))))]
   [IfElseStatement
    (λ (n) (v-append
            (h-append (text "if ")
                      lparen
                      (att-value 'pretty-print (ast-child 'test n))
                      rparen)
            (att-value 'pretty-print (ast-child 'then n))
            (text "else")
            (att-value 'pretty-print (ast-child 'else n))))]
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
   [IfExpression
    (λ (n) (h-append lparen
                     (att-value 'pretty-print (ast-child 'test n))
                     (text " ? ")
                     (att-value 'pretty-print (ast-child 'then n))
                     (text " : ")
                     (att-value 'pretty-print (ast-child 'else n))
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
   [EqualityExpression (λ (n) (h-append eqsign eqsign))]
   [GreaterThanExpression (λ (n) greater)]
   [LessThanExpression (λ (n) less)]
   [GreaterOrEqualExpression (λ (n) (h-append greater eqsign))]
   [LessOrEqualExpression (λ (n) (h-append less eqsign))]
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
    (λ (n) (scope (att-value 'scope-graph-scope (parent-node n))
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'FormalParam* n))))
                  '()))]
   [Block
    (λ (n) (scope (att-value 'scope-graph-scope (parent-node n))
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'Declaration* n))))
                  '()))]
   [Node
    (λ (n) (att-value 'scope-graph-scope (parent-node n)))])

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
   [Declaration (λ (n) (att-value 'illegal-variable-names (parent-node n)))]
   [Expression (λ (n) (att-value 'illegal-variable-names (parent-node n)))]
   )

  (ag-rule
   current-function-return-type
   [Node (λ (n) (error 'current-function-return-type "no default ag-rule"))]
   [Statement (λ (n) (att-value 'current-function-return-type (parent-node n)))]
   [FunctionDefinition (λ (n) (ast-child 'typename n))])

  (ag-rule
   children-type-dict
   ;; For eg. functions to associate a child node with the type it must be
   [Node (λ (n) (error 'children-type-dict "no default ag-rule"))]
   [ExpressionStatement (λ (n) (hasheq (ast-child 'Expression n) #f))]
   [ValueReturnStatement (λ (n) (hasheq (ast-child 'Expression n)
                                        (att-value 'current-function-return-type n)))]
   [IfStatement (λ (n) (hasheq (ast-child 'test n) "int"))]
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
   [BinaryExpression (λ (n) (let ([t (or (att-value 'type-context n) (fresh-var-type))])
                              (hasheq (ast-child 'l n) t
                                      (ast-child 'r n) t)))]
   [ComparisonExpression (λ (n) (let ([t (fresh-var-type)])
                                  (hasheq (ast-child 'l n) t
                                          (ast-child 'r n) t)))]
   [IfExpression (λ (n) (let ([t (or (att-value 'type-context n) (fresh-var-type))])
                          (hasheq (ast-child 'test n) "int"
                                  (ast-child 'then n) t
                                  (ast-child 'else n) t)))]
   ;; TODO - function call, anything with child expressions...
   )
  (ag-rule
   type-context
   [Node (λ (n) (error 'type-context "no default ag-rule"))]
   [Expression (λ (n) (dict-ref (att-value 'children-type-dict (parent-node n))
                                n))]
   )

  (ag-rule
   block-last-statement
   [Block (λ (n) (let ([ns (ast-children (ast-child 'Statement* n))])
                   (and (not (null? ns)) (car (reverse ns)))))]
   [Node (λ (n) (error 'block-last-statement "no default ag-rule"))])
  (ag-rule
   children-return-position-dict
   ;; Dictionary that will contain true for children that are in return position.
   ;; Else nothing or #f -- IE check with #f as default.
   [Block (λ (n) (if (att-value 'in-return-position? n)
                     (hash (att-value 'block-last-statement n) #t)
                     (hash)))]
   [IfElseStatement (λ (n) (if (att-value 'in-return-position? n)
                               (hash (ast-child 'then n) #t
                                     (ast-child 'else n) #t)
                               (hash)))]
   [Statement (λ (n) (hash))]
   [FunctionDefinition (λ (n) (hash (ast-child 'Block n) #t))]
   [Node (λ (n) (error 'children-return-position-dict "no default ag-rule"))])
  (ag-rule
   in-return-position?
   [Statement (λ (n) (let ([rp-dict (att-value 'children-return-position-dict
                                               (parent-node n))])
                       (dict-ref rp-dict n #f)))]
   [Node (λ (n) #f)])


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
    (define/public (respect-return-position holenode)
      this)
    (define/public (block-in-function holenode)
      (and (not (eq? (node-type (parent-node holenode)) 'FunctionDefinition))
           this))
    (super-new)))

(define StatementChoice
  (class cish-ast-choice%
    (define/override (respect-return-position holenode)
      (and (not (att-value 'in-return-position? holenode))
           this))
    (super-new)))
(define NullStatementChoice
  (class StatementChoice
    (define/override (fresh hole-node)
      (fresh-node 'NullStatement))
    (define/override (features) '(null))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (choice-weight) 2)
    (super-new)))
(define ExpressionStatementChoice
  (class StatementChoice
    (define/override (fresh hole-node)
      (fresh-node 'ExpressionStatement (fresh-node 'ExpressionHole)))
    (define/override (wont-over-deepen holenode)
      this)
    (super-new)))
(define IfStatementChoice
  (class StatementChoice
    (define/override (features) '(if-statement))
    (define/override (fresh hole-node)
      (fresh-node 'IfStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define IfElseStatementChoice
  (class IfStatementChoice
    (define/override (fresh hole-node)
      (fresh-node 'IfElseStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)
                  (fresh-node 'StatementHole)))
    (define/override (respect-return-position holenode)
      this)
    (super-new)))
(define BlockChoice
  (class StatementChoice
    (define/override (block-in-function holenode) this)
    (define/override (fresh hole-node)
      (fresh-node 'Block
                  ;; declarations
                  (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                           "standin-name"))
                                        (make-list (random 3) #f)))
                  ;; statements
                  (create-ast-list
                   (map (λ (x) (fresh-node 'StatementHole))
                        (let ([l (make-list (random 5) #f)])
                          (if (att-value 'in-return-position? hole-node)
                              ;; don't allow an empty block in return position
                              (cons #f l)
                              l))))))
    (define/override (respect-return-position holenode)
      this)
    (super-new)))
(define ReturnStatementChoice
  (class StatementChoice
    (define/override (choice-weight) 2)
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (respect-return-position holenode)
      this)
    (super-new)))
(define ValueReturnStatementChoice
  (class ReturnStatementChoice
    (define/override (choice-weight) 2)
    (define/override (fresh hole-node)
      (fresh-node 'ValueReturnStatement (fresh-node 'ExpressionHole)))
    (define/override (respect-return-position holenode)
      this)
    (super-new)))

(define ExpressionChoice
  (class cish-ast-choice%
    (super-new)))
(define LiteralIntChoice
  (class ExpressionChoice
    (define/override (fresh hole-node)
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
    (define/override (fresh hole-node)
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
    (define ref-choices-filtered #f)
    (define (choice-weight) 15)
    (define/override (fresh hole-node)
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
    (define/override (fresh hole-node)
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
(define IfExpressionChoice
  (class ExpressionChoice
    (define/override (fresh hole-node)
      (fresh-node 'IfExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (features) '(if-expression))
    (super-new)))
(define-syntax (define-binary-op-choice stx)
  (syntax-parse stx
    [(_ nodename ;; Name of grammar node, also generates choice name
        feature
        input-typelist ;; types the operator accepts
        output-type) ;; type the operator returns, or #f if it returns its input type
     #:with choicename (format-id #'nodename "~aChoice" #'nodename)
     #'(define choicename
         (class ExpressionChoice
           (define/override (fresh hole-node)
             (fresh-node 'nodename
                         (fresh-node 'ExpressionHole)
                         (fresh-node 'ExpressionHole)))
           (define/override (features) '(feature))
           (define/override (constrain-type holenode)
             (let ([t (att-value 'type-context holenode)])
               (cond [(and t
                           (member t input-typelist)
                           (or (not output-type) (equal? t output-type)))
                      this]
                     [(not t) this]
                     [else #f])))
           (super-new)))]))
(define-binary-op-choice AdditionExpression addition '("int" "float") #f)
(define-binary-op-choice MultiplicationExpression multiplication '("int" "float") #f)
(define-binary-op-choice SubtractionExpression subtraction '("int" "float") #f)
(define-binary-op-choice DivisionExpression division '("int" "float") #f)
(define-binary-op-choice ModulusExpression modulus '("int") #f)

(define-binary-op-choice EqualityExpression modulus '("int" "float") "int")
(define-binary-op-choice LessThanExpression modulus '("int" "float") "int")
(define-binary-op-choice GreaterThanExpression modulus '("int" "float") "int")
(define-binary-op-choice LessOrEqualExpression modulus '("int" "float") "int")
(define-binary-op-choice GreaterOrEqualExpression modulus '("int" "float") "int")

(define DeclarationChoice
  (class cish-ast-choice%
    (super-new)))
(define VariableDeclarationChoice
  (class DeclarationChoice
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (fresh hole-node)
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
    (define/override (fresh hole-node)
      (define p (parent-node hole-node))
      (define main? (and (eq? (node-type p) 'Program)
                         (eq? (ast-child 'main p) hole-node)))
      (fresh-node 'FunctionDefinition
                  (if main? "main" (fresh-var-name "func"))
                  (if main? "int" (fresh-var-type))
                  ;; parameters
                  (if main?
                      (create-ast-list '())
                      (create-ast-list (map (λ (x) (fresh-node 'FormalParam
                                                               (fresh-var-type)
                                                               (fresh-var-name)))
                                            (make-list (random 5) #f))))
                  (fresh-node 'BlockHole (create-ast-list '()) (create-ast-list '()))))
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
        (new IfStatementChoice)
        (new IfElseStatementChoice)
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
        (new VariableReferenceChoice)
        (new EqualityExpressionChoice)
        (new LessThanExpressionChoice)
        (new GreaterThanExpressionChoice)
        (new LessOrEqualExpressionChoice)
        (new GreaterOrEqualExpressionChoice)
        (new IfExpressionChoice)
        ))

(define (declaration-choices)
  (list (new VariableDeclarationChoice)
        (new FunctionDefinitionChoice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (fresh-node type attr-val ...)
  (create-ast spec type (list empty empty attr-val ...)))

(define (node-type n)
  (and (not (ast-list-node? n)) (not (ast-bud-node? n)) (ast-node-type n)))
(define (parent-node n)
  ;; I've had several bugs where I used a parent node that was a list-node
  ;; thinking it was the grandparent node.  The list nodes are generally
  ;; useless, so this function gets the non-list parent node.
  (let ([p (ast-parent n)])
    (if (ast-list-node? p)
        (ast-parent p)
        p)))

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
                                   (respect-return-position hole-node)
                                   (block-in-function hole-node)
                                   (constrain-type hole-node)))
          choice-list))

(define (replace-with-expression n)
  (let ([o (choose-ast (apply-choice-filters (expression-choices) n))])
    (rewrite-subtree n (send o fresh n))))

(define (replace-with-statement n)
  (let ([o (choose-ast (apply-choice-filters (statement-choices) n))])
    (rewrite-subtree n (send o fresh n))))

(define (replace-with-function n)
  (rewrite-subtree n (send (new FunctionDefinitionChoice) fresh n)))

(define (replace-with-declaration n)
  (let ([o (choose-ast (apply-choice-filters (declaration-choices) n))])
    (rewrite-subtree n (send o fresh n))))

(define (generate-random-prog n)
  (let ([fill-in
         (λ (n)
           (if (ast-list-node? n)
               #f
               (case (node-type n)
                 ((ExpressionHole)
                  (replace-with-expression n)
                  #t)
                 ((StatementHole)
                  (replace-with-statement n)
                  #t)
                 ((BlockHole)
                  (replace-with-statement n)
                  #t)
                 ((FunctionDefinitionHole)
                  (replace-with-function n)
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
                (fresh-node 'FunctionDefinitionHole
                            "standin-name"
                            "standin-type"
                            (create-ast-list '())
                            (fresh-node 'BlockHole
                                        (create-ast-list '())
                                        (create-ast-list '())))))
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

(define (make-do-it options)
  (let ((state (make-generator-state)))
    ;; Initialize the state from the options.
    ;; Pretty lame to use `parameterize` just for this.  XXX Fix options API.
    (parameterize ((xsmith-options options))
      (random-seed (xsmith-option 'random-seed)))
    (λ ()
      (parameterize ((xsmith-options options))
        ;; XXX also need to reset the seed, or increase a generation counter,
        ;; or something.  Right now, the seed printed in output program is
        ;; wrong!
        (do-one state options)))))

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
