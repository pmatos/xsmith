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
(define eqsign          (char #\=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-specification spec
  (ast-rule 'Program->FunctionDefinition*-FunctionDefinition<main)

  ;; TODO - the block in a function definition should get some constraints from its parent - eg. it should have a return of the appropriate type in each branch
  (ast-rule 'FunctionDefinition->typename-name-FormalParam*-Block)
  (ast-rule 'FormalParam->typename-name)

  ;(ast-rule 'Type->name)


  (ast-rule 'Statement->)
  (ast-rule 'NullStatement:Statement->)
  (ast-rule 'Block:Statement->Declaration*-Statement*)
  (ast-rule 'ExpressionStatement:Statement->Expression)
  (ast-rule 'ReturnStatement:Statement->)
  (ast-rule 'VoidReturnStatement:ReturnStatement->)
  (ast-rule 'ValueReturnStatement:ReturnStatement->Expression)
  (ast-rule 'StatementHole:Statement->)

  (ast-rule 'Declaration->name)
  (ast-rule 'VariableDeclaration:Declaration->typename-Expression)
  (ast-rule 'DeclarationHole:Declaration->)

  (ast-rule 'Expression->)
  (ast-rule 'ExpressionHole:Expression->)
  ;; TODO LValues?
  (ast-rule 'AssignmentExpression:Expression->name-Expression)
  (ast-rule 'AdditionExpression:Expression->Expression<l-Expression<r)
  (ast-rule 'Number:Expression->val)
  (ast-rule 'VariableReference:Expression->name)
  (ast-rule 'FunctionCall:Expression->name-ArgumentList)

  (ast-rule 'ArgumentList->)
  (ast-rule 'ArgumentListEmpty:ArgumentList->)
  (ast-rule 'ArgumentListNode:ArgumentList->Expression-ArgumentList)

  (compile-ast-specifications 'Program)


  (ag-rule ast-depth
           [Program (λ (n) 0)]
           [Statement (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))]
           [Expression (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))]
           [VariableDeclaration (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))]
           [FunctionDefinition (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))])
  (ag-rule
   pretty-print
   [Program (λ (n)
              (att-value 'pretty-print (ast-child 'main n)))]
   [FunctionDefinition
    (λ (n)
      (v-append
       (h-append
        (text (ast-child 'typename n))
        (text " ")
        (text (ast-child 'name n))
        lparen
        ;; TODO - params
        rparen)
       (att-value 'pretty-print (ast-child 'Block n))))]
   [Block (λ (n)
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
             rbrace))]
   [ExpressionStatement
    (λ (n) (h-append (att-value 'pretty-print (ast-child 1 n))
                     (text ";")))]
   [ValueReturnStatement
    (λ (n) (h-append (text "return ")
                     (att-value 'pretty-print (ast-child 1 n))
                     (text ";")))]
   [NullStatement (λ (n) (text ";"))]
   [VariableDeclaration
    (λ (n) (h-append (text (ast-child 'typename n))
                     (text " ")
                     (text (ast-child 'name n))
                     (text " = ")
                     (att-value 'pretty-print (ast-child 'Expression n))
                     (text ";")))]
   [Number (λ (n) (text (number->string (ast-child 'val n))))]
   [VariableReference (λ (n) (text (ast-child 'name n)))]
   [AdditionExpression
    (λ (n) (h-append lparen
                     (att-value 'pretty-print (ast-child 'l n))
                     (text " + ")
                     (att-value 'pretty-print (ast-child 'r n))
                     rparen))]
   )

  (ag-rule
   scope-graph-binding
   [VariableDeclaration
    (λ (n) (binding (ast-child 'name n)
                    ;; TODO - decide what should really go here
                    (hash 'type (ast-child 'typename n))))]
   [DeclarationHole
    (λ (n) #f)])

  (ag-rule
   scope-graph-scope
   [Program
    ;; TODO - functions are bound here
    (λ (n) (scope #f '() '()))]
   [Block
    ;; TODO - fix variable initialization so it can't initialize to itself
    (λ (n) (scope (att-value 'scope-graph-scope (ast-parent n))
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (ast-children (ast-child 'Declaration* n))))
                  '()))]
   [Declaration
    (λ (n) (att-value 'scope-graph-scope (ast-parent n)))]
   [Statement
    (λ (n) (att-value 'scope-graph-scope (ast-parent n)))]
   [Expression
    (λ (n) (att-value 'scope-graph-scope (ast-parent n)))])

  (ag-rule
   visible-bindings
   [Statement (λ (n) (visible-bindings (att-value 'scope-graph-scope n)))]
   [Expression (λ (n) (visible-bindings (att-value 'scope-graph-scope n)))])
  (ag-rule
   illegal-variable-names
   [Statement (λ (n) '())]
   [Block (λ (n) (map (λ (cn) (ast-child 'name cn))
                      (ast-children (ast-child 'Declaration* n))))]
   [Declaration (λ (n) (att-value 'illegal-variable-names (ast-parent n)))]
   [Expression (λ (n) (att-value 'illegal-variable-names (ast-parent n)))]
   )

  (ag-rule
   current-function-return-type
   [Statement (λ (n) (att-value 'current-function-return-type (ast-parent n)))]
   [FunctionDefinition (λ (n) (ast-child 'typename n))])

  (ag-rule
   children-type-dict
   ;; For eg. functions to associate a child node with the type it must be
   [ExpressionStatement (λ (n) (hasheq (ast-child 'Expression n) #f))]
   [ValueReturnStatement (λ (n) (hasheq (ast-child 'Expression n)
                                        (att-value 'current-function-return-type n)))]
   [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n)
                                       (ast-child 'typename n)))]
   [AdditionExpression (λ (n) (let ([t (att-value 'type-context n)])
                                (hasheq (ast-child 'l n) t
                                        (ast-child 'r n) t)))]
   ;; TODO - function call, anything with child expressions...
   )
  (ag-rule
   type-context
   [Expression (λ (n) (dict-ref (att-value 'children-type-dict (ast-parent n))
                                n))]
   )


  (compile-ag-specifications)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

(define cish-ast-choice%
  (class ast-choice%
    (define/override (choice-weight) 10)
    (define/public (wont-over-deepen holenode)
      (if (<= (att-value 'ast-depth holenode) (xsmith-option 'max-depth))
          this
          #f))
    (define/public (constrain-type holenode)
      this)
    (super-new)))

(define StatementChoice
  (class cish-ast-choice%
    (super-new)))
(define NullStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'NullStatement))
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
(define NumberChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'Number (random 100)))
    (define/override (wont-over-deepen holenode)
      this)
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (equal? t "int")) this]
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
      ;; TODO filter to matching type... once I use more than one type
      (define avail (filter (λ (b) #t) visibles))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names holenode))))
                avail))
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
(define AdditionExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'AdditionExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (define/override (constrain-type holenode)
      (let ([t (att-value 'type-context holenode)])
        (cond [(and t (equal? t "int")) this]
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
                  "int"
                  (fresh-node 'ExpressionHole)))
    (super-new)))

(define (fresh-var-name)
  (symbol->string (gensym "var")))

(define (statement-choices)
  (list (new NullStatementChoice)
        (new ExpressionStatementChoice)
        (new BlockChoice)
        (new ValueReturnStatementChoice)))

(define (expression-choices)
  (list (new NumberChoice)
        (new AdditionExpressionChoice)
        (new VariableReferenceChoice)))

(define (declaration-choices)
  (list (new VariableDeclarationChoice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (fresh-node type attr-val ...)
  (create-ast spec type (list attr-val ...)))

(define-syntax-rule (maybe-send obj method arg ...)
  (and obj (send obj method arg ...)))
(define-syntax-rule (maybe-send+ obj (method arg ...) ...)
  (let* ([tmp obj]
         [tmp (maybe-send tmp method arg ...)] ...)
    tmp))

(define (apply-choice-filters choice-list hole-node)
  (filter (λ (choice) (maybe-send+ choice
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
  (fresh-node 'Program
              (create-ast-list '())
              (fresh-node 'FunctionDefinition
                          "int"
                          "main"
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

(define (do-it)
  (let ((ast (generate-random-prog (fresh-Prog))))
    (pretty-print (att-value 'pretty-print ast) (current-output-port) page-width)
    #;(if (dict-has-key? (xsmith-options) 'output-filename)
        (call-with-output-file (xsmith-option 'output-filename)
          #:exists 'replace
          (lambda (out)
            (pretty-print (att-value 'ppdoc ast) out page-width)))
        (pretty-print (att-value 'ppdoc ast) (current-output-port) page-width))
    ))

;; End of file.
