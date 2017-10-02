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
(require racket/random)
(require "xsmith-options.rkt")
(provide do-it)

(define spec (create-specification))

(define statement-choice-table
  (make-choice-table '((NullStatement 1)
                       (Block 1)
                       ;(Return 1)
                       (ExpressionStatement 1)
                       )))
(define expression-choice-table
  (make-choice-table '((AdditionExpression 1)
                       (Number 1)
                       ;(Assignment 1)
                       ;(FunctionCall 1)
                       )))

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
  (ast-rule 'FunctionDefinition->Type-name-FormalParam*-Block)
  (ast-rule 'FormalParam->Type-name)

  (ast-rule 'Type->name)


  (ast-rule 'Statement->)
  (ast-rule 'NullStatement:Statement->)
  (ast-rule 'Block:Statement->Declaration*-Statement*)
  (ast-rule 'ExpressionStatement:Statement->Expression)
  (ast-rule 'ReturnStatement:Statement->)
  (ast-rule 'VoidReturnStatement:ReturnStatement->)
  (ast-rule 'ValueReturnStatement:ReturnStatement->Expression)
  (ast-rule 'StatementHole:Statement->)

  (ast-rule 'Declaration->)
  (ast-rule 'VariableDeclaration:Declaration->Type-name)

  (ast-rule 'Expression->)
  (ast-rule 'ExpressionHole:Expression->)
  ;; TODO LValues?
  (ast-rule 'AssignmentExpression:Expression->name-Expression)
  (ast-rule 'AdditionExpression:Expression->Expression<l-Expression<r)
  (ast-rule 'Number:Expression->val)
  (ast-rule 'FunctionCall:Expression->name-ArgumentList)

  (ast-rule 'ArgumentList->)
  (ast-rule 'ArgumentListEmpty:ArgumentList->)
  (ast-rule 'ArgumentListNode:ArgumentList->Expression-ArgumentList)

  (compile-ast-specifications 'Program)


  (ag-rule ast-depth
           [Program (λ (n) 0)]
           [Statement (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))]
           [Expression (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))]
           [FunctionDefinition (λ (n) (add1 (att-value 'ast-depth (ast-parent n))))])
  #;(ag-rule choice-table
           [StatementHole (lambda (n)
                            (let ([too-deep? (> (att-value 'ast-depth n)
                                                (xsmith-option 'max-depth))]
                                  [no-names? (null? (att-value 'names-available n))])
                              (cond
                                [(and too-deep? no-names?) term-atoms-choice-table/no-ref]
                                [no-names? term-choice-table/no-ref]
                                [too-deep? term-atoms-choice-table]
                                [else term-choice-table])))])
  (ag-rule
   pretty-print
   [Program (λ (n)
              #;(apply v-append
                       (append (map (λ (cn) (att-value 'pretty-print cn))
                                    (ast-children (ast-child 1 n)))
                               (list (att-value 'pretty-print (ast-child 2 n)))))
              (att-value 'pretty-print (ast-child 'main n)))]
   [FunctionDefinition
    (λ (n)
      (v-append
       (h-append
        (att-value 'pretty-print (ast-child 'Type n))
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
                     (map (λ (cn) (att-value 'pretty-print cn))
                          (ast-children (ast-child 'Statement* n)))))
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
   [Type (λ (n) (text (ast-child 'name n)))]
   [Number (λ (n) (text (number->string (ast-child 'val n))))]
   [AdditionExpression
    (λ (n) (h-append lparen
                     (att-value 'pretty-print (ast-child 'l n))
                     (text " + ")
                     (att-value 'pretty-print (ast-child 'r n))
                     rparen))]
   )

  #|
  ;; interpreter
  (ag-rule value)
  ;; type abstract interpreter
  (ag-rule type)

  (ag-rule pretty-print)

  (ag-rule bindings)
  ;; scope-graphs scope
  (ag-rule scope)
  |#

  (compile-ag-specifications)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

(define StatementChoice
  (class ast-choice%
    (define/override (choice-weight) 1)
    (super-new)))
(define NullStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'NullStatement))
    (super-new)))
(define ExpressionStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'ExpressionStatement (fresh-node 'ExpressionHole)))
    (super-new)))
(define BlockChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'Block
              ;; declarations
              (create-ast-list (list))
              ;; statements
              (create-ast-list (map (λ (x) (fresh-node 'StatementHole))
                                    (make-list (random 5) #f)))))
    (super-new)))
(define ReturnStatementChoice
  (class StatementChoice
    (super-new)))
(define ValueReturnStatementChoice
  (class ReturnStatementChoice
    (define/override (fresh)
      (fresh-node 'ValueReturnStatement (fresh-node 'ExpressionHole)))
    (super-new)))

(define ExpressionChoice
  (class ast-choice%
    (define/override (choice-weight) 1)
    (super-new)))
(define NumberChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'Number (random 100)))
    (super-new)))
(define AdditionExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
      (fresh-node 'AdditionExpression
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (super-new)))

(define statement-choices
  (list (new NullStatementChoice)
        (new ExpressionStatementChoice)
        (new BlockChoice)
        (new ValueReturnStatementChoice)))

(define expression-choices
  (list (new NumberChoice)
        (new AdditionExpressionChoice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (fresh-node type attr-val ...)
  (create-ast spec type (list attr-val ...)))

(define (replace-with-expression n)
  (let ([o (choose-ast expression-choices)])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-statement n)
  (let ([o (choose-ast statement-choices)])
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
                 (else #f))))])
    (perform-rewrites n 'top-down fill-in))
  n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-Prog)
  (fresh-node 'Program
              (create-ast-list '())
              (fresh-node 'FunctionDefinition
                          (fresh-node 'Type "int")
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
