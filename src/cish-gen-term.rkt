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
(require racket/random)
(require "xsmith-options.rkt")
(provide do-it)

(define spec (create-specification))

(define term-choice-table
  ;; Choose any Term production.
  (make-choice-table '((Num 1) (Sum 1) (Subtraction 1) (Ref 1))))
(define term-choice-table/no-ref
  ;; Choose any Term production.
  (make-choice-table '((Num 1) (Sum 1) (Subtraction 1))))
(define term-atoms-choice-table
  ;; Choose an "atomic" Term production.
  (make-choice-table '((Num 1) (Ref 1))))
(define term-atoms-choice-table/no-ref
  ;; Choose an "atomic" Term production.
  (make-choice-table '((Num 1))))
(define stmt-choice-table
  ;; Choose any Stmt production.
  (make-choice-table '((Eval 1) (Block 1) (Let 1))))
(define stmt-atoms-choice-table
  ;; Choose an "atomic" Stmt production.
  (make-choice-table '((Eval 1))))

(define statement-choice-table
  (make-choice-table '((Block 1) (Return 1) (Expression 1) (Null 1))))
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
   [ValueReturnStatement
    (λ (n) (h-append (text "return ")
                     (att-value 'pretty-print (ast-child 1 n))
                     (text ";")))]
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

  #|
  (ag-rule value
           (Num (lambda (n) (ast-child 'val n)))
           (Ref (lambda (n) (let ((def (att-value 'def n (ast-child 'name n))))
                              (if def
                                  (ast-child 'val def)
                                  -1))))
           (Sum (lambda (n) (+ (att-value 'value (ast-child 'left n))
                               (att-value 'value (ast-child 'right n)))))
           (Subtraction (lambda (n) (- (att-value 'value (ast-child 'left n))
                                       (att-value 'value (ast-child 'right n)))))
           )

  (ag-rule def
           (Prog (lambda (n name) #f)) ;; the not-defined error
           (Stmt (lambda (n name) (att-value 'def (ast-parent n) name)))
           (Let (lambda (n name) (if (eq? (ast-child 'name n) name)
                                     n
                                     (att-value 'def (ast-parent n) name))))
           (Term (lambda (n name) (att-value 'def (ast-parent n) name)))
           )

  (ag-rule names-available
           (Prog (λ (n) '()))
           (Let (λ (n) (cons (ast-child 'name n)
                             (att-value 'names-available (ast-parent n)))))
           (Stmt (λ (n) (att-value 'names-available (ast-parent n))))
           (Term (λ (n) (att-value 'names-available (ast-parent n))))
           )

  (ag-rule level
           (Prog (lambda (n) 0))
           (Stmt (lambda (n) (+ 1 (att-value 'level (ast-parent n)))))
           (Term (lambda (n) (+ 1 (att-value 'level (ast-parent n))))))

  (ag-rule choice-table
           (TermHole (lambda (n)
                       (let ([too-deep? (> (att-value 'level n)
                                           (xsmith-option 'max-depth))]
                             [no-names? (null? (att-value 'names-available n))])
                         (cond
                           [(and too-deep? no-names?) term-atoms-choice-table/no-ref]
                           [no-names? term-choice-table/no-ref]
                           [too-deep? term-atoms-choice-table]
                           [else term-choice-table]))))
           (StmtHole (lambda (n)
                       (if (> (att-value 'level n) (xsmith-option 'max-depth))
                           stmt-atoms-choice-table
                           stmt-choice-table))))

  (ag-rule pp
           (Prog (lambda (n) (att-value 'pp (ast-child 1 n))))
           (Let (lambda (n) (list 'let
                                  (ast-child 'name n)
                                  (ast-child 'val n)
                                  (att-value 'pp (ast-child 'body n)))))
           (Eval (lambda (n) (list 'eval (att-value 'pp (ast-child 1 n)))))
           (Block (lambda (n) (list 'block
                                    (att-value 'pp (ast-child 'first n))
                                    (att-value 'pp (ast-child 'second n)))))
           (Num (lambda (n) (ast-child 'val n)))
           (Ref (lambda (n) (ast-child 'name n)))
           (Sum (lambda (n) (list '+
                                  (att-value 'pp (ast-child 'left n))
                                  (att-value 'pp (ast-child 'right n)))))
           (Subtraction (lambda (n) (list '-
                                          (att-value 'pp (ast-child 'left n))
                                          (att-value 'pp (ast-child 'right n))))))

  (ag-rule ppdoc
           (Prog (lambda (n) (att-value 'ppdoc (ast-child 1 n))))
           (Let (lambda (n) (v-append
                             (nest nest-step
                                   (v-append
                                    lbrace
                                    (h-append
                                     (hs-append
                                      (text
                                       (symbol->string (ast-child 'name n)))
                                      eqsign
                                      (text
                                       (number->string (ast-child 'val n))))
                                     semi)
                                    (att-value 'ppdoc (ast-child 'body n))))
                             rbrace)))
           (Eval (lambda (n) (h-append
                              (att-value 'ppdoc (ast-child 1 n))
                              semi)))
           (Block (lambda (n) (v-append
                               (nest nest-step
                                     (v-append
                                      lbrace
                                      (att-value 'ppdoc (ast-child 'first n))
                                      (att-value 'ppdoc (ast-child 'second n))
                                      ))
                               rbrace)))
           (Num (lambda (n) (text (number->string (ast-child 'val n)))))
           (Ref (lambda (n) (text (symbol->string (ast-child 'name n)))))
           (Sum (lambda (n) (h-append
                             lparen
                             (hs-append
                              (vs-append
                               (att-value 'ppdoc (ast-child 'left n))
                               plus
                               (att-value 'ppdoc (ast-child 'right n))))
                             rparen
                             )))
           (Subtraction (lambda (n) (h-append
                                     lparen
                                     (hs-append
                                      (vs-append
                                       (att-value 'ppdoc (ast-child 'left n))
                                       minus
                                       (att-value 'ppdoc (ast-child 'right n))))
                                     rparen
                                     ))))

  (compile-ag-specifications)
  |#
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (fresh-node type attr-val ...)
  (create-ast spec type (list attr-val ...)))

(define (fresh-Num)
  (fresh-node 'Num (random 10)))
(define (fresh-Sum)
  (fresh-node 'Sum
              (fresh-TermHole)
              (fresh-TermHole)))
(define (fresh-Subtraction)
  (fresh-node 'Subtraction
              (fresh-TermHole)
              (fresh-TermHole)))
(define (fresh-Ref var)
  (fresh-node 'Ref var))
(define (fresh-TermHole)
  (fresh-node 'TermHole))

(define (replace-with-term n)
  (let ((c (choose
            #;(att-value 'choice-table n)
            expression-choice-table
                   )))
    (case c
      [(Number)
       ;; Replace with a Num.
       (rewrite-subtree n (fresh-node 'Number 1))]
      [(AdditionExpression)
       (rewrite-subtree n (fresh-node 'AdditionExpression
                                      (fresh-node 'ExpressionHole)
                                      (fresh-node 'ExpressionHole)))]
      [else
       (error 'replace-with-term "invalid choice ~a" c)]
      )))

(define (fresh-Eval)
  (fresh-node 'Eval (fresh-TermHole)))
(define (fresh-Block)
  (fresh-node 'Block
              ;; declarations
              (create-ast-list (list))
              ;; statements
              (create-ast-list (list (fresh-node 'StatementHole)))))
(define (fresh-Let)
  (fresh-node 'Let
              (random-ref '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
              (random 10)
              (fresh-StmtHole)))
(define (fresh-StmtHole)
  (fresh-node 'StmtHole))

#;(define (replace-with-stmt n)
  (let ((c (choose (att-value 'choice-table n))))
    (case c
      ((Eval)
       ;; Replace with an Eval.
       (rewrite-subtree n (fresh-Eval)))
      ((Block)
       ;; Replace with a Block.
       (rewrite-subtree n (fresh-Block)))
      ((Let)
       ;; Replace with a Let.
       (rewrite-subtree n (fresh-Let)))
      (else
       (error 'replace-with-stmt "invalid choice ~a" c))
      )))

(define (generate-random-prog n)
  (let ([fill-in
         (λ (n)
           (if (ast-list-node? n)
               #f
               (case (ast-node-type n)
                 ((ExpressionHole)
                  (replace-with-term n)
                  #t)
                 #;((StatementHole)
                    (replace-with-stmt n)
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
                                       (list
                                        (fresh-node
                                         'ValueReturnStatement
                                         (fresh-node 'ExpressionHole))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How to reflect on the grammar.  (But probably we do not need to do this.)
;;
;; (map symbol->name
;;   (ast-rule->production (specification->find-ast-rule spec 'Block)))
;; => '(Block Stmt Stmt)
;; (map symbol->context-name
;;   (ast-rule->production (specification->find-ast-rule spec 'Block)))
;; => '(Block first second)
;; (map symbol->non-terminal?
;;   (ast-rule->production (specification->find-ast-rule spec 'Block)))
;; => '(#t #<ast-rule> #<ast-rule>)
;;
;; The above, for 'Let:
;; => '(Let name val Stmt)
;; => '(Let name val body)
;; => '(#t #f #f #<ast-rule>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
