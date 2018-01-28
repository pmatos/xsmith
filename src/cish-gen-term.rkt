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
 racket/set
 racket/match
 racket/math
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
(define nest-step-string (text (make-string nest-step #\space)))
(define lbrace          (char #\{))
(define rbrace          (char #\}))
(define lparen          (char #\())
(define rparen          (char #\)))
(define comma           (char #\,))
(define semi            (char #\;))
(define plus            (char #\+))
(define minus           (char #\-))
(define star            (char #\*))
(define slash           (char #\/))
(define percent         (char #\%))
(define eqsign          (char #\=))
(define greater         (char #\>))
(define less            (char #\<))
(define qmark           (char #\?))
(define colon           (char #\:))
(define comment-start   (text "/*"))
(define comment-end     (text "*/"))

(define do:             (text "do"))
(define else:           (text "else"))
(define for:            (text "for"))
(define if:             (text "if"))
(define return:         (text "return"))
(define while:          (text "while"))

(define (comment d)
  (if (eq? d empty)
      empty
      (hs-append comment-start d comment-end)))

#;(define (v-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            (group (h-append d line))
            (group (h-append (comment pre) line d line)))
        (if (eq? pre empty)
            (group (h-append d line (comment post) line))
            (group (h-append (comment pre) line d line (comment post) line))))))

(define (v-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            d
            (group (h-append (comment pre) line d)))
        (if (eq? pre empty)
            (group (h-append d line (comment post)))
            (group (h-append (comment pre) line d line (comment post)))))))

(define (h-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            d
            (hs-append (comment pre) d))
        (if (eq? pre empty)
            (hs-append d (comment post))
            (hs-append (comment pre) d (comment post))))))

(define (nest-if-not-block n)
  (if (equal? (node-type n) 'Block)
      (att-value 'pretty-print n)
      (nest nest-step (h-append nest-step-string
                                (att-value 'pretty-print n)))))

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

(struct hint
  (weight-multiplier)
  #:transparent)

(define bool-hint (hint 8))
(define block-hint (hint 10))
(define assignment-hint (hint 25))
(define application-hint (hint 25))

#|
TYPES
-----

Types can be:
* #f for completely unconstrained
* (list '-> arg ... result) for function types
* basic-type for normal types
|#

(struct basic-type
  ;; Type name can be false or a type (eg int, float, ...),
  ;; constraints fields are lists.
  ;; Constraints are things like nonzero, constant, etc -- things that aren't part of the type, but that affect things like undefined behavior.
  ;; The constrain-type method should always account for every attribute in the list -- if something can't satisfy every attribute it should be out of the running.
  (name constraints)
  #:transparent)

(define (type-satisfies? given-t constraint-t)
  (match constraint-t
    ;; TODO - arrow types
    ;;        But I don't think I'm every comparing two arrow types directly,
    ;;        just comparing return values and argument types...
    [(basic-type cname cconst)
     (match given-t
       [(basic-type gname gconst)
        (and (or (not cname) (equal? gname cname))
             (andmap (λ (c) (member c gconst)) cconst))]
       [else #f])]
    [(list-rest '-> c-args+ret)
     (match given-t
       [(list-rest '-> g-args+ret)
        (and (equal? (length c-args+ret) (length g-args+ret))
             (map type-satisfies? g-args+ret c-args+ret))]
       [else #f])]
    ;; if there is no constraint, anything goes
    [#f #t]))

(define (function-type? t)
  (and (list? t)
       (not (null? t))
       (eq? (car t) '->)))

(define empty-basic-type (basic-type #f (list)))
(define (specify-type t name)
  (cond [(not t) (basic-type name (list))]
        [(basic-type? t) (struct-copy basic-type t
                                      [name name])]
        [else (error 'specify-type "bad case")]))
(define (constrain-type t constraint)
  (cond [(not t) (basic-type #f (list constraint))]
        [(basic-type? t)
         (struct-copy basic-type t
                      [constraints (cons constraint (basic-type-constraints t))])]
        [else (error 'constrain-type "bad case")]))

(define int-type (specify-type empty-basic-type "int"))
(define float-type (specify-type empty-basic-type "float"))
(define nonzero-type (constrain-type empty-basic-type 'nonzero))
(define nonzero-int-type (constrain-type int-type 'nonzero))
(define nonzero-float-type (constrain-type float-type 'nonzero))

(define (print-debug-highlight pretty-print-node)
  ;; add terminal codes to print in magenta, then switch to default
  (h-append (text "\033[35m")
            pretty-print-node
            (text "\033[0m")))

(struct abstract-value/range (low high))
(define abstract-value/range/top (int/range -inf.0 +inf.0))
(define (nan->+inf v)
  (if (nan? v) +inf.0 v))
(define (nan->-inf v)
  (if (nan? v) -inf.0 v))

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

  (ast-rule 'LoopStatement:Statement->Expression<test-Statement<body)
  (ast-rule 'WhileStatement:LoopStatement->)
  (ast-rule 'DoWhileStatement:LoopStatement->)
  (ast-rule 'ForStatement:LoopStatement->Expression<init-Expression<update)

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

  (ast-rule 'IntOnlyBinaryExpression:BinaryExpression->)
  (ast-rule 'ModulusExpression:IntOnlyBinaryExpression->)

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
              (v-comment
               n
               (vb-concat
                (map (λ (cn) (att-value 'pretty-print cn))
                     (append (ast-children (ast-child 'Declaration* n))
                             (list (ast-child 'main n)))))))]
   [FunctionDefinition
    (λ (n)
      (v-comment
       n
       (h-append
        (text (basic-type-name (ast-child 'typename n)))
        space
        (text (ast-child 'name n))
        lparen
        (h-concat
         (add-between
          (map (λ (fp)
                 (h-append (text (basic-type-name (ast-child 'typename fp)))
                           space
                           (text (ast-child 'name fp))))
               (ast-children (ast-child 'FormalParam* n)))
          (h-append comma space)))
        rparen
        line
        (att-value 'pretty-print (ast-child 'Block n)))))]
   [IfStatement
    (λ (n)
      (v-comment
       n
       (h-append
        (h-append if: space lparen
                  (att-value 'pretty-print (ast-child 'test n))
                  rparen)
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'then n)))))))]
   [IfElseStatement
    (λ (n)
      (v-comment
       n
       (h-append
        (h-append if: space lparen
                  (att-value 'pretty-print (ast-child 'test n))
                  rparen)
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'then n))))
        line
        else:
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'else n)))))))]
   [WhileStatement
    (λ (n)
      (v-comment
       n
       (h-append
        (h-append while: space lparen
                  (att-value 'pretty-print (ast-child 'test n))
                  rparen)
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'body n)))))))]
   [DoWhileStatement
    (λ (n)
      (v-comment
       n
       (h-append
        do:
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'body n))))
        line
        (h-append while: space lparen
                  (att-value 'pretty-print (ast-child 'test n))
                  rparen semi))))]
   [ForStatement
    (λ (n)
      (v-comment
       n
       (h-append
        for: space lparen
        (att-value 'pretty-print (ast-child 'init n))
        semi space
        (att-value 'pretty-print (ast-child 'test n))
        semi space
        (att-value 'pretty-print (ast-child 'update n))
        rparen
        (nest nest-step
              (h-append line
                        (att-value 'pretty-print (ast-child 'body n)))))))]
   [Block
    (λ (n)
      (v-comment
       n
       (h-append
        lbrace
        (nest nest-step
              (h-append
               line
               (v-concat
                (append
                 (map (λ (cn) (att-value 'pretty-print cn))
                      (ast-children (ast-child 'Declaration* n)))
                 (map (λ (cn) (att-value 'pretty-print cn))
                      (ast-children (ast-child 'Statement* n)))))))
        line
        rbrace)))]
   [ExpressionStatement
    (λ (n)
      (v-comment
       n
       (h-append (att-value 'pretty-print (ast-child 3 n))
                 semi)))]
   [ValueReturnStatement
    (λ (n)
      (v-comment
       n
       (h-append return: space
                 (att-value 'pretty-print (ast-child 3 n))
                 semi)))]
   [NullStatement
    (λ (n)
      (v-comment
       n
       (h-append semi)))]
   [VariableDeclaration
    (λ (n)
      (v-comment
       n
       (h-append (hs-append
                  (text (basic-type-name (ast-child 'typename n)))
                  (text (ast-child 'name n))
                  eqsign
                  (att-value 'pretty-print (ast-child 'Expression n)))
                 semi)))]
   [AssignmentExpression
    (λ (n)
      (v-comment
       n
       (hs-append (text (ast-child 'name n))
                  eqsign
                  (att-value 'pretty-print (ast-child 'Expression n)))))]
   [LiteralInt
    (λ (n)
      (h-comment
       n
       (text (number->string (ast-child 'val n)))))]
   [LiteralFloat
    (λ (n)
      (h-comment
       n
       (text (number->string (ast-child 'val n)))))]
   [VariableReference
    (λ (n)
      (h-comment
       n
       (text (ast-child 'name n))))]
   [FunctionApplicationExpression
    (λ (n)
      (h-comment
       n
       (h-append
        (text (ast-child 'name n))
        lparen
        (h-concat
         (add-between (map (λ (a) (att-value 'pretty-print a))
                           (ast-children (ast-child 'Expression* n)))
                      (h-append comma space)))
        rparen)))]
   [IfExpression
    (λ (n)
      (h-comment
       n
       (h-append lparen
                 (hs-append (att-value 'pretty-print (ast-child 'test n))
                            qmark
                            (att-value 'pretty-print (ast-child 'then n))
                            colon
                            (att-value 'pretty-print (ast-child 'else n)))
                 rparen)))]
   [BinaryExpression
    (λ (n)
      (h-comment
       n
       (h-append lparen
                 (hs-append (att-value 'pretty-print (ast-child 'l n))
                            (att-value 'pretty-print-op n)
                            (att-value 'pretty-print (ast-child 'r n)))
                 rparen)))]
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
   [AssignmentExpression
    (λ (n) (cons (ast-child 'name n)
                 (att-value 'illegal-variable-names (parent-node n))))]
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
   [IfStatement (λ (n) (hasheq (ast-child 'test n) #f))]
   [LoopStatement (λ (n) (hasheq (ast-child 'test n) #f))]
   [ForStatement (λ (n) (hasheq (ast-child 'test n) #f
                                (ast-child 'init n) #f
                                (ast-child 'update n) #f))]
   [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n)
                                       (ast-child 'typename n)))]
   [AssignmentExpression
    (λ (n) (hasheq (ast-child 'Expression n)
                   (hash-ref (binding-bound
                              (resolve-reference
                               (reference (ast-child 'name n)
                                          (att-value 'scope-graph-scope n))))
                             'type)))]
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
   [IntOnlyBinaryExpression (λ (n) (let* ([t (att-value 'type-context n)]
                                          [t (cond [(not t) int-type]
                                                   [(basic-type? t)
                                                    (specify-type t "int")])])
                                     (hasheq (ast-child 'l n) t
                                             (ast-child 'r n) t)))]
   [ComparisonExpression (λ (n) (let ([t (fresh-var-type)])
                                  (hasheq (ast-child 'l n) t
                                          (ast-child 'r n) t)))]
   [IfExpression (λ (n) (let ([t (or (att-value 'type-context n) (fresh-var-type))])
                          (hasheq (ast-child 'test n) #f
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
                     (hasheq (att-value 'block-last-statement n) #t)
                     (hasheq)))]
   [IfElseStatement (λ (n) (if (att-value 'in-return-position? n)
                               (hasheq (ast-child 'then n) #t
                                       (ast-child 'else n) #t)
                               (hasheq)))]
   [Statement (λ (n) (hasheq))]
   [FunctionDefinition (λ (n) (hasheq (ast-child 'Block n) #t))]
   [Node (λ (n) (error 'children-return-position-dict "no default ag-rule"))])
  (ag-rule
   in-return-position?
   [Statement (λ (n) (let ([rp-dict (att-value 'children-return-position-dict
                                               (parent-node n))])
                       (dict-ref rp-dict n #f)))]
   [Node (λ (n) #f)])

  (ag-rule
   children-hint-dict
   ;; dictionary will contain a list of hints (or nothing) for each child
   [IfExpression (λ (n) (hasheq (ast-child 'test n) (list bool-hint)))]
   [IfStatement (λ (n) (hasheq (ast-child 'test n) (list bool-hint)
                               (ast-child 'then n) (list block-hint)))]
   [LoopStatement (λ (n) (hasheq (ast-child 'test n) (list bool-hint)
                                 (ast-child 'body n) (list block-hint)))]
   [ExpressionStatement
    (λ (n) (hasheq (ast-child 'Expression n)
                   (list assignment-hint application-hint)))]
   [Node (λ (n) (hasheq))])

  (ag-rule hints
           [Node (λ (n) (dict-ref (att-value 'children-hint-dict (ast-parent n))
                                  n
                                  '()))])

  (ag-rule
   children-misc-constraint-dict
   ;; dictionary will contain a set of constraints (or nothing) for each child
   [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n) (list 'constant)))]
   [Node (λ (n) (hasheq))])
  (define (misc-constraint-dict-ref n)
    (dict-ref (att-value 'children-misc-constraint-dict
                         (ast-parent n))
              n
              '()))
  (define (default-misc-constraints n)
    (set-union (misc-constraint-dict-ref n)
               (att-value 'misc-constraints (ast-parent n))))
  (ag-rule
   misc-constraints
   ;; misc-constraints returns a set of symbols
   [Program (λ (n) '())]
   [ExpressionStatement
    (λ (n) (set-subtract
            (default-misc-constraints n)
            ;; allow assignments here
            '(no-assignment)))]
   [ExpressionHole (λ (n) (default-misc-constraints n))]
   [DeclarationHole (λ (n) (default-misc-constraints n))]
   [StatementHole (λ (n) (default-misc-constraints n))]
   [Node (λ (n) (set-union
                 ;; Don't allow assignment except where explicitly allowed
                 '(no-assignment)
                 (default-misc-constraints n)))])


  (define ({abstract-binary-op/range op} node store)
    ;; op is a function of (l-l l-h r-l r-h -> (list low high))
    (match-let* ([(list val-l sto-l) (att-value 'abstract-interp-do/range
                                                (ast-child 'l n))]
                 [(list val-r sto-r) (att-value 'abstract-interp-do/range
                                                (ast-child 'r n))])
      (match val-l
        [(abstract-value/range l-low l-high)
         (match val-r
           [(abstract-value/range r-low r-high)
            (match (op l-low l-high r-low r-high)
              [(list low high)
               (list (abstract-value/range (nan->-inf low) (nan->+inf high))
                     sto-r)]
              [else abstract-value/range/top])]
           [else abstract-value/range/top])]
        [else abstract-value/range/top])))
  (define range-store-top (hash))

  (ag-rule
   abstract-interp/range
   ;; Get the single global result of abstract interpretation of this node.
   [Node (λ (n) (error 'abstract-interp/range "no default ag-rule"))])
  (ag-rule
   abstract-interp-get-store-for-child/range
   ;; Takes a (parent) node, and the child that the store is wanted for.
   [Node (λ (n child) (error 'abstract-interp-get-store-for-child/range
                             "no default ag-rule"))])
  (ag-rule
   ;; For implementing abstract-interp/range.
   ;; For now, store is table from binding to abstract value.
   ;; Returns (list abstract-value new-store)
   ;; Note - For functions and operators, argument evaluation order is unspecified.
   ;;        So the store coming out of each side should be the same.
   ;;        This should be enforced by disallowing assignment in these places.
   abstract-interp-do/range

   ;; TODO !!! store -- I need some sort of abstract store, and anywhere I am punting to top without interpreting sub-children I need to make the whole store go to top, because there could be assignment there...

   ;;; Program
   ;;; TODO

   ;;; Statements
   #|
   TODO
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

   (ast-rule 'LoopStatement:Statement->Expression<test-Statement<body)
   (ast-rule 'WhileStatement:LoopStatement->)
   (ast-rule 'DoWhileStatement:LoopStatement->)
   (ast-rule 'ForStatement:LoopStatement->Expression<init-Expression<update)
   |#

   ;;; Expressions
   #|
   TODO
   (ast-rule 'ExpressionHole:Expression->)

   (ast-rule 'AssignmentExpression:Expression->name-Expression)
   (ast-rule 'FunctionApplicationExpression:Expression->name-Expression*)

   (ast-rule 'FunctionCall:Expression->name-ArgumentList)
   |#

   [LiteralInt
    (λ (n store)
      (list (abstract-value/range (ast-child 'val n) (ast-child 'val n)) store))]
   [LiteralFloat
    (λ (n store)
      (list (abstract-value/range (ast-child 'val n) (ast-child 'val n)) store))]

   [IfExpression
    (λ (n store)
      (match-let ([(list (abstract-value/range low high)
                         new-store)
                   (att-value 'abstract-interp-do/range (ast-child 'test n) store)])
        (cond [(or (and (< 0 low) (< 0 high))
                   (and (> 0 low) (> 0 high)))
               (att-value 'abstract-interp-do/range (ast-child 'then n) new-store)]
              [(and (equal? 0 low) (equal? 0 high))
               (att-value 'abstract-interp-do/range (ast-child 'else n) new-store)]
              [else
               (list abstract-value/range/top range-store-top)])))]

   [VariableReference
    (λ (n store)
      (let ([ref-node (resolve-reference
                       (reference (ast-child 'name n)
                                  (att-value 'scope-graph-scope n)))])
        ;; TODO -- I'm using an empty hash to represent an unknown state of the store,
        ;; or at least an unknown state for a variable.  But once there are more than
        ;; ints and floats I'll need to look at the type of the reference to know what
        ;; kind of value to use for top.
        (list (dict-ref store ref-node abstract-value/range/top)
              store)))]

   [AdditionExpression
    {abstract-binary-op/range
     (λ (l-l l-h r-l r-h)
       (list (+ l-l r-l) (+ l-h r-h)))}]
   [SubtractionExpression
    {abstract-binary-op/range
     (λ (l-l l-h r-l r-h)
       (list (- l-l r-h) (- l-h r-l)))}]
   [MultiplicationExpression
    {abstract-binary-op/range
     (λ (l-l l-h r-l r-h)
       (let ([signl (if (equal? (negative? l-l) (negative? l-h))
                        (if (negative? l-l) '- '+)
                        'both)]
             [signr (if (equal? (negative? r-l) (negative? r-h))
                        (if (negative? r-l) '- '+)
                        'both)])
         (match (list signl signr)
           ['(+ +) (list (* l-l r-l) (* l-h r-h))]
           ['(+ both) (list (* l-h r-l) (* l-h r-h))]
           ['(+ -) (list (* l-h r-l) (* l-l r-h))]
           ['(both +) (list (* l-l r-h) (* l-h r-h))]
           ['(both both) (list (min (* l-l r-h) (* l-h r-l))
                               (max (* l-l r-l) (* l-h r-h)))]
           ['(both -) (list (* l-h r-l) (* l-l r-l))]
           ['(- +) (list (* l-l r-h) (* l-h r-l))]
           ['(- both) (list (* l-l r-h) (* l-l r-l))]
           ['(- -) (list (* l-h r-h) (* l-l r-l))])))}]
   ;; TODO - make a real transfer function
   [DivisionExpression {abstract-binary-op/range (λ args abstract-value/range/top)}]
   [ModulusExpression {abstract-binary-op/range (λ args abstract-value/range/top)}]

   [EqualityExpression
    {abstract-binary-op/range
     (λ args
       (let ([equal-val (foldl (λ (l r) (and (equal? l r) r))
                               (car args) (cdr args))])
         (if equal-val (list equal-val equal-val) (list 0 1))))}]
   ;; TODO -- better transfer functions for < > <= >=
   ;; A default comparison result -- it is always 0 or 1.
   [ComparisonExpression {abstract-binary-op/range (λ args (list 0 1))}]

   [Node (λ (n store) (error 'abstract-interp-do/range "no default ag-rule"))])

  (compile-ag-specifications)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

(define cish-ast-choice%
  (class ast-choice%
    (define/public (misc-constraints)
      this)
    (define/override (choice-weight) 10)
    (define/public (features-enabled)
      (let ((disabled (xsmith-option 'features-disabled)))
        (if (ormap (λ (f) (dict-ref disabled f #f))
                   (send this features))
            #f
            this)))
    (define/public (wont-over-deepen)
      (if (<= (att-value 'ast-depth current-hole) (xsmith-option 'max-depth))
          this
          #f))
    (define/public (constrain-type)
      this)
    (define/public (top-level-declaration-at-top-level)
      this)
    (define/public (respect-return-position)
      this)
    (super-new)))

(define-syntax (hinted-choice-weight stx)
  (syntax-parse stx
    [(_ hint-name)
     #'(define/override (choice-weight)
         (if (member hint-name (att-value 'hints current-hole))
             (* (hint-weight-multiplier hint-name)
                (super choice-weight))
             (super choice-weight)))]))

(define StatementChoice
  (class cish-ast-choice%
    (define/override (respect-return-position)
      (and (not (att-value 'in-return-position? current-hole))
           this))
    (super-new)))
(define NullStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'NullStatement))
    (define/override (features) '(null))
    (define/override (wont-over-deepen)
      this)
    (define/override (choice-weight) 2)
    (super-new)))
(define ExpressionStatementChoice
  (class StatementChoice
    (define/override (fresh)
      (fresh-node 'ExpressionStatement (fresh-node 'ExpressionHole)))
    (define/override (wont-over-deepen)
      this)
    (super-new)))
(define IfStatementChoice
  (class StatementChoice
    (define/override (features) '(if-statement))
    (define/override (fresh)
      (fresh-node 'IfStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define IfElseStatementChoice
  (class IfStatementChoice
    (define/override (fresh)
      (fresh-node 'IfElseStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-block-hole)
                  (fresh-block-hole)))
    (define/override (respect-return-position)
      this)
    (super-new)))
(define LoopStatementChoice
  (class StatementChoice
    (define/override (features) '(loop))
    (super-new)))
(define WhileStatementChoice
  (class LoopStatementChoice
    (define/override (fresh)
      (fresh-node 'WhileStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define DoWhileStatementChoice
  (class LoopStatementChoice
    (define/override (fresh)
      (fresh-node 'DoWhileStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define ForStatementChoice
  (class LoopStatementChoice
    (define/override (fresh)
      (fresh-node 'ForStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)
                  ;; init, update
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'ExpressionHole)))
    (super-new)))
(define BlockChoice
  (class StatementChoice
    (hinted-choice-weight block-hint)
    (define/override (fresh)
      (fresh-node 'Block
                  ;; declarations
                  (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                           "standin-name"))
                                        (make-list (random 3) #f)))
                  ;; statements
                  (create-ast-list
                   (map (λ (x) (fresh-node 'StatementHole))
                        (let ([l (make-list (random 5) #f)])
                          (if (att-value 'in-return-position? current-hole)
                              ;; don't allow an empty block in return position
                              (cons #f l)
                              l))))))
    (define/override (respect-return-position)
      this)
    (super-new)))
(define ReturnStatementChoice
  (class StatementChoice
    (define/override (choice-weight) 2)
    (define/override (wont-over-deepen)
      this)
    (define/override (respect-return-position)
      this)
    (super-new)))
(define ValueReturnStatementChoice
  (class ReturnStatementChoice
    (define/override (choice-weight) 2)
    (define/override (fresh)
      (fresh-node 'ValueReturnStatement (fresh-node 'ExpressionHole)))
    (define/override (respect-return-position)
      this)
    (super-new)))

(define ExpressionChoice
  (class cish-ast-choice%
    (super-new)))

(define-syntax (define-basic-literal-choice stx)
  (syntax-parse stx
    ;; btype is the basic type that it satisfies
    ;; generator-e is the expression to generate a value
    ;; if-zero-generator-e is used if nonzero is required and the first generator gives 0
    [(_ nodename btype feature generator-e if-zero-generator-e)
     #:with choicename (format-id #'nodename "~aChoice" #'nodename)
     #'(define choicename
         (class ExpressionChoice
           (define/override (fresh)
             (let* ([t (att-value 'type-context current-hole)]
                    [v1 generator-e]
                    [constraints (if (basic-type? t) (basic-type-constraints t) '())]
                    [v (if (and (member 'nonzero constraints)
                                (equal? 0 v1))
                           if-zero-generator-e
                           v1)])
               (fresh-node 'nodename v)))
           (define/override (features) '(feature))
           (define/override (wont-over-deepen)
             this)
           (define/override (constrain-type)
             (let ([t (att-value 'type-context current-hole)])
               ;; This isn't necessarily nonzero, but it will be if needed.
               (and (type-satisfies? btype t) this)))
           (super-new)))]))
(define-basic-literal-choice LiteralInt nonzero-int-type int
  (* (random 100)
     (if (equal? 0 (random 1))
         1
         -1))
  (+ 1 (random 10)))
(define-basic-literal-choice LiteralFloat nonzero-float-type float
  (* (random) (random 10))
  (+ .1 (random)))

(define AssignmentExpressionChoice
  (class ExpressionChoice
    (define ref-choices-filtered #f)
    (hinted-choice-weight assignment-hint)
    (define/override (fresh)
      (fresh-node 'AssignmentExpression
                  (binding-name (random-ref ref-choices-filtered))
                  (fresh-node 'ExpressionHole)))
    (define/override (misc-constraints)
      (and (set-empty? (set-intersect '(constant no-assignment)
                                      (att-value 'misc-constraints current-hole)))
           this))
    (define/override (constrain-type)
      (define visibles (att-value 'visible-bindings current-hole))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names current-hole))))
                visibles))
      (define type-needed (att-value 'type-context current-hole))
      (define not-functions (filter (λ (b) (not (function-type?
                                                 (dict-ref (binding-bound b) 'type))))
                                    legal-refs))
      (define legal-with-type
        (if type-needed
            (filter (λ (b) (type-satisfies? (dict-ref (binding-bound b) 'type)
                                            type-needed))
                    not-functions)
            not-functions))
      (set! ref-choices-filtered legal-with-type)
      (and (not (null? legal-with-type)) this))
    (super-new)))
(define VariableReferenceChoice
  (class ExpressionChoice
    (define ref-choices-filtered #f)
    (define (choice-weight) 15)
    (define/override (fresh)
      (fresh-node 'VariableReference (binding-name (random-ref ref-choices-filtered))))
    (define/override (wont-over-deepen)
      this)
    (define/override (constrain-type)
      (define visibles (att-value 'visible-bindings current-hole))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names current-hole))))
                visibles))
      (define type-needed (att-value 'type-context current-hole))
      (define legal-with-type
        (if type-needed
            (filter (λ (b) (type-satisfies? (dict-ref (binding-bound b) 'type)
                                            type-needed))
                    legal-refs)
            legal-refs))
      (set! ref-choices-filtered legal-with-type)
      (and (not (null? legal-with-type)) this))
    (super-new)))
(define FunctionApplicationExpressionChoice
  (class ExpressionChoice
    (define ref-choices-filtered #f)
    (hinted-choice-weight application-hint)
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
    (define/override (constrain-type)
      (define visibles (filter (λ (b) (function-type?
                                       (dict-ref (binding-bound b) 'type)))
                               (att-value 'visible-bindings current-hole)))
      (define legal-refs
        (filter (λ (b) (not (member (binding-name b)
                                    (att-value 'illegal-variable-names current-hole))))
                visibles))
      (define type-needed (att-value 'type-context current-hole))
      (define legal-with-type
        (if type-needed
            (filter (λ (b) (type-satisfies?
                            (car (reverse (dict-ref (binding-bound b) 'type)))
                            type-needed))
                    legal-refs)
            legal-refs))
      (set! ref-choices-filtered legal-with-type)
      (and (not (null? legal-with-type)) this))
    (super-new)))
(define IfExpressionChoice
  (class ExpressionChoice
    (define/override (fresh)
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
        output-type ;; type the operator returns, or #f if it returns its input type
        bool-like ;; #t if the operator returns something bool-y, otherwise #f
        )
     #:with choicename (format-id #'nodename "~aChoice" #'nodename)
     #'(define choicename
         (class ExpressionChoice
           (define/override (fresh)
             (fresh-node 'nodename
                         (fresh-node 'ExpressionHole)
                         (fresh-node 'ExpressionHole)))
           (define/override (features) '(feature))
           (define/override (choice-weight)
             (if (and bool-like (member bool-hint (att-value 'hints current-hole)))
                 (* (hint-weight-multiplier bool-hint)
                    (super choice-weight))
                 (super choice-weight)))
           (define/override (constrain-type)
             (let ([t (att-value 'type-context current-hole)])
               (cond [(if output-type
                          (type-satisfies? output-type t)
                          (ormap (λ (avail-type)
                                   (type-satisfies? avail-type t))
                                 input-typelist))
                      this]
                     [else #f])))
           (super-new)))]))
(define-binary-op-choice AdditionExpression addition
  (list int-type float-type) #f #f)
(define-binary-op-choice MultiplicationExpression multiplication
  (list int-type float-type) #f #f)
(define-binary-op-choice SubtractionExpression subtraction
  (list int-type float-type) #f #f)
(define-binary-op-choice DivisionExpression division
  (list int-type float-type) #f #f)

(define-binary-op-choice ModulusExpression modulus
  (list int-type) #f #f)

(define-binary-op-choice EqualityExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice LessThanExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice GreaterThanExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice LessOrEqualExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice GreaterOrEqualExpression comparisons
  (list int-type float-type) int-type #t)

(define DeclarationChoice
  (class cish-ast-choice%
    (super-new)))
(define VariableDeclarationChoice
  (class DeclarationChoice
    (define/override (wont-over-deepen)
      this)
    (define/override (fresh)
      (fresh-node 'VariableDeclaration
                  (fresh-var-name)
                  (fresh-var-type)
                  (fresh-node 'ExpressionHole)))
    (super-new)))
(define FunctionDefinitionChoice
  (class DeclarationChoice
    (define/override (top-level-declaration-at-top-level)
      (if (att-value 'at-top-level? current-hole)
          this
          #f))
    (define/override (fresh)
      (define p (parent-node current-hole))
      (define main? (and (eq? (node-type p) 'Program)
                         (eq? (ast-child 'main p) current-hole)))
      (fresh-node 'FunctionDefinition
                  (if main? "main" (fresh-var-name "func"))
                  (if main? int-type (fresh-var-type))
                  ;; parameters
                  (if main?
                      (create-ast-list '())
                      (create-ast-list (map (λ (x) (fresh-node 'FormalParam
                                                               (fresh-var-type)
                                                               (fresh-var-name)))
                                            (make-list (random 5) #f))))
                  (fresh-block-hole)))
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
           int-type]
          [(dict-ref disabled 'int #f)
           float-type]
          [else
           (random-ref (list int-type float-type))])))

(define (statement-choices hole)
  (list (new NullStatementChoice [hole hole])
        (new ExpressionStatementChoice [hole hole])
        (new BlockChoice [hole hole])
        (new IfStatementChoice [hole hole])
        (new IfElseStatementChoice [hole hole])
        ;; TODO - loop statements need to have some analysis so they at least sometimes terminate...
        (new WhileStatementChoice [hole hole])
        (new DoWhileStatementChoice [hole hole])
        (new ForStatementChoice [hole hole])
        (new ValueReturnStatementChoice [hole hole])))

(define (expression-choices hole)
  (list (new LiteralIntChoice [hole hole])
        (new LiteralFloatChoice [hole hole])
        (new FunctionApplicationExpressionChoice [hole hole])
        (new AdditionExpressionChoice [hole hole])
        (new SubtractionExpressionChoice [hole hole])
        (new MultiplicationExpressionChoice [hole hole])
        (new DivisionExpressionChoice [hole hole])
        (new ModulusExpressionChoice [hole hole])
        (new VariableReferenceChoice [hole hole])
        (new EqualityExpressionChoice [hole hole])
        (new LessThanExpressionChoice [hole hole])
        (new GreaterThanExpressionChoice [hole hole])
        (new LessOrEqualExpressionChoice [hole hole])
        (new GreaterOrEqualExpressionChoice [hole hole])
        (new IfExpressionChoice [hole hole])
        (new AssignmentExpressionChoice [hole hole])
        ))

(define (declaration-choices hole)
  (list (new VariableDeclarationChoice [hole hole])
        (new FunctionDefinitionChoice [hole hole])))

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

(define (apply-choice-filters choice-list)
  (filter (λ (choice) (maybe-send+ choice
                                   (features-enabled)
                                   (top-level-declaration-at-top-level)
                                   (wont-over-deepen)
                                   (respect-return-position)
                                   (misc-constraints)
                                   (constrain-type)))
          choice-list))

(define (replace-with-expression n)
  (let ([o (choose-ast (apply-choice-filters (expression-choices n)))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-statement n)
  (let ([o (choose-ast (apply-choice-filters (statement-choices n)))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-declaration n)
  (let ([o (choose-ast (apply-choice-filters (declaration-choices n)))])
    (rewrite-subtree n (send o fresh))))

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
                 ((DeclarationHole)
                  (replace-with-declaration n)
                  #t)
                 ((BlockHole)
                  (rewrite-subtree n (send (new BlockChoice [hole n]) fresh))
                  #t)
                 ((FunctionDefinitionHole)
                  (rewrite-subtree n (send (new FunctionDefinitionChoice [hole n])
                                           fresh))
                  #t)
                 (else #f))))])
    (perform-rewrites n 'top-down fill-in))
  n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-block-hole)
  (fresh-node 'BlockHole
              (create-ast-list '())
              (create-ast-list '())))

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
                            (fresh-block-hole))))
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
