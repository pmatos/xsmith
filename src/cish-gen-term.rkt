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
(define block-hint (hint 50))
(define assignment-hint (hint 70))
(define application-hint (hint 50))

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

;; TODO - I need one of these for each int type, when there are multiple
;; TODO - I should also use the actual values that these will take...
(define INT_MIN -10001)
(define INT_MAX 10000)

(struct abstract-value/range
  (low high)
  #:transparent)
(define abstract-value/range/top (abstract-value/range -inf.0 +inf.0))
(define range-store-top (hash))
(define (nan->+inf v)
  (if (nan? v) +inf.0 v))
(define (nan->-inf v)
  (if (nan? v) -inf.0 v))

(define (abstract-value-merge/range a b)
  (match-let ([(abstract-value/range a-l a-h) a]
              [(abstract-value/range b-l b-h) b])
    (abstract-value/range (min a-l b-l) (max a-h b-h))))
(define ({abstract-store-merge value-merger key->top-value} a b)
  (for/hash ([key (remove-duplicates (append (dict-keys a) (dict-keys b)))])
    (let ([top (key->top-value key)])
      (values key (value-merger (dict-ref a key top)
                                (dict-ref b key top))))))

(struct abstract-flow-control-return
  #|
  * maybes is a list of potential returns (corresponding to places a return
    statement may or may not execute)
  * must is a single return (corresponding to a place where a return always executes)
  |#
  (maybes must) #:transparent)
(define empty-abstract-flow-control-return (abstract-flow-control-return '() #f))
(define ({abstract-flow-control-return->val-store-list val-merge* store-merge*} r)
  (match-let* ([(abstract-flow-control-return maybes must) r]
               [(list (list v s) ...) (if must (cons must maybes) maybes)])
    (list (apply val-merge* v) (apply store-merge* s))))


(define (maybe-return returns val store)
  (match returns
    [(abstract-flow-control-return maybes must)
     (abstract-flow-control-return (cons (list val store) maybes) must)]))
(define (must-return returns val store)
  (match returns
    [(abstract-flow-control-return maybes must)
     (abstract-flow-control-return maybes (list val store))]))

(define (abstract-flow-control-return-merge a b)
  (match a
    [(abstract-flow-control-return a-maybes a-must)
     (match b
       [(abstract-flow-control-return b-maybes b-must)
        (if (and a-must b-must)
            (abstract-flow-control-return (append (list a-must) a-maybes b-maybes)
                                          b-must)
            (abstract-flow-control-return (append a-maybes b-maybes)
                                          (or a-must b-must)))])]))

(define (abstract-flow-control-return-only-maybe-ify r)
  (match r
    [(abstract-flow-control-return maybes must)
     (if must
         (abstract-flow-control-return (cons must maybes) #f)
         r)]))

(define ({merge-*-ify merge-func} . args)
  (cond [(empty? args) (error 'merge-*-ify "this shouldn't happen -- merge-*-ify client got no arguments")]
        [else (foldl merge-func (first args) (rest args))]))

(define {abstract-store-merge* value-merger key->top-value}
  {merge-*-ify {abstract-store-merge value-merger key->top-value}})

(define abstract-store-merge*/range
  {abstract-store-merge* abstract-value-merge/range
                         (λ (k) abstract-value/range/top)})
(define abstract-value-merge*/range {merge-*-ify abstract-value-merge/range})
(define abstract-flow-control-return-merge*
  {merge-*-ify abstract-flow-control-return-merge})

(define abstract-flow-control-return->val-store-list/range
  {abstract-flow-control-return->val-store-list
   abstract-value-merge*/range abstract-store-merge*/range})


(define current-abstract-interp-call-stack (make-parameter '()))

;;; wrapper for the ag-rule to prevent function cycles
(define (abstract-interp-wrap/range n store flow-returns)
  (let* ([result
          (if (member (ast-node-type n) '(FunctionApplicationExpression
                                          FunctionDefinition))
              (let ([name (ast-child 'name n)])
                (if (member name (current-abstract-interp-call-stack))
                    (let* ([assignments
                            (att-value 'find-transitive-assignments
                             (if (node-subtype? n 'FunctionDefinition)
                                 n
                                 (let ([ref (resolve-variable-reference-node n)])
                                   (dict-ref (binding-bound ref) 'declaration-node))))]
                           [new-store (for/fold ([store store])
                                            ([a assignments])
                                    (dict-set store a abstract-value/range/top))])
                      (list abstract-value/range/top new-store flow-returns))
                    (parameterize ([current-abstract-interp-call-stack
                                    (cons name (current-abstract-interp-call-stack))])
                      (att-value 'abstract-interp-do/range n store flow-returns))))
              (att-value 'abstract-interp-do/range n store flow-returns))]
         [result-hash (att-value 'abstract-interp-result-hash/range n)]
         [node-id (ast-child 'serialnumber n)])
    (hash-set! result-hash node-id (cons result (hash-ref result-hash node-id '())))
    result))

(define-syntax (values->list stx)
  (syntax-parse stx
    [(_ e ...)
     #'(call-with-values
        (λ () e ...)
        list)]))

(define (resolve-variable-reference-node n)
  (resolve-reference
   (reference (ast-child 'name n)
              (att-value 'scope-graph-scope n))))

(define (ast-children/flat n)
  (flatten
   (map (λ (x) (if (and (ast-node? x) (ast-list-node? x))
                   (ast-children x)
                   x))
        (if (ast-node? n)
            (ast-children n)
            '()))))




(define ({binary-expression-print/infix op-sym} n)
  (h-comment
   n
   (h-append lparen
             (hs-append (att-value 'pretty-print (ast-child 'l n))
                        op-sym
                        (att-value 'pretty-print (ast-child 'r n)))
             rparen)))
(define ({binary-expression-print/function f-name} n)
  (h-comment
   n
   (h-append f-name
             lparen
             (hs-append (att-value 'pretty-print (ast-child 'l n))
                        comma
                        (att-value 'pretty-print (ast-child 'r n)))
             rparen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-specification spec
  (ast-rule 'Node->precomment-postcomment-serialnumber)
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
  (ast-rule 'ForStatement:LoopStatement->Declaration<init-Expression<update)

  (ast-rule 'Expression:Node->)
  (ast-rule 'ExpressionHole:Expression->)
  ;; TODO LValues?
  (ast-rule 'AssignmentExpression:Expression->name-Expression)
  (ast-rule 'FunctionApplicationExpression:Expression->name-Expression*)
  (ast-rule 'BinaryExpression:Expression->Expression<l-Expression<r)
  (ast-rule 'AdditionExpression:BinaryExpression->)
  (ast-rule 'UnsafeAdditionExpression:AdditionExpression->)
  (ast-rule 'SubtractionExpression:BinaryExpression->)
  (ast-rule 'UnsafeSubtractionExpression:SubtractionExpression->)
  (ast-rule 'MultiplicationExpression:BinaryExpression->)
  (ast-rule 'UnsafeMultiplicationExpression:MultiplicationExpression->)
  (ast-rule 'DivisionExpression:BinaryExpression->)
  (ast-rule 'UnsafeDivisionExpression:DivisionExpression->)

  (ast-rule 'IntOnlyBinaryExpression:BinaryExpression->)
  (ast-rule 'ModulusExpression:IntOnlyBinaryExpression->)
  (ast-rule 'UnsafeModulusExpression:ModulusExpression->)

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

  (ast-rule 'ArgumentList:Node->)
  (ast-rule 'ArgumentListEmpty:ArgumentList->)
  (ast-rule 'ArgumentListNode:ArgumentList->Expression-ArgumentList)

  (compile-ast-specifications 'Node) ; Program


  (ag-rule
   ast-depth
   [Program (λ (n) 0)]
   [Block (λ (n) (if (member (node-type (parent-node n))
                             '(IfStatement
                               IfElseStatement
                               FunctionDefinition
                               ForStatement
                               WhileStatement
                               DoWhileStatement))
                     (att-value 'ast-depth (parent-node n))
                     (add1 (att-value 'ast-depth (parent-node n)))))]
   ;; Some nodes shouldn't really increase depth
   [ExpressionStatement (λ (n) (att-value 'ast-depth (parent-node n)))]
   [AssignmentExpression (λ (n) (att-value 'ast-depth (parent-node n)))]
   [Declaration (λ (n) (att-value 'ast-depth (parent-node n)))]
   ;; By default increase
   [Node (λ (n) (add1 (att-value 'ast-depth (parent-node n))))])

  ;; IE are declarations here global?
  (ag-rule at-top-level?
           [Program (λ (n) #t)]
           [Node (λ (n) (let ([p (parent-node n)])
                          (or (equal? (node-type p) 'Program)
                              (and (ast-list-node? p)
                                   (equal? (node-type (parent-node p)) 'Program)))))])

  (ag-rule top-level-node
           [Program (λ (n) n)]
           [Node (λ (n) (att-value 'top-level-node (parent-node n)))])

  (ag-rule
   pretty-print
   [Program (λ (n)
              (define children (append (ast-children (ast-child 'Declaration* n))
                                       (list (ast-child 'main n))))
              (define global-vars (filter (λ (d) (node-subtype? d 'VariableDeclaration))
                                          children))
              (define functions (filter (λ (d) (node-subtype? d 'FunctionDefinition))
                                        children))
              (v-append
               (v-comment
                n
                (vb-concat
                 (map (λ (cn) (att-value 'pretty-print cn))
                      (append global-vars functions))))
               ;; Hack to get a newline...
               (text "")))]
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
        ;; since init is a declaration it already prints a semicolon
        space
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
       (h-append (att-value 'pretty-print (ast-child 'Expression n))
                 semi)))]
   [ValueReturnStatement
    (λ (n)
      (v-comment
       n
       (h-append return: space
                 (att-value 'pretty-print (ast-child 'Expression n))
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

   ;; TODO -- gen the name of the safe op function from the type of the operator
   [AdditionExpression {binary-expression-print/function (text "safe_add")}]
   [SubtractionExpression {binary-expression-print/function (text "safe_sub")}]
   [MultiplicationExpression {binary-expression-print/function (text "safe_mul")}]
   [DivisionExpression {binary-expression-print/function (text "safe_div")}]
   [ModulusExpression {binary-expression-print/function (text "safe_mod")}]

   [UnsafeAdditionExpression {binary-expression-print/infix plus}]
   [UnsafeSubtractionExpression {binary-expression-print/infix minus}]
   [UnsafeMultiplicationExpression {binary-expression-print/infix star}]
   [UnsafeDivisionExpression {binary-expression-print/infix slash}]
   [UnsafeModulusExpression {binary-expression-print/infix percent}]

   [EqualityExpression {binary-expression-print/infix (h-append eqsign eqsign)}]
   [GreaterThanExpression {binary-expression-print/infix greater}]
   [LessThanExpression {binary-expression-print/infix less}]
   [GreaterOrEqualExpression {binary-expression-print/infix (h-append greater eqsign)}]
   [LessOrEqualExpression {binary-expression-print/infix (h-append less eqsign)}]

   [Node (λ (n) (error 'pretty-print "no default ag-rule"))]
   )


  (ag-rule
   scope-graph-binding
   [Node (λ (n) (error 'scope-graph-binding "no default ag-rule"))]
   [FunctionDefinition
    (λ (n) (binding (ast-child 'name n)
                    ;; TODO - decide what should really go here
                    (hash 'declaration-node n
                          'type (append (list '->)
                                        (map (λ (fp) (ast-child 'typename fp))
                                             (ast-children (ast-child 'FormalParam* n)))
                                        (list (ast-child 'typename n))))))]
   [VariableDeclaration
    (λ (n) (binding (ast-child 'name n)
                    ;; TODO - decide what should really go here
                    (hash 'declaration-node n
                          'type (ast-child 'typename n))))]
   [FormalParam
    (λ (n) (binding (ast-child 'name n)
                    (hash 'declaration-node n
                          'type (ast-child 'typename n))))]
   [DeclarationHole
    (λ (n) #f)])

  (ag-rule
   scope-graph-scope
   [Program
    (λ (n) (scope #f
                  (filter (λ(x)x)
                          (map (λ (cn) (att-value 'scope-graph-binding cn))
                               (cons (ast-child 'main n)
                                     (ast-children (ast-child 'Declaration* n)))))
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
   [ForStatement
    (λ (n) (scope (att-value 'scope-graph-scope (parent-node n))
                  (filter (λ(x)x)
                          (list (att-value 'scope-graph-binding (ast-child 'init n))))
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
                              (resolve-variable-reference-node n))
                             'type)))]
   [FunctionApplicationExpression
    (λ (n)
      (let ([f-bind (resolve-variable-reference-node n)])
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
            '(no-assignment constant)))]
   [ExpressionHole (λ (n) (default-misc-constraints n))]
   [DeclarationHole (λ (n) (default-misc-constraints n))]
   [StatementHole (λ (n) (default-misc-constraints n))]
   [Node (λ (n) (set-union
                 ;; Don't allow assignment except where explicitly allowed
                 '(no-assignment)
                 (default-misc-constraints n)))])


  (define ({abstract-binary-op/range op #:unsafe [unsafe #f]} node store flow-returns)
    ;; op is a function of (l-l l-h r-l r-h -> (list low high))
    (match-let* ([(list val-l sto-l ret-l) (abstract-interp-wrap/range
                                            (ast-child 'l node)
                                            store
                                            flow-returns)]
                 [(list val-r sto-r ret-r) (abstract-interp-wrap/range
                                            (ast-child 'r node)
                                            sto-l
                                            ret-l)])
      (match val-l
        [(abstract-value/range l-low l-high)
         (match val-r
           [(abstract-value/range r-low r-high)
            (match (op l-low l-high r-low r-high)
              [(list low high)
               (let ([result (abstract-value/range (nan->-inf low) (nan->+inf high))])
                 (if unsafe
                     (list result
                           sto-r
                           ret-r)
                     (list (abstract-value-merge/range result val-l)
                           sto-r
                           ret-r)))]
              [else abstract-value/range/top])]
           [else abstract-value/range/top])]
        [else abstract-value/range/top])))

  ;;; safety-pred is a function of (l-l l-h r-l r-h -> bool)
  (define ({safe-binary-op-swap/range unsafe-version safety-pred} n)
    (define l (ast-child 'l n))
    (define r (ast-child 'r n))
    (define l-result (att-value 'abstract-interp/range (ast-child 'l n)))
    (define r-result (att-value 'abstract-interp/range (ast-child 'r n)))
    (if (member 'dead (list l-result r-result))
        #f
        (match-let ([(list (abstract-value/range l-l l-h) store-l returns-l) l-result]
                    [(list (abstract-value/range r-l r-h) store-r returns-r) r-result])
          (if (safety-pred l-l l-h r-l r-h)
              unsafe-version
              #f))))

  (define {abstract-comparison-op/range op opposite-op}
    {abstract-binary-op/range
     (λ (l-l l-h r-l r-h)
       (cond [(and (op l-l r-l)
                   (op l-l r-h)
                   (op l-h r-l)
                   (op l-h r-h))
              (list 1 1)]
             [(and (opposite-op l-l r-l)
                   (opposite-op l-l r-h)
                   (opposite-op l-h r-l)
                   (opposite-op l-h r-h))
              (list 0 0)]
             [else (list 0 1)]))
     #:unsafe #t})

  (define ({abstract-interp-do/range/if one-sided?} n store flow-returns)
    (match-let ([(list (abstract-value/range low high)
                       new-store
                       new-rets)
                 (abstract-interp-wrap/range
                  (ast-child 'test n) store flow-returns)])
      (cond
        ;; Never false
        [(or (and (< 0 low) (< 0 high))
             (and (> 0 low) (> 0 high)))
         (abstract-interp-wrap/range (ast-child 'then n) new-store new-rets)]
        ;; Never true
        [(and (equal? 0 low) (equal? 0 high))
         (if one-sided?
             (list abstract-value/range/top new-store new-rets)
             (abstract-interp-wrap/range (ast-child 'else n) new-store new-rets))]
        ;; Maybe sometimes true and sometimes false...
        [else
         ;; TODO -- interp BOTH sides and merge the result values and stores
         ;;; (abstract-flow-control-return-merge flow-returns r)
         (match-let ([(list then-v then-s then-r)
                      (abstract-interp-wrap/range (ast-child 'then n)
                                                  new-store
                                                  new-rets)]
                     [(list else-v else-s else-r)
                      (if one-sided?
                          (list abstract-value/range/top new-store new-rets)
                          (abstract-interp-wrap/range (ast-child 'else n)
                                                      new-store
                                                      new-rets))])
           (list (abstract-value-merge/range then-v else-v)
                 (abstract-store-merge*/range then-s else-s)
                 (abstract-flow-control-return-merge then-r else-r)))])))

  (define (abstract-interp-loop/body n store flow-returns)
    (define altered-refs (att-value 'find-transitive-assignments n))
    (define new-store
      (for/fold ([s store])
                ([a altered-refs])
        (dict-set s a abstract-value/range/top)))
    (define has-return?
      (att-value 'find-a-descendant
                 n
                 (λ (node)
                   (and (ast-node? node)
                        (node-subtype? node 'ReturnStatement)))))
    ;; interp the test once generically for analysis info to be properly generic
    (abstract-interp-wrap/range (ast-child 'test n)
                                range-store-top
                                empty-abstract-flow-control-return)
    (if has-return?
        (match-let ([(list v s r)
                     (abstract-interp-wrap/range (ast-child 'body n)
                                                 new-store
                                                 flow-returns)])
          (list abstract-value/range/top
                s
                (abstract-flow-control-return-only-maybe-ify
                 (abstract-flow-control-return-merge flow-returns r))))
        (list abstract-value/range/top new-store flow-returns)))


  (define addition-op/range
    (λ (l-l l-h r-l r-h)
      (list (+ l-l r-l) (+ l-h r-h))))
  (define subtraction-op/range
    (λ (l-l l-h r-l r-h)
      (list (- l-l r-h) (- l-h r-l))))
  (define multiplication-op/range
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
          ['(- -) (list (* l-h r-h) (* l-l r-l))]))))
  ;; TODO - make a real transfer functions.
  ;; Division and modulus are both undefined when the divisor is 0,
  ;; division is undefined if the numerator is INT_MIN and the denominator is -1,
  ;; and I'm not sure about modulus in that case.
  (define division-op/range
    (λ (l-l l-h r-l r-h) (list -inf.0 +inf.0)))
  (define modulus-op/range
    (λ (l-l l-h r-l r-h) (list -inf.0 +inf.0)))

  (ag-rule
   abstract-interp/range
   ;; Get the single global result of abstract interpretation of this node.
   [Node (λ (n)
           ;; Interp the containing function to fill the result hash.
           (define result
             (abstract-interp-wrap/range
              (att-value 'get-containing-function-definition n)
              range-store-top
              empty-abstract-flow-control-return))
           ;; If the code is unreachable then it will have no result here.
           (match (hash-ref
                   (att-value 'abstract-interp-result-hash/range n)
                   (ast-child 'serialnumber n)
                   'dead)
             ['dead 'dead]
             [(list (list vs ss rs) ...)
              (list (apply abstract-value-merge*/range vs)
                    (apply abstract-store-merge*/range ss)
                    (apply abstract-flow-control-return-merge* rs))]))])
  (ag-rule
   get-containing-function-definition
   [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main"))
                                  n
                                  (ast-parent n)))]
   [Program (λ (n) n)]
   [Node (λ (n) (att-value 'get-containing-function-definition (ast-parent n)))])

  (ag-rule
   ;; To get a general interp result for any given node, we need to cache
   ;; the results of evaluation when we interpret the whole function it is in.
   ;; We collect results in this hash, then merge if there are multiple results.
   ;; The hash is MUTATED during interpretation to achieve this.
   ;; The hash holds a list of results for each node (or nothing -- use '() as
   ;; a default).  Although an empty result should mean the node is dead code.
   abstract-interp-result-hash/range
   [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main"))
                                  (make-hasheq)
                                  (att-value 'abstract-interp-result-hash/range
                                             (ast-parent n))))]
   [Program (λ (n) (make-hasheq))]
   [Node (λ (n) (att-value 'abstract-interp-result-hash/range (ast-parent n)))]
   )

  (ag-rule
   ;; For implementing abstract-interp/range.
   ;; For now, store is table from binding to abstract value.
   ;; Returns (list abstract-value new-store)
   ;; Note - For functions and operators, argument evaluation order is unspecified.
   ;;        So the store coming out of each side should be the same.
   ;;        This should be enforced by disallowing assignment in these places.
   abstract-interp-do/range

   ;;; Program
   [Program
    (λ (n store flow-returns)
      (define init-store
        (for/fold ([store range-store-top])
                  ([global (filter (λ (cn) (node-subtype? cn 'VariableDeclaration))
                                   (ast-children (ast-child 'Declaration* n)))])
          (match-let* ([(list v n-store n-rets)
                        (abstract-interp-wrap/range
                         global
                         store
                         empty-abstract-flow-control-return)])
            n-store)))
      (abstract-interp-wrap/range
       (ast-child 'main n)
       init-store
       empty-abstract-flow-control-return))]
   [VariableDeclaration
    (λ (n store flow-returns)
      (match-let* ([ref (resolve-variable-reference-node n)]
                   [(list v n-store n-rets)
                    (abstract-interp-wrap/range
                     (ast-child 'Expression n)
                     store
                     empty-abstract-flow-control-return)])
        (list v (dict-set n-store ref v) n-rets)))]

   ;;; Statements
   ;;; Statements return a store but aside from return statements the
   ;;; result value is meaningless
   [StatementHole
    (λ (n store flow-returns)
      (list abstract-value/range/top range-store-top
            (maybe-return abstract-value/range/top range-store-top)))]

   [NullStatement (λ (n store flow-returns)
                    (list abstract-value/range/top store flow-returns))]
   #|
   TODO - there are no void functions yet, so once there are this (and all
   non-return statements) should return void.
   |#
   [VoidReturnStatement
    (λ (n store flow-returns)
      (list abstract-value/range/top store
            (must-return flow-returns abstract-value/range/top store)))]
   [ExpressionStatement
    (λ (n store flow-returns)
      (abstract-interp-wrap/range
       (ast-child 'Expression n) store flow-returns))]
   [ValueReturnStatement
    (λ (n store flow-returns)
      (match (abstract-interp-wrap/range
              (ast-child 'Expression n) store flow-returns)
        [(list v s r)
         (list v s (must-return r v s))]))]
   [Block
    (λ (n store flow-returns)
      (define-values (store-with-decls ret-with-decls)
        (for/fold ([s store]
                   [r flow-returns])
                  ([decl (ast-children (ast-child 'Declaration* n))])
          ;; There are declaration holes and such, so check that it is a variable decl.
          (if (equal? (ast-node-type decl) 'VariableDeclaration)
              (match-let* ([(list v n-store n-rets)
                            (abstract-interp-wrap/range decl s r)])
                (values n-store n-rets))
              (values s r))))
      (define-values (store-after-statements rets-after-statements)
        (for/fold ([s store-with-decls]
                   [r ret-with-decls])
                  ([statement (ast-children (ast-child 'Statement* n))])
          #:break (abstract-flow-control-return-must r)
          (match-let ([(list v n-store n-rets) (abstract-interp-wrap/range
                                                statement
                                                s
                                                r)])
            (values n-store n-rets))))
      (list abstract-value/range/top store-after-statements rets-after-statements))]
   [IfStatement
    {abstract-interp-do/range/if #t}]
   [IfElseStatement
    {abstract-interp-do/range/if #f}]

   [ForStatement
    (λ (n store flow-returns)
      (match-let*
          ([(list v store flow-returns)
            (abstract-interp-wrap/range (ast-child 'init n) store flow-returns)]
           [(list (abstract-value/range low high) store flow-returns)
            (abstract-interp-wrap/range (ast-child 'test n) store flow-returns)])
        (if (and (equal? low 0) (equal? high 0))
            (list abstract-value/range/top store flow-returns)
            (begin

              ;; interp the update once generically for analysis info to be properly generic
              (abstract-interp-wrap/range (ast-child 'update n)
                                          range-store-top
                                          empty-abstract-flow-control-return)
              (abstract-interp-loop/body n store flow-returns)))))]
   [WhileStatement
    (λ (n store flow-returns)
      (match-let* ([(list (abstract-value/range low high) store flow-returns)
                    (abstract-interp-wrap/range (ast-child 'test n) store flow-returns)])
        (if (and (equal? low 0) (equal? high 0))
            (list abstract-value/range/top store flow-returns)
            (abstract-interp-loop/body n store flow-returns))))]
   [DoWhileStatement
    (λ (n store flow-returns)
      (abstract-interp-loop/body n store flow-returns))]

   ;;; Expressions
   [ExpressionHole
    (λ (n store flow-returns)
      (list abstract-value/range/top range-store-top flow-returns))]

   [LiteralInt
    (λ (n store flow-returns)
      (list (abstract-value/range (ast-child 'val n) (ast-child 'val n))
            store flow-returns))]
   [LiteralFloat
    (λ (n store flow-returns)
      (list (abstract-value/range (ast-child 'val n) (ast-child 'val n))
            store flow-returns))]

   [IfExpression
    {abstract-interp-do/range/if #f}]

   ;; Function Definitions will be evaluated with store and arguments as top for
   ;; analyzing for potential code transformations.
   [FunctionDefinition
    (λ (func-def-node store flow-returns)
      (match (abstract-interp-wrap/range
              (ast-child 'Block func-def-node)
              range-store-top
              empty-abstract-flow-control-return)
        [(list v s returns)
         (append (abstract-flow-control-return->val-store-list/range returns)
                 (list flow-returns))]))]
   ;; Function applications will be evaluated with the arguments given.
   [FunctionApplicationExpression
    (λ (n store flow-returns)
      (match-let* ([binding (resolve-variable-reference-node n)]
                   [bound (binding-bound binding)]
                   [func-def-node (dict-ref bound 'declaration-node)]
                   [func-block (ast-child 'Block func-def-node)]
                   [func-params (ast-children (ast-child 'FormalParam* func-def-node))]
                   [(list reversed-args store rets)
                    (values->list
                     (for/fold ([args-so-far '()]
                                [store store]
                                [rets flow-returns])
                               ([expr (ast-children (ast-child 'Expression* n))])
                       (match-define (list v s r)
                         (abstract-interp-wrap/range expr store rets))
                       (values (cons v args-so-far) s r)))]
                   [store-for-func (for/fold ([store store])
                                             ([fp func-params]
                                              [arg (reverse reversed-args)])
                                     (dict-set store fp arg))])
        (match (abstract-interp-wrap/range
                func-block
                store-for-func
                empty-abstract-flow-control-return)
          [(list v s returns)
           (append (abstract-flow-control-return->val-store-list/range returns)
                   (list flow-returns))])))]

   [AssignmentExpression
    (λ (n store flow-returns)
      (match-let ([(list val new-store new-rets)
                   (abstract-interp-wrap/range
                    (ast-child 'Expression n)
                    store
                    flow-returns)]
                  [ref-node (resolve-variable-reference-node n)])
        (list val (dict-set new-store ref-node val) new-rets)))]
   [VariableReference
    (λ (n store flow-returns)
      (let ([ref-node (resolve-variable-reference-node n)])
        ;; TODO -- I'm using an empty hash to represent an unknown state of the store,
        ;; or at least an unknown state for a variable.  But once there are more than
        ;; ints and floats I'll need to look at the type of the reference to know what
        ;; kind of value to use for top.
        (list (dict-ref store ref-node abstract-value/range/top)
              store
              flow-returns)))]

   [AdditionExpression
    {abstract-binary-op/range addition-op/range}]
   [UnsafeAdditionExpression
    {abstract-binary-op/range addition-op/range #:unsafe #t}]
   [SubtractionExpression
    {abstract-binary-op/range subtraction-op/range}]
   [UnsafeSubtractionExpression
    {abstract-binary-op/range subtraction-op/range #:unsafe #t}]
   [MultiplicationExpression
    {abstract-binary-op/range multiplication-op/range}]
   [UnsafeMultiplicationExpression
    {abstract-binary-op/range multiplication-op/range #:unsafe #t}]
   [DivisionExpression
    {abstract-binary-op/range division-op/range}]
   [UnsafeDivisionExpression
    {abstract-binary-op/range division-op/range #:unsafe #t}]
   [ModulusExpression
    {abstract-binary-op/range modulus-op/range}]
   [UnsafeModulusExpression
    {abstract-binary-op/range modulus-op/range #:unsafe #t}]

   [EqualityExpression
    {abstract-binary-op/range
     (λ (l-l l-h r-l r-h)
       (let ([equal-val (foldl (λ (l r) (and (equal? l r) r))
                               l-l (list l-h r-l r-h))]
             [no-overlap? (or (and (< l-l r-l r-h)
                                   (< l-h r-l r-h))
                              (and (< r-l l-l l-h)
                                   (< r-h l-l l-h)))])
         (cond [equal-val (list 1 1)]
               [no-overlap? (list 0 0)]
               [else (list 0 1)])))}]
   [LessThanExpression {abstract-comparison-op/range < >=}]
   [GreaterThanExpression {abstract-comparison-op/range > <=}]
   [LessOrEqualExpression {abstract-comparison-op/range <= >}]
   [GreaterOrEqualExpression {abstract-comparison-op/range >= <}]

   [Node (λ (n store flow-returns)
           (error 'abstract-interp-do/range "no default ag-rule"))])

  (define ({bounded-range min max} low high)
    (and (<= min low) (<= min high) (>= max low) (>= max high)))
  (define ({result-in-bounds/range range-op min max} l-l l-h r-l r-h)
    (apply (bounded-range min max) (range-op l-l l-h r-l r-h)))
  (define (division-safety-check/range l-l l-h r-l r-h)
    (and
     ;; No division by zero.
     (or (and (< 0 r-l) (< 0 r-h))
         (and (> 0 r-l) (> 0 r-h)))
     ;; No INT_MIN / -1
     (or (< INT_MIN l-h)
         (or (and (< -1 r-l) (< -1 r-h))
             (and (> -1 r-l) (> -1 r-h))))))

  (ag-rule
   ;;; Return #f if the unsafe op can't be safely used,
   ;;; otherwise return the name of the unsafe type to be used
   ;;; as a refinement.
   unsafe-op-if-possible
   [AdditionExpression
    {safe-binary-op-swap/range
     'UnsafeAdditionExpression
     {result-in-bounds/range addition-op/range INT_MIN INT_MAX}}]
   [SubtractionExpression
    {safe-binary-op-swap/range
     'UnsafeSubtractionExpression
     {result-in-bounds/range subtraction-op/range INT_MIN INT_MAX}}]
   [MultiplicationExpression
    {safe-binary-op-swap/range
     'UnsafeMultiplicationExpression
     {result-in-bounds/range multiplication-op/range INT_MIN INT_MAX}}]
   [DivisionExpression
    {safe-binary-op-swap/range
     'UnsafeDivisionExpression
     division-safety-check/range}]
   [ModulusExpression
    {safe-binary-op-swap/range
     'UnsafeModulusExpression
     division-safety-check/range}]
   [Node (λ (n) (error 'unsafe-op-if-possible "No default implementation"))]
   )

  (define (fresh-symbolic-var type)
    (define type-pred (cond [(equal? type "int") integer?]
                            [(equal? type "float") float?]))
    (define-symbolic* var type-pred)
    var)

  (define (symbolic-store-merge s1 pc1 s2 pc2)
    (for/hash ([k (set-union (hash-keys s1) (hash-keys s2))])
      (define v1 (hash-ref s1 k 'not-found))
      (define v2 (hash-ref s2 k 'not-found))
      (cond [(equal? v1 'not-found) (values k v2)]
            [(equal? v2 'not-found) (values k v1)]
            [(equal? v1 v2) (values k v1)]
            [else
             (define fv (fresh-symbolic-var (dict-ref (binding-bound k) 'type)))
             (assert (=> (apply && pc1) (= fv v1)))
             (assert (=> (apply && pc2) (= fv v2)))
             (values k fv)])))

  (define {symbolic-if-interp one-sided? type}
    (λ (n store path-condition return-variable)
      (match-define (list test-val test-store)
        (symbolic-interp-wrap (ast-child n 'test)
                              store path-condition return-variable))

      (match-define-values
       ((list then-v then-store) then-asserts+)
       (with-asserts (begin (assert test-val)
                            (symbolic-interp-wrap (ast-child n 'then)
                                                  test-store
                                                  (cons test-val path-condition)
                                                  return-variable))))
      (define then-asserts (set-subtract then-asserts+ (asserts)))
      (assert (=> cond-v (apply && cond-asserts)))

      (if one-sided?
          (list #t (symbolic-store-merge test-store (not test-val) then-store test-val))
          (let ()
            (match-define-values
             ((list else-v else-store) else-asserts+)
             (with-asserts
               (begin (assert (not test-val))
                      (symbolic-interp-wrap (ast-child n 'else)
                                            test-store
                                            (cons (not test-val) path-condition)
                                            return-variable))))
            (define else-asserts (set-subtract else-asserts+ (asserts)))
            (assert (=> (not cond-v) (apply && else-asserts)))
            (if (not type)
                (list #t (symbolic-store-merge else-store (not test-val)
                                               then-store test-val))
                (let ([v (fresh-symbolic-var type)])
                  (assert (&& (=> cond-v (= then-v v))
                              (=> (not cond-v) (= else-v v))))
                  (list v (symbolic-store-merge else-store (not test-val)
                                                then-store test-val))))))))

  (ag-rule
   ;; This rule adds rosette assertions, so it should only be called when
   ;; the rosette environment has been prepared (eg. pushed/popped an
   ;; interpreter, so two interpretations don't step on each other).

   ;; `solver-push` and `solver-pop` let you push/pop assertions, but not get the current assertions!
   ;; `asserts` lets you get the current global assertions, but provides no push/pop!
   ;; Maybe I can use `with-asserts` for things like conditionals/ifs -- interp one side with-asserts, interp the other side with-asserts, and then OR the resulting assert lists together.
   ;; I should keep track of the path conditions and in the store I should put PC -> variable-value
   ;; I can also tie the path conditions to return values, or have a return variable for the function that the path condition implies to be some value.

   ;; When I set things in the store, I should AND the path condition with the value, and OR it with any value that is currently there AND NOT path condition.
   symbolic-interp-do
   #|
   TODO
   (ast-rule 'Program:Node->Declaration*-FunctionDefinition<main)
   (ast-rule 'FunctionDefinition:Declaration->typename-FormalParam*-Block)

   (ast-rule 'NullStatement:Statement->)
   (ast-rule 'Block:Statement->Declaration*-Statement*)
   (ast-rule 'ExpressionStatement:Statement->Expression)
   (ast-rule 'ReturnStatement:Statement->)
   (ast-rule 'VoidReturnStatement:ReturnStatement->)
   (ast-rule 'ValueReturnStatement:ReturnStatement->Expression)
   (ast-rule 'StatementHole:Statement->)
   (ast-rule 'BlockHole:Block->)

   (ast-rule 'LoopStatement:Statement->Expression<test-Statement<body)
   (ast-rule 'WhileStatement:LoopStatement->)
   (ast-rule 'DoWhileStatement:LoopStatement->)
   (ast-rule 'ForStatement:LoopStatement->Declaration<init-Expression<update)

   (ast-rule 'AssignmentExpression:Expression->name-Expression)
   (ast-rule 'FunctionApplicationExpression:Expression->name-Expression*)
   (ast-rule 'BinaryExpression:Expression->Expression<l-Expression<r)
   (ast-rule 'AdditionExpression:BinaryExpression->)
   (ast-rule 'UnsafeAdditionExpression:AdditionExpression->)
   (ast-rule 'SubtractionExpression:BinaryExpression->)
   (ast-rule 'UnsafeSubtractionExpression:SubtractionExpression->)
   (ast-rule 'MultiplicationExpression:BinaryExpression->)
   (ast-rule 'UnsafeMultiplicationExpression:MultiplicationExpression->)
   (ast-rule 'DivisionExpression:BinaryExpression->)
   (ast-rule 'UnsafeDivisionExpression:DivisionExpression->)

   (ast-rule 'IntOnlyBinaryExpression:BinaryExpression->)
   (ast-rule 'ModulusExpression:IntOnlyBinaryExpression->)
   (ast-rule 'UnsafeModulusExpression:ModulusExpression->)

   (ast-rule 'ComparisonExpression:BinaryExpression->)
   (ast-rule 'EqualityExpression:ComparisonExpression->)
   (ast-rule 'GreaterThanExpression:ComparisonExpression->)
   (ast-rule 'LessThanExpression:ComparisonExpression->)
   (ast-rule 'LessOrEqualExpression:ComparisonExpression->)
   (ast-rule 'GreaterOrEqualExpression:ComparisonExpression->)

   (ast-rule 'LiteralInt:Expression->val)
   (ast-rule 'LiteralFloat:Expression->val)
   (ast-rule 'VariableReference:Expression->name)

   (ast-rule 'ArgumentList:Node->)
   (ast-rule 'ArgumentListEmpty:ArgumentList->)
   (ast-rule 'ArgumentListNode:ArgumentList->Expression-ArgumentList)
   |#
   aoeu

   [IfExpression
    {symbolic-if-interp #f
                        (or (att-value 'type-context n)
                            (att-value 'type-context
                                       (ast-child n 'then)))}]
   [IfStatement {symbolic-if-interp #t #f}]
   [IfElseStatement {symbolic-if-interp #f #f}]
   [VariableDeclaration
    (λ (n store path-condition return-variable)
      (let* ([name (ast-child n 'name)]
             [type (ast-child n 'typename)]
             [sym-var (fresh-symbolic-var type)]
             [ref (resolve-variable-reference-node n)])
        (define-values (v n-store)
          (symbolic-interp-wrap (ast-child 'Expression n)
                                store path-condition return-variable))
        (assert (= sym-var v))
        (list v (dict-set n-store ref sym-var))))]
   [NullStatement
    (λ (n store path-condition return-variable)
      (list #f store))]
   [Node (λ (n store path-condition return-variable)
           (error 'symbolic-interp "No default implementation"))])

  (ag-rule
   ;;; Find all children satisfying the predicate (the given node included)
   find-descendants
   [Node (λ (n predicate)
           (define children (filter ast-node? (ast-children/flat n)))
           (define matches
             (apply append (map (λ (x) (att-value 'find-descendants x predicate))
                                children)))
           (if (predicate n)
               (cons n matches)
               matches))])

  (ag-rule
   ;;; Find the first node that satisfies the predicate (the given node included)
   find-a-descendant
   [Node (λ (n predicate)
           (if (predicate n)
               n
               (for/or ([c (filter ast-node? (ast-children/flat n))])
                 (att-value 'find-a-descendant c predicate))))])

  (ag-rule
   find-direct-resolved
   [Node (λ (n node-type)
           (remove-duplicates
            (map resolve-variable-reference-node
                 (att-value 'find-descendants
                            n (λ (cn) (node-subtype? cn node-type))))))])
  (ag-rule
   find-direct-assignments
   [Node (λ (n) (att-value 'find-direct-resolved n 'AssignmentExpression))])
  (ag-rule
   find-direct-function-call-refs
   [Node (λ (n) (att-value 'find-direct-resolved n 'FunctionApplicationExpression))])
  (ag-rule
   find-direct-variable-references
   [Node (λ (n) (att-value 'find-direct-resolved n 'VariableReference))])

  (ag-rule
   find-transitive-function-call-refs
   [Node (λ (n)
           (define (rec to-search searched)
             (if (empty? to-search)
                 searched
                 (let* ([calls (att-value 'find-direct-function-call-refs
                                          (hash-ref (binding-bound (car to-search))
                                                    'declaration-node))]
                        [searched (cons (car to-search) searched)]
                        [new-calls (set-subtract calls searched)]
                        [to-search (append new-calls (cdr to-search))])
                   (rec to-search searched))))
           (rec (att-value 'find-direct-function-call-refs n) '()))])

  (ag-rule
   find-transitive-resolved
   [Node (λ (n node-type)
           (remove-duplicates
            (append (att-value 'find-direct-resolved n node-type)
                    (flatten
                     (map (λ (r) (att-value 'find-direct-resolved
                                            (hash-ref (binding-bound r)
                                                      'declaration-node)
                                            node-type))
                          (att-value 'find-transitive-function-call-refs n))))))])
  (ag-rule
   find-transitive-assignments
   [Node (λ (n) (att-value 'find-transitive-resolved n 'AssignmentExpression))])
  (ag-rule
   find-transitive-variable-references
   [Node (λ (n) (att-value 'find-transitive-resolved n 'VariableReference))])


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
    [(_ base-weight hint-name)
     #'(define/override (choice-weight)
         (define bw base-weight)
         (define w (or bw (super choice-weight)))
         (if (member hint-name (att-value 'hints current-hole))
             (* (hint-weight-multiplier hint-name)
                w)
             w))]
    [(rec hint-name)
     #'(rec #f hint-name)]))

(define-syntax (top-heavy-choice stx)
  ;; TODO - this should take into account the max depth, because higher max-depths
  ;;        now produce really wonky stuff.
  (syntax-parse stx
    [(_ base-weight)
     #'(max 0
            (- base-weight
               (* 2 (att-value 'ast-depth current-hole))))]))

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
    (define/override (choice-weight) 1)
    (super-new)))
(define ExpressionStatementChoice
  (class StatementChoice
    ;; High choice-weich, because this will turn out to be an assignment expression usually.
    (define/override (choice-weight) 70)
    (define/override (fresh)
      (fresh-node 'ExpressionStatement (fresh-node 'ExpressionHole)))
    (define/override (wont-over-deepen)
      this)
    (super-new)))
(define IfStatementChoice
  (class StatementChoice
    (define/override (choice-weight) (top-heavy-choice 5))
    (define/override (features) '(if-statement))
    (define/override (fresh)
      (fresh-node 'IfStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define IfElseStatementChoice
  (class IfStatementChoice
    (define/override (choice-weight) (top-heavy-choice 5))
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
    (define/override (choice-weight) (top-heavy-choice 2))
    (define/override (fresh)
      (fresh-node 'WhileStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define DoWhileStatementChoice
  (class LoopStatementChoice
    (define/override (choice-weight) (top-heavy-choice 2))
    (define/override (fresh)
      (fresh-node 'DoWhileStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)))
    (super-new)))
(define ForStatementChoice
  (class LoopStatementChoice
    (define/override (choice-weight) (top-heavy-choice 7))
    (define/override (fresh)
      (fresh-node 'ForStatement
                  (fresh-node 'ExpressionHole)
                  (fresh-node 'StatementHole)
                  ;; init, update
                  (fresh-node 'DeclarationHole "standin-name")
                  (fresh-node 'ExpressionHole)))
    (super-new)))
(define BlockChoice
  (class StatementChoice
    (hinted-choice-weight 1 block-hint)
    (define/override (fresh)
      (fresh-node 'Block
                  ;; declarations
                  (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                           "standin-name"))
                                        (make-list (random 2) #f)))
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
    (define/override (choice-weight) 1)
    (define/override (wont-over-deepen)
      this)
    (define/override (respect-return-position)
      this)
    (super-new)))
(define ValueReturnStatementChoice
  (class ReturnStatementChoice
    (define/override (choice-weight) 1)
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
           (define/override (choice-weight) 3)
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
    (define/override (wont-over-deepen) this)
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
            (filter (λ (b) (not (function-type? (dict-ref (binding-bound b) 'type))))
                    legal-refs)))
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
      (define final-choices (filter (λ (b) (not (equal? "main" (binding-name b))))
                                    legal-with-type))
      (set! ref-choices-filtered final-choices)
      (and (not (null? final-choices)) this))
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
    (define/override (choice-weight) 20)
    (define/override (fresh)
      (define name (if (equal? (att-value 'top-level-node current-hole)
                               (parent-node current-hole))
                       (fresh-var-name "global_")
                       (fresh-var-name "local_")))
      (fresh-node 'VariableDeclaration
                  name
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
                  (if main? "main" (fresh-var-name "func_"))
                  (if main? int-type (fresh-var-type))
                  ;; parameters
                  (if main?
                      (create-ast-list '())
                      (create-ast-list (map (λ (x) (fresh-node 'FormalParam
                                                               (fresh-var-type)
                                                               (fresh-var-name "arg_")))
                                            (make-list (random 5) #f))))
                  (fresh-block-hole)))
    (super-new)))

(define (fresh-var-name [base "var_"])
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
  (create-ast spec type (list empty empty (node-id) attr-val ...)))

(define node-id-counter 0)
(define (node-id)
  (begin0
      node-id-counter
    (set! node-id-counter (add1 node-id-counter))))

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
(define (node-subtype? n t)
  (when (not (ast-node? n))
    (error 'node-subtype "called on non-ast-node.  Arguments: ~a ~a" n t))
  (and (ast-node? n) (ast-subtype? n t)))

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
                                      (make-list (random 7) #f)))
                (fresh-node 'FunctionDefinitionHole
                            "main"
                            "int"
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

(define (ast-add-unsafe-math ast)
  (define ops (att-value 'find-descendants ast
                         (λ (n) (member (ast-node-type n)
                                        '(AdditionExpression
                                          SubtractionExpression
                                          MultiplicationExpression
                                          DivisionExpression
                                          ModulusExpression
                                          )))))
  (define (transformer n)
    ;; Perform any rewrites and return #t if a rewrite was performed else #f
    (if (member (node-type n)
                '(AdditionExpression
                  SubtractionExpression
                  MultiplicationExpression
                  DivisionExpression
                  ModulusExpression
                  ))
        (let ([refined-type (att-value 'unsafe-op-if-possible n)])
          (and refined-type
               (begin
                 (rewrite-refine n refined-type)
                 #t)))
        #f))
  (perform-rewrites ast 'bottom-up transformer)
  ast)

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
    (let* ([ast (generate-random-prog (fresh-Prog))]
           [ast (ast-add-unsafe-math ast)])
      (if (dict-has-key? (xsmith-options) 'output-filename)
          (call-with-output-file (xsmith-option 'output-filename)
            #:exists 'replace
            (lambda (out)
              (pretty-print (att-value 'pretty-print ast)
                            out
                            page-width)))
          (begin
            (pretty-print (att-value 'pretty-print ast)
                          (current-output-port)
                          page-width)
            #;(printf "\n\n/*\nabstract return: ~a\n*/\n"
                    (car
                     (abstract-interp-wrap/range ast range-store-top
                                                 empty-abstract-flow-control-return)))))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
