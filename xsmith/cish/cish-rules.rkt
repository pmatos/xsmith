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

(provide cish-rules)

(require
 "../main.rkt"
 "cish-utils.rkt"

 racr
 (except-in pprint
            semi rparen rbrace lparen lbrace comma
            colon
            )
 racket/dict
 racket/set
 racket/match
 racket/class
 racket/random
 (prefix-in rt: rosette)
 (except-in racket/list empty)
 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  ))


(define-spec-component cish-rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ag rules

(define-syntax-parser ag
  [(_ arg ...)
   #'(add-att-rule cish-rules arg ...)])



(ag ast-serial-number
    ;; This is basically just a hack to signal stale state for the Rosette assertion stack.
    ;; I don't think this is really necessary -- checking eq? on the top-ancestor-node should be equivalent to checking this serial number.
    [Program (λ (n) (fresh-int!))]
    [Node (λ (n) (att-value 'ast-serial-number (parent-node n)))])

(define (base-type->string t)
  (if (bool-type? t)
      "int"
      (symbol->string
       (base-type-name t))))

(define (type->string t)
  (cond [(base-type? t) (base-type->string t)]
        [(nominal-record-type? t) (format "struct ~a" (nominal-record-type-name t))]
        [(volatile-type? t) (format "volatile ~a"
                                    (type->string (volatile-type-type t)))]
        [else (error 'type->string "not yet implemented for type ~a" t)]))

(ag
 pretty-print
 [Program (λ (n)
            (define structs (ast-children (ast-child 'structdefinitions n)))
            (define global-vars (ast-children (ast-child 'globalvariables n)))
            (define functions (ast-children (ast-child 'functions n)))
            (v-append
             (v-comment
              n
              (vb-concat
               (list*
                (text "#include \"xsmith_safe_math.h\"\n")
                (text "#include <stdio.h>\n")
                (map (λ (cn) (att-value 'pretty-print cn))
                     (append (reverse structs)
                             global-vars
                             functions
                             (list (ast-child 'main n)))))))
             (text "")
             (text "int main(){")
             (text "  int main_ret = 0;")
             (text "  main_ret = main_inner();")
             (apply v-append
                    (map (λ (v) (text (format "  printf(\"~a\\n\", ~a);\n"
                                              (match (type-qualifier-unwrap
                                                      (ast-child 'type v))
                                                [(? int-type?) "%d"]
                                                [(? bool-type?) "%d"]
                                                [(? float-type?) "%f"])
                                              (ast-child 'name v))))
                         (filter (λ (x) (base-type? (type-qualifier-unwrap
                                                     (ast-child 'type x))))
                                 global-vars)))
             (text "  printf(\"%d\\n\", main_ret);")
             (text "  return 0;")
             (text "}")
             ;; Hack to get a newline...
             (text "")))]
 [FunctionDefinition
  (λ (n)
    (v-comment
     n
     (h-append
      (text (type->string (function-type-return-type (ast-child 'type n))))
      space
      (text (ast-child 'name n))
      lparen
      (h-concat
       (add-between
        (map (λ (fp)
               (h-append (text (type->string (ast-child 'type fp)))
                         space
                         (text (ast-child 'name fp))))
             (ast-children (ast-child 'params n)))
        (h-append comma space)))
      rparen
      line
      (att-value 'pretty-print (ast-child 'Block n)))))]
 [StructDefinition
  (λ (n)
    (v-append
     (h-append (text "struct") space (text (ast-child 'name n)))
     (nest nest-step
           (apply v-append
                  lbrace
                  (for/list ([(field-name field-type)
                              (in-dict
                               (let ([t (nominal-record-definition-type
                                         (any-nominal-record-type))])
                                 (unify! t (ast-child 'type n))
                                 (nominal-record-type-inners
                                  (nominal-record-definition-type-type
                                   t))))])
                    (h-append (text (type->string field-type))
                              space
                              (text field-name)
                              semi))))
     (h-append rbrace semi)))]
 [LiteralStruct
  (λ (n)
    (define constant? (member 'constant (att-value 'misc-constraints n)))
    (define braces-part
      (h-append
       lbrace
       (h-concat
        (add-between (map (λ (c) (att-value 'pretty-print c))
                          (ast-children (ast-child 'vals n)))
                     (h-append comma space)))
       rbrace))
    ;; When constants are needed, cast operations are not only not needed but
    ;; can cause compilation failure.  But elsewhere a cast operator can be needed
    ;; for the compiler to tell what kind of struct you are constructing.
    (if constant?
        braces-part
        (h-append
         lparen
         (text "struct ")
         (text (ast-child 'name (ast-child 'structdefref n)))
         rparen
         braces-part)))]
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
                    (ast-children (ast-child 'declarations n)))
               (map (λ (cn) (att-value 'pretty-print cn))
                    (ast-children (ast-child 'statements n)))))))
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
                (text (type->string (ast-child 'type n)))
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
 [VolatileVariableReference
  (λ (n) (att-value 'pretty-print (ast-child 'VariableReference n)))]
 [VolatileInitializer
  (λ (n) (att-value 'pretty-print (ast-child 'Expression n)))]
 [FunctionApplicationExpression
  (λ (n)
    (h-comment
     n
     (h-append
      (att-value 'pretty-print (ast-child 'function n))
      lparen
      (h-concat
       (add-between (map (λ (a) (att-value 'pretty-print a))
                         (ast-children (ast-child 'args n)))
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

 [StructReference
  (λ (n)
    (h-append lparen
              (att-value 'pretty-print (ast-child 'structval n))
              rparen
              period
              (text (ast-child 'fieldname n))))]
 [StructSetField
  (λ (n)
    (h-append lparen
              lparen
              (att-value 'pretty-print (ast-child 'structval n))
              rparen
              period
              (text (ast-child 'fieldname n))
              space
              eqsign
              space
              (att-value 'pretty-print (ast-child 'updateval n))
              rparen))]

 ;; TODO -- gen the name of the safe op function from the type of the operator
 [AdditionExpression {binary-expression-print/function type->print-add}]
 [SubtractionExpression {binary-expression-print/function type->print-sub}]
 [MultiplicationExpression {binary-expression-print/function type->print-mul}]
 [DivisionExpression {binary-expression-print/function type->print-div}]
 [ModulusExpression {binary-expression-print/function type->print-mod}]

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
 )




(ag
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

(ag hints
    [Node (λ (n) (dict-ref (att-value 'children-hint-dict (ast-parent n))
                           n
                           '()))])

(ag
 children-misc-constraint-dict
 ;; dictionary will contain a set of constraints (or nothing) for each child
 [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n) (list 'constant)))]
 [Node (λ (n) (hasheq))])

(ag misc-constraints
    [Node
     (λ (n)
       (define p (parent-node n))
       (if p
           (let ([cs (set-union (dict-ref (att-value 'children-misc-constraint-dict p)
                                          n
                                          '())
                                (att-value 'misc-constraints p))])
             (if (node-subtype? p 'ExpressionStatement)
                 ;; ExpressionStatements allow assignments and non-constants,
                 ;; but assignments are disallowed everywhere else.
                 (set-subtract cs '(no-assignment constant))
                 (set-union cs '(no-assignment))))
           '()))])


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
    (att-value 'xsmith_find-a-descendant
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

(define rosette-last-ast-serial-number -1)
(define (maybe-reset-rosette-assertions! n)
  (define current-ast-serial (att-value 'ast-serial-number n))
  (when (not (equal? rosette-last-ast-serial-number
                     current-ast-serial))
    (set! rosette-last-ast-serial-number current-ast-serial)
    (rt:clear-asserts!)))

(add-att-rule
 cish-rules
 symbolic-interp
 ;; Get the single global result
 [Node (λ (n)
         (maybe-reset-rosette-assertions! n)
         ;; Interp the containing function to fill the result hash.
         (define result
           (symbolic-interp-wrap
            (att-value 'get-containing-function-definition n)
            symbolic-store-top
            '()
            #f
            '()))
         ;; If the code is unreachable then it will have no result here.
         (match (hash-ref
                 (att-value 'symbolic-interp-result-hash n)
                 n
                 'dead)
           ['dead (list 'dead 'dead)]
           [(list (list vals stores always-rets assert-sets) ...)
            (define fv (fresh-symbolic-var (att-value 'type-context n)))
            (define val-assert (apply rt:|| (map (λ (v) (rt:= fv v)) vals)))
            (list fv (apply set-union (list val-assert) assert-sets))]))])
(ag
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
                 n
                 'dead)
           ['dead 'dead]
           [(list (list vs ss rs) ...)
            (list (apply abstract-value-merge*/range vs)
                  (apply abstract-store-merge*/range ss)
                  (apply abstract-flow-control-return-merge* rs))]))])
(ag
 get-containing-function-definition
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main_inner"))
                                n
                                (ast-parent n)))]
 [Program (λ (n) n)]
 [Node (λ (n) (att-value 'get-containing-function-definition (ast-parent n)))])

(ag
 ;; This is essentially the same as abstract-interp-result-hash/range
 symbolic-interp-result-hash
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main_inner"))
                                (make-hasheq)
                                (att-value 'symbolic-interp-result-hash
                                           (ast-parent n))))]
 [Program (λ (n) (make-hasheq))]
 [Node (λ (n) (att-value 'symbolic-interp-result-hash (ast-parent n)))]
 )

(ag
 ;; To get a general interp result for any given node, we need to cache
 ;; the results of evaluation when we interpret the whole function it is in.
 ;; We collect results in this hash, then merge if there are multiple results.
 ;; The hash is MUTATED during interpretation to achieve this.
 ;; The hash holds a list of results for each node (or nothing -- use '() as
 ;; a default).  Although an empty result should mean the node is dead code.
 abstract-interp-result-hash/range
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main_inner"))
                                (make-hasheq)
                                (att-value 'abstract-interp-result-hash/range
                                           (ast-parent n))))]
 [Program (λ (n) (make-hasheq))]
 [Node (λ (n) (att-value 'abstract-interp-result-hash/range (ast-parent n)))]
 )

(ag
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
                                 (ast-children (ast-child 'declarations n)))])
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
 [NullStatement (λ (n store flow-returns)
                  (list abstract-value/range/top store flow-returns))]
 #|
 TODO - there are no void functions yet, so once there are this (and all
 non-return statements) should return void.
 |#
 #;[VoidReturnStatement
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
                ([decl (ast-children (ast-child 'declarations n))])
        ;; There are declaration holes and such, so check that it is a variable decl.
        (if (equal? (ast-node-type decl) 'VariableDeclaration)
            (match-let* ([(list v n-store n-rets)
                          (abstract-interp-wrap/range decl s r)])
              (values n-store n-rets))
            (values s r))))
    (define-values (store-after-statements rets-after-statements)
      (for/fold ([s store-with-decls]
                 [r ret-with-decls])
                ([statement (ast-children (ast-child 'statements n))])
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
    (match-let* ([binding (resolve-variable-reference-node (ast-child 'function n))]
                 [func-def-node (binding-ast-node binding)]
                 [func-block (ast-child 'Block func-def-node)]
                 [func-params (ast-children (ast-child 'params func-def-node))]
                 [(list reversed-args store rets)
                  (values->list
                   (for/fold ([args-so-far '()]
                              [store store]
                              [rets flow-returns])
                             ([expr (ast-children (ast-child 'args n))])
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
 )

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

(define ({result-in-bounds/range range-op min max} l-l l-h r-l r-h)
  (define ({bounded-range min max} low high)
    (and (<= min low) (<= min high) (>= max low) (>= max high)))
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

(define (verify assert-to-verify other-asserts)
  (rt:solver-push (rt:current-solver))
  (rt:solver-assert (rt:current-solver) other-asserts)
  (rt:solver-assert (rt:current-solver) (list (rt:! assert-to-verify)))
  (define result
    (with-handlers ([(λ _ #t) (λ _ #f)])
      ;; TODO - sometimes I get what looks like a Rosette error:
      ;; “solution: unrecognized solver output: (error line 19 column 10: model is not available)”
      (rt:solver-check (rt:current-solver))))
  (rt:solver-pop (rt:current-solver))
  (rt:unsat? result))

(define ({result-in-bounds/symbolic range-op min max} l r asserts)
  (define ({bounded-symbolic min max} v)
    (verify (rt:&& (rt:>= v min) (rt:<= v max))
            asserts))
  ({bounded-symbolic min max} (range-op l r)))
(define (division-safety-check/symbolic l r asserts)
  (verify (rt:&& (rt:! (rt:= r 0))
                 (rt:|| (rt:! (rt:= l INT_MIN))
                        (rt:! (rt:= r -1))))
          asserts))
(define ({safe-binary-op-swap/symbolic unsafe-version safety-pred} n)
  ;; safety-pred is a predicate on two symbolic values
  (define l (ast-child 'l n))
  (define r (ast-child 'r n))
  (match-define (list l-result l-asserts)
    (att-value 'symbolic-interp (ast-child 'l n)))
  (match-define (list r-result r-asserts)
    (att-value 'symbolic-interp (ast-child 'r n)))
  (if (member 'dead (list l-result r-result))
      #f
      (if (safety-pred l-result r-result (set-union l-asserts r-asserts))
          unsafe-version
          #f)))

(define {symbolic-binary-op #:safety-clause [make-violation-condition #f]
                            #:result-clause make-normal-result
                            #:bool-result? [bool-result? #f]}
  ;; safety-clause and result-clause are both functions that take a
  ;; left and right rosette clause and returns a new clause.
  ;; The safety-clause should be true if undefined behavior would be tripped,
  ;; the result-clause should be the result of the unsafe operation.
  (λ (n store path-condition return-variable assertions)
    (define type (att-value 'type-context n))
    (match-let* ([(list v-l s-l ar-l asserts-l)
                  (symbolic-interp-wrap
                   (ast-child 'l n)
                   store path-condition return-variable assertions)]
                 [(list v-r s-r ar-r asserts-r)
                  (symbolic-interp-wrap
                   (ast-child 'r n)
                   s-l path-condition return-variable asserts-l)]
                 ;; rt:/ errors if the right hand arg is known to be 0...
                 ;; So even though I'm going to check it later, I can't allow
                 ;; it here either.
                 [div-guard? (member make-normal-result (list rt:/ rt:modulo))]
                 [fresh-r (if div-guard? (fresh-symbolic-var type) #f)]
                 [normal-result (if div-guard?
                                    (make-normal-result v-l fresh-r)
                                    (make-normal-result v-l v-r))]
                 [div-guard-assert (if div-guard?
                                       (rt:=> (rt:! (rt:= 0 v-r))
                                              (rt:= v-r fresh-r))
                                       #t)]
                 [fv (fresh-symbolic-var (att-value 'type-context n))])
      (cond [make-violation-condition
             (let* ([violation-condition (make-violation-condition v-l v-r)]
                    [safety-assertion (rt:&& (rt:=> violation-condition
                                                    (rt:= fv v-l))
                                             (rt:=> (rt:! violation-condition)
                                                    (rt:= fv normal-result)))])
               (list fv s-r #f (set-union asserts-r
                                          (list safety-assertion div-guard-assert))))]
            [bool-result?
             (list fv s-r #f (set-union asserts-r
                                        (list (rt:=> (rt:! normal-result)
                                                     (rt:= 0 fv))
                                              (rt:=> normal-result
                                                     (rt:= 1 fv))
                                              div-guard-assert)))]
            [else (list fv s-r #f (set-union asserts-r
                                             (list (rt:= fv normal-result)
                                                   div-guard-assert)))]))))

(define {make-normal-symbolic-safety-func op}
  (λ (l r) (rt:|| (rt:> (op l r) INT_MAX)
                  (rt:< (op l r) INT_MIN))))
(define symbolic-addition-safety {make-normal-symbolic-safety-func rt:+})
(define symbolic-subtraction-safety {make-normal-symbolic-safety-func rt:-})
(define symbolic-multiplication-safety {make-normal-symbolic-safety-func rt:*})
(define symbolic-division-safety
  (λ (l r)
    (rt:|| (rt:= r 0)
           (rt:&& (rt:= l INT_MIN)
                  (rt:= r -1)))))



(ag
 ;;; Return #f if the unsafe op can't be safely used,
 ;;; otherwise return the name of the unsafe type to be used
 ;;; as a refinement.
 unsafe-op-if-possible/range
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
   division-safety-check/range}])

(ag
 unsafe-op-if-possible/symbolic
 [AdditionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeAdditionExpression
   {result-in-bounds/symbolic rt:+ INT_MIN INT_MAX}}]
 [SubtractionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeSubtractionExpression
   {result-in-bounds/symbolic rt:- INT_MIN INT_MAX}}]
 [MultiplicationExpression
  {safe-binary-op-swap/symbolic
   'UnsafeMultiplicationExpression
   {result-in-bounds/symbolic rt:* INT_MIN INT_MAX}}]
 [DivisionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeDivisionExpression
   division-safety-check/symbolic}]
 [ModulusExpression
  {safe-binary-op-swap/symbolic
   'UnsafeModulusExpression
   division-safety-check/symbolic}])

(define (symbolic-store-merge s1 pc1 s2 pc2)
  (define new-asserts '())
  (define new-store
    (for/hash ([k (set-union (hash-keys s1) (hash-keys s2))])
      (define v1 (hash-ref s1 k 'not-found))
      (define v2 (hash-ref s2 k 'not-found))
      (cond [(equal? v1 'not-found) (values k v2)]
            [(equal? v2 'not-found) (values k v1)]
            [(equal? v1 v2) (values k v1)]
            [else
             (define fv (fresh-symbolic-var (binding-type k)))
             (set! new-asserts (set-add new-asserts (rt:=> (asserts->condition pc1)
                                                           (rt:= fv v1))))
             (set! new-asserts (set-add new-asserts (rt:=> (asserts->condition pc2)
                                                           (rt:= fv v2))))
             (values k fv)])))
  (values new-store new-asserts))

(define (asserts->condition asserts)
  (apply rt:&& (set->list asserts)))

(define {symbolic-if-interp one-sided? type}
  (λ (n store path-condition return-variable assertions)
    (match-define (list test-val test-store test-always-ret assertions-with-test)
      (symbolic-interp-wrap (ast-child 'test n)
                            store path-condition return-variable assertions))
    (define assert-test
      (if (rt:boolean? test-val)
          test-val
          (rt:! (rt:= 0 test-val))))
    (define pc-true (set-add path-condition assert-test))
    (define pc-false (set-add path-condition (rt:! assert-test)))
    (define then-rets
      (symbolic-interp-wrap (ast-child 'then n)
                            test-store
                            pc-true
                            return-variable
                            (set-add assertions-with-test assert-test)))
    (match-define (list then-v then-store then-always-rets then-asserts-pre)
      then-rets)
    (define then-asserts-unique (set-subtract then-asserts-pre assertions-with-test))
    (define asserts-with-then
      (set-add assertions-with-test
               (rt:=> (asserts->condition pc-true)
                      (asserts->condition then-asserts-unique))))

    (if one-sided?
        (let-values ([(merged-store merge-asserts)
                      (symbolic-store-merge test-store pc-false then-store pc-true)])
          (list #t
                merged-store
                #f
                (set-union asserts-with-then merge-asserts)))
        (let ()
          (match-define
            (list else-v else-store else-always-rets else-asserts-pre)
            (symbolic-interp-wrap (ast-child 'else n)
                                  test-store
                                  pc-false
                                  return-variable
                                  (set-add assertions-with-test (rt:! assert-test))))
          (define else-asserts-unique
            (set-subtract else-asserts-pre assertions-with-test))
          (define asserts-with-else
            (set-add asserts-with-then
                     (rt:=> (asserts->condition pc-false)
                            (asserts->condition else-asserts-unique))))
          (define always-ret? (and then-always-rets else-always-rets))
          (define-values (merged-store merge-asserts)
            (symbolic-store-merge else-store pc-false then-store pc-true))
          (define asserts-post-merge
            (set-union asserts-with-else merge-asserts))
          (if (not type)
              (list #t
                    merged-store
                    always-ret?
                    asserts-post-merge)
              (let ([v (fresh-symbolic-var type)])
                (list v
                      merged-store
                      always-ret?
                      (set-add asserts-post-merge
                               (rt:&& (rt:=> (asserts->condition pc-true)
                                             (rt:= then-v v))
                                      (rt:=> (asserts->condition pc-false)
                                             (rt:= else-v v)))))))))))

(define (symbolic-interp-loop/body n store path-condition return-variable assertions)
  (define altered-refs (att-value 'find-transitive-assignments n))
  (define new-store
    (for/fold ([s store])
              ([a altered-refs])
      (dict-set s a (fresh-symbolic-var (binding-type a)))))
  (define has-return?
    (att-value 'xsmith_find-a-descendant
               n
               (λ (node)
                 (and (ast-node? node)
                      (node-subtype? node 'ReturnStatement)))))
  (match-define (list test-val t-store t-always-rets t-asserts)
    (symbolic-interp-wrap (ast-child 'test n)
                          new-store
                          path-condition
                          return-variable
                          assertions))

  (define assert-test
    (if (rt:boolean? test-val)
        test-val
        (rt:! (rt:= 0 test-val))))
  (define pc-true (set-add path-condition assert-test))
  (define pc-false (set-add path-condition (rt:! assert-test)))
  ;; TODO - I'm not sure right now what I want to do with the loop body.
  ;;        I don't want to try some sort of unrolling, so for now I'll
  ;;        just punt any assigned variables to fresh variables.
  ;;        I really should capture potential returns.  I'll do that later.
  (match-define (list b-v b-store b-always-rets n-asserts)
    (symbolic-interp-wrap (ast-child 'body n)
                          t-store
                          (set-add path-condition assert-test)
                          return-variable
                          assertions))
  (list #f b-store #f t-asserts))

(ag
 symbolic-interp-do
 ;; This rule adds rosette assertions, so it should only be called when
 ;; the rosette environment has been prepared (eg. pushed/popped an
 ;; interpreter, so two interpretations don't step on each other).

 ;; Returns (list val=[symbolic expression] store=[binding->symbolic-expression hash] always-returns=[bool])

 ;;; Declarations
 [Program
  (λ (n store path-condition return-variable assertions)
    (define-values (init-store init-asserts)
      (for/fold ([store store]
                 [assertions assertions])
                ([global (filter (λ (cn) (node-subtype? cn 'VariableDeclaration))
                                 (ast-children (ast-child 'declarations n)))])
        (match-let* ([(list v n-store n-rets n-asserts)
                      (symbolic-interp-wrap
                       global
                       store
                       path-condition
                       return-variable
                       assertions)])
          (values n-store n-asserts))))
    (define main (ast-child 'main n))
    (define main-ret (fresh-symbolic-var int))
    (symbolic-interp-wrap
     main
     init-store
     path-condition
     main-ret
     init-asserts))]
 ;; Function Definitions will be evaluated with store and arguments as top for
 ;; analyzing for potential code transformations.
 [FunctionDefinition
  (λ (n store path-condition return-variable assertions)
    (define ret-var (fresh-symbolic-var (car (reverse (ast-child 'type n)))))
    (symbolic-interp-wrap
     (ast-child 'Block n)
     symbolic-store-top
     '()
     ret-var
     assertions))]
 [VariableDeclaration
  (λ (n store path-condition return-variable assertions)
    (let* ([name (ast-child 'name n)]
           [type (ast-child 'type n)]
           [sym-var (fresh-symbolic-var type)]
           [ref (resolve-variable-reference-node n)])
      (match-define (list v n-store always-rets n-asserts)
        (symbolic-interp-wrap (ast-child 'Expression n)
                              store path-condition return-variable assertions))
      (list v
            (dict-set n-store ref sym-var)
            #f
            (set-add n-asserts (rt:= sym-var v)))))]

 ;;; Statements
 [WhileStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-loop/body n store path-condition return-variable assertions))]
 [DoWhileStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-loop/body n store path-condition return-variable assertions))]
 [ForStatement
  (λ (n store path-condition return-variable assertions)
    (match-define (list i-v i-store always-rets i-asserts)
      (symbolic-interp-wrap (ast-child 'init n)
                            store path-condition return-variable assertions))
    (define u-store
      (for/fold ([s i-store])
                ([a (att-value 'find-transitive-assignments (ast-child 'update n))])
        (dict-set s a (fresh-symbolic-var (binding-type a)))))
    (symbolic-interp-loop/body n u-store path-condition return-variable i-asserts))]
 [Block
  (λ (n store path-condition return-variable assertions)
    (define (rec store asserts children-left)
      (if (null? children-left)
          (list #f store #f asserts)
          (match-let ([(list v n-store always-rets n-asserts)
                       (symbolic-interp-wrap (car children-left)
                                             store
                                             path-condition
                                             return-variable
                                             asserts)])
            (if always-rets
                (list #f n-store always-rets n-asserts)
                (rec n-store n-asserts (cdr children-left))))))
    (rec store assertions (ast-children (ast-child 'statements n))))]
 [ValueReturnStatement
  (λ (n store path-condition return-variable assertions)
    (match-define (list v n-store always-rets n-asserts)
      (symbolic-interp-wrap (ast-child 'Expression n)
                            store path-condition return-variable assertions))
    (list v
          n-store
          #t
          (set-add n-asserts (rt:=> (apply rt:&& path-condition)
                                    (rt:= v return-variable)))))]
 [IfStatement {symbolic-if-interp #t #f}]
 [IfElseStatement {symbolic-if-interp #f #f}]
 [ExpressionStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-wrap (ast-child 'Expression n)
                          store path-condition return-variable assertions))]
 [NullStatement
  (λ (n store path-condition return-variable assertions)
    (list #f store #f assertions))]

 ;;; Expressions
 [FunctionApplicationExpression
  (λ (n store path-condition return-variable assertions)
    ;; TODO - this is almost the same as the one for abstract-interp/range
    ;; I should abstract over them somehow.
    (define ref-node (resolve-variable-reference-node (ast-child 'function n)))
    (define def-node (binding-ast-node ref-node))
    (define func-block (ast-child 'Block def-node))
    (define func-params (ast-children (ast-child 'params def-node)))
    (match-define (list reversed-args store-post-arguments asserts-post-arguments)
      (values->list
       (for/fold ([args-so-far '()]
                  [store store]
                  [asserts assertions])
                 ([expr (ast-children (ast-child 'args n))])
         (match-define (list v s ar n-asserts)
           (symbolic-interp-wrap expr store path-condition return-variable asserts))
         (values (cons v args-so-far) s n-asserts))))
    (define store-for-func (for/fold ([store store-post-arguments])
                                     ([fp func-params]
                                      [arg (reverse reversed-args)])
                             (dict-set store
                                       (resolve-variable-reference-node fp)
                                       arg)))

    (define ret-type (car (reverse (ast-child 'type def-node))))
    (define return-var-for-func (fresh-symbolic-var ret-type))

    (match (symbolic-interp-wrap func-block
                                 store-for-func
                                 path-condition
                                 return-var-for-func
                                 asserts-post-arguments)
      [(list v s always-rets as)
       (list return-var-for-func s #f as)]))]
 [IfExpression
  (λ (n store path-condition return-variable assertions)
    ({symbolic-if-interp #f
                         (or (att-value 'type-context n)
                             (att-value 'type-context
                                        (ast-child 'then n)))}
     n store path-condition return-variable assertions))]
 [VariableReference
  (λ (n store path-condition return-variable assertions)
    (define ref (resolve-variable-reference-node n))
    (define val (hash-ref store ref
                          (λ () (fresh-symbolic-var
                                 (binding-type ref)))))
    (list val
          store
          #f
          assertions))]
 [AssignmentExpression
  (λ (n store path-condition return-variable assertions)
    (match-let ([(list val new-store always-rets n-asserts)
                 (symbolic-interp-wrap
                  (ast-child 'Expression n)
                  store
                  path-condition
                  return-variable
                  assertions)]
                [ref-node (resolve-variable-reference-node n)])
      (list val (dict-set new-store ref-node val) #f n-asserts)))]
 [LiteralInt
  (λ (n store path-condition return-variable assertions)
    (list (ast-child 'val n) store #f assertions))]
 [LiteralFloat
  (λ (n store path-condition return-variable assertions)
    (list (ast-child 'val n) store #f assertions))]
 [AdditionExpression
  {symbolic-binary-op #:safety-clause symbolic-addition-safety
                      #:result-clause rt:+}]
 [SubtractionExpression
  {symbolic-binary-op #:safety-clause symbolic-subtraction-safety
                      #:result-clause rt:-}]
 [MultiplicationExpression
  {symbolic-binary-op #:safety-clause symbolic-multiplication-safety
                      #:result-clause rt:*}]
 [DivisionExpression
  {symbolic-binary-op #:safety-clause symbolic-division-safety
                      #:result-clause rt:/}]
 [ModulusExpression
  {symbolic-binary-op #:safety-clause symbolic-division-safety
                      #:result-clause rt:modulo}]

 [UnsafeAdditionExpression
  {symbolic-binary-op #:result-clause rt:+}]
 [UnsafeSubtractionExpression
  {symbolic-binary-op #:result-clause rt:-}]
 [UnsafeMultiplicationExpression
  {symbolic-binary-op #:result-clause rt:*}]
 [UnsafeDivisionExpression
  {symbolic-binary-op #:result-clause rt:/}]
 [UnsafeModulusExpression
  {symbolic-binary-op #:result-clause rt:modulo}]

 [EqualityExpression
  {symbolic-binary-op #:result-clause rt:=
                      #:bool-result? #t}]
 [LessThanExpression
  {symbolic-binary-op #:result-clause rt:<
                      #:bool-result? #t}]
 [GreaterThanExpression
  {symbolic-binary-op #:result-clause rt:>
                      #:bool-result? #t}]

 [LessOrEqualExpression
  {symbolic-binary-op #:result-clause rt:<=
                      #:bool-result? #t}]
 [GreaterOrEqualExpression
  {symbolic-binary-op #:result-clause rt:>=
                      #:bool-result? #t}]
 )


(ag
 find-direct-resolved
 [Node (λ (n node-type)
         (remove-duplicates
          (map resolve-variable-reference-node
               (att-value 'xsmith_find-descendants
                          n (λ (cn) (node-subtype? cn node-type))))))])
(ag
 find-direct-assignments
 [Node (λ (n) (att-value 'find-direct-resolved n 'AssignmentExpression))])
(ag
 find-direct-function-call-refs
 [Node (λ (n) (att-value 'find-direct-resolved n 'FunctionApplicationExpression))])
(ag
 find-direct-variable-references
 [Node (λ (n) (att-value 'find-direct-resolved n 'VariableReference))])

(ag
 find-transitive-function-call-refs
 [Node (λ (n)
         (define (rec to-search searched)
           (if (empty? to-search)
               searched
               (let* ([calls (att-value 'find-direct-function-call-refs
                                        (binding-ast-node (car to-search)))]
                      [searched (cons (car to-search) searched)]
                      [new-calls (set-subtract calls searched)]
                      [to-search (append new-calls (cdr to-search))])
                 (rec to-search searched))))
         (rec (att-value 'find-direct-function-call-refs n) '()))])

(ag
 find-transitive-resolved
 [Node (λ (n node-type)
         (remove-duplicates
          (append (att-value 'find-direct-resolved n node-type)
                  (flatten
                   (map (λ (r) (att-value 'find-direct-resolved
                                          (binding-ast-node r)
                                          node-type))
                        (att-value 'find-transitive-function-call-refs n))))))])
(ag
 find-transitive-assignments
 [Node (λ (n) (att-value 'find-transitive-resolved n 'AssignmentExpression))])
(ag
 find-transitive-variable-references
 [Node (λ (n) (att-value 'find-transitive-resolved n 'VariableReference))])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; choice rules


(define-syntax-parser cm
  [(_ method [node-name lambda-body] ...+)
   #'(add-choice-rule cish-rules method [node-name (λ () lambda-body)] ...)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (hinted-choice-weight stx)
  (syntax-parse stx
    [(_ base-weight hint-name)
     #'(begin
         (define bw base-weight)
         ;(define w (or bw (super choice-weight)))
         ;; TODO - use super choice-weight, not 5.  This is a temporary fix to move debugging along.
         (define w (or bw 5))
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
               (* 2 (att-value 'xsmith_ast-depth current-hole))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (misc-constraints-choice-rule-default cur-hole)
  (let ([cs (att-value 'misc-constraints cur-hole)])
    (not (set-member? cs 'constant))))

(cm misc-constraints
    [Node (misc-constraints-choice-rule-default (current-hole))]
    [LiteralInt #t]
    [LiteralFloat #t]
    [LiteralStruct #t]
    [VolatileInitializer #t]
    [VariableReference
     (or (let ([p (parent-node (current-hole))])
           (and (ast-subtype? p 'LiteralStruct)
                (eq? (current-hole) (ast-child 'structdefref p))))
         (misc-constraints-choice-rule-default (current-hole)))]
    [AssignmentExpression
     (set-empty? (set-intersect '(constant no-assignment)
                                (att-value 'misc-constraints
                                           current-hole)))]
    [FunctionDefinition
     (equal? (node-type (parent-node current-hole)) 'Program)])

(add-prop
 cish-rules
 choice-weight
 [Node 10]
 [NullStatement 1]
 [ExpressionStatement 70]
 [IfStatement (top-heavy-choice 5)]
 [IfElseStatement (top-heavy-choice 5)]
 [WhileStatement (top-heavy-choice 2)]
 [DoWhileStatement (top-heavy-choice 2)]
 [ForStatement (top-heavy-choice 7)]
 [Block (hinted-choice-weight 1 block-hint)]
 [ReturnStatement 1]
 [ValueReturnStatement 1]
 [AssignmentExpression (hinted-choice-weight assignment-hint)]
 [FunctionApplicationExpression
  (hinted-choice-weight application-hint)]
 [VariableDeclaration 20]
 ;; Don't choose variable reference often when I'm already in a max-depth lift.
 [VariableReference (if (>= (att-value 'xsmith_ast-depth current-hole)
                            (xsmith-max-depth))
                        1
                        10)]
 )


(add-prop
 cish-rules
 wont-over-deepen
 [ExpressionStatement #t]
 [ValueReturnStatement #t]
 [AssignmentExpression #t]
 [VariableDeclaration #t]
 )

(cm features-enabled
    [Node (andmap xsmith-feature-enabled?
                  (send this features))])

(cm features
    [Node '()]
    [NullStatement '(null-statement)]
    [IfStatement '(if-statement)]
    [LoopStatement '(loop-statement)]
    [IfExpression '(if-expression)]
    [LiteralInt '(int)]
    [LiteralFloat '(float)]
    [AdditionExpression '(addition)]
    [MultiplicationExpression '(multiplication)]
    [SubtractionExpression '(subtraction)]
    [DivisionExpression '(division)]
    [ModulusExpression '(modulus)]
    [EqualityExpression '(comparisons)]
    [LessThanExpression '(comparisons)]
    [GreaterThanExpression '(comparisons)]
    [LessOrEqualExpression '(comparisons)]
    [GreaterOrEqualExpression '(comparisons)]
    [VolatileVariableReference '(volatile)]
    [VolatileInitializer '(volatile)]
    [StructDefinition '(structs)]
    [LiteralStruct '(structs)]
    [StructReference '(structs)]
    [StructSetField '(structs)]
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
