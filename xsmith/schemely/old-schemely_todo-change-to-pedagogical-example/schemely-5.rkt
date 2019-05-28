#!/usr/bin/env racket
#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019 The University of Utah
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
 xsmith
 racr
 xsmith/racr-convenience
 racket/pretty
 racket/random
 racket/list
 racket/class
 racket/string
 )

(define-spec-component schemely-core)

(define (arg-length)
  (random 4))

(add-to-grammar
 schemely-core
 [DefinitionContext #f ([definitions : Definition * = (random 3)]
                        [expressions : Expression * = (add1 (random 3))])
   #:prop strict-child-order? #t]
 [Program DefinitionContext ()]

 [Definition #f ([type]
                 [name]
                 Expression)
   #:prop binder-info (name type definition)]

 [Expression #f ()
             #:prop may-be-generated #f]

 [Application Expression
              ([procedure : Expression]
               [arguments : Expression * = (arg-length)])
              #:prop choice-weight 50]
 [Lambda Expression ([params : FormalParam * = (arg-length)]
                     ;;[body : DefinitionContext]
                     [body : Expression]
                     )
         #:prop wont-over-deepen #t]
 [FormalParam #f (type [name = (fresh-var-name "arg-")])
              #:prop binder-info (name type parameter)]

 [LetStar Expression ([definitions : Definition * = (random 3)]
                      [body : DefinitionContext])
          #:prop strict-child-order? #t]

 [VariableReference Expression (name)
                    #:prop reference-info (read name)
                    #:prop choice-weight 10]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write name)]

 [LiteralBool Expression ([v = (even? (random 2))])]
 [Not Expression ([Expression])]
 ;; TODO - many of these I've defined as binary but they could be variadic instead.
 [And Expression ([l : Expression] [r : Expression])]
 [Or Expression ([l : Expression] [r : Expression])]

 [LiteralNumber Expression ([v = (* (random 1000000)
                                    (if (equal? 0 (random 2)) -1 1))])]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]
 [Times Expression ([l : Expression] [r : Expression])]
 [SafeDivide Expression ([l : Expression] [r : Expression])]
 [LessThan Expression ([l : Expression] [r : Expression])]
 [GreaterThan Expression ([l : Expression] [r : Expression])]

 [LiteralString Expression ([v = (random-ref (list "foo" "bar" "baz" "quux"))])]
 [StringAppend Expression ([l : Expression] [r : Expression])]
 [StringLength Expression (Expression)]

 [LiteralEmptyList Expression ()]
 [List Expression ([arguments : Expression * = (add1 (random 8))])]
 [Cons Expression ([v : Expression]
                   [l : Expression])]
 [SafeCar Expression ([default : Expression] [list : Expression])]
 [SafeCdr Expression (Expression)]
 [EmptyP Expression (Expression)]

 [EqualP Expression ([l : Expression] [r : Expression])]

 [If Expression ([test : Expression] [then : Expression] [else : Expression])
     #:prop strict-child-order? #t]

 )


;; helper for to-s-exp
(define (->se sym . children-refs)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp (ast-child x n)))
                       children-refs))))
(define (->se* sym children-ref)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp x))
                       (ast-children (ast-child children-ref n))))))

(add-att-rule
 schemely-core
 to-s-exp
 [DefinitionContext (λ (n) `(,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'definitions n)))
                             ,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'expressions n)))))]
 [Definition (λ (n) `(define ,(string->symbol (ast-child 'name n))
                       ,(att-value 'to-s-exp (ast-child 'Expression n))))]
 [Lambda (λ (n) `(lambda (,@(map (λ (x) (string->symbol (ast-child 'name x)))
                                 (ast-children (ast-child 'params n))))
                   ,(att-value 'to-s-exp (ast-child 'body n))))]
 [Application (λ (n) `(,(att-value 'to-s-exp (ast-child 'procedure n))
                       ,@(map (λ (x) (att-value 'to-s-exp x))
                              (ast-children (ast-child 'arguments n)))))]

 [LetStar (λ (n) `(let* (,@(map (λ (d) `(,(string->symbol (ast-child 'name d))
                                         ,(att-value 'to-s-exp
                                                     (ast-child 'Expression d))))
                                (ast-children (ast-child 'definitions n))))
                    ,@(att-value 'to-s-exp (ast-child 'body n))))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
 [SetBangRet (λ (n) `(begin
                       (set! ,(string->symbol (ast-child 'name n))
                             ,(att-value 'to-s-exp (ast-child 'Expression n)))
                       ,(string->symbol (ast-child 'name n))))]

 [LiteralBool (λ (n) (ast-child 'v n))]
 [Not (->se 'not 'Expression)]
 [And (->se 'and 'l 'r)]
 [Or (->se 'or 'l 'r)]

 [LiteralNumber (λ (n) (ast-child 'v n))]
 [Plus (->se '+ 'l 'r)]
 [Minus (->se '- 'l 'r)]
 [Times (->se '* 'l 'r)]
 [SafeDivide (->se 'safe-/ 'l 'r)]
 [LessThan (->se '< 'l 'r)]
 [GreaterThan (->se '> 'l 'r)]

 [LiteralString (λ (n) (ast-child 'v n))]
 [StringAppend (->se 'string-append 'l 'r)]
 [StringLength (->se 'string-length 'Expression)]

 [LiteralEmptyList (λ (n) ''())]
 [List (->se* 'list 'arguments)]
 [Cons (->se 'cons 'v 'l)]
 [SafeCar (->se 'safe-car 'default 'list)]
 [SafeCdr (->se 'safe-cdr 'Expression)]
 [EmptyP (->se 'null? 'Expression)]

 [EqualP (->se 'equal? 'l 'r)]

 [If (->se 'if 'test 'then 'else)]
 )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

(define number (base-type 'number))
(define (number-type? x) (can-unify? x number))
(define bool (base-type 'bool))
(define (bool-type? x) (can-unify? x bool))
(define string (base-type 'string))
(define (string-type? x) (can-unify? x string))

(define (type-thunks-for-concretization) (list (λ()number) (λ()bool) (λ()string)))

(define-generic-type list-type (type))
(define (fresh-list-type) (list-type (fresh-type-variable)))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (concretize-type (fresh-type-variable))))


(define numeric-bin-op-type (λ (n t) (hash 'l number 'r number)))
(add-prop
 schemely-core
 type-info
 [DefinitionContext [(fresh-type-variable)
                     (λ (n t)
                       (define last-expression
                         (car (reverse (ast-children
                                        (ast-child 'expressions n)))))
                       (hash last-expression t
                             'definitions (λ (c) (fresh-type-variable))
                             'expressions (λ (c) (fresh-type-variable))))]]
 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [VariableReference [(fresh-type-variable) (no-child-types)]]
 ;; TODO - the type of a variable write must not be function, which is checked dynamically.  But it would be better if this were accomplished in some way that just let me write an unconstrained type variable here and filter out the possibility of set! on a function rather than having a runtime error.  Even this verbose type doesn't capture all the non-function types I can have, since there are a ton of possible list types (including nested lists).
 [SetBangRet [(fresh-type-variable number bool string
                                   (list-type (fresh-type-variable
                                               number bool string)))
              (λ (n t) (hash 'Expression t))]]
 [LetStar [(fresh-type-variable) (λ (n t)
                                   (hash 'body t
                                         'definitions (λ (c) (fresh-type-variable))))]]
 [Lambda [(function-type (product-type #f) (fresh-type-variable))
          (λ (n t)
            (define args-type (product-type
                               (map (λ(x)(fresh-type-variable))
                                    (ast-children (ast-child 'params n)))))
            (define return-type (fresh-type-variable))
            (unify! (function-type args-type return-type)
                    t)
            (define args-list (product-type-inner-type-list args-type))
            (hash-set
             (for/hash ([c (ast-children (ast-child 'params n))]
                        [at args-list])
               (values c at))
             (ast-child 'body n)
             return-type))]]
 [Application [(fresh-type-variable)
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
 [FormalParam [(fresh-type-variable) (no-child-types)]]

 [LiteralBool [bool (no-child-types)]]
 [Not [bool (λ (n t) (hash 'Expression bool))]]
 [And [bool (λ (n t) (hash 'l bool 'r bool))]]
 [Or [bool (λ (n t) (hash 'l bool 'r bool))]]

 [LiteralNumber [number (no-child-types)]]
 [Plus [number numeric-bin-op-type]]
 [Minus [number numeric-bin-op-type]]
 [Times [number numeric-bin-op-type]]
 [SafeDivide [number numeric-bin-op-type]]
 [LessThan [bool numeric-bin-op-type]]
 [GreaterThan [bool numeric-bin-op-type]]

 [LiteralString [string (no-child-types)]]
 [StringAppend [string (λ (n t) (hash 'l string 'r string))]]
 [StringLength [number (λ (n t) (hash 'Expression string))]]

 [LiteralEmptyList [(fresh-list-type) (no-child-types)]]
 [List [(fresh-list-type) (λ (n t)
                            (define lt (fresh-list-type))
                            (unify! t lt)
                            (define inner (list-type-type lt))
                            (hash 'arguments inner))]]
 [Cons [(fresh-list-type) (λ (n t)
                            (define lt (fresh-list-type))
                            (unify! t lt)
                            (define inner (list-type-type lt))
                            (hash 'v inner 'l t))]]
 [SafeCar [(fresh-type-variable) (λ (n t) (hash 'default t 'list (list-type t)))]]
 [SafeCdr [(fresh-list-type) (λ (n t) (hash 'Expression t))]]
 [EmptyP [bool (λ (n t) (hash 'Expression (fresh-list-type)))]]

 [EqualP [bool (λ (n t)
                 (define arg-type (fresh-type-variable))
                 (hash 'l arg-type 'r arg-type))]]

 [If [(fresh-type-variable) (λ (n t) (hash 'test bool 'then t 'else t))]]
 )

(add-prop
 schemely-core
 fresh
 [VariableReference
  ;; TODO - getting a name for a reference should be automatic.
  (hash 'name (binding-name (send this xsmith_get-reference!)))]
 [SetBangRet
  ;; TODO - getting a name for a reference should be automatic.
  (hash 'name (binding-name (send this xsmith_get-reference!)))]
 [Lambda (let* ([type (att-value 'xsmith_type current-hole)]
                [ftype (function-type
                        (product-type #f)
                        (fresh-type-variable))]
                [unification-dumb-return-value (unify! ftype type)]
                [params (map (λ (t) (make-fresh-node 'FormalParam
                                                     (hash 'type t)))
                             (or (product-type-inner-type-list
                                  (function-type-arg-type ftype))
                                 (map (λ (x) (fresh-type-variable))
                                      (make-list (arg-length) #f))))])
           (hash
            'type type
            'params params))]
 [Definition (hash 'name (if (equal? (top-ancestor-node current-hole)
                                     (parent-node current-hole))
                             (fresh-var-name "global-")
                             (fresh-var-name "local-"))
                   'type (fresh-concrete-var-type))]
 )


(assemble-spec-components
 ;; TODO - have this macro check the name -- it can't have dashes or other things that RACR doesn't allow...
 schemely
 schemely-core)

(define (generate-and-print)
  (define (pp x)
    (pretty-print x (current-output-port) 1))
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (define program (schemely-generate-ast 'Program))
    (define forms (att-value 'to-s-exp program))
    ;; TODO - just assume racket for now, but later any scheme...
    (printf "#lang racket/base\n")
    (pp '(define (safe-car safe l)
           (if (null? l)
               safe
               (car l))))
    (pp '(define (safe-cdr l)
           (if (null? l)
               '()
               (cdr l))))
    (pp '(define (safe-/ l r)
           (if (equal? r 0)
               0
               (/ l r))))
    (printf ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")
    (for ([form forms])
      (pp form))
    (for ([def (ast-children (ast-child 'definitions program))])
      (pp `(write ,(string->symbol (ast-child 'name def))))
      (pp '(newline)))))

(module+ main
  (xsmith-command-line generate-and-print
                       #:comment-wrap (λ (lines)
                                        (string-join
                                         (map (λ (x) (format ";; ~a" x)) lines)
                                         "\n")))
  )

