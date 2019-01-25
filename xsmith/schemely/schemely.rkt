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

;; This is totally broken, but is sitting here as a reference while I build up a new one that isn't, taking smaller bites at a time...

(require
 "../main.rkt"
 racr
 (except-in pprint
            semi rparen rbrace lparen lbrace comma
            colon
            )
 racket/dict
 )


(define-spec-component schemely-core)

(add-to-grammar
 schemely-core
 [DefinitionContext #f ([definitions : Definition * = (random 3)]
                        [expressions : Expression * = (add1 (random 3))])
   [#:prop strict-child-order? #t]
   [#:prop lift-predicate (λ (n type) #t)]]

 [Program DefinitionContext ()]

 [Definition #f (type
                 name
                 Expression)
   [#:prop binder-info (name type definition)]]

 [FormalParam #f (type name)
              [#:prop binder-info (name type parameter)]]

 [Expression #f ()
             [#:prop may-be-generated #f]]

 [VariableReference #f (name)
                    [#:prop reference-info (read name)]]
 ;; TODO - set!

 #;[Begin Expression ([expressions : Expression * = (add1 (random 3))])
          [#:prop strict-child-order? #t]]

 [LiteralBool Expression (v)]
 [Not Expression ([Expression])]
 ;; TODO - many of these I've defined as binary but they could be variadic instead.
 [And Expression ([l : Expression] [r : Expression])]
 [Or Expression ([l : Expression] [r : Expression])]

 [LiteralNumber Expression (v)]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]
 [Times Expression ([l : Expression] [r : Expression])]
 [Divide Expression ([l : Expression] [r : Expression])]
 [LessThan Expression ([l : Expression] [r : Expression])]
 [GreaterThan Expression ([l : Expression] [r : Expression])]

 [LiteralString Expression (v)]
 [StringAppend Expression ([l : Expression] [r : Expression])]
 [StringLength Expression (Expression)]

 [List Expression ([arguments : Expression *])]
 [Car Expression (Expression)]
 [Cdr Expression (Expression)]
 [EmptyP Expression (Expression)]

 [EqualP Expression ([l : Expression] [r : Expression])]

 ;;[Lambda Expression ([params : FormalParam *]
 ;;                    [body : DefinitionContext])
 ;;        [#:prop lift-predicate #f]]
 ;;[Application ([procedure : Expression]
 ;;              [arguments : Expression *])]

 [Let Expression ([definitions : Definition * = (random 3)]
                  [body : DefinitionContext])
      [#:prop lift-predicate (λ (n type) #t)]]

 [If Expression ([test : Expression] [then : Expression] [else : Expression])
     [#:prop strict-child-order? #t]]

 ;; TODO - what else?  Loops?  Other control structures?
 )

;(add-prop
; schemely-core
; fresh
; [VariableReference])

(add-prop schemely-core
          lift-type->ast-binder-type
          [#f (λ (type) 'Definition)])


(add-prop
 schemely-core
 choice-filters-to-apply
 [#f (
      ;features-enabled
      ;misc-constraints
      )])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

(define number (base-type 'number))
(define (number-type? x) (can-unify? x number))
(define bool (base-type 'bool))
(define (bool-type? x) (can-unify? x bool))
(define string (base-type 'string))
(define (string-type? x) (can-unify? x string))

(define (type-thunks-for-concretization) (list (λ()number) (λ()bool) (λ()string)))
(define (concrete-types) (map (λ(x)(x)) (type-thunks-for-concretization)))

(define (list-type x)
  (generic-type 'list (list x)))
(define (list-type? x)
  (and (generic-type? x)
       (eq? (generic-type-name x)
            'list)))
(define (list-type-type x)
  (when (not (list-type? (concretize-type x)))
    (error 'list-type-type "given ~a\n" x))
  (car (generic-type-type-arguments (concretize-type x))))
(define (fresh-list-type) (list-type (fresh-type-variable)))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (concretize-type (fresh-type-variable))))

#;(define (fresh-concrete-function-type)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (concretize-type (function-type (product-type #f)
                                    (fresh-type-variable)))))

(add-prop
 schemely-core
 type-info
 [#f [(error 'typing-base-node) (no-child-types)]]
 [DefinitionContext [(fresh-type-variable)
                     (λ (n t)
                       (define definitions (ast-children (ast-child 'definitions n)))
                       (define expressions (ast-children (ast-child 'expressions n)))
                       (define last-expression (car (reverse expressions)))
                       (for/hash ([s (append expressions definitions)])
                         (values s
                                 (if (eq? s last-expression)
                                     t
                                     (fresh-type-variable)))))]]

 [Definition [(fresh-type-variable)
              (λ (n t)
                (let ([annotation (ast-child 'type n)])
                  (unify! t annotation)
                  (hash 'Expression annotation)))]]

 [VariableReference [(fresh-type-variable) (no-child-types)]]
 [LiteralBool [bool (no-child-types)]]
 [Not [bool (λ (n t) (hash 'Expression bool))]]
 [And [bool (λ (n t) (hash 'l bool 'r bool))]]
 [or [bool (λ (n t) (hash 'l bool 'r bool))]]

 [LiteralNumber [number (no-child-types)]]
 [Plus [number (λ (n t) (hash 'l number 'r number))]]
 [Minus [number (λ (n t) (hash 'l number 'r number))]]
 [Times [number (λ (n t) (hash 'l number 'r number))]]
 [Divide [number (λ (n t) (hash 'l number 'r number))]]
 [LessThan [bool (λ (n t) (hash 'l number 'r number))]]
 [GreaterThan [bool (λ (n t) (hash 'l number 'r number))]]

 [LiteralString [string (no-child-types)]]
 [StringAppend [string (λ (n t) (hash 'l string 'r string))]]
 [StringLength [number (λ (n t) (hash 'Expression string))]]

 [List [(fresh-list-type) (λ (n t)
                            (define lt (fresh-list-type))
                            (unify! t lt)
                            (define inner (list-type-type lt))
                            (for/hash ([c (ast-children (ast-child 'arguments n))])
                              (values c inner)))]]
 [Car [(fresh-type-variable) (λ (n t) (hash 'Expression (list-type t)))]]
 [Cdr [(fresh-list-type) (λ (n t) (hash 'Expression t))]]
 [EmptyP [bool (λ (n t) (hash 'Expression (fresh-list-type)))]]

 [EqualP [bool (λ (n t)
                 (define arg-type (fresh-type-variable))
                 (hash 'l arg-type 'r arg-type))]]

 ;; Lambda
 ;; Application

 [Let [(fresh-type-variable)
       (λ (n t)
         (hash-set (for/hash ([c (ast-children (ast-child 'definitions n))])
                     (values c (fresh-type-variable)))
                   'body
                   t))]]

 [If [(fresh-type-variable) (λ (n t) (hash 'test bool 'then t 'else t))]]

 )


(define (print-unary-op op-name)
  (λ (n) (h-append (text (format "(~a " op-name))
                   (att-value 'print (ast-child 'Expression n))
                   (text ")"))))
(define (print-binary-op op-name)
  (λ (n) (h-append (text (format "(~a " op-name))
                   (att-value 'print (ast-child 'l n))
                   (text " ")
                   (att-value 'print (ast-child 'r n))
                   (text ")"))))
(define (print-variadic-op op-name)
  (λ (n) (h-append (text (format "(~a " op-name))
                   (map (λ (c)
                          (h-append
                           (att-value 'print c)
                           (text " ")))
                        (ast-children (ast-child 'arguments n)))
                   (text ")"))))

(add-ag-rule
 schemely-core
 print
 [DefinitionContext
   (λ (n) (apply v-append
                 (map (λ (x) (att-value 'print x))
                      (append (ast-children (ast-child 'definitions n))
                              (ast-children (ast-child 'expressions n))))))]
 ;; Program...
 [Definition (λ (n) (h-append (text "(define ")
                              (att-value 'print (ast-child 'name n))
                              (text " ")
                              (att-value 'print (ast-child 'Expression n))
                              (text ")")))]
 ;; FormalParam...
 [VariableReference (λ (n) (text (ast-child 'name n)))]
 [LiteralBool (λ (n) (text (if (ast-child 'v n) "#t" "#f")))]
 [Not (λ (n) (print-unary-op "not"))]
 [And (print-binary-op "and")]
 [Or (print-binary-op "or")]

 [LiteralNumber (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (print-binary-op "plus")]
 [Minus (print-binary-op "minus")]
 [Times (print-binary-op "times")]
 [Divide (print-binary-op "divide")]
 [LessThan (print-binary-op "<")]
 [GreaterThan (print-binary-op ">")]

 [LiteralString (λ (n) (text (format "~v" (ast-child 'v n))))]
 [StringAppend (print-binary-op "string-append")]
 [StringLength (print-binary-op "string-length")]

 [List (print-variadic-op "list")]
 [Car (print-unary-op "car")]
 [Cdr (print-unary-op "cdr")]
 [EmptyP (print-unary-op "null?")]

 [EqualP (print-binary-op "equal?")]

 ;; Lambda
 ;; Application

 [Let (λ (n) (v-append
              (text "(let")
              (text "(")
              (apply v-append (map (λ (d) (h-append (text "(")
                                                    (text (ast-child 'name d))
                                                    (text " ")
                                                    (att-value
                                                     'print
                                                     (ast-child 'Expression d))
                                                    (text ")")))
                                   (ast-children (ast-child 'definitions n))))
              (text ")")
              (h-append
               (att-value 'print (ast-child 'body n))
               (text ")"))))]

 [If (λ (n) (v-append
             (text "(if")
             (att-value 'print (ast-child 'test))
             (text " ")
             (att-value 'print (ast-child 'then))
             (text " ")
             (att-value 'print (ast-child 'else))
             (text ")")))]
 )

(add-ag-rule
 schemely-core
 choice-weight
 [#f 10]
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assemble-spec-components
 schemely
 #:properties
 (depth-increase
  fresh
  choice-filters-to-apply
  may-be-generated
  choice-weight
  child-node-name-dict
  wont-over-deepen
  introduces-scope
  binder-info
  lift-predicate
  lift-type->ast-binder-type
  binding-structure
  io
  )
 schemely-core)

(define (schemely-generate-and-print)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (let* ([ast (schemely-generate-ast 'Program)]
           [page-width 80])
      (if (dict-has-key? (xsmith-options) 'output-filename)
          (call-with-output-file (xsmith-option 'output-filename)
            #:exists 'replace
            (lambda (out)
              (pretty-print (att-value 'print ast)
                            out
                            page-width)))
          (begin
            (pretty-print (att-value 'print ast)
                          (current-output-port)
                          page-width)
            #;(printf "\n\n/*\nabstract return: ~a\n*/\n"
                      (car
                       (abstract-interp-wrap/range ast range-store-top
                                                   empty-abstract-flow-control-return)))))
      )))

(module+ main
  (require "../main.rkt")
  (xsmith-command-line schemely-generate-and-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
