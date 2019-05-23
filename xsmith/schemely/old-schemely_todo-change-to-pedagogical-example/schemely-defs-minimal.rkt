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

(require xsmith racr racket/pretty racket/random racket/list racket/class)

(define-spec-component schemely-core)

(add-to-grammar
 schemely-core
 [DefinitionContext #f ([definitions : Definition * = (random 3)]
                        [expressions : Expression * = (add1 (random 3))])
   #:prop strict-child-order? #t]
 [Program DefinitionContext ()]

 [Definition #f ([type = (concretize-type (fresh-type-variable))]
                 [name]
                 Expression)
   #:prop binder-info (name type definition)]

 [Expression #f ()
             #:prop may-be-generated #f]

 [LetStar Expression ([definitions : Definition * = (random 3)]
                      [body : DefinitionContext])
          #:prop strict-child-order? #t]

 [VariableReference Expression (name)
                    #:prop reference-info (read name)
                    #:prop choice-weight 10]

 [LiteralNumber Expression ([v = (* (random 1000000)
                                    (if (equal? 0 (random 2)) -1 1))])]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]


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
 ;[Program (λ (n) `(begin ,@(map (λ (x) (att-value 'to-s-exp x))
 ;                               (ast-children (ast-child 'definitions n)))
 ;                        ,@(map (λ (x) (att-value 'to-s-exp x))
 ;                               (ast-children (ast-child 'expressions n)))))]
 [DefinitionContext (λ (n) `(,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'definitions n)))
                             ,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'expressions n)))))]
 [Definition (λ (n) `(define ,(string->symbol (ast-child 'name n))
                       ,(att-value 'to-s-exp (ast-child 'Expression n))))]
 [LetStar (λ (n) `(let* (,@(map (λ (d) `(,(string->symbol (ast-child 'name d))
                                         ,(att-value 'to-s-exp
                                                     (ast-child 'Expression d))))
                                (ast-children (ast-child 'definitions n))))
                    ,@(att-value 'to-s-exp (ast-child 'body n))))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]

 [LiteralNumber (λ (n) (ast-child 'v n))]
 [Plus (->se '+ 'l 'r)]
 [Minus (->se '- 'l 'r)]
 )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

(define number (base-type 'number))
(define (number-type? x) (can-unify? x number))
(define (type-thunks-for-concretization) (list (λ()number)))

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
                       (for/hash ([c (append
                                             (ast-children
                                              (ast-child 'definitions n))
                                             (ast-children
                                              (ast-child 'expressions n)))])
                         (values c
                                 (if (equal? c last-expression)
                                     t
                                     (fresh-type-variable)))))]]
 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [VariableReference [(fresh-type-variable) (no-child-types)]]
 [LetStar [(fresh-type-variable) (λ (n t) (hash-set
                                           (for/hash ([c (ast-children
                                                          (ast-child 'definitions n))])
                                             (values c (fresh-type-variable)))
                                           'body
                                           t))]]

 [LiteralNumber [number (no-child-types)]]
 [Plus [number numeric-bin-op-type]]
 [Minus [number numeric-bin-op-type]]
 )

(add-prop
 schemely-core
 fresh
 [VariableReference
  ;; TODO - getting a name for a reference should be automatic.
  (hash 'name (binding-name (send this xsmith_get-reference!)))]
 ;; TODO - this should not be necessary
 [Definition (let* ([hole-name (ast-child 'name current-hole)]
                    [name (if (string? hole-name)
                              hole-name
                              (if (equal? (top-ancestor-node current-hole)
                                          (parent-node current-hole))
                                  (fresh-var-name "global-")
                                  (fresh-var-name "local-")))]
                    [hole-type (ast-child 'type current-hole)]
                    [type (if (and hole-type
                                   (not (and (ast-node? hole-type)
                                             (ast-bud-node? hole-type))))
                              hole-type
                              (fresh-concrete-var-type))])
               (hash 'name name
                     'type type))]
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
    (define forms (att-value 'to-s-exp (schemely-generate-ast 'Program)))
    ;; TODO - just assume racket for now, but later any scheme...
    (printf "#lang racket/base\n")
    (pp '(define (safe-car safe l)
           (if (null? l)
               safe
               l)))
    (pp '(define (safe-cdr l)
           (if (null? l)
               '()
               l)))
    (for ([form forms])
      (pp form))))

(module+ main
  (xsmith-command-line generate-and-print)
  )