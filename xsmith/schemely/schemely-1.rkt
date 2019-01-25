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

(require xsmith racr racket/pretty)

(define-spec-component schemely-core)

(add-to-grammar
 schemely-core
 [Program #f ([expressions : Expression * = (add1 (random 5))])]
 [Expression #f ()
             [#:prop may-be-generated #f]]
 [LiteralInt Expression ([v = (random 100)])
             [#:prop wont-over-deepen #t]]
 [Addition Expression ([l : Expression] [r : Expression])
           [#:prop choice-weight 20]])


(add-ag-rule
 schemely-core
 to-s-exp
 [Program (λ (n) `(begin ,@(map (λ (x) (att-value 'to-s-exp x))
                                (ast-children (ast-child 'expressions n)))))]
 [LiteralInt (λ (n) (ast-child 'v n))]
 [Addition (λ (n) `(+ ,(att-value 'to-s-exp (ast-child 'l n))
                      ,(att-value 'to-s-exp (ast-child 'r n))))]
 )


(define int (base-type 'int))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop schemely-core type-info
          [Program [int (λ (n t) (for/hash ([c (ast-children
                                                (ast-child 'expressions n))])
                                   (values c int)))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])

(assemble-spec-components
 ;; TODO - have this macro check the name -- it can't have dashes or other things that RACR doesn't allow...
 schemely
 schemely-core)

(define (generate-and-print)
  (pretty-print
   (att-value 'to-s-exp (schemely-generate-ast 'Program))
   (current-output-port)
   1))

(xsmith-command-line generate-and-print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
