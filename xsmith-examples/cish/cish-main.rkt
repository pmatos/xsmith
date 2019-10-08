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

(provide cish-features-list)

(require
 "cish-grammar.rkt"
 "cish-rules.rkt"
 "cish-utils.rkt"

 xsmith
 racr
 (except-in pprint
            semi rparen rbrace lparen lbrace comma
            colon
            )
 racket/dict
 racket/string
 )


(assemble-spec-components
 cish
 cish-grammar
 cish-rules
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ({ast-add-unsafe-math refinement-func} ast)
  (define ops (att-value 'xsmith_find-descendants ast
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
        (let ([refined-type (refinement-func n)])
          (and refined-type
               (begin
                 (rewrite-refine n refined-type)
                 (printf "Removing a safe math op.\n")
                 #t)))
        #f))
  (perform-rewrites ast 'bottom-up transformer)
  ast)
(define ast-add-unsafe-math/range
  {ast-add-unsafe-math (λ (n) (att-value 'unsafe-op-if-possible/range n))})
(define ast-add-unsafe-math/symbolic
  {ast-add-unsafe-math (λ (n) (att-value 'unsafe-op-if-possible/symbolic n))})


(define (cish-generate-and-print)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (let* ([ast (cish-generate-ast 'Program)]
           [pre-analysis-print (printf "/*\n")]
           [ast (if (xsmith-feature-enabled? 'unsafe-math/range)
                    (begin
                      (printf "Starting range analysis...\n")
                      (ast-add-unsafe-math/range ast))
                    ast)]
           [ast (if #f #;(xsmith-feature-enabled? 'unsafe-math/symbolic)
                    (begin
                      (printf "Starting symbolic analysis...\n")
                      (ast-add-unsafe-math/symbolic ast))
                    ast)]
           [post-analysis-print (printf "*/\n")]
           )
      (begin
        (pretty-print (att-value 'pretty-print ast)
                      (current-output-port)
                      page-width)
        #;(printf "\n\n/*\nabstract return: ~a\n*/\n"
                  (car
                   (abstract-interp-wrap/range ast range-store-top
                                               empty-abstract-flow-control-return))))
      )))

(define cish-features-list
  '([int #t]
    [float #t]

    [addition #t]
    [subtraction #t]
    [multiplication #t]
    [division #t]
    [modulus #t]

    [comparisons #t]

    [if-expression #t]
    [if-statement #t]
    [loop-statement #t]
    [null-statement #t]

    [structs #t]
    [volatile #t]

    [unsafe-math/range
     #f ("Replace “safe math” operations with raw C operators"
         "where a range analysis proves they're safe.")]
    #;[unsafe-math/symbolic
     #f ("Replace “safe math” operations with raw C operators"
         "where a symbolic interpretation proves they're safe.")]
    ))

(module+ main
  (require "../main.rkt")
  (xsmith-command-line cish-generate-and-print
                       #:fuzzer-name "cish"
                       #:fuzzer-version xsmith-version-string/no-name
                       #:comment-wrap (λ (lines) (format "/*\n~a\n*/"
                                                         (string-join lines "\n")))
                       #:features cish-features-list
                       #:default-max-depth 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
