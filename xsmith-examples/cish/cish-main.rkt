#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2020 The University of Utah
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


(define (cish-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (cish-generate-ast 'Program)))

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

    [unsafe-math/range  ;; TODO - this feature no longer correlates to anything
     #f ("Replace “safe math” operations with raw C operators"
         "where a range analysis proves they're safe.")]
    #;[unsafe-math/symbolic  ;; TODO - fix bugs in symbolic interpretation
     #f ("Replace “safe math” operations with raw C operators"
         "where a symbolic interpretation proves they're safe.")]
    ))

(module+ main
  (require xsmith)
  (xsmith-command-line
   cish-generate
   #:fuzzer-name "cish"
   #:fuzzer-version xsmith-version-string/no-name
   #:comment-wrap (λ (lines) (format "/*\n~a\n*/"
                                     (string-join lines "\n")))
   #:features cish-features-list
   #:default-max-depth 9
   #:format-render (λ (d) (pretty-format d 120))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
