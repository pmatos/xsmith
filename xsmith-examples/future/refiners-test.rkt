#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019-2020 The University of Utah
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
 )

(define-spec-component tg)

(add-to-grammar
 tg
 [Prog #f ([val : Val])]
 [Val #f ([v = (random 100)])
      #:prop wont-over-deepen #t]
 )

(add-property
 tg
 render-node-info
 [Prog (λ (n) (render-node (ast-child 'val n)))]
 [Val (λ (n) (number->string (ast-child 'v n)))]
 )

(define-refiner
  tg
  evens-only
  [#f [(λ (n) #f)]]
  [Val [(λ (n) (odd? (ast-child 'v n)))
        (λ (n) (begin
                 (eprintf (format "odd value encountered: ~a\n" (ast-child 'v n)))
                 (make-fresh-node 'Val (hash 'v (+ 1 (ast-child 'v n))))))]])

#;(define-refiner
  tg
  times-two
  #:follows evens-only
  [#f [(λ (n) #f)]]
  [Val [(λ (n) (begin
                 (eprintf (format "multiplying by two: ~a\n" (ast-child 'v n)))
                 (make-fresh-node 'Val (hash 'v (* 2 (ast-child 'v n))))))]])

(assemble-spec-components t tg)

(xsmith-command-line
 (λ () (t-generate-ast 'Prog))
 #:fuzzer-name "trans-test-fuzzer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
