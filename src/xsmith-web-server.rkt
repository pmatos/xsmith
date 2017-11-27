#lang racket
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017 The University of Utah
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

(require web-server/servlet
         web-server/servlet-env)
(require racket/cmdline)
(require "cish-gen-term.rkt")
(require "xsmith-options.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port 8080)
(define given-seed #f)

(command-line
 #:program "xsmith-web-server"
 #:once-each
 [("--port") n "Use port n instead of 8080." (set! port (string->number n))]
 [("--seed" "-s") seed "Set the random seed." (set! given-seed (string->number seed))]
 )

(define options (xsmith-options-defaults))

(dict-set! options 'command-line (make-vector 0))
;; Use an explicit random seed.
(unless (dict-has-key? options 'random-seed)
  (dict-set! options 'random-seed (or given-seed (random (expt 2 31)))))

(define do-it-once (make-do-it options))

(define (start req)
  (let ((out (open-output-string)))
    (parameterize ((current-output-port out))
      (do-it-once))
    (response/xexpr
     `(html (head (title "Random C Program"))
            (body (pre ,(get-output-string out)))))))

(serve/servlet start #:port port #:command-line? #t)

;; Visit: <http://localhost:8080/servlets/standalone.rkt>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
