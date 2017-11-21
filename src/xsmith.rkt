#lang racket/base
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require "gen-term.rkt")
(require
 racket/dict
 "cish-gen-term.rkt"
 "xsmith-options.rkt"
 "xsmith-version.rkt"
 )

(define options (xsmith-options-defaults))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (require racket/cmdline)

  (define features-disabled (dict-ref options 'features-disabled))

  (command-line
   #:help-labels
   "[[GENERAL OPTIONS]]"
   #:once-each
   [("--seed" "-s")
    seed
    "Set the random seed"
    (dict-set! options 'random-seed (string->number seed))]
   [("--output-file" "-o")
    filename
    "Output generated program to <filename>"
    (dict-set! options 'output-filename filename)]

   #:help-labels
   "[[LANGUAGE-GENERATION OPTIONS]]"
   #:once-each
   ["--max-depth"
    n
    "Set maximum tree depth"
    (dict-set! options 'max-depth (string->number n))]
   #:multi
   ["--with"
    feature-name
    "Enable language <feature-name>"
    ;; Or, one could just remove the key from the dict...
    (dict-set! features-disabled (string->symbol feature-name) #f)]
   ["--without"
    feature-name
    "Disable language <feature-name>"
    (dict-set! features-disabled (string->symbol feature-name) #t)]

   #:help-labels
   "[[INFORMATION OPTIONS]]"
   #:once-each
   [("--version" "-v")
    "Show program version information and exit"
    (displayln xsmith-version-string)
    (exit 0)]
   )

  ;; Save the command-line options so that we can output them into the
  ;; generated program.
  (dict-set! options 'command-line (current-command-line-arguments))

  ;; Use an explicit random seed --- again, so that we can output it into the
  ;; generated program.
  (unless (dict-has-key? options 'random-seed)
    (dict-set! options 'random-seed (random (expt 2 31))))

  (do-it options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
