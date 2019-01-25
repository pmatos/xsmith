#lang racket
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

(require (for-syntax racket/port
                     racket/string
                     racket/system))
(require setup/getinfo)
(provide xsmith-version-string)

(define xsmith-info (get-info (list "xsmith")))

;;
;; Determine the short hash of the git commit reerenced by HEAD at the time
;; this source file is compiled, i.e., "the current git commit."  Return this
;; as a string syntax object.
;;
(define-syntax (compile-time-git-commit-string stx)
  (syntax-case stx ()
    [_ (let* [(rc #f)
              (head-rev
               (string-trim
                (parameterize [(current-error-port (open-output-nowhere))]
                  (with-output-to-string
                    (lambda ()
                      (set! rc (system "git rev-parse --short HEAD")))))))]
         (if rc
             #`#,head-rev
             #'"unknown"))]))

;;
;; The short hash of the git commit corresponding to this version of Xsmith.
;;
;; This is set by checking two sources.  First, look for the hash in file
;; `info.rkt` (because `git archive` put it there).  Second, use the hash that
;; was determined when this file was compiled.
;;
(define xsmith-git-commit-string
  (if xsmith-info
      (let [(rev (xsmith-info 'git-commit))]
        ;; Was the git commit hash inserted into `info.rkt` by `git archive`?
        ;; If so, the first character of `rev` will not be #\$.
        (if (not (char=? (string-ref rev 0) #\$))
            rev
            (compile-time-git-commit-string)))
      "unknown"))

;;
;; The descriptive version string for Xsmith.
;;
(define xsmith-version-string
  (if xsmith-info
      (format "~a ~a (~a)"
              (xsmith-info 'name)
              (xsmith-info 'version)
              xsmith-git-commit-string)
      "unable to determine program version"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
