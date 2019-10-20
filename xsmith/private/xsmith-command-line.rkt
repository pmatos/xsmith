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

(require
 racket/contract
 racket/contract/base
 (only-in racr ast-node?))
(provide
 (contract-out
  [xsmith-command-line
   (->* ((-> ast-node?))
        (#:comment-wrap (-> (listof string?) string?)
         #:fuzzer-name (or/c #f string?)
         #:fuzzer-version (or/c #f string?)
         #:features (listof
                     (or/c (list/c symbol? boolean?)
                           (list/c symbol? boolean? (listof string?))))
         #:default-max-depth number?
         #:format-print (-> any/c void?))
        void?)]
  )
 xsmith-feature-enabled?
 )


(require
 racket/dict
 racket/match
 racket/cmdline
 racket/string
 racket/exn
 racket/port
 racket/pretty
 racket/list
 raco/command-name
 "xsmith-parameters.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
 "xsmith-version.rkt"
 "core-properties.rkt"
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (xsmith-command-line generate-func
                             #:fuzzer-name [fuzzer-name #f]
                             #:fuzzer-version [fuzzer-version #f]
                             #:comment-wrap [comment-func (λ (lines) "")]
                             #:features [features-list '()]
                             #:default-max-depth [default-max-depth 5]
                             #:format-print [format-print-func #f])

  (define true-strings  '("true"  "t" "#t" "yes" "y"))
  (define false-strings '("false" "f" "#f" "no"  "n"))
  (define (string->bool bool-string var-name)
    (define s (string-downcase bool-string))
    (cond [(member s true-strings) #t]
          [(member s false-strings) #f]
          [else (error
                 'string->bool
                 (string-append
                  "While parsing argument for ~a,\n"
                  "expected “true”, “t”, “#t”, “false”, “f”, or “#f”.\n"
                  "Got ~a.\n")
                 var-name
                 bool-string)]))


  (define features (for/hash ([x features-list])
                     (values (first x) (second x))))
  (define (set-feature! k v)
    (set! features (dict-set features k v)))
  (define server-port 8080)
  (define server-path "/")
  (define listen-ip "127.0.0.1")
  (define given-seed #f)
  (define server? #f)
  (define max-depth default-max-depth)
  (define options (xsmith-options-defaults))

  ;; Save the command-line options so that we can output them into the
  ;; generated program.
  (hash-set! options 'command-line (current-command-line-arguments))

  (define (bool->string b)
    (if b "true" "false"))

  (define features-command-line-segment
    (if (null? features-list)
        '()
        `((help-labels "")
          (help-labels "[[LANGUAGE-SPECIFIC FEATURES]]")
          (help-labels "Default to true unless otherwise specified.")
          (once-each
           ,@(map (λ (spec)
                    (define-values (name default docstrings)
                      (match spec
                        [(list n d (list doc ...)) (values n d doc)]
                        [(list n d) (values n d '(""))]))
                    `[(,(string-append "--with-" (symbol->string name)))
                      ,(λ (flag v) (set-feature! name (string->bool v name)))
                      ([,@(append docstrings
                                  (filter (λ(x)x)
                                          (list
                                           (and (not default)
                                                (format "Defaults to ~a"
                                                        (bool->string default))))))]
                       "bool")])
                  features-list)))))

  (define version-info
    (format "~a~a, in Racket ~a"
            (cond [(and fuzzer-name fuzzer-version)
                   (format "~a ~a, " fuzzer-name fuzzer-version)]
                  [fuzzer-name (format "~a, " fuzzer-name)]
                  [else ""])
            xsmith-version-string
            (version)))

  (parse-command-line
   (short-program+command-name)
   (current-command-line-arguments)
   `((help-labels "[[GENERAL OPTIONS]]")
     (once-each
      [("--seed" "-s")
       ,(λ (flag seed)
          (dict-set! options 'random-seed (string->number seed)))
       ("Set the random seed" "seed")]
      [("--output-file" "-o")
       ,(λ (flag filename) (dict-set! options 'output-filename filename))
       ("Output generated program to <filename>" "filename")]
      [("--server")
       ,(λ (flag run-as-server?)
          (set! server? (string->bool run-as-server? 'run-as-server?)))
       ("Run as a web server instead of generating a single program."
        "run-as-server?")]
      [("--server-port")
       ,(λ (flag n) (set! server-port (string->number n)))
       ("Use port n instead of 8080 (when running as server)." "n")]

      [("--server-ip")
       ,(λ (flag ip) (set! listen-ip (if (member (string-downcase ip)
                                                 false-strings)
                                         #f ip)))
       (["Make the server listen on given IP address."
         "Use `false` to listen on all IP addresses."
         "Defaults to 127.0.0.1"]
        "ip")]
      [("--server-path")
       ,(λ (flag path) (set! server-path (if (not (string-prefix? path "/"))
                                             (string-append "/" path)
                                             path)))
       (["Run the fuzzer with the given path."
         "Defaults to /"]
        "path")]
      )
     (help-labels "")
     (help-labels "[[LANGUAGE-GENERATION OPTIONS]]")
     (once-each
      [("--max-depth")
       ,(λ (flag n) (set! max-depth (string->number n)))
       (,(format "Set maximum tree depth (default ~a)" default-max-depth)
        "number")])
     ,@features-command-line-segment

     (help-labels "")
     (help-labels "[[INFORMATION OPTIONS]]")
     (once-each
      [("--version" "-v")
       ,(λ (flag)
          (displayln version-info)
          (exit 0))
       ("Show program version information and exit")])
     )
   ;; Finish-proc
   (λ (flag-accum)
     (void))
   ;; arg-help-strs
   '()
   ;; help-proc
   ;; unknown-proc
   )


  (define (generate-and-print!)
    (parameterize ([current-xsmith-max-depth max-depth]
                   [current-xsmith-features features]
                   [xsmith-options options]
                   [xsmith-state (make-generator-state)])
      (let ([seed (xsmith-option 'random-seed)])
        (random-seed seed)
        (define option-lines
          (append
           (if fuzzer-name
               (list (format "Fuzzer: ~a" fuzzer-name))
               (list))
           (list (format "Version: ~a" version-info)
                 (format "Options: ~a" (string-join
                                        (vector->list
                                         (xsmith-option 'command-line))))
                 (format "Seed: ~a" seed))))
        (define error? #f)
        (define program-output
          (with-output-to-string
            (λ ()
              (with-handlers ([(λ (e) #t)
                               (λ (e) (set! error? e))])
                (let* ([ast (generate-func)]
                       [out (print-node ast)])
                  (if format-print-func
                      (format-print-func out)
                      (display (format "~a\n" out))))))))
        (if error?
            ;; TODO - I think there should be some signal here that there is an
            ;;   error that a driver script can check for...
            (begin
              (printf "!!! Xsmith Error !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
              (printf "Error generating program!\n\n")
              (printf "Options:\n")
              (for ([line option-lines])
                (printf "~a\n" line))
              (printf "\nDebug Log:\n~a\n\n"
                      (get-xsmith-debug-log!))
              (printf "Exception:\n~a\n" (exn->string error?))
              (printf "\n")
              (when (not (equal? "" program-output))
                (printf "Program output captured:\n~a\n\n" program-output)))
            (begin
              (printf "~a\n" (comment-func
                              (cons "This is a RANDOMLY GENERATED PROGRAM."
                                    option-lines)))
              (printf "~a\n" program-output)))
        (dict-set! (xsmith-options)
                   'random-seed
                   (modulo (add1 seed)
                           random-seed-max)))))

  (if server?
      (let ([serve/servlet (dynamic-require 'web-server/servlet-env 'serve/servlet)]
            [response (dynamic-require 'web-server/http/response-structs 'response)])
        (define (servlet-start req)
          (let ((out (open-output-string)))
            (parameterize ((current-output-port out))
              (generate-and-print!))
            (response 200
                      #"OK"
                      (current-seconds)
                      #"text/plain"
                      '()
                      (λ (op)
                        (write-bytes
                         (string->bytes/utf-8 (get-output-string out))
                         op)))))
        (eprintf "Starting server...\n")
        (eprintf "Visit: http://localhost:~a~a\n" server-port server-path)
        (serve/servlet servlet-start
                       #:port server-port
                       #:command-line? #t
                       #:listen-ip listen-ip
                       #:servlet-path server-path))
      (if (dict-ref options 'output-filename #f)
          (call-with-output-file (dict-ref options 'output-filename)
            #:exists 'replace
            (lambda (out)
              (parameterize ([current-output-port out])
                (generate-and-print!))))
          (generate-and-print!)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
