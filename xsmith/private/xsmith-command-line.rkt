#lang xsmith/private/base
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

(require
 racket/contract
 racket/contract/base
 (only-in racr ast-node?)
 racket/string
 )

(define (dash-dash-string? s)
  (and (string? s)
       (string-prefix? s "--")))

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
         #:extra-parameters (listof (list/c dash-dash-string?
                                            string?
                                            parameter?
                                            (or/c procedure? #f)))
         #:default-max-depth number?
         #:format-render (-> any/c string?))
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
 "random.rkt"
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
                             #:comment-wrap [comment-func (λ (lines)
                                                            (string-join
                                                             (map (λ (l) (format ";; ~a" l))
                                                                  lines)
                                                             "\n"))]
                             #:features [features-list '()]
                             #:extra-parameters [extra-parameters (list)]
                             #:default-max-depth [default-max-depth 5]
                             #:format-render [format-render-func #f])

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
  (define tree-on-error? #f)
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

  (define extra-param-box-default (gensym))
  (match-define (list (list extra-param-boxes
                            extra-param-handlers
                            extra-param-params)
                      ...)
    (map (λ (p)
           (match p
             [(list name docstring param normalizer)
              (let* ([b (box extra-param-box-default)]
                     [handler (λ (v) (set-box! b
                                               (if normalizer
                                                   (normalizer v)
                                                   v)))])
                (list b handler param))]))
         extra-parameters))

  ;; Check uniqueness of extra-param details.
  (let ([extra-param-list (map third extra-parameters)]
        [extra-param-names (map first extra-parameters)])
    (when (not (equal? extra-param-list
                       (remove-duplicates extra-param-list)))
      (error 'xsmith-command-line
             "extra parameters must be unique"))
    (when (not (equal? extra-param-names
                       (remove-duplicates extra-param-names)))
      (error 'xsmith-command-line
             "extra-parameter names must be unique")))

  (define extra-parameters-command-line-segment
    (if (null? extra-parameters)
        '()
        `((help-labels "")
          (help-labels "[[LANGUAGE-SPECIFIC EXTRA PARAMETERS]]")
          (once-each
           ,@(for/list ([param-spec extra-parameters]
                        [handler extra-param-handlers])
               (match param-spec
                 [(list name docstring param normalizer)
                  `[(,name)
                    ,(λ (flag v) (handler v))
                    [,docstring "arg"]]]))))))

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
      [("--tree-on-error")
       ,(λ (flag show-tree-on-error?)
          (set! tree-on-error? (string->bool show-tree-on-error? 'show-tree-on-error?)))
       (["Print the partial tree (using the render-node-info property) if an"
         "error is encountered."
         "Defaults to false."]
        "show-tree-on-error?")]
      )
     (help-labels "")
     (help-labels "[[LANGUAGE-GENERATION OPTIONS]]")
     (once-each
      [("--max-depth")
       ,(λ (flag n) (set! max-depth (string->number n)))
       (,(format "Set maximum tree depth (default ~a)" default-max-depth)
        "number")])
     ,@features-command-line-segment
     ,@extra-parameters-command-line-segment

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

  (define (generate-and-print!/xsmith-parameterized)
    (parameterize ([current-xsmith-max-depth max-depth]
                   [current-xsmith-features features]
                   [xsmith-options options]
                   [xsmith-state (make-generator-state)])
      (let ([seed (xsmith-option 'random-seed)])
        (let/ec abort
          (set-prg-seed! seed)
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
          (define captured-output "")
          (define (output-error err msg [partial-prog #f])
            (let* ([output
                    (format
                     (string-join
                      '("!!! Xsmith Error !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                        "~a"  ;; Error message.
                        ""
                        "Options:"
                        "~a"  ;; Option lines.
                        "Debug Log:"
                        "~a"  ;; Debug log.
                        ""
                        "Exception:"
                        "~a"  ;; Exception.
                        )
                      "\n")
                     msg
                     (string-join
                      option-lines
                      "\n")
                     (get-xsmith-debug-log!)
                     (exn->string err))]
                   [output (if (not (eq? captured-output ""))
                               (string-append
                                output
                                (format "\nProgram output captured:\n~a\n"
                                        captured-output))
                               output)]
                   [output (if partial-prog
                               (string-append
                                output
                                (format "\nPartially generated program:\n~a\n"
                                        partial-prog))
                               output)])
              (display output)))
          ;; Compute the result of a procedure, capturing all output to
          ;; `captured-output` and returning the procedure's result.
          (define (capture-output! proc)
            (let* ([result #f]
                   [out (with-output-to-string
                          (λ () (set! result (proc))))])
              #;(set! captured-output out)
              (when (not (eq? out ""))
                (set! captured-output (string-append captured-output out)))
              result))
          ;;;;;;;;;;;;;;;;
          ;; Actual generation and printing starts here.
          ;;;;
          ;; Convert an AST to a string.
          (define (ast->string root)
            (let ([ppr (render-node root)])
              (if format-render-func
                  (format-render-func ppr)
                  (format "~a\n" ppr))))
          ;; Attempt to generate the AST.
          (define error? #f)
          (define error-root #f)
          (define ast
            (capture-output!
             (λ () (with-handlers
                     ([(match-lambda [(list 'ast-gen-error e root) #t]
                                     [_ #f])
                       (λ (l)
                         (set! error? (second l))
                         (set! error-root (third l)))]
                      [(λ (e) #t)
                       (λ (e) (set! error? e))])
                     (generate-func)))))
          (when error?
            ;; If the user asked for it (and if any AST was salvaged from the
            ;; generation stage), attempt to convert the partially completed AST
            ;; to pre-print representation (PPR).
            (if (and tree-on-error?
                     error-root)
                (let* ([ppr-error? #f]
                       [original-captured-output captured-output]
                       [partial-prog (capture-output!
                                      (λ () (with-handlers ([(λ (e) #t)
                                                             (λ (e) (set! ppr-error? e))])
                                              (ast->string error-root))))])
                  (if ppr-error?
                      (begin
                        ;; Something went wrong during printing.
                        (output-error
                         ppr-error?
                         "Error 001: Error encountered in printing while intercepting another error in AST generation.")
                        (display "Original error reproduced below:\n\n")
                        (set! captured-output original-captured-output)
                        (output-error
                         error?
                         "Error 002: Error generating program!"))
                      (begin
                        ;; Printing was successful, so we can show the partial program.
                        (set! captured-output original-captured-output)
                        (output-error
                         error?
                         "Error 003: Error encountered while generating program!"
                         partial-prog))))
                (begin
                  ;; Just print the base error message and quit.
                  (output-error
                   error?
                   "Error 004: Error encountered while generating program!")))
            ;; Quit further execution.
            (abort))
          ;; Convert the AST to PPR.
          (define program
            (capture-output!
             (λ () (with-handlers ([(λ (e) #t)
                                    (λ (e) (set! error? e))])
                     (ast->string ast)))))
          (when error?
            ;; Something went wrong during printing.
            (output-error
             error?
             "Error 005: Error encountered while printing program.")
            (abort))
          ;; Everything was successful!
          (display (comment-func (cons "This is a RANDOMLY GENERATED PROGRAM."
                                       option-lines)))
          (display (format "\n\n~a\n" program))
          (when (non-empty-string? captured-output)
            (display "\n")
            (display (comment-func (flatten
                                    (list "!!! The following output was captured during execution:"
                                          ""
                                          (string-split captured-output "\n")))))
            (display "\n"))

          (display "\n"))
        ;; Update the seed. (This is used in server mode.)
        (dict-set! (xsmith-options)
                   'random-seed
                   (modulo (add1 seed)
                           random-seed-max)))))

  (define (generate-and-print!)
    (define (do-parameterized param-pairs thunk)
      (match param-pairs
        ['() (thunk)]
        [(list (cons param param-val) rest ...)
         (parameterize ([param param-val])
           (do-parameterized rest thunk))]))
    (define param-pairs
      (filter
       (λ(x)x)
       (map (λ (p b) (and (not (eq? (unbox b) extra-param-box-default))
                          (cons p (unbox b))))
            extra-param-params
            extra-param-boxes)))
    (do-parameterized param-pairs generate-and-print!/xsmith-parameterized))

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
