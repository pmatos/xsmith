#lang racket/base
(provide xsmith-command-line)

(require
 racket/dict
 "xsmith-utils.rkt"
 "xsmith-options.rkt"
 "xsmith-version.rkt"
 )

(define options (xsmith-options-defaults))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/cmdline)

(define (xsmith-command-line generate-and-print-func)
  (define features-disabled (dict-ref options 'features-disabled))
  (define port 8080)
  (define given-seed #f)
  (define server? #f)

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
   [("--server") run-as-server?
                 "Run as a web server instead of generating a single program."
                 (set! server? run-as-server?)]
   [("--port") n "Use port n instead of 8080 (when running as server)."
               (set! port (string->number n))]

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

  (define (generate-and-print!)
    (parameterize ([xsmith-options options]
                   [xsmith-state (make-generator-state)])
      ;; XXX also need to reset the seed, or increase a generation counter,
      ;; or something.  Right now, the seed printed in output program is
      ;; wrong!
      (let ([seed (xsmith-option 'random-seed)])
        (random-seed seed)
        (generate-and-print-func)
        (dict-set! (xsmith-options)
                   'random-seed
                   (add1 seed)))))

  (if server?
      (let ([serve/servlet (dynamic-require 'web-server/servlet-env 'serve/servlet)]
            [response/xexpr (dynamic-require 'web-server/servlet 'response/xexpr)])
        (define (servlet-start req)
          (let ((out (open-output-string)))
            (parameterize ((current-output-port out))
              (generate-and-print!))
            (response/xexpr
             `(html (head (title "Random C Program"))
                    (body (pre ,(get-output-string out)))))))
        (eprintf "Starting server...\n")
        (eprintf "Visit: http://localhost:~a/servlets/standalone.rkt\n" port)
        (serve/servlet servlet-start #:port port #:command-line? #t))
      (generate-and-print!))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of file.
