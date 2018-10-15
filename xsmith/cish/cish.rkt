#!/usr/bin/env racket
#lang racket/base

(require
 "../grammar-macros.rkt"
 "cish2-grammar.rkt"
 "cish2-rules.rkt"

 "cish2-utils.rkt"
 "../core-properties.rkt"

 racr
 (except-in pprint
            semi rparen rbrace lparen lbrace comma
            colon
            )
 racket/dict
 "../xsmith-options.rkt"
 "../xsmith-version.rkt"
 )


(assemble-spec-components
 cish2
 #:properties
 (
  ;; Most of these don't need to be listed here, but I'm leaving them to remember which ones I need to make enabled by default...
  may-be-generated
  depth-increase
  choice-weight
  fresh
  wont-over-deepen
  introduces-scope
  binder-info
  lift-predicate
  )
 cish2-grammar
 cish2-rules
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-precomment! n)
  (rewrite-terminal 'precomment n
                    (h-append
                     line
                     (vb-append
                      (text "This is a RANDOMLY GENERATED PROGRAM.")
                      (hs-append
                       (fill 10 (text "Generator:"))
                       (text xsmith-version-string))
                      (hs-append
                       (fill 10 (text "Options:"))
                       (apply hs-append
                              (map text
                                   (vector->list
                                    (xsmith-option 'command-line)))))
                      (hs-append
                       (fill 10 (text "Seed:"))
                       (text (number->string (xsmith-option 'random-seed))))
                      soft-break))))

(define ({ast-add-unsafe-math refinement-func} ast)
  (define ops (att-value 'find-descendants ast
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
  (let* ([ast (cish2-generate-ast 'Program)]
         [nothing (add-precomment! ast)]
         [pre-analysis-print (printf "/*\n")]
         [ast (if (hash-ref (xsmith-option 'features-disabled)
                            'unsafe-math/range #t)
                  ast
                  (begin
                    (printf "Starting range analysis...\n")
                    (ast-add-unsafe-math/range ast)))]
         [ast (if (hash-ref (xsmith-option 'features-disabled)
                            'unsafe-math/symbolic #t)
                  ast
                  (begin
                    (printf "Starting symbolic analysis...\n")
                    (ast-add-unsafe-math/symbolic ast)))]
         [post-analysis-print (printf "*/\n")]
         )
    (if (dict-has-key? (xsmith-options) 'output-filename)
        (call-with-output-file (xsmith-option 'output-filename)
          #:exists 'replace
          (lambda (out)
            (pretty-print (att-value 'pretty-print ast)
                          out
                          page-width)))
        (begin
          (pretty-print (att-value 'pretty-print ast)
                        (current-output-port)
                        page-width)
          #;(printf "\n\n/*\nabstract return: ~a\n*/\n"
                    (car
                     (abstract-interp-wrap/range ast range-store-top
                                                 empty-abstract-flow-control-return)))))
    ))

(module+ main
  (require "../xsmith-command-line.rkt")
  (xsmith-command-line cish-generate-and-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
