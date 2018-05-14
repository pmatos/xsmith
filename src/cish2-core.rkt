#lang racket/base

(require
 "grammar-macros.rkt"

 racr
 racr/testing ;; racr/testing is needed for print-ast
 pprint
 racket/random
 racket/string
 racket/dict
 racket/set
 racket/match
 racket/math
 racket/class
 (prefix-in rt: rosette)
 (except-in racket/list empty)
 "random.rkt"
 "choice.rkt"
 "scope-graph.rkt"
 "xsmith-options.rkt"
 "xsmith-version.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(provide do-it make-do-it)





(assemble-spec-parts
 "cish2-grammar.rkt"
 "cish2-rules.rkt"
 "cish2-cms.rkt"
 )




(define (statement-choices hole)
  (list (new NullStatementChoice [hole hole])
        (new ExpressionStatementChoice [hole hole])
        (new BlockChoice [hole hole])
        (new IfStatementChoice [hole hole])
        (new IfElseStatementChoice [hole hole])
        ;; TODO - loop statements need to have some analysis so they at least sometimes terminate...
        (new WhileStatementChoice [hole hole])
        (new DoWhileStatementChoice [hole hole])
        (new ForStatementChoice [hole hole])
        (new ValueReturnStatementChoice [hole hole])))

(define (expression-choices hole)
  (list (new LiteralIntChoice [hole hole])
        (new LiteralFloatChoice [hole hole])
        (new FunctionApplicationExpressionChoice [hole hole])
        (new AdditionExpressionChoice [hole hole])
        (new SubtractionExpressionChoice [hole hole])
        (new MultiplicationExpressionChoice [hole hole])
        (new DivisionExpressionChoice [hole hole])
        (new ModulusExpressionChoice [hole hole])
        (new VariableReferenceChoice [hole hole])
        (new EqualityExpressionChoice [hole hole])
        (new LessThanExpressionChoice [hole hole])
        (new GreaterThanExpressionChoice [hole hole])
        (new LessOrEqualExpressionChoice [hole hole])
        (new GreaterOrEqualExpressionChoice [hole hole])
        (new IfExpressionChoice [hole hole])
        (new AssignmentExpressionChoice [hole hole])
        ))

(define (declaration-choices hole)
  (list (new VariableDeclarationChoice [hole hole])
        (new FunctionDefinitionChoice [hole hole])))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (replace-with-expression n)
  (let ([o (choose-ast (apply-choice-filters (expression-choices n)))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-statement n)
  (let ([o (choose-ast (apply-choice-filters (statement-choices n)))])
    (rewrite-subtree n (send o fresh))))

(define (replace-with-declaration n)
  (let ([o (choose-ast (apply-choice-filters (declaration-choices n)))])
    (rewrite-subtree n (send o fresh))))

(define (generate-random-prog n)
  (let ([fill-in
         (λ (n)
           (if (ast-list-node? n)
               #f
               (case (node-type n)
                 ((ExpressionHole)
                  (replace-with-expression n)
                  #t)
                 ((StatementHole)
                  (replace-with-statement n)
                  #t)
                 ((DeclarationHole)
                  (replace-with-declaration n)
                  #t)
                 ((BlockHole)
                  (rewrite-subtree n (send (new BlockChoice [hole n]) fresh))
                  #t)
                 ((FunctionDefinitionHole)
                  (rewrite-subtree n (send (new FunctionDefinitionChoice [hole n])
                                           fresh))
                  #t)
                 (else #f))))])
    (perform-rewrites n 'top-down fill-in))
  n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fresh-block-hole)
  (fresh-node 'BlockHole
              (create-ast-list '())
              (create-ast-list '())))

(define (fresh-Prog)
  (define p
    (fresh-node 'Program
                (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                         "standin-name"))
                                      (make-list (random 7) #f)))
                (fresh-node 'FunctionDefinitionHole
                            "main"
                            int-type
                            (create-ast-list '())
                            (fresh-block-hole))))
  (rewrite-terminal 'precomment p
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
                      soft-break)))
  p)

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
                 (eprintf "Removing a safe math op.\n")
                 #t)))
        #f))
  (perform-rewrites ast 'bottom-up transformer)
  ast)
(define ast-add-unsafe-math/range
  {ast-add-unsafe-math (λ (n) (att-value 'unsafe-op-if-possible/range n))})
(define ast-add-unsafe-math/symbolic
  {ast-add-unsafe-math (λ (n) (att-value 'unsafe-op-if-possible/symbolic n))})

(define (do-it options)
  (let ((state (make-generator-state)))
    ;; Initialize the state from the options.
    ;; Pretty lame to use `parameterize` just for this.  XXX Fix options API.
    (parameterize ((xsmith-options options))
      (random-seed (xsmith-option 'random-seed)))
    (do-one state options)))

(define (make-do-it options)
  (let ((state (make-generator-state)))
    ;; Initialize the state from the options.
    ;; Pretty lame to use `parameterize` just for this.  XXX Fix options API.
    (parameterize ((xsmith-options options))
      (random-seed (xsmith-option 'random-seed)))
    (λ ()
      (parameterize ((xsmith-options options))
        ;; XXX also need to reset the seed, or increase a generation counter,
        ;; or something.  Right now, the seed printed in output program is
        ;; wrong!
        (do-one state options)))))

(define (do-one state options)
  (parameterize ((xsmith-state state)
                 (xsmith-options options))
    (let* ([ast (generate-random-prog (fresh-Prog))]
           [pre-analysis-print (eprintf "/*\n")]
           [ast (if (hash-ref (xsmith-option 'features-disabled)
                              'unsafe-math/range #t)
                    ast
                    (begin
                      (eprintf "Starting range analysis...\n")
                      (ast-add-unsafe-math/range ast)))]
           [ast (if (hash-ref (xsmith-option 'features-disabled)
                              'unsafe-math/symbolic #t)
                    ast
                    (begin
                      (eprintf "Starting symbolic analysis...\n")
                      (ast-add-unsafe-math/symbolic ast)))]
           [post-analysis-print (eprintf "*/\n")]
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
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
