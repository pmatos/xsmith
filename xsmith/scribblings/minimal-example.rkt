#lang racket/base
(require xsmith racr)

(define-spec-component arith)

(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([l : Expression] [r : Expression])])

(add-prop
 arith may-be-generated
 [Expression #f])
(add-prop
 arith wont-over-deepen
 [LiteralInt #t])

(add-ag-rule
 arith ugly-print
 [Program (λ (n) (att-value 'ugly-print (ast-child 'Expression n)))]
 [LiteralInt (λ (n) (number->string (ast-child 'v n)))]
 [Addition (λ (n) (format "(~a + ~a)"
                          (att-value 'ugly-print (ast-child 'l n))
                          (att-value 'ugly-print (ast-child 'r n))))])

(add-prop arith choice-weight
          ;; The default weight is 10.
          [Addition 20])

(define int (base-type 'int))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop arith type-info
          [Program [int (λ (n t) (hash 'Expression int))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])

(assemble-spec-components arithmetic arith)

(define (arithmetic-generate-and-print)
  ;; TODO - the generated generate-ast function should maybe generate a hole and then fill it.  Then I would not need to add a Program node to this language.
  (displayln (att-value 'ugly-print (arithmetic-generate-ast 'Program))))

(xsmith-command-line arithmetic-generate-and-print)
