#lang racket/base
(require xsmith racr racket/string)

(define-spec-component arith)

(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([l : Expression] [r : Expression])
           ;; The default weight is 10.
           #:prop choice-weight 20])

(define int (base-type 'int))
(add-prop arith type-info
          [Program [int (λ (n t) (hash 'Expression int))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])

(add-att-rule
 arith ugly-print
 [Program (λ (n) (att-value 'ugly-print (ast-child 'Expression n)))]
 [LiteralInt (λ (n) (number->string (ast-child 'v n)))]
 [Addition (λ (n) (format "(~a + ~a)"
                          (att-value 'ugly-print (ast-child 'l n))
                          (att-value 'ugly-print (ast-child 'r n))))])

;; This line defines `arithmetic-generate-ast`.
(assemble-spec-components arithmetic arith)

(define (arithmetic-generate-and-print)
  (displayln (att-value 'ugly-print (arithmetic-generate-ast 'Program))))

(xsmith-command-line arithmetic-generate-and-print
                     #:comment-wrap (λ (lines)
                                      (string-join
                                       (map (λ (x) (format "// ~a" x)) lines)
                                       "\n")))
