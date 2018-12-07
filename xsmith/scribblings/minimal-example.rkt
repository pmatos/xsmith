#lang racket/base
(require xsmith/grammar-macros
         xsmith/core-properties
         xsmith/xsmith-command-line
         racr)

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

(add-prop arith choice-filters-to-apply
          ;; TODO - this property should not need to be specified.
          [#f ()])
(add-prop arith choice-weight
          ;; TODO - there should be a default choice weight if none is specified.
          ;; Maybe 10.
          [LiteralInt 10]
          [Addition 20])

;; TODO - this should not need to be specified when there are no binders or just one.
(add-prop arith lift-type->ast-binder-type
          [#f (λ (n t) (error 'this-should-not-be-called))])

(define int (base-type 'int))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop arith type-info
          [Program [int (λ (n t) (hash 'Expression int))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])

(assemble-spec-components
 arithmetic
 ;; TODO - these core properties should not need to be specified here.
 #:properties (depth-increase
               fresh
               choice-filters-to-apply
               may-be-generated
               choice-weight
               child-node-name-dict
               wont-over-deepen
               introduces-scope
               binder-info
               lift-predicate
               lift-type->ast-binder-type
               binding-structure
               )
 arith)

(define (arithmetic-generate-and-print)
  ;; TODO - the generated generate-ast function should maybe generate a hole and then fill it.  Then I would not need to add a Program node to this language.
  (displayln (att-value 'ugly-print (arithmetic-generate-ast 'Program))))

(xsmith-command-line arithmetic-generate-and-print)
