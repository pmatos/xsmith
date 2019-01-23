#!/usr/bin/env racket
#lang racket/base

(require xsmith racr racket/pretty)

(define-spec-component schemely-core)

(add-to-grammar
 schemely-core
 [Program #f ([expressions : Expression * = (add1 (random 5))])]
 [Expression #f ()]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([l : Expression] [r : Expression])])

(add-prop
 schemely-core may-be-generated
 [Expression #f])
(add-prop
 schemely-core wont-over-deepen
 [LiteralInt #t])

(add-ag-rule
 schemely-core
 to-s-exp
 [Program (λ (n) `(begin ,@(map (λ (x) (att-value 'to-s-exp x))
                                (ast-children (ast-child 'expressions n)))))]
 [LiteralInt (λ (n) (ast-child 'v n))]
 [Addition (λ (n) `(+ ,(att-value 'to-s-exp (ast-child 'l n))
                      ,(att-value 'to-s-exp (ast-child 'r n))))]
 )

(add-prop schemely-core choice-filters-to-apply
          ;; TODO - this property should not need to be specified.
          [#f ()])
(add-prop schemely-core choice-weight
          ;; TODO - there should be a default choice weight if none is specified.
          ;; Maybe 10.
          [#f 10]
          [Addition 20])

;; TODO - this should not need to be specified when there are no binders or just one.
(add-prop schemely-core lift-type->ast-binder-type
          [#f (λ (n t) (error 'this-should-not-be-called))])

(define int (base-type 'int))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop schemely-core type-info
          [Program [int (λ (n t) (for/hash ([c (ast-children
                                                (ast-child 'expressions n))])
                                   (values c int)))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])

(assemble-spec-components
 ;; TODO - have this macro check the name -- it can't have dashes or other things that RACR doesn't allow...
 schemely
 schemely-core)

(define (generate-and-print)
  (pretty-print
   (att-value 'to-s-exp (schemely-generate-ast 'Program))
   (current-output-port)
   1))

(xsmith-command-line generate-and-print)
