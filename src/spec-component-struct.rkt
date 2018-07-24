#lang racket/base

(provide
 (struct-out spec-component-struct)
 set-spec-component-struct-grammar-info
 set-spec-component-struct-ag-rule-info
 set-spec-component-struct-choice-rule-info
 set-spec-component-struct-property-info

 spec-component

 spec-component-struct-ref
 spec-component-struct-ref-ref
 )

(require syntax/parse)

(struct spec-component-struct
  (grammar-info ag-rule-info choice-rule-info property-info)
  #:transparent
  )
(define (set-spec-component-struct-grammar-info s v)
  (struct-copy spec-component-struct s
               [grammar-info v]))
(define (set-spec-component-struct-ag-rule-info s v)
  (struct-copy spec-component-struct s
               [ag-rule-info v]))
(define (set-spec-component-struct-choice-rule-info s v)
  (struct-copy spec-component-struct s
               [choice-rule-info v]))
(define (set-spec-component-struct-property-info s v)
  (struct-copy spec-component-struct s
               [property-info v]))

(struct spec-component-struct-ref (ref))

(define-syntax-class spec-component
  (pattern component-name:id
           #:when (let ([slv (syntax-local-value #'component-name (λ () #f))])
                    (spec-component-struct-ref? slv))))
