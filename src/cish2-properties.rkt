#lang racket/base

(provide
 may-be-generated
 depth-increase-predicate
 )

(require
 "grammar-macros.rkt"
 "cish2-utils.rkt"
 racr
 racket/class
 (for-syntax
  racket/base
  syntax/parse
  racket/dict
  ))

(define-syntax (define-non-inheriting-rule-property stx)
  (define-syntax-class rule-type
    (pattern (~or (~datum choice-rule) (~datum ag-rule))))
  (syntax-parse stx
    [(_ property-name:id
        rt:rule-type
        (~or
         (~optional (~seq #:rule-name rule-name:id))
         (~once (~seq #:default default-value:expr))
         (~optional (~seq #:transformer value-transformer:expr))
         )
        ...)
     (with-syntax ([transformer (or (attribute value-transformer) #'(λ (x) x))]
                   [rule-name (or (attribute rule-name) #'property-name)])
       #'(define-property property-name
           #:reads (grammar)
           #:appends (rt rule-name)
           #:transformer
           (λ (this-prop-info grammar-info)
             (define rule-info
               (for/hash ([node-name (dict-keys grammar-info)])
                 (define prop-vals
                   (dict-ref this-prop-info node-name #f))
                 (values node-name
                         (transformer
                          (if prop-vals
                              (syntax-parse prop-vals
                                [(a) #'a]
                                [(a b ...+)
                                 (raise-syntax-error
                                  #f
                                  (format
                                   "duplicate definition of ~a property for node ~a."
                                   'property-name
                                   node-name)
                                  #'a)])
                              (quote-syntax default-value))))))
             (list rule-info))))]))

(define-non-inheriting-rule-property
  may-be-generated
  choice-rule
  #:rule-name may-be-generated-method
  #:default #t
  #:transformer (syntax-parser [#t #'(λ () this)]
                               [#f #'(λ () #f)]))

(define-non-inheriting-rule-property
  depth-increase-predicate
  ag-rule
  #:rule-name ast-depth
  #:default (λ (n) #t)
  #:transformer (syntax-parser
                  [pred:expr
                   #'(λ (n)
                       (cond [(pred n) (let ([p (parent-node n)])
                                         (if p
                                             (add1 (att-value 'ast-depth p))
                                             0))]
                             [else (att-value 'ast-depth (parent-node n))]))]))
