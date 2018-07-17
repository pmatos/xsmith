#lang racket/base

(provide
 may-be-generated
 )

(require
 "grammar-macros.rkt"
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
    [(_ rt:rule-type name:id default-value:expr value-transformer:expr)
     #'(define-property name
         #:reads (grammar)
         #:appends (rt name)
         #:transformer
         (λ (this-prop-info grammar-info)
           (define rule-info
             (for/hash ([node-name (dict-keys grammar-info)])
               (define prop-vals
                 (dict-ref this-prop-info node-name #f))
               (values node-name
                       (value-transformer
                        (if prop-vals
                            (syntax-parse prop-vals
                              [(a) #'a]
                              [(a b ...+)
                               (raise-syntax-error
                                #f
                                (format
                                 "duplicate definition of ~a property for node ~a."
                                 'name
                                 node-name)
                                #'a)])
                            (quote-syntax default-value))))))
           (list rule-info)))]))

(define-non-inheriting-rule-property
  choice-rule
  may-be-generated
  #t
  (syntax-parser [#t #'(λ () this)]
                 [#f #'(λ () #f)]))
