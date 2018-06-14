#lang racket/base

(provide
 may-be-generated
 )

(require
 "grammar-macros.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/dict
  ))

;; This is a lot to say "this property maps to a non-inheriting choice-rule".
;; The define-property macro should be abstracted over with some more
;; macros to have some easy shorthands for simple cases.
(define-property may-be-generated
  #:reads (grammar)
  #:appends (choice-rule may-be-generated)
  #:transformer
  (λ (may-be-generated-prop-info grammar-info)
    (define may-be-generated-choice-rule-info
      (for/hash ([node-name (dict-keys grammar-info)])
        (define prop-vals
          (dict-ref may-be-generated-prop-info node-name #'(#t)))
        (values
         node-name
         (syntax-parse prop-vals
           [(#t) #'(λ (n) #t)]
           [(#f) #'(λ (n) #f)]
           [(a b ...+)
            (raise-syntax-error
             'may-be-generated
             (format
              "duplicate definition of may-be-generated property for node: ~a"
              node-name)
             #'a)]
           [bad-stx (raise-syntax-error
                     'may-be-generated
                     "bad value for may-be-generated property, should be #t or #f"
                     #'bad-stx)]))))
    (list may-be-generated-choice-rule-info)))
