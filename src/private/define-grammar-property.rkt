#lang racket/base
(provide
 define-property
 define-non-inheriting-rule-property
 )
(require
 (for-syntax
  racket/base
  syntax/parse
  racket/dict
  "grammar-properties.rkt"
  ))

(define-syntax (define-property stx)
  (syntax-parse stx
    [(_ name:id
        (~or
         (~optional (~seq #:allow-duplicates? allow-duplicates?))
         (~optional (~seq #:reads read-arg:property-arg ...+))
         (~optional (~seq #:rewrites rewrite-arg:property-arg ...+))
         (~optional (~seq #:appends append-arg:property-arg ...+))
         (~optional (~seq #:transformer transformer-func:expr)))
        ...)
     (when (and (not (attribute transformer-func))
                (or (attribute read-arg)
                    (attribute rewrite-arg)
                    (attribute append-arg)))
       (raise-syntax-error
        'define-property
        "transformer function needed for read, rewrite, or append arguments to make sense"
        stx))
     (when (and (attribute transformer-func)
                (not (or (attribute read-arg)
                         (attribute rewrite-arg)
                         (attribute append-arg))))
       (raise-syntax-error
        'define-property
        "transformer function must declare read, rewrite, or append arguments"
        stx))
     (define transformer-func-use
       (if (attribute transformer-func)
           ;; TODO - this disallows duplicates when the transformer for this property is run.  But if a property reads another property then it will still have to do duplicate checking itself.  This should be implemented in a way where the macro that runs transformers first checks for duplicates itself based on this duplicate setting.
           (syntax-parse (or (attribute allow-duplicates?) #'#f)
             [#t (attribute transformer-func)]
             [#f #`(λ (this-prop-info . other-args)
                     (define checked-this-prop-info
                       (for/hash ([n (dict-keys this-prop-info)])
                         (values
                          n
                          (syntax-parse (dict-ref this-prop-info n)
                            [(a) #'a]
                            [(a b (... ...))
                             (raise-syntax-error
                              #f
                              (format
                               "duplicate definition of ~a property for node ~a."
                               'name
                               n)
                              #'a)]))))
                     (apply #,(attribute transformer-func)
                            checked-this-prop-info
                            other-args))])
           #'#f))
     ;; TODO - check for duplicates or conflicts in read/rewrite/append specs
     #`(define-syntax name
         (grammar-property #,transformer-func-use
                           #,(if (attribute read-arg)
                                 #'(quote-syntax (read-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute rewrite-arg)
                                 #'(quote-syntax (rewrite-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute append-arg)
                                 #'(quote-syntax (append-arg ...))
                                 #'(quote-syntax ()))))]))

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
                          (or prop-vals (quote-syntax default-value))))))
             (list rule-info))))]))
