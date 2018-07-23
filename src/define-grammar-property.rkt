#lang racket/base
(provide define-property)
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
