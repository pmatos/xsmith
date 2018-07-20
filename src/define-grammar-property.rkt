#lang racket/base
(provide define-property)
(require
 (for-syntax
  racket/base
  syntax/parse
  "grammar-properties.rkt"
  ))

(define-syntax (define-property stx)
  (syntax-parse stx
    [(_ name:id
        (~or
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
     ;; TODO - check for duplicates or conflicts in read/rewrite/append specs
     #`(define-syntax name
         (grammar-property #,(if (attribute transformer-func)
                                 #'transformer-func
                                 #'#f)
                           #,(if (attribute read-arg)
                                 #'(quote-syntax (read-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute rewrite-arg)
                                 #'(quote-syntax (rewrite-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute append-arg)
                                 #'(quote-syntax (append-arg ...))
                                 #'(quote-syntax ()))))]))
