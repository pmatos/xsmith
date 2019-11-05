#lang racket/base

(provide
 (all-defined-out))

(require
 syntax/parse/define
 (for-syntax
  racket/base
  "spec-component-struct.rkt"
  ))

(module grammar-transformer-struct racket/base
    (provide (struct-out grammar-transformer))
    (struct grammar-transformer
      (name predicate transformer)
      #:property prop:procedure (λ (stx) (raise-syntax-error
                                          'grammar-transformer
                                          "Can't be used directly as a macro."
                                          stx))))

(require (for-syntax 'grammar-transformer-struct))

;; 1. create new grammar-transformer struct w/ filled fields
;; 2. add struct to spec-component

(define-syntax-parser define-transformer
  [(_ component:spec-component
      trans-name:id
      #:predicate trans-pred:expr
      #:transformer trans-func:expr)
   #'(define-syntax trans-name
       (grammar-transformer 'trans-name
                            #'trans-pred
                            #'trans-func))])
