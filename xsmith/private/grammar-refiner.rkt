#lang racket/base

(provide
 (struct-out grammar-transformer))

(struct grammar-transformer
  (name predicate transformer)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-transformer
                                      "Can't be used directly as a macro."
                                      stx)))
