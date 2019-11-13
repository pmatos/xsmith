#lang racket/base

(provide
 (struct-out grammar-refiner))

(struct grammar-refiner
  (name follows)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-refiner
                                      "Can't be used directly as a macro."
                                      stx)))
