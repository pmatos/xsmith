#lang clotho/racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2019 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out spec-component-struct)
 set-spec-component-struct-grammar-info
 set-spec-component-struct-att-rule-info
 set-spec-component-struct-choice-rule-info
 set-spec-component-struct-property-info
 set-spec-component-struct-refiner-info

 spec-component

 spec-component-struct-ref
 spec-component-struct-ref-ref
 )

(require syntax/parse)

(struct spec-component-struct
  (grammar-info att-rule-info choice-rule-info property-info refiner-info)
  #:transparent
  )
(define (set-spec-component-struct-grammar-info s v)
  (struct-copy spec-component-struct s
               [grammar-info v]))
(define (set-spec-component-struct-att-rule-info s v)
  (struct-copy spec-component-struct s
               [att-rule-info v]))
(define (set-spec-component-struct-choice-rule-info s v)
  (struct-copy spec-component-struct s
               [choice-rule-info v]))
(define (set-spec-component-struct-property-info s v)
  (struct-copy spec-component-struct s
               [property-info v]))
(define (set-spec-component-struct-refiner-info s v)
  (struct-copy spec-component-struct s
               [refiner-info v]))

(struct spec-component-struct-ref (ref))

(define-syntax-class spec-component
  (pattern component-name:id
           #:when (let ([slv (syntax-local-value #'component-name (λ () #f))])
                    (spec-component-struct-ref? slv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
