#lang xsmith/private/base
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
 define-property
 define-non-inheriting-rule-property
 )
(require
 (for-syntax
  syntax/parse
  racket/dict
  xsmith/private/base
  "grammar-properties.rkt"
  ))

(define-syntax (define-property stx)
  (syntax-parse stx
    [(_ name:id
        (~or
         (~optional (~seq #:allow-duplicates? allow-duplicates?:boolean))
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
       (or (attribute transformer-func) #'#f))
     ;; TODO - check for duplicates or conflicts in read/rewrite/append specs
     #`(define-syntax name
         (grammar-property 'name
                           #,transformer-func-use
                           #,(if (attribute read-arg)
                                 #'(quote-syntax (read-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute rewrite-arg)
                                 #'(quote-syntax (rewrite-arg ...))
                                 #'(quote-syntax ()))
                           #,(if (attribute append-arg)
                                 #'(quote-syntax (append-arg ...))
                                 #'(quote-syntax ()))
                           #,(or (attribute allow-duplicates?) #'#f)))]))

(define-syntax (define-non-inheriting-rule-property stx)
  (define-syntax-class rule-type
    (pattern (~or (~datum choice-rule) (~datum att-rule))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
