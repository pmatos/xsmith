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

(require racket/contract)
(provide
 effect-variable

 effect-read-variable
 effect-read-variable?
 effect-write-variable
 effect-write-variable?
 effect-read-mutable-container
 effect-read-mutable-container?
 effect-write-mutable-container
 effect-write-mutable-container?
 effect-io
 effect-io?
 any-effect
 any-effect?
 )

(struct effect (type variable) #:transparent)

(define (effect-read-variable v)
  (effect 'read-variable v))
(define (effect-read-variable? x)
  (and (effect? x)
       (eq? 'read-variable (effect-type x))))
(define (effect-write-variable v)
  (effect 'write-variable v))
(define (effect-write-variable? x)
  (and (effect? x)
       (eq? 'write-variable (effect-type x))))
(define (effect-read-mutable-container v)
  (effect 'read-mutable-container v))
(define (effect-read-mutable-container? x)
  (and (effect? x)
       (eq? 'read-mutable-container (effect-type x))))
(define (effect-write-mutable-container v)
  (effect 'write-mutable-container v))
(define (effect-write-mutable-container? x)
  (and (effect? x)
       (eq? 'write-mutable-container (effect-type x))))
(define (effect-io)
  (effect 'io #f))
(define (effect-io? x)
  (and (effect? x)
       (eq? 'io (effect-type x))))
(define (any-effect)
  (effect 'any-effect #f))
(define (any-effect? x)
  (and (effect? x)
       (eq? 'any-effect (effect-type x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
