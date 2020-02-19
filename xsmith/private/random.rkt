#lang racket/base
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
 random
 random-seed
 random-ref)

(require
 (prefix-in rand: (only-in racket/base
                           random
                           random-seed
                           make-pseudo-random-generator
                           pseudo-random-generator?
                           current-pseudo-random-generator
                           pseudo-random-generator->vector
                           vector->pseudo-random-generator
                           vector->pseudo-random-generator!
                           pseudo-random-generator-vector?))
 (prefix-in rand: racket/random))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitute Functions
;;
;; These functions are intended to replace the same-named functions in the
;; racket/base and racket/random libraries. This allows for greater control over
;; the generation of random values in Xsmith.

;; TODO - `random`
(define (random . args)
  (eprintf "(random . ~a)\n" args)
  (apply rand:random args))

;; TODO - `random-seed`
(define (random-seed . args)
  (eprintf "(random-seed . ~a)\n" args)
  (apply rand:random-seed args))

;; TODO - `random-ref`
(define (random-ref . args)
  (eprintf "(random-ref . ~a)\n" args)
  (apply rand:random-ref args))
