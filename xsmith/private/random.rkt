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
;; Acronyms
;;
;; PRG = Pseudo-Random-Generator
;; RGV = Random Generator Vector, a six-element integer vector used for the
;;       creation of new PRGs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO Items
;;
;; [ ] Coverage-Guided Generation
;;   [ ] Initialize/store source of randomness (generator or bit sequence)
;;     [X] Initialize from PRG
;;     [ ] Initialize from sequence
;;       [ ] Determine a representation for the sequence (bits/bytes/etc,
;;           list/vector/etc)
;;     [X] Default initialization
;;   [ ] Consult correct source of randomness
;;   [ ] Provide functions for all common randomness use cases
;;     [ ] random
;;     [ ] random-seed
;;     [ ] random-ref
;;   [ ] Provide functions for custom use cases
;;   [ ] Consider specialized distribution functions
;; [ ] Investigate More Specific Sequence Representations
;;    -  Instead of just storing the random bits, consider storing the results
;;       of computations along with their generation constraints. This would
;;       allow for better tailoring of values during re-generation from a given
;;       sequence.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Source of Randomness
;;
;; The source of randomness is a global singleton. If not set explicitly, the
;; default action is to initialize a regular pseudo-random generator and use
;; that for the duration.

;; This is the singleton that should be used everywhere.
(define random-source #f)

;; These distinguish the type of random-source being used.
(define rstype-prg 'prg)
(define rstype-seq 'seq)

;; Initialize the random-source with a pseudo-random generator.
(define (use-prg-as-source [rgv #f])
  (unless (rgv? rgv)
    (raise-argument-error 'use-prg-as-source "rgv?" rgv))
  (set! random-source
        (cons rstype-prg
              (if rgv
                  (rand:vector->pseudo-random-generator rgv)
                  (rand:make-pseudo-random-generator)))))

;; Initialize the random-source with a given sequence.
;; TODO - what is the representation of the sequence?
(define (use-seq-as-source seq)
  (set! random-source
        (cons rstype-seq
              seq)))

;; Check that the random-source has been initialized. If it has not, use a PRG
;; as the source.
;; This function has a short name because it will be used frequently and I
;; wanted to reduce syntactic overhead.
(define (rnd-chk!)
  (when (eq? #f random-source)
    (use-prg-as-source)))

;; Poll whether the random-source is a PRG or not.
;; (A #f value implies that the random-source is a sequence.)
(define (rnd-prg?)
  (rnd-chk!)
  (eq? rstype-prg (car random-source)))

;; This macro allows for easily parameterizing the
;; current-pseudo-random-generator with random-source.
(define-syntax-rule (begin-rnd body ...+)
  (parameterize ([current-pseudo-random-generator random-source])
    (begin body ...+)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pseudo-Random Generator Generation
;;
;; Xsmith branches will produce a new pseudo-random generator, but the
;; production of these generators must be deterministic from a single input
;; seed value.

;; Produce an integer suitable for one of the first three elements of an RGV.
(define (rgv-first [inc #f])
  (random (if inc 1 0) 4294967087))

;; Produce an integer suitable for on of the last three elements of an RGV.
(define (rgv-second [inc #f])
  (random (if inc 1 0) 4294944443))

;; Produce a triad list of elements built according to `elem-func`. However, if
;; the first two elements are 0, then the third element will be incremented to
;; ensure the condition that at least one of the three is non-zero.
(define (make-triad elem-func)
  (let* ([e1 (elem-func)]
         [e2 (elem-func)]
         [e3 (if (and (eq? 0 e1) (eq? 0 e2))
                 (elem-func #t)
                 (elem-func))])
    (list e1 e2 e3)))

;; Produce an RGV.
(define (make-rgv)
  (define first-triad (make-triad rgv-first))
  (define second-triad (make-triad rgv-second))
  (list->vector (append first-triad
                        second-triad)))

;; Test if a value is an RGV.
(define (rgv? v)
  (rand:pseudo-random-generator-vector? v))

;; Produce a PRG.
(define (make-prg)
  (vector->pseudo-random-generator (make-rgv)))
