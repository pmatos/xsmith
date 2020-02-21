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
 use-prg-as-source
 use-seq-as-source
 set-prg-seed!
 random
 random-int
 random-uint)

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
 (prefix-in rand: racket/random)
 (for-syntax racket/base
             syntax/parse
             syntax/parse/define)
 racket/set)


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
;;     [ ] Connect randomness source to command-line arguments
;;   [ ] Consult correct source of randomness
;;   [ ] Provide functions for all common randomness use cases
;;     [X] random
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
;; The source of randomness is a singleton parameter. If not set explicitly, the
;; default action is to initialize a regular pseudo-random generator and use
;; that for the duration.

;; This is the parameter that should be used everywhere.
(define random-source (make-parameter #f))

;; Check if the random-source has been initialized.
(define (random-source-initialized?)
  (not (eq? #f (random-source))))

;; Set the random-source.
(define (set-random-source! type value)
  (unless (set-member? valid-rstypes type)
    (raise-user-error
     'set-random-source!
     (format "Must use an existing type for setting random source! Options are: ~a\n"
             valid-rstypes)))
  (set! random-source (make-parameter (cons type value))))

;; Get the random-source's type.
(define (random-source-type)
  (and (random-source)
       (car (random-source))))

;; Get the random-source's value.
(define (random-source-value)
  (and (random-source)
       (cdr (random-source))))

;; These distinguish the type of random-source being used.
(define rstype-prg 'prg)
(define rstype-seq 'seq)

;; A set containing the recognized types of random-source.
(define valid-rstypes
  (seteqv rstype-prg rstype-seq))

;; Initialize the random-source with a pseudo-random generator.
(define (use-prg-as-source [rgv #f])
  ;; If rgv has a value, check that it's valid.
  (when (and rgv (not (rgv? rgv)))
    (raise-argument-error 'use-prg-as-source "rgv?" rgv))
  ;; Set the random-source based on whether rgv has been given. If not, use a
  ;; regular PRG.
  (set-random-source!
   rstype-prg
   (if rgv
       (rand:vector->pseudo-random-generator rgv)
       (rand:make-pseudo-random-generator))))

;; Initialize the random-source with a given sequence.
;; TODO - what is the representation of the sequence?
(define (use-seq-as-source seq)
  (set-random-source!
   rstype-seq
   seq))

;; Set the current random seed in a PRG random-source.
;; Raises an error if the random-source is not a PRG.
(define (set-prg-seed! k)
  (unless (rnd-prg?)
    (raise-user-error
     'set-random-seed!
     "Cannot set random seed for non-PRG source of randomness."))
  (begin-racket-rand
    (rand:random-seed k)))

;; Check that the random-source has been initialized. If it has not, use a PRG
;; as the source.
;; This function has a short name because it will be used frequently and I
;; wanted to reduce syntactic overhead.
(define (rnd-chk!)
  (unless (random-source-initialized?)
    (use-prg-as-source)))

;; Poll whether the random-source is a PRG or not.
;; (A #f value implies that the random-source is a sequence.)
(define (rnd-prg?)
  (rnd-chk!)
  (eq? rstype-prg (random-source-type)))

;; This macro allows for easily parameterizing the
;; current-pseudo-random-generator with random-source.
(define-syntax (begin-racket-rand stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(begin
         (rnd-chk!)
         (parameterize ([rand:current-pseudo-random-generator
                         (random-source-value)])
           (begin body ...)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generation Functions
;;
;; These functions are used to get values from the random-source. It is
;; intended that users implementing fuzzers will primarily be using the
;; functions defined in this section.

;; Produce a random value on the corresponding interval:
;; (random)         - [0, 1)     (floating-point number)
;; (random k)       - [0, k)     (exact integer)
;; (random min max) - [min, max) (exact integer)
(define (random [min #f] [max #f])
  (define args (append (if min (list min) '())
                       (if max (list max) '())))
  ;; FIXME - this only works if rnd-prg? is true!
  (begin-racket-rand
    (apply rand:random args)))

;; Produce an unsigned integer on the range [0, 2^32 - 209].
(define (random-uint)
  (random 0 4294967087))

;; Produce a signed integer on the range [-(2^31 - 104), 2^31 - 105].
(define (random-int)
  (random -2147483544 2147483543))


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
