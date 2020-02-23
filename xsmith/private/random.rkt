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
 set-prg-seed!
 use-seq-as-source
 begin-with-random-seed
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
;;     [X] Initialize from sequence
;;       [X] Determine a representation for the sequence (bits/bytes/etc,
;;           list/vector/etc)
;;     [X] Default initialization
;;     [ ] Connect randomness source to command-line arguments
;;   [ ] Consult correct source of randomness
;;   [ ] Provide functions for all common randomness use cases
;;     [X] random
;;     [X] random-seed
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

;; Create a new random-source parameter.
(define (make-random-source type value)
  (unless (set-member? valid-rstypes type)
    (raise-user-error
     'set-random-source!
     (format "Must use an existing type for setting random source! Options are: ~a\n"
             (set->list valid-rstypes))))
  (cons type value))

;; Set the random-source.
(define (set-random-source! type value)
  (set! random-source (make-parameter (make-random-source type value))))

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

;; Set the current random seed in a PRG random-source.
;; Raises an error if the random-source is not a PRG.
(define (set-prg-seed! k)
  (unless (rnd-prg?)
    (raise-user-error
     'set-random-seed!
     "Cannot set random seed for non-PRG source of randomness."))
  (begin-racket-rand
    (rand:random-seed k)))

;; Poll whether the random-source is a PRG or not.
;; (A #f value implies that the random-source is a sequence.)
(define (rnd-prg?)
  (rnd-chk!)
  (eq? rstype-prg (random-source-type)))

;; Check that the random-source has been initialized. If it has not, use a PRG
;; as the source.
;; This function has a short name because it will be used frequently and I
;; wanted to reduce syntactic overhead.
(define (rnd-chk!)
  (unless (random-source-initialized?)
    (use-prg-as-source)))

(define seq-chunk-size 4)

;; Initialize the random-source with a given sequence.
;; The sequence value is actually a 3-element list consisting of:
;;   1. A byte string with at least 4 bytes.
;;   2. An index into the string representing the current location.
;;   3. A PRG seeded from the first byte in the byte string, which will be used
;;      for extending the string as needed.
(define (use-seq-as-source seq)
  (unless (and (bytes? seq)
               (<= seq-chunk-size (bytes-length seq)))
    (raise-argument-error
     'use-seq-as-source
     "bytes? of at least length 8"
     seq))
  (set-random-source!
   rstype-seq
   (seq-val seq
            seq-chunk-size
            (let* ([seed-bytes (subbytes seq 0 seq-chunk-size)]
                   [seed-int (integer-bytes->integer seed-bytes #f)]
                   ;; The bounds on this value are documented by random-seed.
                   [seed-val (modulo seed-int (sub1 (expt 2 31)))])
              (make-prg seed-val)))))

(struct seq-val
  ([seq #:mutable]
   [idx #:mutable]
   [prg]))

;; Poll whether the random-source is a sequence or not.
;; (A #f value implies that the random-source is a PRG.)
(define (rnd-seq?)
  (not (rnd-prg?)))

;; Get the bytes from the random-source sequence.
(define (rnd-seq-bytes)
  (unless (rnd-seq?)
    (raise-user-error
     'rnd-seq-bytes
     "Source of randomness is not a sequence!"))
  (seq-val-seq (random-source-value)))

;; Get the current index from the random-source sequence.
(define (rnd-seq-index)
  (unless (rnd-seq?)
    (raise-user-error
     'rnd-seq-index
     "Source of randomness is not a sequence!"))
  (seq-val-idx (random-source-value)))

;; Move the sequence's index forward by `seq-chunk-size`.
(define (rnd-seq-advance-index!)
  (unless (rnd-seq?)
    (raise-user-error
     'rnd-seq-index
     "Source of randomness is not a sequence!"))
  (set-seq-val-idx! (random-source-value) (+ seq-chunk-size (rnd-seq-index))))

;; Get the PRG from the random-source sequence.
(define (rnd-seq-prg)
  (unless (rnd-seq?)
    (raise-user-error
     'rnd-seq-prg
     "Source of randomness is not a sequence!"))
  (seq-val-prg (random-source-value)))

;; Get the next `seq-chunk-size` bytes from the sequence and convert them into
;; an unsigned integer value.
(define (consume-uint-from-seq!)
  (when (eq? #f (rnd-seq-index))
    (raise-user-error
     'consume-int-from-seq!
     "Cannot consume values from an already-consumed sequence!"))
  (begin0
      (integer-bytes->integer
       (subbytes (rnd-seq-bytes) (rnd-seq-index) (+ seq-chunk-size (rnd-seq-index)))
       #f)
    (rnd-seq-advance-index!)
    ;; If the index has gone out of bounds of the existing sequence, set it to
    ;; #f as a sentinel so the sequence will no longer be consulted.
    (when (>= (rnd-seq-index) (bytes-length (rnd-seq-bytes)))
      (set-seq-val-idx! (random-source-value) #f))))

;; Extend the current sequence with a new sequence of bytes.
;; This is meant to be used when the existing sequence has been consume and new
;; values are being generated by the seq-val's PRG.
(define (extend-seq! new-bytes-seq)
  (unless (eq? #f (rnd-seq-index))
    (raise-user-error
     'extend-seq!
     "Cannot extend sequence before existing sequence has been consume!"))
  (unless (and (bytes? new-bytes-seq)
               (eq? 0 (modulo (bytes-length new-bytes-seq)
                              seq-chunk-size)))
    (raise-argument-error
     'extend-seq!
     (format "bytes? length modulo ~a" seq-chunk-size)
     new-bytes-seq))
  (set-seq-val-seq! (random-source-value)
                    (bytes-append (rnd-seq-bytes)
                                  new-bytes-seq)))

;; Get an integer from the seq-val's PRG. It will be modified by modulo to
;; ensure it fits within a chunk of the byte sequence and can also be used as a
;; random-seed value.
(define (generate-uint-from-seq-val)
  (modulo
   (begin-with-prg (rnd-seq-prg)
                   (random-int))
   (sub1 (expt 2 31))))

;; Produce an unsigned integer for a sequenced random-source.
(define (next-uint-from-seq)
  ;; If the byte sequence hasn't been fully consumed, take the next integer
  ;; from there.
  (unless (eq? #f (rnd-seq-index))
    (consume-uint-from-seq!))
  ;; Otherwise, produce a new integer using the seq-val's PRG, extend the byte
  ;; sequence to include this new integer, and then return the integer.
  (let* ([next-uint (generate-uint-from-seq-val)]
         [new-bytes (integer->integer-bytes next-uint seq-chunk-size #f)])
    (extend-seq! new-bytes)
    next-uint))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpful Macros
;;
;; These macros allow for easier use of the given system, automatically
;; parameterizing the necessary values as needed.

;; Install the random-source as the Racket-wide PRG so that calls to Racket's
;; randomness functions will work with our random-source, then execute the body
;; expressions like `begin`.
(define-syntax (begin-racket-rand stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(begin
         (rnd-chk!)
         (parameterize ([rand:current-pseudo-random-generator
                         (random-source-value)])
           (begin body ...)))]))

;; Create a new PRG and seed it with the given value to allow for computing
;; random values without affecting the main random-source.
;;
;; The idea is that this can be used for deterministic sub-computations of
;; randomness, e.g.:
;;
;;   (begin-with-random-seed (random-uint)
;;      ...)
;;
;; This can be helpful for wrapping operations that use built-in standard
;; library randomness functions in a deterministic way.
(define-syntax (begin-with-random-seed stx)
  (syntax-parse stx
    [(_ k:integer body ...+)
     #'(begin-with-prg (make-prg)
                       (set-prg-seed! k)
                       body ...)]))

;; Given a PRG, execute the body statements with that PRG installed as the
;; random-source.
(define-syntax (begin-with-prg stx)
  (syntax-parse stx
    [(_ prg:expr body ...+)
     #'(parameterize ([random-source (make-random-source rstype-prg prg)])
         (begin body ...))]))


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
;; These generators are produced using crypto-random-bytes, meaning they are
;; cryptographically secure and are entirely unimpacted by the existing
;; random-source.
;;
;; To use these deterministically, invoke the `set-prg-seed!` function.

;; Produce an RGV vector element.
;; `max` represents the maximum value this element can have.
;; `inc` determines whether the element must be non-zero.
(define (rgv-element max [non-zero? #f])
  (let* ([val (integer-bytes->integer (rand:crypto-random-bytes 8) #f)]
         [val (modulo val max)]
         [val (if (and non-zero? val)
                  (add1 val)
                  val)])
    val))

;; Produce an integer suitable for one of the first three elements of an RGV.
(define (rgv-first [non-zero? #f])
  (rgv-element 4294967087 non-zero?))

;; Produce an integer suitable for on of the last three elements of an RGV.
(define (rgv-second [non-zero? #f])
  (rgv-element 4294944443 non-zero?))

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
(define (make-prg [seed #f])
  (define prg (vector->pseudo-random-generator (make-rgv)))
  (when seed
    (begin-with-prg prg (set-prg-seed! seed)))
  prg)
