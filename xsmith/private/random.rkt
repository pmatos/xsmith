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
 ;; Source selection/initialization.
 use-prg-as-source
 set-prg-seed!
 use-seq-as-source
 ;; Macros.
 begin-with-random-seed
 ;; Random generation functions.
 random
 random-uint
 random-int
 random-bool
 random-ref
 random-char
 random-ascii-lower-char
 random-ascii-upper-char
 random-ascii-alpha-char
 random-ascii-numeral-char
 random-ascii-alphanumeric-char
 random-ascii-word-char
 random-string
 random-ascii-lower-string
 random-ascii-upper-string
 random-ascii-alpha-string
 random-ascii-numeral-string
 random-ascii-alphanumeric-string
 random-ascii-word-string
 random-ascii-sentence)

(require
 (prefix-in racket: (only-in racket/base
                             random
                             random-seed
                             make-pseudo-random-generator
                             pseudo-random-generator?
                             current-pseudo-random-generator
                             pseudo-random-generator->vector
                             vector->pseudo-random-generator
                             vector->pseudo-random-generator!
                             pseudo-random-generator-vector?))
 (prefix-in racket: racket/random)
 (for-syntax racket/base
             syntax/parse
             syntax/parse/define)
 racket/list
 racket/set
 racket/string)


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
;;   [X] Consult correct source of randomness
;;   [X] Provide functions for all common randomness use cases
;;     [X] random
;;     [X] random-seed
;;     [X] random-ref
;;     [X] random-int
;;     [X] random-char
;;     [X] random-string
;;   [X] Provide functions for custom use cases
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
  (random-source (make-random-source type value)))

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
       (racket:vector->pseudo-random-generator rgv)
       (racket:make-pseudo-random-generator))))

;; Set the current random seed in a PRG random-source.
;; Raises an error if the random-source is not a PRG.
(define (set-prg-seed! k)
  (unless (rnd-prg?)
    (raise-user-error
     'set-random-seed!
     "Cannot set random seed for non-PRG source of randomness."))
  (begin-with-racket-prg
    (random-source-value)
    (racket:random-seed k)))

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

;; Given a PRG, execute the body statements with that PRG installed as the
;; Racket-wide pseudo-random-generator.
(define-syntax (begin-with-racket-prg stx)
  (syntax-parse stx
    [(_ prg body ...+)
     #'(parameterize ([racket:current-pseudo-random-generator prg])
         (begin body ...))]))

;; Given a PRG, execute the body statements with that PRG installed as the
;; random-source.
(define-syntax (begin-with-prg stx)
  (syntax-parse stx
    [(_ prg body ...+)
     #'(parameterize ([random-source (make-random-source rstype-prg prg)])
         (begin body ...))]))

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
    [(_ seed body ...+)
       #'(begin
           (define prg (make-prg seed))
           (begin-with-prg prg
                           body ...))]))


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
  (cond
    [(rnd-prg?)
     ;; If we have a PRG, use it as the Racket-wide PRG and call Racket's
     ;; built-in random function.
     (begin-with-racket-prg
       (random-source-value)
       (apply racket:random args))]
    [(rnd-seq?)
     ;; If we have a byte sequence, install a new PRG using the next integer
     ;; from the sequence as the seed and then call this random function
     ;; recursively (to use the previous case's handling).
     (begin-with-random-seed
       (next-uint-from-seq)
       (apply random args))]
    [else
     (raise-user-error
      'random
      "Invalid random-source state detected.")]))

;; Produce an unsigned integer on the range [0, 2^32 - 209].
(define (random-uint)
  (random 0 4294967087))

;; Produce a signed integer on the range [-(2^31 - 104), 2^31 - 105].
(define (random-int)
  (random -2147483544 2147483543))

;; Produce a Boolean.
(define (random-bool)
  (>= 0.5 (random)))

;; Get a random element from a list.
(define (random-ref lst)
  (list-ref lst (random (length lst))))

;; Produce a character.
(define (random-char)
  (integer->char
   ;; Determine whether to use the first generation range or the second.
   ;; These values are documented in Racket's `integer->char` function.
   (if (random-bool)
       (random 0 55295)
       (random 57344 1114111))))

;; These are character class definitions for generating individual characters.
(define ascii-lower-range (range 97 123))   ;; [a-z]
(define ascii-upper-range (range 65 90))    ;; [A-Z]
(define ascii-alpha-range                   ;; [a-zA-Z]
  (append ascii-lower-range
          ascii-upper-range))
(define ascii-numeral-range (range 48 57))  ;; [0-9]
(define ascii-alphanumeric-range            ;; [a-zA-Z0-9]
  (append ascii-alpha-range
          ascii-numeral-range))
(define ascii-word-range                    ;; [a-zA-Z0-9_]
  (append ascii-alphanumeric-range
          (list 95)))

;; Randomly select an integer from the indicated range and convert it to a char.
(define (rnd-char-in-range range)
  (integer->char (random-ref range)))

;; Each of these functions produces a character of the corresponding range.
(define (random-ascii-lower-char)
  (rnd-char-in-range ascii-lower-range))
(define (random-ascii-upper-char)
  (rnd-char-in-range ascii-upper-range))
(define (random-ascii-alpha-char)
  (rnd-char-in-range ascii-alpha-range))
(define (random-ascii-numeral-char)
  (rnd-char-in-range ascii-numeral-range))
(define (random-ascii-alphanumeric-char)
  (rnd-char-in-range ascii-alphanumeric-range))
(define (random-ascii-word-char)
  (rnd-char-in-range ascii-word-range))

;; The default bound for string generation.
(define default-string-bound 128)

;; Generate a string given a character-generation function.
;; If given a bound, the string's length  will be less than or equal to that
;; bound (though all strings will have a length of at least 1). If given a
;; pre-func, the first character will be generated by that function. If given a
;; post-func, the final character will be generated by that function.
(define (rnd-string-from-char-func func
                                   [bound #f]
                                   [pre-func #f]
                                   [post-func #f])
  (unless pre-func
    (set! pre-func func))
  (unless bound
    (set! bound default-string-bound))
  (define len
    (if (<= bound 1)
        1
        (random 1 bound)))
  (string-append
   (string (pre-func))
   (apply string (for/list ([_ (in-range (- len
                                            (if post-func
                                                2
                                                1)))])
                   (func)))
   (if (and (>= bound 2) post-func)
       (string (post-func))
       "")))

;; Produce a string of characters.
(define (random-string [bound #f])
  (rnd-string-from-char-func random-char))

;; Each of these functions produces a string consisting of characters of the
;; corresponding range.
(define (random-ascii-lower-string [bound #f])
  (rnd-string-from-char-func random-ascii-lower-char bound))
(define (random-ascii-upper-string [bound #f])
  (rnd-string-from-char-func random-ascii-upper-char bound))
(define (random-ascii-alpha-string [bound #f])
  (rnd-string-from-char-func random-ascii-alpha-char bound))
(define (random-ascii-numeral-string [bound #f])
  (rnd-string-from-char-func random-ascii-numeral-char bound))
(define (random-ascii-alphanumeric-string [bound #f])
  (rnd-string-from-char-func random-ascii-alphanumeric-char bound))
(define (random-ascii-word-string [bound #f])
  (rnd-string-from-char-func random-ascii-word-char
                             bound
                             random-ascii-alpha-char))

;; Produce a string of words (from random-ascii-word-string) separated by
;; spaces.
(define (random-ascii-sentence [word-bound 16]
                               [word-length-bound 8])
  (string-join
   (for/list ([i (in-range (random 1 word-bound))])
     (random-ascii-word-string (random 1 word-length-bound)))))


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
  (let* ([val (integer-bytes->integer (racket:crypto-random-bytes 8) #f)]
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
  (racket:pseudo-random-generator-vector? v))

;; Produce a PRG.
(define (make-prg [seed #f])
  (define prg (racket:vector->pseudo-random-generator (make-rgv)))
  (when seed
    (begin-with-racket-prg prg (racket:random-seed seed)))
  prg)
