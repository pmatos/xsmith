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
 (all-defined-out)
 ;; Constants
 max-seed-value
 ;; Source selection/initialization.
 random-source
 make-random-source
 get-random-source-byte-sequence
 ;; Macros.
 begin-external-random
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
 racket/contract/base
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
;; [X] Coverage-Guided Generation
;;   [X] Initialize/store source of randomness (generator or bit sequence)
;;     [X] Initialize from PRG
;;     [X] Initialize from sequence
;;       [X] Determine a representation for the sequence (bits/bytes/etc,
;;           list/vector/etc)
;;     [X] Default initialization
;;     [X] Connect randomness source to command-line arguments
;;   [X] Consult correct source of randomness
;;   [X] Provide functions for all common randomness use cases
;;     [X] random
;;     [X] random-seed
;;     [X] random-ref
;;     [X] random-int
;;     [X] random-char
;;     [X] random-string
;;   [X] Provide functions for custom use cases
;;   [X] Consider specialized distribution functions
;; [ ] Investigate More Specific Sequence Representations
;;    -  Instead of just storing the random bits, consider storing the results
;;       of computations along with their generation constraints. This would
;;       allow for better tailoring of values during re-generation from a given
;;       sequence.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Intended Method of Use
;;
;; This library is intended to replace any other randomness functions used in
;; Xsmith. The goal is to provide a source of manipulable randomness to ensure
;; deterministic execution when it is need while not requiring significant
;; programmer overhead to incorporate.
;;
;; The implementation is inspired by Zest [1]. To use this system, you need only
;; parameterize the `random-source` with a source made from `make-random-source`
;; (implemented below). Within this parameterization, all of the provided
;; randomness primitive functions will work deterministically.
;;
;; [1] "Semantic Fuzzing with Zest". Padhye et al, 2019.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants
;;
;; These are constants that are useful.

;; The maximum value for a PRG's seed is (2^31 - 1).
(define max-seed-value (sub1 (expt 2 31)))

;; These are character class definitions for generating individual characters.
(define ascii-lower-range (range 97 123))   ;; [a-z]
(define ascii-upper-range (range 65 91))    ;; [A-Z]
(define ascii-alpha-range                   ;; [a-zA-Z]
  (append ascii-lower-range
          ascii-upper-range))
(define ascii-numeral-range (range 48 58))  ;; [0-9]
(define ascii-alphanumeric-range            ;; [a-zA-Z0-9]
  (append ascii-alpha-range
          ascii-numeral-range))
(define ascii-word-range                    ;; [a-zA-Z0-9_]
  (append ascii-alphanumeric-range
          (list 95)))

;; The default bound for string generation.
(define default-string-bound 128)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Source of Randomness
;;
;; The source of randomness is a singleton parameter. If not set explicitly, the
;; default action is to initialize a regular pseudo-random generator and use
;; that for the duration.

;; This is the parameter that should be used everywhere.
(define random-source (make-parameter #f))

;; Create a new random-source parameter.
(define (assemble-random-source type value)
  (unless (set-member? valid-rstypes type)
    (raise-user-error
     'assemble-random-source
     (format "Must use an existing type for setting random source! Options are: ~a\n"
             (set->list valid-rstypes))))
  (cons type value))

;; Create a new random-source. This is generally meant to be used during
;; parameterization of the random-source value.
;;
;; In all cases, this will create a sequence-based random-source, but the mode
;; of creation depends on the value passed in as argument.
(define (make-random-source [val #f])
  (make-byte-sequence-random-source
   (if (bytes? val)
       ;; If we have a byte sequence, just use that.
       val
       ;; Otherwise, we need to build a new byte sequence.
       (integer->integer-bytes
        (if (and (integer? val)
                 (<= 0 val max-seed-value))
            ;; If the value is an integer that can be used as a seed, just
            ;; convert it directly to a byte sequence.
            val
            ;; Otherwise, we need to get a seed value from a PRG.
            (begin-with-racket-prg
              (make-pseudo-random-generator val)
              (racket:random 0 (add1 max-seed-value))))
        seq-chunk-size
        #f))))

;; Return the sequence of bytes that has been generated so far.
(define (get-random-source-byte-sequence)
  (rnd-seq-bytes))

;; Check if the random-source has been initialized.
(define (random-source-initialized?)
  (not (eq? #f (random-source))))

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

;;;;;;;;
;; PRG-as-a-source

;; Create a new pseudo-random-generator-based source.
(define (make-pseudo-random-generator-random-source [val #f])
  (assemble-random-source
   rstype-prg
   (make-pseudo-random-generator val)))

;; Create a pseudo-random generator.
(define (make-pseudo-random-generator [val #f])
  (cond
    ;; If there's no value, generate a random PRG.
    [(not val)
     (make-prg)]
    ;; If the value is an RGV, use it to initialize a new PRG.
    [(racket:pseudo-random-generator-vector? val)
     (racket:vector->pseudo-random-generator val)]
    ;; If the value is a seed, make a PRG with that seed.
    [(and (integer? val)
          (<= 0 val max-seed-value))
     (make-prg val)]
    ;; Otherwise, the value is invalid.
    [else
     (raise-user-error
      'make-pseudo-random-generator
      "Received invalid value for random generator input. See documentation.")]))

;; Poll whether the random-source is a PRG or not.
;; (A #f value implies that the random-source is a sequence.)
(define (rnd-prg?)
  (eq? rstype-prg (random-source-type)))


;;;;;;;;
;; Sequence-as-a-source

;; The number of bytes to use per chunk.
(define seq-chunk-size 4)

;; Create a random source initialized from a byte sequence.
(define (make-byte-sequence-random-source seq)
  (assemble-random-source
   rstype-seq
   (make-byte-sequence-value seq)))

;; Create a sequence-based random-source with a given sequence.
;; The sequence value is actually a 3-element list consisting of:
;;   1. A byte string with at least 4 bytes.
;;   2. An index into the string representing the current location.
;;   3. A PRG seeded from the first byte in the byte string, which will be used
;;      for extending the string as needed.
(define (make-byte-sequence-value seq)
  ;; First make sure a byte string was given.
  (unless (bytes? seq)
    (raise-argument-error
     'make-byte-sequence-value
     "bytes?"
     seq))
  ;; If the given byte string is of insufficient length (i.e., smaller than the
  ;; seq-chunk-size) or if the total number of bytes is not divisible by
  ;; seq-chunk-size, pad the needed amount with \0 bytes.
  (let* ([seq-len (bytes-length seq)]
         [bytes-in-last-chunk (modulo seq-len seq-chunk-size)]
         [needed-padding (- seq-chunk-size bytes-in-last-chunk)])
    (when (or (<= seq-len seq-chunk-size)
              (not (eq? 0 bytes-in-last-chunk)))
      (set! seq (bytes-append seq (make-bytes needed-padding)))))
  ;; Now that we know the byte string is definitely long enough, create a
  ;; seq-val using that byte string.
  (let* ([seed-bytes (subbytes seq 0 seq-chunk-size)]
         [seed-int (integer-bytes->integer seed-bytes #f)]
         [seed-val (modulo seed-int (add1 max-seed-value))]
         [init-idx (bytes-length seed-bytes)])
    (seq-val
     ;; The initial sequence is just the sequence that was passed in.
     seq
     ;; The index is initialized to be just after the bytes used to seed the
     ;; PRG, unless this would consume all available bytes in which case the
     ;; index becomes #f.
     (if (eq? init-idx (bytes-length seq))
         #f
         init-idx)
     ;; The PRG is initialized using the first few bytes in the sequence.
     (make-prg seed-val))))

(struct seq-val
  ([seq #:mutable]
   [idx #:mutable]
   [prg]))

;; Poll whether the random-source is a sequence or not.
;; (A #f value implies that the random-source is a PRG.)
(define (rnd-seq?)
  (eq? rstype-seq (random-source-type)))

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
  (modulo (begin-with-prg
            (rnd-seq-prg)
            (random-int))
          (add1 max-seed-value)))

;; Produce an unsigned integer for a sequenced random-source.
(define (next-uint-from-seq)
  (if (eq? #f (rnd-seq-index))
      ;; If the byte sequence is fully consumed (i.e., there are no values
      ;; remaining to consume), produce a new integer using the seq-val's PRG
      ;; and extend the byte sequence to include this new integer. Then, return
      ;; the new integer.
      (let* ([next-uint (generate-uint-from-seq-val)]
             [new-bytes (integer->integer-bytes next-uint seq-chunk-size #f)])
        (extend-seq! new-bytes)
        next-uint)
      ;; Otherwise, simply consume the next integer in the byte sequence.
      (consume-uint-from-seq!)))


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

;; Generate a PRG from the random-source and install it as the Racket-wide
;; pseudo-random generator.
(define-syntax (begin-external-random stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(let ()
         (define seed (random 0 (add1 max-seed-value)))
         (define prg (make-prg seed))
         (begin-with-racket-prg
           prg
           body ...))]))

;; Given a PRG, execute the body statements with that PRG installed as the
;; random-source.
(define-syntax (begin-with-prg stx)
  (syntax-parse stx
    [(_ prg body ...+)
     #'(parameterize ([random-source (assemble-random-source rstype-prg prg)])
         (begin body ...))]))

;; Create a new PRG and seed it with the given value to allow for computing
;; random values without affecting the main random-source.
;;
;; The idea is that this can be used for deterministic sub-computations of
;; randomness, e.g.:
;;
;;   (begin-with-random-seed
;;     (random-uint)
;;     ...)
;;
;; This can be helpful for wrapping operations that use built-in standard
;; library randomness functions in a deterministic way.
(define-syntax (begin-with-random-seed stx)
  (syntax-parse stx
    [(_ seed body ...+)
     #'(begin
         (let ([prg (make-prg seed)])
           (begin-with-prg prg
                           body ...)))]))


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
   (apply string
          (for/list ([_ (in-range (- len (if post-func
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
;; Stateful Random-Source Manipulation Functions
;;
;; Sometimes it is useful to be able to manipulate the random-source directly,
;; instead of using parameterization. For example, in the case of using this
;; module from the Racket REPL it can be desirable to instantiate the
;; random-source without wrapping every call in a `parameterize` call. This
;; sub-module allows for this to be done easily.

(module* stateful #f
  (provide (all-defined-out))

  ;; Update the parameter to take the new value without directly exposing the
  ;; parameter externally.
  (define (set-random-source! value)
    (random-source value))

  ;; Statefully create a new random-source with a randomly-generated seed.
  (define (initialize-random-source)
    (begin-with-racket-prg
      (make-prg)
      (define seed (racket:random 0 (add1 max-seed-value)))
      (initialize-random-source-from-seed seed)))

  ;; Given a seed, statefully create a new random-source from it.
  (define (initialize-random-source-from-seed seed)
    (set-random-source! (make-pseudo-random-generator-random-source seed)))

  ;; Given a sequence, statefully create a new random-source from it.
  (define (initialize-random-source-from-sequence seq)
    (set-random-source! (make-byte-sequence-random-source seq))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Distribution Functions
;;
;; To provide the math/distributions functions with deterministic randomness,
;; this submodule is provided. It obtains a list of all the names defined in
;; the math/distributions library, then re-provides the functions that it finds,
;; wrapping them in a macro that will ensure the randomness is deterministic
;; according to the random-source.

(module* distributions #f
  (require math/distributions
           (for-syntax racket/base
                       syntax/parse
                       ))

  (define-for-syntax (module-names mod)
    (define (extract-names l)
      (if (null? l)
          '()
          (map car (cdar l))))
    (define (get-all-names . ls)
      (append (map extract-names ls)))
    (define names
      (let-values ([(vars stxs) (module->exports mod)])
        (append
         (extract-names vars)
         (extract-names stxs))))
    names)

  (define-for-syntax math-names (module-names 'math/distributions))

  (define-syntax (wrap+provide-math/distributions stx)
    #`(begin
        #,@(for/list
               ([x-sym math-names]
                [x-wrapped (generate-temporaries math-names)])
             (with-syntax ([x (datum->syntax #'here x-sym)]
                           [x-name x-wrapped])
               #'(begin
                   (define-syntax (x-name x-stx)
                     (syntax-parse x-stx
                       [(_ arg (... ...))
                        #'(begin-external-random (x arg (... ...)))]
                       [not-parenthesized:id
                        #'(λ args (begin-external-random (apply x args)))]))
                   (provide (rename-out [x-name x])))))))

  (wrap+provide-math/distributions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pseudo-Random Generator Generation
;;
;; These generators are produced using crypto-random-bytes, meaning they are
;; cryptographically secure and are entirely unimpacted by the existing
;; random-source.

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
    (begin-with-racket-prg
      prg
      (racket:random-seed seed)))
  prg)
