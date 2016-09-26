#!/usr/bin/env racket
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2016 The University of Utah
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

#lang racket

(provide make-choice-table
         choose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; A `choice-table' describes a weighted choice.
;;
;; It is represented as a vector.  An item's weight is equal to the number of
;; times that the item appears in the vector.  Thus, "weighted choice" is
;; simply a uniformly distributed choice of a slot in the vector.
;;
;; This implementation was chosen so that weighted choices take constant time.
;;
(struct choice-table
  (vec))

(define (make-choice-table choice-list)
  (choice-table (make-choice-table-vec choice-list)))

(define (make-choice-table-vec choice-list)
  (let* ((len (for/sum ([c choice-list]) (second c)))
         (vec (make-vector len)))
    (let ((vec-i 0))
      (for/list ([c choice-list])
        (let ((value (first c))
              (weight (second c)))
          (for ((i (in-range vec-i (+ vec-i weight))))
            (vector-set! vec i value))
          (set! vec-i (+ vec-i weight)))))
    vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Randomly select and return an item from the given `choice-table'.
;;
(define (choose ct)
  (let ((v (choice-table-vec ct)))
    (vector-ref v (random (vector-length v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  (check-equal? (make-choice-table-vec '((a 1)))
                #(a))
  (check-equal? (make-choice-table-vec '((a 3)))
                #(a a a))
  (check-equal? (make-choice-table-vec '((a 1) (b 1)))
                #(a b))
  (check-equal? (make-choice-table-vec '((a 1) (b 3)))
                #(a b b b))
  (check-equal? (make-choice-table-vec '((a 0) (b 3)))
                #(b b b))
  (check-equal? (make-choice-table-vec '((a 1) (b 1) (c 1)))
                #(a b c))
  (check-equal? (make-choice-table-vec '((a 1) (b 2) (c 3)))
                #(a b b c c c))
  )

(module+ test
  (require rackunit)
  (check-eq? (choose (make-choice-table '((a 1))))
             'a)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
