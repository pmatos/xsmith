#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017 The University of Utah
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
 ast-choice%
 choose-ast
 current-hole
 )

(require
 racket/class
 racket/list
 (for-syntax
  racket/base
  ))

#|
Choices for AST growth should be some sort of object.

* They should have some "fresh-me" method that generates a fresh ast-node with appropriate holes, etc.  What exactly it generates could be altered by constraints available.
* They should have some weight score that affects their chances of being chosen.
* They should have an associated set of features that affects (enables/disables)
*   their availability to be chosen.
* They should be able to be refined based on new constraints, and should detect when the choice is no longer possible due to conflicting constraints.
** For some things this could be done by choices having a list of sub-choices that can be filtered, but for others (eg. with large choice spaces) this should happen in some other way.

|#

(define-syntax (current-hole stx)
  ;; identifier macro to make it slightly easier to access the hole node.
  (syntax-case stx ()
    [current-hole #'(get-field hole this)]))

(define ast-choice%
  (class object%
    (init-field hole)
    (define/public (fresh hole-node) (error 'fresh-node "no default implementation"))
    (define/public (choice-weight) (error 'choice-weight "no default implementation"))
    (define/public (features) '())
    (super-new)
    ))

(define (choose-ast ast-choice-list)
  ;; Weights are integers.
  ;; A random number should be generated between 0 and the sum of the weights.
  ;; Make a list of lists where sublists have the low-value for the bucket
  ;; and the value when the random number falls in that bucket.
  (when (null? ast-choice-list)
    (error 'choose-ast "given empty ast choice list"))
  (define-values (total-weight choice-list)
    (for/fold ([sum 0]
               [clist '()])
              ([c ast-choice-list])
      (define c-weight (send c choice-weight))
      (values (+ sum c-weight)
              (cons (list sum c) clist))))
  (define r (random total-weight))
  (let loop ([choices choice-list])
    (if (>= r (first (first choices)))
        (second (first choices))
        (loop (rest choices)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
