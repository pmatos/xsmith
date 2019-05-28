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
 fresh-var-name
 fresh-int!

 xd-printf
 get-xsmith-debug-log!
 )
(module+ for-private
  (provide
   ast-children/flat
   make-generator-state
   xsmith-state
   (struct-out generator-state)

   expr->ast-list
   expr->ast-list
   node-type
   parent-node
   ast-ancestors
   ancestor-nodes
   top-ancestor-node
   node-subtype?
   ))
(module+ for-racr-convenience
  (provide
   expr->ast-list
   expr->ast-list
   node-type
   parent-node
   ast-ancestors
   ancestor-nodes
   top-ancestor-node
   node-subtype?
   ))

(require
 racket/list
 racr
 (for-syntax
  racket/base
  syntax/parse
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The mutable state of the code generator.
;; XXX --- should encapsulaye the RNG?  Currently, the RNG is a separate
;;   parameter, automatically manged by Racket.
;; XXX --- should this reference the options, too?  Right now, the options are
;;   are separate parameter.
(define xsmith-state (make-parameter #f))

(struct generator-state
  ((fresh-name-counter #:mutable))
  )

(define (make-generator-state)
  (generator-state 1))

(define (fresh-int!)
  (let ([n (generator-state-fresh-name-counter (xsmith-state))])
    (set-generator-state-fresh-name-counter! (xsmith-state) (add1 n))
    n))
(define (fresh-var-name [base "var_"])
  (format "~a~a" base (fresh-int!)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RACR convenience functions

(define (ast-children/flat n)
  (flatten
   (map (λ (x) (if (and (ast-node? x) (ast-list-node? x))
                   (ast-children x)
                   x))
        (if (ast-node? n)
            (ast-children n)
            '()))))

(define-syntax expr->ast-list
  (syntax-parser
    [(_ length:expr e:expr)
     #'(create-ast-list
        (map (λ (x) e)
             (make-list length #f)))]))

(define (node-type n)
  (and (ast-node? n)
       (not (ast-list-node? n))
       (not (ast-bud-node? n))
       (ast-node-type n)))

(define (parent-node n)
  ;; I've had several bugs where I used a parent node that was a list-node
  ;; thinking it was the grandparent node.  The list nodes are generally
  ;; useless, so this function gets the non-list parent node.
  (let ([p (with-handlers ([(λ _ #t) (λ _ #f)])
             ;; ast-parent raises an exception if there is no parent, I want #f
             (ast-parent n))])
    (cond [(not p) #f]
          [(ast-list-node? p) (ast-parent p)]
          [else p])))

(define (ast-ancestors n)
  (if (ast-has-parent? n)
      (cons (ast-parent n) (ast-ancestors (ast-parent n)))
      '()))
(define (ancestor-nodes n)
  (filter (λ (x) (and (not (ast-bud-node? x))
                      (not (ast-list-node? x))))
          (ast-ancestors n)))

(define (top-ancestor-node n)
  (let ([p (parent-node n)])
    (if p (top-ancestor-node p) n)))

(define (node-subtype? n t)
  (when (not (ast-node? n))
    (error 'node-subtype? "called on non-ast-node.  Arguments: ~a ~a" n t))
  (and (ast-node? n) (ast-subtype? n t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging
(define xsmith-debug-log-port (open-output-string))
(define (xd-printf . args)
  (apply fprintf xsmith-debug-log-port args))
(define (get-xsmith-debug-log!)
  (begin0
      (get-output-string xsmith-debug-log-port)
    (set! xsmith-debug-log-port (open-output-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
