#lang racket/base

(provide (all-defined-out))

(require
 racket/dict)

(define random-seed-max (expt 2 31))

(define xsmith-options
  (make-parameter #f))
(define (xsmith-options-defaults)
  (make-hasheq
   (list (cons 'random-seed (random random-seed-max)))))
(define (xsmith-option
         key
         [default (λ () (error 'xsmith-option "key not found: ~a" key))])
  (when (not (dict? (xsmith-options)))
    (error 'xsmith-options "xsmith options not parameterized."))
  (dict-ref (xsmith-options) key default))

(define current-xsmith-max-depth
  (make-parameter #f))
(define (xsmith-max-depth)
  (current-xsmith-max-depth))

(define current-xsmith-features
  (make-parameter (hash)))
(define (xsmith-feature-enabled? key)
  (dict-ref
   (current-xsmith-features)
   key
   (λ () (error
          'xsmith-feature
          "Feature not in dictionary: ~a.  Did you set it in xsmith-command-line?"
          key))))
