#lang racket/base

(provide
 ast-choice%
 choose-ast
 )

(require
 racket/class
 racket/list
 )

#|
Choices for AST growth should be some sort of object.

* They should have some "fresh-me" method that generates a fresh ast-node with appropriate holes, etc.  What exactly it generates could be altered by constraints available.
* They should have some weight score that affects their chances of being chosen.
* They should be able to be refined based on new constraints, and should detect when the choice is no longer possible due to conflicting constraints.
** For some things this could be done by choices having a list of sub-choices that can be filtered, but for others (eg. with large choice spaces) this should happen in some other way.

|#

(define ast-choice%
  (class object%
    (define/public (fresh) (error 'fresh-node "no default implementation"))
    (define/public (choice-weight) (error 'choice-weight "no default implementation"))
    (super-new)
    ))

(define (choose-ast ast-choice-list)
  ;; Weights are integers.
  ;; A random number should be generated between 0 and the sum of the weights.
  ;; Make a list of lists where sublists have the low-value for the bucket
  ;; and the value when the random number falls in that bucket.
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
