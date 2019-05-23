#lang at-exp racket/base

(provide (all-defined-out))

(require
 (only-in scribble/core make-element)
 scribble/base
 )

(define (verb . content)
  (make-element 'tt content))


(define (racr)
  @seclink["racr"]{RACR})

