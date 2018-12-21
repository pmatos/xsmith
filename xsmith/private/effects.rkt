#lang racket/base
(require racket/contract)
(provide
 effect-variable

 effect-read-variable
 effect-read-variable?
 effect-write-variable
 effect-write-variable?
 effect-io
 effect-io?
 )

(struct effect (type variable) #:transparent)

(define (effect-read-variable v)
  (effect 'read-variable v))
(define (effect-read-variable? x)
  (and (effect? x)
       (eq? 'read-variable (effect-type x))))
(define (effect-write-variable v)
  (effect 'write-variable v))
(define (effect-write-variable? x)
  (and (effect? x)
       (eq? 'write-variable (effect-type x))))
(define (effect-io)
  (effect 'io #f))
(define (effect-io? x)
  (and (effect? x)
       (eq? 'io (effect-type x))))
