#lang racket/base

(provide
 xsmith-state
 (struct-out generator-state)
 make-generator-state
 fresh-var-name

 ast-children/flat
 expr->ast-list
 node-type
 parent-node
 top-ancestor-node
 node-subtype?
 )

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

(define (fresh-var-name [base "var_"])
  (let ((n (generator-state-fresh-name-counter (xsmith-state))))
    (set-generator-state-fresh-name-counter! (xsmith-state) (add1 n))
    (format "~a~a" base n)))


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

(define (top-ancestor-node n)
  (let ([p (parent-node n)])
    (if p (top-ancestor-node p) n)))

(define (node-subtype? n t)
  (when (not (ast-node? n))
    (error 'node-subtype? "called on non-ast-node.  Arguments: ~a ~a" n t))
  (and (ast-node? n) (ast-subtype? n t)))
