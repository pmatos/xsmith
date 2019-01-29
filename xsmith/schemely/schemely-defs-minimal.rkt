#!/usr/bin/env racket
#lang racket/base

(require xsmith racr racket/pretty racket/random racket/list racket/class)

(define-spec-component schemely-core)

(define (arg-length)
  (random 10))

(add-to-grammar
 schemely-core
 [DefinitionContext #f ([definitions : Definition * = (random 3)]
                        [expressions : Expression * = (add1 (random 3))])
   [#:prop strict-child-order? #t]]
 [Program DefinitionContext ()
          ;; TODO - this should not be necessary...
          [#:prop lift-predicate (λ (n t) #t)]]

 [Definition #f ([type = (concretize-type (fresh-type-variable))]
                 [name]
                 Expression)
   [#:prop binder-info (name type definition)]]

 [Expression #f ()
             [#:prop may-be-generated #f]]

 [LetStar Expression ([definitions : Definition * = (random 3)]
                      [body : DefinitionContext])
          [#:prop strict-child-order? #t]]

 [VariableReference Expression (name)
                    [#:prop reference-info (read name)]
                    [#:prop wont-over-deepen #t]
                    [#:prop choice-weight 10]]

 [LiteralNumber Expression ([v = (* (random 1000000)
                                    (if (equal? 0 (random 2)) -1 1))])
                [#:prop wont-over-deepen #t]]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]


 )


;; helper for to-s-exp
(define (->se sym . children-refs)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp (ast-child x n)))
                       children-refs))))
(define (->se* sym children-ref)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp x))
                       (ast-children (ast-child children-ref n))))))

(add-ag-rule
 schemely-core
 to-s-exp
 ;[Program (λ (n) `(begin ,@(map (λ (x) (att-value 'to-s-exp x))
 ;                               (ast-children (ast-child 'definitions n)))
 ;                        ,@(map (λ (x) (att-value 'to-s-exp x))
 ;                               (ast-children (ast-child 'expressions n)))))]
 [DefinitionContext (λ (n) `(,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'definitions n)))
                             ,@(map (λ (x) (att-value 'to-s-exp x))
                                    (ast-children (ast-child 'expressions n)))))]
 [Definition (λ (n) `(define ,(string->symbol (ast-child 'name n))
                       ,(att-value 'to-s-exp (ast-child 'Expression n))))]
 [LetStar (λ (n) `(let* (,@(map (λ (d) `(,(string->symbol (ast-child 'name d))
                                         ,(att-value 'to-s-exp
                                                     (ast-child 'Expression d))))
                                (ast-children (ast-child 'definitions n))))
                    ,@(att-value 'to-s-exp (ast-child 'body n))))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]

 [LiteralNumber (λ (n) (ast-child 'v n))]
 [Plus (->se '+ 'l 'r)]
 [Minus (->se '- 'l 'r)]
 )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

(define number (base-type 'number))
(define (number-type? x) (can-unify? x number))
(define (type-thunks-for-concretization) (list (λ()number)))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (concretize-type (fresh-type-variable))))


(define numeric-bin-op-type (λ (n t) (hash 'l number 'r number)))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop
 schemely-core
 type-info
 [DefinitionContext [(fresh-type-variable)
                     (λ (n t)
                       (define last-expression
                         (car (reverse (ast-children
                                        (ast-child 'expressions n)))))
                       (for/hash ([c (append
                                             (ast-children
                                              (ast-child 'definitions n))
                                             (ast-children
                                              (ast-child 'expressions n)))])
                         (values c
                                 (if (equal? c last-expression)
                                     t
                                     (fresh-type-variable)))))]]
 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [VariableReference [(fresh-type-variable) (no-child-types)]]
 [LetStar [(fresh-type-variable) (λ (n t) (hash-set
                                           (for/hash ([c (ast-children
                                                          (ast-child 'definitions n))])
                                             (values c (fresh-type-variable)))
                                           'body
                                           t))]]

 [LiteralNumber [number (no-child-types)]]
 [Plus [number numeric-bin-op-type]]
 [Minus [number numeric-bin-op-type]]
 )

(add-prop
 schemely-core
 fresh
 [VariableReference
  ;; TODO - getting a name for a reference should be automatic.
  (hash 'name
        (λ ()
          (let* ([choice* (random-ref (send this
                                            xsmith_reference-options!))]
                 [choice (if (procedure? choice*)
                             (choice*)
                             choice*)]
                 [parent (parent-node (current-hole))])
            (binding-name choice))))]
 ;; TODO - this should not be necessary
 [Definition (let* ([hole-name (ast-child 'name current-hole)]
                    [name (if (string? hole-name)
                              hole-name
                              (if (equal? (top-ancestor-node current-hole)
                                          (parent-node current-hole))
                                  (fresh-var-name "global-")
                                  (fresh-var-name "local-")))]
                    [hole-type (ast-child 'type current-hole)]
                    [type (if (and hole-type
                                   (not (and (ast-node? hole-type)
                                             (ast-bud-node? hole-type))))
                              hole-type
                              (fresh-concrete-var-type))])
               (hash 'name name
                     'type type))]
 )

;; TODO - this really shouldn't be necessary...
(add-prop
 schemely-core
 lift-type->ast-binder-type
 [#f (λ (type) 'Definition)])

(assemble-spec-components
 ;; TODO - have this macro check the name -- it can't have dashes or other things that RACR doesn't allow...
 schemely
 schemely-core)

(define (generate-and-print)
  (define (pp x)
    (pretty-print x (current-output-port) 1))
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (define forms (att-value 'to-s-exp (schemely-generate-ast 'Program)))
    ;; TODO - just assume racket for now, but later any scheme...
    (printf "#lang racket/base\n")
    (pp '(define (safe-car safe l)
           (if (null? l)
               safe
               l)))
    (pp '(define (safe-cdr l)
           (if (null? l)
               '()
               l)))
    (for ([form forms])
      (pp form))))

(xsmith-command-line generate-and-print)
