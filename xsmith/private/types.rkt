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
#|
WIP checklist:
* I need to put anything important from `unify!` and `can-unify?` and put them into their subtype counterparts.  Then I need to make the normal unify version just do two reversed subtype versions.
* I changed the base-type struct, I need to ripple that change through everywhere that uses it.
* I changed the type variable struct, anything that matches it needs to change
* I changed the product type struct, I need to propagate changes
* I need to review code comments and make sure they still make sense.
|#

(require racket/contract)
(provide
 type-variable?

 ;(struct-out base-type)
 (contract-out
  [rename mk-base-type base-type (->* (any/c) (base-type?)
                                      base-type?)])
 base-type-name
 base-type-supertype
 base-type?

 ;(struct-out alias-type)
 ;; TODO - tuple types -- what should the API be?
 ;(rename-out [mk-record-type record-type])
 product-type?
 [rename-out [product-type-inner-type-list/resolve product-type-inner-type-list]]
 ;product-type-inner-type-list
 ;record-type?
 ;record-type-name

 define-generic-type
 generic-type?
 generic-type-name
 generic-type-type-arguments

 ;nominal-record-type
 nominal-record-type-with
 any-nominal-record-type
 nominal-record-type?
 nominal-record-type-name
 nominal-record-type-inners

 (rename-out [make-nominal-record-definition nominal-record-definition-type])
 nominal-record-definition-type?
 nominal-record-definition-type-type

 type?

 (contract-out
  [fresh-type-variable (->* () () #:rest (listof type?) type?)]
  [unify! (-> type? type? any/c)]
  [can-unify? (-> type? type? any/c)]
  [concretize-type (-> type? type?)]
  [rename concrete? concrete-type? (-> type? any/c)]
  [rename mk-product-type product-type (-> (or/c #f (listof type?)) type?)]
  [function-type (-> type? type? type?)]
  )
 function-type?
 function-type-arg-type
 function-type-return-type

 current-xsmith-type-constructor-thunks

 type->type-variable-list
 at-least-as-concrete
 contains-type-variables?
 )

(require
 racket/match
 racket/random
 racket/dict
 racket/list
 racket/set
 "scope-graph.rkt"
 "xsmith-utils.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))
(module+ test (require rackunit))


#|
TODO - record types that use scope graphs
TODO - generic types (eg. array types)
|#



#|
Type variables need to be able to partially constrain types in various ways for nodes to be able to specify what types they can fulfill, including:
• unconstrained (classic type variable) for eg FunctionApplication (could return anything)
• List of potential types (for eg. generic operations like AdditionExpression can be int or float but not unconstrained).

The following are maybe not variables but constrain how I need to think about them:
• function with unconstrained return type and arg list as an unconstrained type list (eg. even length is unconstrained) eg. for lambda expressions (This is not a variable but informs how they are used because it may contain them.)
• function application will be similar but will constrain the return type
• product types with an unspecified length (or a list which may or may not contain variables)
• record constrained to have a field of a certain type but otherwise unconstrained
|#

#|
Type variable innards contain a set of handles (type variables with that innard),
a "type", which is either:
• #f - unconstrained
• a non-variable type (though it may contain type variables)
• a list - constrained to be one of the types in the list
A type variable list may not contain type variables and may not contain more than one of each compound type.  Eg. it may contain any number of base types but only one function type or record type.  However, the function or record type contained my be only partially specified (eg. may contain type variables).

A forward field, which is #f when the type variable innard has not been forwarded to a different one, or points to another type variable.  This is to be more lazy about updating the upper and lower bounds lists.

And upper and lower bounds lists (for subtyping).  The bounds lists include any number of other type variable innards.

When type variables are subtype-unified, the variables are set in each other's upper/lower bound list.  Their type lists are filtered only to types that have a sub/super type in the other list.  This also affects transitive upper/lower bounds.
If a (transitive) upper bound is ever equal to a (transitive) lower bound, that part of the lattice is unified into one type-variable-innard.

|#
(struct type-variable ([tvi #:mutable])
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (define innard (type-variable-tvi v))
     (match v
       [(type-variable (type-variable-innard _ t _ _ _))
        (fprintf output-port
                 "#<type-variable ~a>"
                 t)]))])
(struct type-variable-innard
  ([handle-set #:mutable]
   [type #:mutable]
   [forward #:mutable]
   [lower-bounds #:mutable]
   [upper-bounds #:mutable]))

(define (innard->forward-resolve innard)
  (match innard
    [(type-variable-innard _ _ #f _ _) innard]
    [(type-variable-innard _ _ forwarded _ _)
     (innard->forward-resolve forwarded)]))

(define ((type-variable-innard-DIR-bounds! dir set-dir!) innard)
  ;; This version updates forwarded bounds.
  (define ret1 (dir innard))
  (define ret2 (map innard->forward-resolve ret1))
  (define ret3 (remove-duplicates ret2))
  (set-dir! innard ret3)
  ret3)
(define type-variable-innard-lower-bounds!
  (type-variable-innard-DIR-bounds! type-variable-innard-lower-bounds
                                    set-type-variable-innard-lower-bounds!))
(define type-variable-innard-upper-bounds!
  (type-variable-innard-DIR-bounds! type-variable-innard-upper-bounds
                                    set-type-variable-innard-upper-bounds!))


(define ((type-variable-innard->transitive-DIR-bounds dir) tvi)
  (define immediates (dir tvi))
  (let loop ([uppers immediates]
             [work-list immediates])
    (cond [(null? work-list) uppers]
          [else
           (define maybe-new-ones (dir (car work-list)))
           (define new-ones (set-subtract maybe-new-ones uppers))
           (define new-uppers (set-union uppers new-ones))
           (define new-work-list (append new-ones (cdr work-list)))
           (loop new-uppers
                 new-work-list)])))
(define type-variable-innard->transitive-lower-bounds
  (type-variable-innard->transitive-DIR-bounds type-variable-innard-lower-bounds))
(define type-variable-innard->transitive-upper-bounds
  (type-variable-innard->transitive-DIR-bounds type-variable-innard-upper-bounds))

(define (fresh-type-variable . args)
  (when (memf type-variable? args)
    (error 'fresh-type-variable
           "partially constrained type variables can not include type variables in the constraint list.  Given ~a\n"
           args))
  (define (composite-error)
    (error 'fresh-type-variable
           "partially constrained type variables can not include multiple of any composite type in the constraint list.  Given ~a\n"
           args))
  (when (<= 2 (length (filter function-type? args))) (composite-error))
  (when (<= 2 (length (filter product-type? args))) (composite-error))
  ;(when (<= 2 (length (filter sum-type? args))) (composite-error))
  (when (<= 2 (length (filter nominal-record-type? args))) (composite-error))
  ;(when (<= 2 (length (filter record-type? args))) (composite-error))
  ;; TODO - I probably only want to allow one of each kind of generic type
  (define type (if (null? args)
                   #f
                   (map (λ (x) (if (base-type? x)
                                   (base-type-range x x)
                                   x))
                        args)))
  (define handle-set (weak-seteq))
  (define tvi (type-variable-innard handle-set type #f '() '()))
  (define tv (type-variable tvi))
  (set-add! handle-set tv)
  tv)

#|
Base types can be declared as subtypes of other base types.
Inside a type variable, they are always placed in a base-type-range, which gives a minimum and maximum type.
The minimum may be #f to mean any subtype of the maximum type.
|#
(struct base-type (name supertype) #:transparent)

(define (base-type->parent-chain bt)
  (if (not (base-type-supertype bt))
      (list bt)
      (cons bt (base-type->parent-chain (base-type-supertype bt)))))

(define (mk-base-type name [parent #f])
  (base-type name parent))

(define (base-type->superest bt)
  (if (not (base-type-supertype bt))
      bt
      (base-type->superest (base-type-supertype bt))))

(define (base-type-least-upper-bound a b)
  (match (list a b)
    [(list #f _) b]
    [(list _ #f) a]
    [else
     (define chain-a (base-type->parent-chain a))
     (define chain-b (base-type->parent-chain b))
     (define len-a (length chain-a))
     (define len-b (length chain-b))
     (define chain-a* (if (< len-a len-b)
                          chain-a
                          (drop chain-a (- len-a len-b))))
     (define chain-b* (if (< len-b len-a)
                          chain-b
                          (drop chain-b (- len-b len-a))))
     (let loop ([as chain-a*]
                [bs chain-b*])
       (cond [(null? as) (error 'base-type-least-upper-bound
                                "incompatible base types: ~v and ~v" a b)]
             [(equal? as bs) (car as)]
             [else (loop (cdr as) (cdr bs))]))]))

(define (base-type-greatest-lower-bound a b)
  (match (list a b)
    [(list #f _) a]
    [(list _ #f) b]
    [else
     (define chain-a (base-type->parent-chain a))
     (define chain-b (base-type->parent-chain b))
     (define len-a (length chain-a))
     (define len-b (length chain-b))
     (define chain-a* (if (< len-a len-b)
                          chain-a
                          (drop chain-a (- len-a len-b))))
     (define chain-b* (if (< len-b len-a)
                          chain-b
                          (drop chain-b (- len-b len-a))))
     (if (equal? chain-a* chain-b*)
         (if (< len-a len-b)
             b
             a)
         (error 'base-type-greatest-lower-bound
                "incompatible base types: ~v and ~v" a b))]))

(struct base-type-range (sub super) #:transparent)
(define (base-type->range bt)
  (base-type-range #f bt))

(struct function-type (arg-type return-type) #:transparent)


;; Product types may have #f as the inner list to specify that the length and inner types are yet unconstrained.
#|
inner-type-list may be:
• #f for unconstrained
• a list of types (which may contain type variables)
upper-bounds and lower-bounds are lists of other product types that a given one has been subtype-unified with.  Once any product type among these has a list (instead of #f) for its inner type list, all the others will have a list of the same length created and filled with type variables.  Then they are all subtype-unified.
|#
(struct product-type
  ([inner-type-list #:mutable]
   [lower-bounds #:mutable]
   [upper-bounds #:mutable])
  #:transparent)
(define (mk-product-type inners)
  (if (not inners)
      (product-type #f '() '())
      (product-type inners '() '())))
(define (product-type-inner-type-list/resolve pt)
  (unbox* (product-type-inner-type-list pt)))
;(struct sum-type (inner-type-list) #:transparent)


#|
TODO - when generating a record ref, I'll need to compare something like (record-with-a-field-of-type t) to available record types.
|#
#;(struct record-type (name scope) #:transparent)
#;(define (mk-record-type #:name [name #f] name-type-dict)
  (record-type name (scope #f
                           (map (λ (k) (binding k
                                                #f
                                                (dict-ref name-type-dict k)
                                                'definition))
                                (dict-keys name-type-dict))
                           '())))

(struct nominal-record-type
  ;; If name is not #f, inners is an ordered dict of name to type.
  ;; If name is #f, then the type is variable and inners is still an ordered dict, but names may be #f to just specify that an int needs to be available.
  ;; By ordered dict, I specifically mean an alist.
  (name inners)
  #:mutable
  #:transparent)

(define (nominal-record-type-with field type)
  (nominal-record-type #f (hash field type)))
(define (any-nominal-record-type)
  (nominal-record-type #f (hash)))

(struct nominal-record-definition-type
  ;; This is a wrapper to be the type for the definition site of a nominal-record-type.
  ;; IE instances of a nominal record use the `inner` type, but nominal types need
  ;; definitions themselves.  This is the type for those definitions...
  (type)
  #:transparent)
(define (make-nominal-record-definition nominal-record-type)
  ;; TODO - this should be verified to be actually a nominal-record-type
  (nominal-record-definition-type nominal-record-type))


;; Generic types are given a name which is a symbol.  But it is just for printing.
;; They are compared with `eq?` on their constructor, which is bound to the name
;; by `define-generic-type`.
(struct generic-type (name constructor type-arguments) #:transparent)
(define-syntax (define-generic-type stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     (define field-length (length (syntax->list #'(field ...))))
     (with-syntax ([(accessor-name ...) (map (λ (x) (format-id #'name
                                                               "~a-~a"
                                                               #'name
                                                               x))
                                             (syntax->list #'(field ...)))]
                   [(accessor-index ...)
                    (map (λ (x) (datum->syntax
                                 #'name
                                 (- field-length
                                    (length
                                     (member x (syntax->list #'(field ...)))))))
                         (syntax->list #'(field ...)))]
                   [predicate-name (format-id #'name "~a?" #'name)]
                   [field-length-stx (datum->syntax #f field-length)])
       #'(begin
           (define (name field ...)
             (generic-type 'name name (list field ...)))
           (define (predicate-name x)
             (and (generic-type? x)
                  (eq? name (generic-type-constructor x))))
           (define (accessor-name x)
             (when (not (predicate-name x))
               (error 'accessor-name "not a ~a: ~a" 'predicate-name x))
             (list-ref (generic-type-type-arguments x) accessor-index))
           ...))]))

;; TODO - maybe I should have a base struct with no fields called type, then allow the user to define their own new types with custom rules for subtyping (at least to specify which fields are covariant, contravariant, or invariant) and for where to recur during unification.
(define (type? x)
  (or
   (type-variable? x)
   (base-type? x)
   (function-type? x)
   (product-type? x)
   ;(sum-type? x)
   (nominal-record-type? x)
   (nominal-record-definition-type? x)
   ;(record-type? x)
   (generic-type? x)
   ))

(define (unbox* b)
  (if (box? b)
      (unbox* (unbox b))
      b))
(define (unbox*- b)
  ;; unbox all but the LAST box
  (when (not (box? b))
    (error 'unbox*- "received not a box: ~a" b))
  (let ([ub (unbox b)])
    (if (box? ub)
        (unbox*- ub)
        b)))
(define (set-all-boxes! b target)
  ;; Set all nested boxes to the given target.
  (when (box? b)
    (let ([inner (unbox b)])
      (set-box! b target)
      (set-all-boxes! inner target))))

(module+ test
  (define b1 (box #f))
  (define-values (b1-chain b1-chain-list)
    (for/fold ([chain b1]
               [chain-list '()])
              ([i (in-range 5)])
      (define newbox (box chain))
      (values newbox (cons newbox chain-list))))
  (check-eq? (unbox*- b1-chain) b1)
  (define b2 (box 'foo))
  (set-all-boxes! b1-chain b2)
  (check-eq? (unbox b1) b2)
  (for ([b b1-chain-list])
    (check-eq? (unbox*- b) b2))
  (check-eq? (unbox* b1-chain) 'foo)
  )


#;(define (can-unify? t1 t2)
  (match (list t1 t2)
    [(list (type-variable (type-variable-innard _ #f)) _) #t]
    [(list (type-variable (type-variable-innard _ (list inner ...))) _)
     (for/or ([i inner])
       (can-unify? i t2))]
    [(list (type-variable (type-variable-innard _ inner)) _)
     (can-unify? inner t2)]
    [(list _ (type-variable _))
     (can-unify? t2 t1)]
    [(list (product-type inner1) (product-type inner2))
     (match (list inner1 inner2)
       [(list (? (compose not unbox*)) _) #t]
       [(list _ (? (compose not unbox*))) #t]
       [else (and (equal? (length (unbox* inner1)) (length (unbox* inner2)))
                  (for/and ([a (unbox* inner1)]
                            [b (unbox* inner2)])
                    (can-unify? a b)))])]
    [(list (nominal-record-type #f inners1) (nominal-record-type #f inners2))
     ;; For now, just be conservative to not need to change variable representation...
     #f]
    [(list (nominal-record-type #f inners1) (nominal-record-type name2 inners2))
     (define inner-vals (dict-values inners2))
     (for/and ([k (dict-keys inners1)])
       (cond [(not k) (member (dict-ref inners1 k) inner-vals)]
             [else (and (dict-has-key? inners2 k)
                        (can-unify? (dict-ref inners1 k) (dict-ref inners2 k)))]))]
    [(list (nominal-record-type name1 inners1) (nominal-record-type #f inners2))
     (can-unify? t2 t1)]
    [(list (nominal-record-type name1 inners1) (nominal-record-type name2 inners2))
     ;; TODO - verify that names are unique?
     (equal? name1 name2)]
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     (can-unify? inner1 inner2)]
    [else (can-or-do-unify-shared-code can-unify? t1 t2)]))

#;(define (unify! t1 t2)
  (define (fail) (error 'unify! "Can't unify types: ~a ~a" t1 t2))
  (define (unify-innard-with-single-type! innard t)
    (match innard
      [(type-variable-innard _ #f)
       (set-type-variable-innard-type! innard t)]
      [(type-variable-innard _ (list ts ...))
       (define unifier
         (or (for/or ([inner-t ts])
               (and (can-unify? inner-t t) inner-t))
             (fail)))
       (unify! unifier t)
       (set-type-variable-innard-type! innard unifier)]
      [(type-variable-innard _ inner-t)
       (unify! inner-t t)]))
  (match (list t1 t2)
    [(list (type-variable innard1) (type-variable innard2))
     (when (not (eq? innard1 innard2))
       (match (list innard1 innard2)
         [(list (type-variable-innard s1 c1) (type-variable-innard s2 c2))
          ;; We now make innard1 the true innard for both type variables,
          ;; giving it the union of the sets of type variable handles
          ;; and unifying the types of both.
          (match (list c1 c2)
            [(list #f _)
             (set-type-variable-innard-type! innard1 c2)]
            [(list _ #f) (void)]
            [(list (list ls ...) (list rs ...))
             (define intersection
               (filter (λ(x)x)
                       (for/list ([l ls])
                         (for/or ([r rs])
                           (and (can-unify? l r)
                                (unify! l r)
                                l)))))
             (when (null? intersection) (fail))
             (set-type-variable-innard-type! innard1 (if (null? (cdr intersection))
                                                         (car intersection)
                                                         intersection))]
            [(list (list ls ...) r)
             (unify-innard-with-single-type! innard1 r)]
            [(list l (list rs ...))
             (set-type-variable-innard-type! innard1 rs)
             (unify-innard-with-single-type! innard1 l)]
            [(list l r)
             (unify! l r)])
          (set-union! s1 s2)
          (for ([handle s2])
            (set-type-variable-tvi! handle innard1))]))]
    [(list (type-variable innard) _) (unify-innard-with-single-type! innard t2)]
    [(list _ (type-variable innard)) (unify-innard-with-single-type! innard t1)]
    ;;;; No more variables.  At least at the top level.
    [(list (product-type inner1) (product-type inner2))
     (match (list inner1 inner2)
       [(list #f #f)
        (define b (box #f))
        (set-product-type-inner-type-list! t1 b)
        (set-product-type-inner-type-list! t2 b)]
       [(list #f _) (set-product-type-inner-type-list! t1 inner2)]
       [(list _ #f) (set-product-type-inner-type-list! t2 inner1)]
       [(list (and b (box (? (compose not unbox*)))) _)
        ;; inner2 is either a box or a list.
        ;; Set the inner1 box so shared things are updated,
        ;; but also set the struct field to optimize pointer chasing.
        (set-all-boxes! b inner2)
        (set-product-type-inner-type-list! t1 inner2)]
       [(list _ (box (? (compose not unbox*)))) (unify! t2 t1)]
       ;; No more falses!
       [else
        (define l (unbox* inner1))
        (define r (unbox* inner2))
        (if (equal? (length l) (length r))
            (for ([a l]
                  [b r])
              (unify! a b))
            (fail))])]
    [(list (nominal-record-type #f inners1) (nominal-record-type name2 inners2))
     ;; TODO - do a sanity check that the inners match up and error if they don't.
     (set-nominal-record-type-name! t1 name2)
     (set-nominal-record-type-inners! t1 inners2)]
    [(list (nominal-record-type name1 inners1) (nominal-record-type #f inners2))
     ;; TODO - do a sanity check that the inners match up and error if they don't.
     (set-nominal-record-type-name! t2 name1)
     (set-nominal-record-type-inners! t2 inners1)]
    [(list (nominal-record-type name1 inners1) (nominal-record-type name2 inners2))
     (when (not (equal? name1 name2))
       (fail))]
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     (unify! inner1 inner2)]
    [else (let ([rec-result (can-or-do-unify-shared-code unify! t1 t2)])
            (unless rec-result (fail)))]))


#;(define (can-or-do-unify-shared-code rec t1 t2)
  ;; rec is either can-unify? or unify!, but to handle both this returns #t/#f
  ;; in cases where no recursion is necessary.
  (match (list t1 t2)
    [(list (base-type n1) (base-type n2)) (equal? n1 n2)]
    #;[(list (record-type n1 s1) (record-type n2 s2))
     ;; TODO - for now let's assume records start fully specified, but eventually I need to recur on the names and subtypes
     (equal? s1 s2)]
    [(list (generic-type name1 ctor1 inners1) (generic-type name2 ctor2 inners2))
     (and (equal? ctor1 ctor2)
          (andmap rec inners1 inners2))]
    [(list (function-type a1 r1) (function-type a2 r2))
     (and (rec a1 a2)
          (rec r1 r2))]
    [else #f]))

(define (type->skeleton-with-vars t)
  (match t
    [(generic-type name constructor inners)
     (apply constructor (map (λ(x) (fresh-type-variable))
                             inners))]
    [(? product-type?) (mk-product-type #f)]
    [(? nominal-record-type?) (nominal-record-type #f (hash))]
    [(? function-type?) (function-type (fresh-type-variable) (fresh-type-variable))]))

(define (base-type-ranges->unified-versions sub super)
  ;; returns a list of a new sub-range and a new super-range if compatible,
  ;; otherwise return #f
  (match (list sub super)
    [(list (base-type-range lsub lsup) (base-type-range rsub rsup))
     (and (can-subtype-unify? sub super)
          (let ()
            ;; lsup must not be higher than rsup -- IE the upper bound of the supertype is also an upper bound on the subtype.
            (define new-lsup (base-type-greatest-lower-bound lsup rsup))
            ;; rsub must not be lower than lsub -- IE the lower bound of the subtype is also a lower bound on the supertype.
            (define new-rsub (base-type-least-upper-bound lsub rsub))
            (define new-l (base-type-range lsub new-lsup))
            (define new-r (base-type-range new-rsub rsub))
            (list new-l new-r)))]
    [else #f]))
(define (type-lists->unified-base-types sub-list super-list)
  (define result
    (filter (λ(x)x)
            (for*/list ([sub sub-list]
                        [sup super-list])
              (base-type-ranges->unified-versions sub sup))))
  (define new-subs (map first result))
  (define new-sups (map second result))
  ;; TODO - filter out any that are sub-ranges within each of these lists
  (list new-subs new-sups))


(define (subtype-unify! sub super)
  #|
  * This sets variables to be in each others' upper- and lower-bounds.
  * As variables are unified, possibilities that don't fit with variables they are unified with (or concrete types they are unified with) are filtered out.
  * Unification transitively affects all upper and lower bounds of a variable.
  * Subtype-unified type variables form a lattice, and any time a lower bound becomes an upper bound (or vice-versa), the lattice between those two nodes is squashed to a single node.
  * recursion into inner type structures (function, generic, etc) will operate on type-specific meanings of subtyping -- generics will have a way of specifying per field whether the field is invariant (the default), covariant, or contravariant
  |#
  (match (list sub super)
    ;; type variable x2
    [(list (type-variable tvi-sub)
           (type-variable tvi-sup))

     ;; TODO - check that one is not recursively contained in the structure of the other.

     ;; Type variables may have any number of base-type-ranges as possibilities.
     ;; When subtype-unifying, each base-type-range pair is tried for unification.
     ;; All successes then replace the old base-type-ranges.
     ;; (Except the new ranges are tested against each other -- any range that fits entirely within another is eliminated.)

     (define tvi-sub-uppers (type-variable-innard->transitive-upper-bounds tvi-sub))
     (define tvi-sub-lowers (type-variable-innard->transitive-lower-bounds tvi-sub))
     (define already-done?
       (or (eq? tvi-sub tvi-sup)
           (member tvi-sup tvi-sub-uppers)))
     (define squash-case?
       ;; When a lower bound needs to become an upper bound, it means they need to be unified/squashed.
       (member tvi-sup tvi-sub-lowers))

     (cond
       [already-done? (void)]
       [squash-case?
        (define tvi-sup-lowers (type-variable-innard->transitive-lower-bounds tvi-sup))
        (define intersection
          (set-union (list tvi-sup tvi-sub)
                     (set-intersect tvi-sup-lowers tvi-sub-uppers)))
        (define new-handles (apply set-union
                                   ;; The innard handle sets are mutable sets,
                                   ;; and here I need immutable sets.
                                   (map (λ (x) (set->list
                                                (type-variable-innard-handle-set x)))
                                        intersection)))
        (match-define (list lower-change upper-change)
          (subtype-unify!/type-variable-innards tvi-sub tvi-sup))

        (define new-type
          (type-variable-innard-type tvi-sub))

        (define new-lowers
          (set-subtract (apply set-union
                               (map type-variable-innard-lower-bounds
                                    intersection))
                        intersection))
        (define new-uppers
          (set-subtract (apply set-union
                               (map type-variable-innard-upper-bounds
                                    intersection))
                        intersection))
        (define new-innard
          (type-variable-innard new-handles new-type #f new-lowers new-uppers))
        (for ([h new-handles])
          (set-type-variable-tvi! h new-innard))
        (for ([i intersection])
          (set-type-variable-innard-forward! i new-innard))
        (when (or lower-change upper-change)
          (ripple-subtype-unify-changes '() (list new-innard)))]
       [else
        (set-type-variable-innard-upper-bounds!
         tvi-sub
         (cons tvi-sup (type-variable-innard-upper-bounds tvi-sub)))
        (set-type-variable-innard-lower-bounds!
         tvi-sup
         (cons tvi-sub (type-variable-innard-lower-bounds tvi-sup)))

        (define dones (list (cons tvi-sub tvi-sup)))
        (match (subtype-unify!/type-variable-innards tvi-sub tvi-sup)
          [(list #f #f) (void)]
          [(list #f #t) (ripple-subtype-unify-changes dones
                                                      (list tvi-sup))]
          [(list #t #f) (ripple-subtype-unify-changes dones
                                                      (list tvi-sub))]
          [(list #t #t) (ripple-subtype-unify-changes dones
                                                      (list tvi-sub tvi-sup))])])]
    ;; type variable left
    [(list (type-variable tvi-sub)
           _)
     (define t (type-variable-innard-type tvi-sub))
     (match t
       [(list possibilities ...)
        (define new-possibilities (filter (λ (p) (can-subtype-unify? p super))
                                          possibilities))
        (set-type-variable-innard-type!
         tvi-sub
         (match new-possibilities
           [(list) (error 'subtype-unify!
                          "can't unify these types: ~v and ~v"
                          sub super)]
           [(list (? base-type-range?) ...)
            (define super-range (base-type-range super super))
            (define new-ranges
              (filter-map
               (λ (sub)
                 (define x (base-type-ranges->unified-versions sub super-range))
                 (and x (car x)))
               new-possibilities))
            (match new-ranges
              [(list) (error 'subtype-unify!
                             "can't unify types: ~v and ~v (this error hopefully is unreachable...)"
                             sub super)]
              [(list one) one]
              [else new-ranges])]
           [(list non-base)
            (subtype-unify! non-base super)
            non-base]))]
       [#f (match super
             [(? base-type?)
              (set-type-variable-innard-type! tvi-sub (base-type-range #f super))]
             [else
              (set-type-variable-innard-type! tvi-sub
                                              (type->skeleton-with-vars super))
              (subtype-unify! (type-variable-innard-type tvi-sub) super)])]
       [non-variable (subtype-unify! non-variable super)])

     (when (not (equal? t (type-variable-innard-type tvi-sub)))
       (ripple-subtype-unify-changes '() (list tvi-sub)))]

    ;; type variable right -- this code is basically the same as the above... maybe it could be unified better...
    [(list _
           (type-variable tvi-sup))

     (define t (type-variable-innard-type tvi-sup))
     (match t
       [(list possibilities ...)
        (define new-possibilities (filter (λ (p) (can-subtype-unify? sub p))
                                          possibilities))
        (set-type-variable-innard-type!
         tvi-sup
         (match new-possibilities
           [(list) (error 'subtype-unify!
                          "can't unify the following types: ~v and ~v"
                          sub super)]
           [(list (? base-type-range?) ...)
            (define sub-range (base-type-range sub sub))
            (define new-ranges
              (filter-map
               (λ (super)
                 (define x (base-type-ranges->unified-versions sub-range super))
                 (and x (car x)))
               new-possibilities))
            (match new-ranges
              [(list) (error 'subtype-unify!
                             "can't unify types: ~v and ~v (this error hopefully is unreachable...)"
                             sub super)]
              [(list one) one]
              [else new-ranges])]
           [(list non-base)
            (subtype-unify! sub non-base)
            non-base]))]
       [#f (match sub
             [(? base-type?)
              (set-type-variable-innard-type!
               tvi-sup
               (base-type-range sub (base-type->superest sub)))]
             [else
              (set-type-variable-innard-type!
               tvi-sup
               (type->skeleton-with-vars sub))
              (subtype-unify! sub (type-variable-innard-type tvi-sup))])]
       [non-variable (subtype-unify! sub non-variable)])

     (when (not (equal? t (type-variable-innard-type tvi-sup)))
       (ripple-subtype-unify-changes '() (list tvi-sup)))]


    ;; product type
    [(list (product-type inner1 lowers1 uppers1)
           (product-type inner2 lowers2 uppers2))

     (define (inner-unify! sub super)
       (for-each (λ (l r) (subtype-unify! l r))
                 (product-type-inner-type-list sub)
                 (product-type-inner-type-list super)))

     (define (ripple-length! len pt done-list)
       ;; Propagate length to all related product types.
       ;; At each step, unify the lists.
       ;; This basically initializes all of the inner lists of a graph
       ;; of product-types that had been subtype-unified to each other
       ;; with none of them having a concrete length yet.
       ;; Once they are initialized, the inner type variables can carry
       ;; all the info about subtype relations, and the outer product types
       ;; are free to be simple lists.
       (if (memq pt done-list)
           done-list
           (let ([supers (product-type-upper-bounds pt)]
                 [subs (product-type-lower-bounds pt)])
             (set-product-type-inner-type-list!
              pt
              (map (λ (x) (fresh-type-variable))
                   (make-list len #f)))
             (define done-list-1
               (for/fold ([done-list (cons pt done-list)])
                         ([super supers])
                 (define new-list (ripple-length! len super done-list))
                 (inner-unify! pt super)
                 new-list))
             (define done-list-2
               (for/fold ([done-list done-list-1])
                         ([sub subs])
                 (define new-list (ripple-length! len sub done-list))
                 (inner-unify! sub pt)
                 new-list))
             (set-product-type-upper-bounds! pt '())
             (set-product-type-lower-bounds! pt '())
             done-list-2)))

     (define l1 (and inner1 (length inner1)))
     (define l2 (and inner2 (length inner2)))

     (when (and l1 l2 (not (equal? l1 l2)))
       (error 'subtype-unify!
              "Tried to unify two product types with unequal lengths: ~v, ~v"
              sub super))

     (match (list inner1 inner2)
       [(list #f #f)
        (set-product-type-upper-bounds!
         sub
         (cons super (product-type-upper-bounds sub)))
        (set-product-type-lower-bounds!
         super
         (cons sub (product-type-lower-bounds super)))]
       [(list #f _)
        (ripple-length! l2 sub '())
        (inner-unify! sub super)]
       [(list _ #f)
        (ripple-length! l1 super '())
        (inner-unify! sub super)]
       [else (inner-unify! sub super)])]


    ;; nominal record type
    [(list (nominal-record-type name1 innards1)
           (nominal-record-type name2 innards2))
     ;; TODO - nominal record types for the first pass should not be subtypable.  It should be easy to later add a supertype field -- subtyping with nominal records should be easy compared to various other things.
     ;;(todo-code "If both are fully specified I can just check that they are equal, otherwise I need to check that the partial specification fits and mutate if the other is fully specified.")
     ;; TODO - the below is the implementation of non-subtype `unify!`.  For now, let's assume nominal records don't subtype unify, only normal unify.
     (define (fail)
       (error 'subtype-unify!
              "can't unify types: ~v and ~v"
              sub super))
     (define t1 sub)
     (define t2 super)

     (match (list sub super)
       [(list (nominal-record-type #f inners1) (nominal-record-type name2 inners2))
        ;; TODO - do a sanity check that the inners match up and error if they don't.
        (set-nominal-record-type-name! t1 name2)
        (set-nominal-record-type-inners! t1 inners2)]
       [(list (nominal-record-type name1 inners1) (nominal-record-type #f inners2))
        ;; TODO - do a sanity check that the inners match up and error if they don't.
        (set-nominal-record-type-name! t2 name1)
        (set-nominal-record-type-inners! t2 inners1)]
       [(list (nominal-record-type name1 inners1) (nominal-record-type name2 inners2))
        (when (not (equal? name1 name2))
          (fail))]
       [(list (nominal-record-definition-type inner1)
              (nominal-record-definition-type inner2))
        (unify! inner1 inner2)])
     ]


    ;; function type
    [(list (function-type arg-l ret-l)
           (function-type arg-r ret-r))
     ;; covariant return
     (subtype-unify! ret-l ret-r)
     ;; contravariant arguments
     (subtype-unify! arg-r arg-l)]
    ;; generic type
    [(list (generic-type name1 constructor1 type-arguments1)
           (generic-type name2 constructor2 type-arguments2))
     (unless (eq? constructor1 constructor2)
       (error 'subtype-unify!
              "TODO - better message -- tried to unify different generic types."))
     ;; TODO - generic types need to store the variance type for each field.
     ;;        For a start, let's assume all fields are invariant.
     (for-each (λ (isub isuper)
                 (subtype-unify! isub isuper)
                 (subtype-unify! isuper isub))
               type-arguments1
               type-arguments2)]

    ;; base type
    [(list (or (? base-type?) (? base-type-range?))
           (or (? base-type?) (? base-type-range?)))
     (unless (can-subtype-unify? sub super)
       (error 'subtype-unify!
              "Base types: type ~v is not a subtype of type ~v."
              sub super))]
    [else (error 'subtype-unify! "case analysis reached end: can't unify types: ~v and ~v" sub super)]))

(define (ripple-subtype-unify-changes done-pair-list innard-work-list)
  (define (done-pair-list-remove-with done-list target)
    ;; filter the done list to elements that don't include the target
    ;; TODO - this could be really slow since it will be done frequently.  If so, I should change the representation to be a pair of hash tables, maybe?
    (filter
     (λ (pair) (and (not (equal? (car pair) target))
                    (not (equal? (cdr pair) target))))
     done-list))
  (if (null? innard-work-list)
      (void)
      (let ([innard (innard->forward-resolve (car innard-work-list))]
            [innard-work-list (cdr innard-work-list)])
        (define (fold-body dones work lower upper)
          (cond
            [(member (cons lower upper) done-pair-list) (values dones work)]
            [else
             (match-define (list subchange superchange)
               (subtype-unify!/type-variable-innards lower upper))
             (define new-dones1 (if subchange
                                    (done-pair-list-remove-with dones lower)
                                    dones))
             (define new-dones2 (if superchange
                                    (done-pair-list-remove-with new-dones1 upper)
                                    dones))
             (define new-dones3 (cons (cons lower innard) new-dones2))
             (define new-work1 (if subchange
                                   (set-add lower work)
                                   work))
             (define new-work2 (if superchange
                                   (set-add upper work)
                                   work))
             (values new-dones3
                     new-work2)]))
        (define-values (dones1 work1)
          (for/fold ([dones done-pair-list]
                     [work innard-work-list])
                    ([lower (type-variable-innard-lower-bounds! innard)])
            (fold-body dones work lower innard)))
        (define-values (dones2 work2)
          (for/fold ([dones dones1]
                     [work work1])
                    ([upper (type-variable-innard-upper-bounds! innard)])
            (fold-body dones work innard upper)))
        (ripple-subtype-unify-changes dones2 work2))))

(define (subtype-unify!/type-variable-innards sub super)
  #|
  This is a helper function that takes only type-variable-innards, and only updates their type lists.  IE it does not add them to each others' upper/lower bounds lists.
  It returns a list of two bools.  The first one is true iff the sub list was modified, the second one is true iff the super list was modified.
  |#
  (match (list (flatten (list (type-variable-innard-type sub)))
               (flatten (list (type-variable-innard-type super))))
    [(list (list #f) (list #f)) (list #f #f)]
    [(list (list #f) r)
     (map (λ (t)
            (match t
              [(base-type-range low high) (base-type-range #f high)]
              [else
               (define inner-sub (type->skeleton-with-vars t))
               (subtype-unify! inner-sub t)
               inner-sub]))
          r)
     (list #t #f)]
    [(list l (list #f))
     (map (λ (t)
            (match t
              [(base-type-range low high)
               (base-type-range low (base-type->superest high))]
              [else
               (define inner-sup (type->skeleton-with-vars t))
               (subtype-unify! t inner-sup)
               inner-sup]))
          l)
     (list #f #t)]
    [(list subtypes supertypes)

     (match-define (list sub-bases super-bases)
       (type-lists->unified-base-types subtypes supertypes))

     (define compound-type-pairs
       (filter
        (λ(x)x)
        (for*/list ([sub (filter (λ(x) (not (base-type-range? x)))
                                 subtypes)]
                    [sup (filter (λ(x) (not (base-type-range? x)))
                                 supertypes)])
          (and (can-subtype-unify? sub sup)
               (subtype-unify! sub sup)
               (list sub sup)))))
     (define sub-compounds (map first compound-type-pairs))
     (define super-compounds (map second compound-type-pairs))

     (define (->use t-list)
       (match t-list
         [(list) (error 'subtype-unify!
                        "can't unify types ~v and ~v (this one shouldn't happen...)"
                        sub super)]
         [(list t) t]
         [(list ts ...) ts]))
     (define all-sub (->use (append sub-bases sub-compounds)))
     (define all-super (->use (append super-bases super-compounds)))

     (set-type-variable-innard-type! sub all-sub)
     (set-type-variable-innard-type! super all-super)

     (list (set-equal? subtypes all-sub)
           (set-equal? supertypes all-super))]))

(define (can-subtype-unify? sub super)
  (match (list sub super)
    ;; 2x type-variable
    [(list (type-variable tvi-sub)
           (type-variable tvi-sup))
     (match (list (flatten (list (type-variable-innard-type tvi-sub)))
                  (flatten (list (type-variable-innard-type tvi-sup))))
       [(list (list #f) _) #t]
       [(list _ (list #f)) #t]
       [(list subs sups)
        (for*/or ([sub subs]
                  [sup sups])
          (match (list sub sup)
            [(list (base-type-range #f lsup) (base-type-range _ rsup))
             (equal? (base-type->superest lsup)
                     (base-type->superest rsup))]
            [(list (base-type-range lsub _) (base-type-range _ rsup))
             (can-subtype-unify? lsub rsup)]
            [else (can-subtype-unify? sub sup)]))])]
    ;; left type-variable
    [(list (type-variable tvi-sub) _)
     (cond
       [(not (type-variable-innard-type tvi-sub)) #t]
       [else
        (define t (flatten (list (type-variable-innard-type tvi-sub))))
        (define (struct-rec predicate)
          (match (filter predicate t)
            [(list) #f]
            [(list one) (can-subtype-unify? one super)]))
        (match super
          [(base-type name superbase)
           (for/or ([possibility t])
             (match possibility
               [(base-type-range low high)
                (can-subtype-unify? possibility super)]
               [else #f]))]
          [(? function-type?) (struct-rec function-type?)]
          [(? product-type?) (struct-rec product-type?)]
          [(? nominal-record-type?) (struct-rec nominal-record-type?)]
          [(generic-type name constructor type-arguments)
           (define inner-matched
             (filter (λ (x) (match x
                              [(generic-type _ iconstructor _)
                               (eq? constructor iconstructor)]
                              [else #f]))
                     t))
           (match inner-matched
             [(list) #f]
             [(list one) (can-subtype-unify? one super)])])])]
    ;; right type-variable
    [(list _ (type-variable tvi-sup))
     (cond
       [(not (type-variable-innard-type tvi-sup)) #t]
       [else
        (define t (flatten (list (type-variable-innard-type tvi-sup))))
        (define (struct-rec predicate)
          (match (filter predicate t)
            [(list) #f]
            [(list one) (can-subtype-unify? sub one)]))
        (match sub
          [(base-type name superbase)
           (for/or ([possibility t])
             (match possibility
               [(base-type-range low high)
                (can-subtype-unify? sub possibility)]
               [else #f]))]
          [(? function-type?) (struct-rec function-type?)]
          [(? product-type?) (struct-rec product-type?)]
          [(? nominal-record-type?) (struct-rec nominal-record-type?)]
          [(generic-type name constructor type-arguments)
           (define inner-matched
             (filter (λ (x) (match x
                              [(generic-type _ iconstructor _)
                               (eq? constructor iconstructor)]
                              [else #f]))
                     t))
           (match inner-matched
             [(list) #f]
             [(list one) (can-subtype-unify? sub one)])])])]
    ;; function-type
    [(list (function-type arg-l ret-l)
           (function-type arg-r ret-r))
     (and
      ;; covariant return
      (can-subtype-unify? ret-l ret-r)
      ;; contravariant arguments
      (can-subtype-unify? arg-r arg-l))]
    ;; product-type
    [(list (product-type inner1 lowers1 uppers1)
           (product-type inner2 lowers2 uppers2))
     (cond
       [(not inner1) #t]
       [(not inner2) #t]
       [(not (equal? (length inner1) (length inner2))) #f]
       [else
        (for/and ([l inner1]
                  [r inner2])
          (can-subtype-unify? l r))])]
    ;; nominal-record-type
    [(list (? nominal-record-type?) (? nominal-record-type?))
     ;; TODO - this is just copied from symmetric can-unify
     (define t1 sub)
     (define t2 super)
     (match (list sub super)
       [(list (nominal-record-type #f inners1) (nominal-record-type #f inners2))
        ;; For now, just be conservative to not need to change variable representation...
        #f]
       [(list (nominal-record-type #f inners1) (nominal-record-type name2 inners2))
        (define inner-vals (dict-values inners2))
        (for/and ([k (dict-keys inners1)])
          (cond [(not k) (not (not (member (dict-ref inners1 k) inner-vals)))]
                [else (and (dict-has-key? inners2 k)
                           (can-unify? (dict-ref inners1 k) (dict-ref inners2 k)))]))]
       [(list (nominal-record-type name1 inners1) (nominal-record-type #f inners2))
        (can-unify? t2 t1)]
       [(list (nominal-record-type name1 inners1) (nominal-record-type name2 inners2))
        ;; TODO - verify that names are unique?
        (equal? name1 name2)]
       [(list (nominal-record-definition-type inner1)
              (nominal-record-definition-type inner2))
        (can-unify? inner1 inner2)])]
    ;; generic-type
    [(list (generic-type name1 constructor1 type-arguments1)
           (generic-type name2 constructor2 type-arguments2))
     ;; TODO - generic types need to store the variance type for each field.
     ;;        For a start, let's assume all fields are invariant.
     (and (eq? constructor1 constructor2)
          (for/and ([l type-arguments1]
                    [r type-arguments2])
            (and (can-subtype-unify? l r)
                 (can-subtype-unify? l r))))]
    ;; base-type
    [(list (base-type lname lsuper) (base-type rname rsuper))
     (not (not (member super (base-type->parent-chain sub))))]
    ;; While base-type-ranges can only be in type variables, it is convenient to recursively use this function to test them
    [(list (base-type-range l-low l-high) (base-type-range r-low r-high))
     (if l-low
         (not (not (member r-high (base-type->parent-chain l-low))))
         (and (equal? (base-type->superest l-high) (base-type->superest r-high))
              (or (equal? l-high r-high)
                  (not (member l-high (base-type->parent-chain r-high))))))]
    [(list (base-type _ _) (base-type-range _ _))
     (can-subtype-unify? (base-type-range sub sub) super)]
    [(list (base-type-range _ _) (base-type _ _))
     (can-subtype-unify? sub (base-type-range super super))]
    [else #f]))

(define (unify! t1 t2)
  (begin (subtype-unify! t1 t2)
         (subtype-unify! t2 t1)))
(define (can-unify? t1 t2)
  (and (can-subtype-unify? t1 t2)
       (can-subtype-unify? t2 t1)))

;; A parameter to hold the list of constructors for base or composite types (with minimally constrained type variables inside).
(define current-xsmith-type-constructor-thunks (make-parameter '()))
;; TODO - this should be configurable.
(define type-max-depth 5)
(define record-type-max-fields 5)

(define (concretize-type t)
  (define (recur t depth)
    (define (r t) (recur t (add1 depth)))
    (match t
      ;; TODO - type generation needs some kind of depth limit if composite types can contain composite types.
      [(type-variable
        (type-variable-innard _ (and maybe-options (or #f (list _ ...))) _ _ _))
       (define options (or maybe-options
                           (map (λ (x) (x))
                                (current-xsmith-type-constructor-thunks))))
       (define options-filtered
         (if (< depth type-max-depth)
             options
             (filter base-type? options)))
       (define options-use (if (null? options-filtered) options options-filtered))
       (when (null? options-use)
         (error 'concretize-type "Received an empty list for options.  Was current-xsmith-type-constructor-thunks parameterized?"))
       ;; TODO - fresh-type-variable is invalid here as it causes infinite recursion.
       (when (not (empty? (filter (λ (tv) (and (type-variable? tv)
                                               (equal? #f (type-variable-innard-type (type-variable-tvi tv)))))
                                  options-use)))
         (error 'concretize-type (format "Received a typeless type variable in options.  Don't use (fresh-type-variable) when parameterizing current-xsmith-type-constructor-thunks." options-use)))
       (r (random-ref options-use))]
      [(type-variable (type-variable-innard _ non-list-type _ _ _))
       (r non-list-type)]
      [(base-type _ _) t]
      [(base-type-range low high)
       ;; TODO - this should be a random choice.  But I also need to deal with the #f low case and enumerate all possibilities.  For now I just want to get the code working again.
       high]
      [(product-type inner lb ub)
       (define inner-types (unbox* inner))
       (if inner-types
           (mk-product-type (map r inner-types))
           (mk-product-type (map (λ (x) (r (fresh-type-variable)))
                                 (make-list (random 6) #f))))]
      [(nominal-record-type #f inner-needed)
       (define needed (dict-ref inner-needed #f (λ () (fresh-type-variable))))
       (define n-random-fields (random record-type-max-fields))
       (define field-list (cons needed
                                (map (λ (x) (fresh-type-variable))
                                     (make-list n-random-fields #f))))
       (concretize-type
        (nominal-record-type (fresh-var-name "record_")
                             (for/list ([f field-list])
                               (cons (fresh-var-name "field_")
                                     (r f)))))]
      [(nominal-record-type name inners) t]
      [(nominal-record-definition-type inner)
       (nominal-record-definition-type (r inner))]
      [(function-type arg return) (function-type (r arg)
                                                 (r return))]
      [(generic-type name ctor inners) (generic-type name ctor (map r inners))]
      [else (error 'concretize-type "internal error -- no case for type: ~a" t)]))
  (recur t 0))

(module+ test
  (define integer (mk-base-type 'integer))
  (define float (mk-base-type 'float))
  (define string (mk-base-type 'string))

  (check-true (can-unify? integer integer))
  (check-false (can-unify? integer float))

  (define int-int->int (function-type (mk-product-type (list integer integer))
                                      integer))
  (define int->int (function-type (mk-product-type (list integer)) integer))
  (define str->int (function-type (mk-product-type (list string)) integer))

  (check-true (can-unify? int->int int->int))
  (check-false (can-unify? int->int int-int->int))
  (check-false (can-unify? int->int str->int))
  (check-not-exn (λ () (unify! int->int int->int)))
  (check-exn exn? (λ () (unify! int->int str->int)))



  (define (or-int-str) (fresh-type-variable integer string))

  ;; Partially constrained type variables can not include variables in the constraint.
  (check-exn exn? (λ () (fresh-type-variable (fresh-type-variable integer))))
  ;; Though it can include composite types that include variables.
  (check-not-exn (λ () (fresh-type-variable
                        (function-type (mk-product-type (list (or-int-str)))
                                       (or-int-str)))))
  ;; But not more than one of a given composite.
  (check-exn exn? (λ () (fresh-type-variable
                         (function-type (mk-product-type (list (or-int-str)))
                                        (or-int-str))
                         int->int)))

  (define t1 (or-int-str))
  (check-true (can-unify? integer t1))
  (check-true (can-unify? string t1))
  (check-not-exn (λ () (unify! integer t1)))
  (check-false (can-unify? string t1))
  (check-exn exn? (λ () (unify! string t1)))

  (define (or1->int) (function-type (mk-product-type (list (or-int-str))) integer))
  (define (or2->int) (function-type (mk-product-type #f) integer))
  (define an-or1->int (or1->int))
  (define an-or2->int (or2->int))
  (define an-or3->int (fresh-type-variable))
  (check-true (can-unify? an-or1->int an-or2->int))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or1->int str->int))
  (check-false (can-unify? an-or1->int int-int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or2->int str->int))
  (check-true (can-unify? an-or2->int int-int->int))
  (check-true (can-unify? an-or3->int int->int))
  (check-true (can-unify? an-or3->int str->int))
  (check-true (can-unify? an-or3->int int-int->int))
  (check-not-exn (λ () (unify! an-or1->int an-or3->int)))
  (check-true (can-unify? an-or3->int int->int))
  (check-true (can-unify? an-or3->int str->int))
  (check-false (can-unify? an-or3->int int-int->int))
  (check-not-exn (λ () (unify! an-or1->int an-or2->int)))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or1->int str->int))
  (check-true (can-unify? an-or2->int str->int))
  (check-false (can-unify? an-or1->int int-int->int))
  (check-false (can-unify? an-or2->int int-int->int))
  (check-true (can-unify? an-or1->int an-or2->int))
  (check-not-exn (λ () (unify! an-or2->int int->int)))
  ;; Unifying 2 also affected 1 and 3
  (check-false (can-unify? an-or1->int str->int))
  (check-false (can-unify? an-or2->int str->int))
  (check-false (can-unify? an-or3->int str->int))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or3->int int->int))
  )


(define (concrete? t)
  (match t
    [(type-variable (type-variable-innard _ it _ _ _))
     (and it (not (list? it)) (concrete? it))]
    [(base-type _ _) #t]
    [(base-type-range l r) (equal? l r)]
    [(function-type a r)
     (and (concrete? a) (concrete? r))]
    [(nominal-record-type name inners)
     ;; If a name is set then it's concrete.
     (not (not name))]
    [(nominal-record-definition-type inner) (concrete? inner)]
    [(product-type itl lb ub)
     (define itl* (unbox* itl))
     (and (list? itl*) (andmap concrete? itl*))]
    [(generic-type _ _ inners)
     (andmap concrete? inners)]))

(define (at-least-as-concrete v constraint-type)
  ;; TODO - this function was broken even before adding subtyping.  Now it is probably more so.  I need to re-examine how it is used to fix it.  But it is there just as an optimization that maybe isn't entirely necessary, so I'm just not going to worry about it for now.
  #|
  Returns #t when:
  • they are different types
  • any case in a type variable of v is available in constraint-type
  This is a helper to tell how much of the tree needs to be walked to decide
  whether it is meaningful to test `can-unify?` when choosing a production
  to create.  A type may unify with a cousin node's type, constraining the
  nodes we can choose at a given point.  But we don't want to traverse more
  of the tree than we need to.
  |#
  (match (list v constraint-type)
    [(list _ (type-variable c-innard))
     (match c-innard
       [(type-variable-innard _ #f _ _ _) #t]
       [(type-variable-innard _ (list cs ...) _ _ _)
        (match v
          [(type-variable (type-variable-innard _ #f _ _ _)) #f]
          [(type-variable (type-variable-innard _ (list ts ...) _ _ _))
           (or
            ;; check if every case in ts is covered in cs
            (for/and ([t ts])
              (match t
                [(base-type _ _) (for/or ([c (filter base-type? cs)])
                                   ;; TODO - this may have been broken by switching to subtypes...  I'm not sure which direction needs to be the subtype, and it may change...
                                   ;; The original code checked names, this was silly
                                   ;(equal? name (base-type-name c))
                                   ;; I've now added a subtype-unify check... but is it the right direction?
                                   ;(TODO-code -- check that this subtype check is correct)
                                   (can-subtype-unify? t c)
                                   )]
                [(? function-type?)
                 (ormap (λ (c) (at-least-as-concrete t c))
                        (filter function-type? cs))]
                [(? product-type?)
                 (ormap (λ (c) (at-least-as-concrete t c))
                        (filter product-type? cs))]
                [(? generic-type?)
                 (ormap (λ (c) (at-least-as-concrete t c))
                        (filter (λ (c) (and (generic-type? c)
                                            (eq? (generic-type-constructor c)
                                                 (generic-type-constructor t))))
                                cs))]))
            ;; check if they have nothing in common
            (for/and ([c cs])
              (match c
                [(base-type cn _) (for/and ([t (filter base-type? ts)])
                                    ;(TODO-code -- this probably needs to be a subtype check, but it is probably bi-directional...)
                                    (not (equal? cn (base-type-name t))))]
                [(? function-type?)
                 (null? (filter function-type? ts))]
                [(? product-type?)
                 (null? (filter product-type? ts))]
                [(? generic-type?)
                 (null? (filter generic-type? ts))])))]
          [(type-variable (type-variable-innard _ t _ _ _))
           (for/and ([c cs]) (at-least-as-concrete t c))]
          [else (for/and ([c cs]) (at-least-as-concrete v c))])]
       [(type-variable-innard _ c _ _ _) (at-least-as-concrete v c)])]
    [(list (type-variable innard) _)
     (match innard
       [(type-variable-innard _ #f _ _ _) #f]
       [(type-variable-innard _ (list t ...) _ _ _) #f]
       [(type-variable-innard _ t _ _ _) (at-least-as-concrete t constraint-type)])]
    ;; No more variables
    [(list (base-type _ _) _) #t]
    [(list (function-type v-arg v-ret) (function-type c-arg c-ret))
     (and (at-least-as-concrete v-arg c-arg)
          (at-least-as-concrete v-ret c-ret))]
    [(list (product-type v-inner-list _ _) (product-type c-inner-list _ _))
     (let ([ts (unbox* v-inner-list)]
           [cs (unbox* c-inner-list)])
       (match (list ts cs)
         [(list _ #f) #t]
         [(list #f _) #f]
         [else (if (equal? (length ts) (length cs))
                   (andmap at-least-as-concrete ts cs)
                   #t)]))]
    ;[(list (sum-type aoeu) (sum-type aoeu))]
    ;[(list (record-type aoeu) (record-type aoeu))]
    [(list (nominal-record-type v-name v-inners)
           (nominal-record-type c-name c-inners))
     ;; For now be conservative.
     (not (not v-name))]
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     (at-least-as-concrete inner1 inner2)]
    [(list (generic-type v-n v-ctor v-inners) (generic-type c-n c-ctor c-inners))
     (if (eq? v-ctor c-ctor)
         (andmap at-least-as-concrete v-inners c-inners)
         #t)]
    [else #t]))

(module+ test
  (check-true (at-least-as-concrete (fresh-type-variable) (fresh-type-variable)))
  (check-true (at-least-as-concrete (mk-base-type 'foo) (fresh-type-variable)))
  (check-true (at-least-as-concrete (mk-base-type 'foo)
                                    (fresh-type-variable (mk-base-type 'foo)
                                                         (mk-base-type 'bar))))
  (check-true (at-least-as-concrete (mk-base-type 'foo)
                                    (fresh-type-variable (mk-base-type 'foo)
                                                         (mk-base-type 'bar))))
  (check-false (at-least-as-concrete (fresh-type-variable)
                                     (fresh-type-variable (mk-base-type 'foo)
                                                          (mk-base-type 'bar))))
  (check-false (at-least-as-concrete (function-type (fresh-type-variable)
                                                    (mk-base-type 'foo))
                                     (function-type (mk-base-type 'bar)
                                                    (mk-base-type 'foo))))
  ;; TODO - this test raises an exception, but at-least-as-concrete is not even used right now
  #;(check-true (at-least-as-concrete (fresh-type-variable (mk-product-type #f))
                                    (fresh-type-variable (mk-base-type 'foo)
                                                         (mk-base-type 'bar))))
  (check-false (at-least-as-concrete (fresh-type-variable (mk-product-type #f))
                                     (fresh-type-variable
                                      (mk-product-type (list (fresh-type-variable)))
                                      (mk-base-type 'bar))))
  )


;;; True if any of the variables is anywhere in the type.
(define (contains-type-variables? t vs)
  ;; TODO - subtyping -- does the meaning of this need to change in any way to account for variables that are not in a type variable directly but are in an upper/lower bound of the variable?
  ;; Here "type variables" can be type variables or product-type-inners boxes
  (define innards
    (flatten (map (λ (x) (cond [(type-variable? x) (type-variable-tvi x)]
                               [else x]))
                  vs)))
  (contains-type-variable-innards? t innards))
(define (contains-type-variable-innards? t innards)
  (define (rec t) (contains-type-variable-innards? t innards))
  (match t
    [(base-type _ _) #f]
    [(base-type-range _ _) #f]
    [(function-type arg ret) (or (rec arg) (rec ret))]
    [(product-type inners lb ub)
     (match (unbox* inners)
       [#f (memq t innards)]
       [(list ts ...) (ormap rec ts)])]
    ;[(sum-type)]
    ;[(record-type)]
    [(nominal-record-type name inners)
     (ormap rec (dict-values inners))]
    [(nominal-record-definition-type inner) (rec inner)]
    [(generic-type name constructor inners)
     (ormap rec inners)]
    [(type-variable t-innard)
     (or (memq t-innard innards)
         (match t-innard
           [(type-variable-innard _ #f _ _ _) #f]
           [(type-variable-innard _ (list ts ...) _ _ _) (ormap rec ts)]
           [(type-variable-innard _ inner-t _ _ _) (rec inner-t)]))]))

;;; Returns a list of every type variable contained in a type.
;; TODO - subtyping -- do I need to worry about upper/lower bounds containing a type?
(define (type->type-variable-list t)
  (define (rec t)
    (match t
      [(base-type _ _) '()]
      [(base-type-range _ _) '()]
      [(function-type arg ret) (append (rec arg) (rec ret))]
      [(product-type inners lb ub)
       (if inners
           (flatten (map rec inners))
           (cons t (append (product-type-upper-bounds t)
                           (product-type-lower-bounds t))))]
      ;[(sum-type)]
      ;[(record-type)]
      [(nominal-record-type name inners) (flatten (map rec (dict-values inners)))]
      [(nominal-record-definition-type inner) (rec inner)]
      [(generic-type name constructor inners) (flatten (map rec inners))]
      [(type-variable innard)
       (match innard
         [(type-variable-innard _ (list its ...) _ _ _)
          (cons t (flatten (map rec its)))]
         [(type-variable-innard _ #f _ _ _) (list t)]
         [(type-variable-innard _ it _ _ _) (cons t (rec it))])]))
  (remove-duplicates (map type-variable->canonical-type-variable (rec t))
                     eq?))

(define (type-variable->canonical-type-variable tv)
  (match tv
    [(type-variable (type-variable-innard handles _ _ _ _)) (set-first handles)]
    [else tv]))

(module+ test
  (define v1 (fresh-type-variable))
  (define v2 (fresh-type-variable))
  (define v3 (fresh-type-variable (mk-base-type 'foo)
                                  (function-type v1 v2)))
  (define p1 (mk-product-type #f))
  (define p2 (mk-product-type #f))
  (define p3 (mk-product-type #f))
  (define v4 (fresh-type-variable (mk-base-type 'bar)
                                  (function-type p1 v3)))

  (check-not-false (contains-type-variables? v1 (type->type-variable-list v4)))
  (check-not-false (contains-type-variables? v1 (type->type-variable-list v3)))
  (check-false (contains-type-variables? v1 (type->type-variable-list v2)))

  (define (s= l r)
    (set=? (apply seteq l)
           (apply seteq r)))
  (check-true
   (s= (type->type-variable-list v3)
       (list v1 v2 v3)))
  (check-true
   (s= (type->type-variable-list v4)
       (flatten (list v1 v2 v3 v4 (type->type-variable-list p1)))))
  (unify! v1 v2)
  (check-true
   (s= (type->type-variable-list v1)
       (type->type-variable-list v2)))
  (check-true
   (s= (type->type-variable-list v3)
       (map type-variable->canonical-type-variable (list v1 v3))))
  (check-not-false (contains-type-variables? v1 (type->type-variable-list v2)))

  (unify! p1 p2)
  (check-not-false (contains-type-variables? v4 (list v1)))
  (check-not-false (contains-type-variables? v4 (type->type-variable-list p1)))
  (check-not-false (contains-type-variables? v4 (type->type-variable-list p2)))
  (check-false (contains-type-variables? v4 (type->type-variable-list p3)))
  (unify! p2 (mk-product-type (list v1 v2 v3)))
  (check-true
   (s= (type->type-variable-list p1)
       (map type-variable->canonical-type-variable (list v1 v3))))

  (unify! p3 p1)
  (check-true (s= (type->type-variable-list p1)
                  (type->type-variable-list p2)))
  (check-true (s= (type->type-variable-list p1)
                  (type->type-variable-list p3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
