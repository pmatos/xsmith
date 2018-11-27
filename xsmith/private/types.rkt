#lang racket/base
(require racket/contract)
(provide
 type-variable?
 (struct-out base-type)
 ;(struct-out alias-type)
 ;; TODO - tuple types -- what should the API be?
 (rename-out [mk-record-type record-type])
 product-type
 product-type?
 product-type-inner-type-list
 record-type?
 record-type-name
 (struct-out generic-type)

 type?

 (contract-out
  [fresh-type-variable (->* () () #:rest (listof type?) type?)]
  [unify! (-> type? type? any/c)]
  [can-unify? (-> type? type? any/c)]
  [concretize-type (-> type? type?)]
  [function-type (-> type? type? type?)]
  )
 function-type?
 function-type-arg-type
 function-type-return-type

 current-xsmith-type-constructor-thunks
 )

(require
 racket/match
 racket/random
 racket/dict
 racket/list
 "../scope-graph.rkt"
 )
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
Type variables contain either:
• #f - unconstrained
• another type variable - constrained to be the same as the other variable
• a list - constrained to be one of the types in the list
A type variable list may not contain type variables and may not contain more than one of each compound type.  Eg. it may contain any number of base types but only one function type or record type.  However, the function or record type contained my be only partially specified (eg. may contain type variables).
|#
(struct type-variable ([type #:mutable]) #:transparent)
(define (fresh-type-variable . args)
  (if (null? args)
      (type-variable #f)
      (let ()
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
        (when (<= 2 (length (filter sum-type? args))) (composite-error))
        (when (<= 2 (length (filter record-type? args))) (composite-error))
        ;; TODO - I probably only want to allow one of each kind of generic type
        (andmap type? args)
        (type-variable args))))

(struct base-type (name) #:transparent)
(struct function-type (arg-type return-type) #:transparent)

;; TODO - I think something like this should exist, but I haven't thought about what to do with it.
(struct alias-type (name inner-type) #:transparent)

;; Product types may have #f as the inner list to specify that the length and inner types are yet unconstrained.
#|
inner-type-list may be:
• #f to signify that even the length is unconstrained
• a list of types (which may contain type variables)
• a box which contains an inner-type-list (so #f product types can be unified without forcing a length.)
|#
(struct product-type ([inner-type-list #:mutable]) #:transparent)
(struct sum-type (inner-type-list) #:transparent)


#|
TODO - when generating a record ref, I'll need to compare something like (record-with-a-field-of-type t) to available record types.
|#
(struct record-type (name scope) #:transparent)
(define (mk-record-type #:name [name #f] name-type-dict)
  (record-type name (scope #f
                           (map (λ (k) (binding k #f (dict-ref name-type-dict k)))
                                (dict-keys name-type-dict))
                           '())))

(struct generic-type (name type-arguments) #:transparent)

;; TODO - maybe I should have a base struct with no fields called type, then allow the user to define their own new types with custom rules for subtyping (at least to specify which fields are covariant, contravariant, or invariant) and for where to recur during unification.
(define (type? x)
  (or
   (type-variable? x)
   (base-type? x)
   (function-type? x)
   (product-type? x)
   (sum-type? x)
   (record-type? x)
   (generic-type? x)
   ))

(define (unbox* b)
  (if (box? b)
      (unbox* (unbox b))
      b))
(define (set-all-boxes! b target)
  ;; Set all nested boxes to the given target.
  (when (box? b)
    (let ([inner (unbox b)])
      (set-box! b target)
      (set-all-boxes! inner target))))

(define (can-unify? t1 t2)
  (match (list t1 t2)
    [(list (type-variable #f) _) #t]
    [(list (type-variable (list inner ...)) _)
     (for/or ([i inner])
       (can-unify? i t2))]
    [(list (type-variable inner) _)
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
    [else (can-or-do-unify-shared-code can-unify? t1 t2)]))

(define (unify! t1 t2)
  (define (fail) (error 'unify! "Can't unify types: ~a ~a" t1 t2))
  (match (list t1 t2)
    [(list (type-variable #f) _) (set-type-variable-type! t1 t2)]
    [(list _ (type-variable #f)) (set-type-variable-type! t2 t1)]
    [(list (type-variable (and inner (type-variable inner-inner))) _)
     (unify! inner t2)]
    [(list _ (type-variable (and inner (type-variable inner-inner))))
     (unify! t1 inner)]
    [(list (type-variable (list inner1 ...)) (type-variable (list inner2 ...)))
     (define intersection
       (filter (λ(x)x)
               (for/list ([l inner1])
                 (for/or ([r inner2])
                   (and (can-unify? l r)
                        (unify! l r)
                        l)))))
     (when (null? intersection) (fail))
     (set-type-variable-type! t1 (if (null? (cdr intersection))
                                     (car intersection)
                                     intersection))
     (set-type-variable-type! t2 t1)]
    [(list (type-variable (list inner ...)) _)
     ;; t2 can not be a type variable here.
     (define unifier
       (or (for/or ([i inner])
             (and (can-unify? i t2) i))
           (fail)))
     (unify! unifier t2)
     (set-type-variable-type! t1 unifier)]
    [(list (type-variable non-variable) _)
     (unify! non-variable t2)]
    [(list _ (type-variable _))
     ;; Handle any other type variable cases by swapping left and right terms.
     (unify! t2 t1)]
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
    [else (let ([rec-result (can-or-do-unify-shared-code unify! t1 t2)])
            (unless rec-result (fail)))]))

(define (can-or-do-unify-shared-code rec t1 t2)
  ;; rec is either can-unify? or unify!, but to handle both this returns #t/#f
  ;; in cases where no recursion is necessary.
  (match (list t1 t2)
    [(list (base-type n1) (base-type n2)) (equal? n1 n2)]
    [(list (record-type n1 s1) (record-type n2 s2))
     ;; TODO - for now let's assume records start fully specified, but eventually I need to recur on the names and subtypes
     (equal? s1 s2)]
    [(list (generic-type name1 inners1) (generic-type name2 inners2))
     (and (equal? name1 name2)
          ;; This is redundant...
          (equal? (length inners1) (length inners2))
          (andmap rec inners1 inners2))]
    [(list (function-type a1 r1) (function-type a2 r2))
     (and (rec a1 a2)
          (rec r1 r2))]
    [else #f]))

;; A parameter to hold the list of constructors for base or composite types (with minimally constrained type variables inside).
(define current-xsmith-type-constructor-thunks (make-parameter '()))
;; TODO - this should be configurable.
(define type-max-depth 5)

(define (concretize-type t)
  (define (recur t depth)
    (define (r t) (recur t (add1 depth)))
    (match t
      ;; TODO - type generation needs some kind of depth limit if composite types can contain composite types.
      [(type-variable (and maybe-options (or #f (list _ ...))))
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
       (r (random-ref options-use))]
      [(type-variable inner) (r inner)]
      [(base-type _) t]
      [(product-type inner)
       (define inner-types (unbox* inner))
       (if inner-types
           (product-type (map r inner-types))
           (product-type (map (λ (x) (r (fresh-type-variable)))
                              (make-list (random 6) #f))))]
      [(record-type n1 s1) (error 'concretize-type "not yet implemented for records")]
      [(function-type arg return) (function-type (r arg)
                                                 (r return))]
      [(generic-type name inners) (generic-type name (map r inners))]
      [else (error 'concretize-type "internal error -- no case for type: ~a" t)]))
  (recur t 0))


(module+ test
  (define integer (base-type 'integer))
  (define float (base-type 'float))
  (define string (base-type 'string))

  (check-true (can-unify? integer integer))
  (check-false (can-unify? integer float))

  (define int-int->int (function-type (product-type (list integer integer)) integer))
  (define int->int (function-type (product-type (list integer)) integer))
  (define str->int (function-type (product-type (list string)) integer))

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
                        (function-type (product-type (list (or-int-str)))
                                       (or-int-str)))))
  ;; But not more than one of a given composite.
  (check-exn exn? (λ () (fresh-type-variable
                         (function-type (product-type (list (or-int-str)))
                                        (or-int-str))
                         int->int)))

  (define t1 (or-int-str))
  (check-true (can-unify? integer t1))
  (check-true (can-unify? string t1))
  (check-not-exn (λ () (unify! integer t1)))
  (check-false (can-unify? string t1))
  (check-exn exn? (λ () (unify! string t1)))

  (define (or1->int) (function-type (product-type (list (or-int-str))) integer))
  (define (or2->int) (function-type (product-type #f) integer))
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

