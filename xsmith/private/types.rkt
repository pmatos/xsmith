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
Type variable innards contain a set of handles (type variables with that innard)
and either:
• #f - unconstrained
• a non-variable type (though it may contain type variables)
• a list - constrained to be one of the types in the list
A type variable list may not contain type variables and may not contain more than one of each compound type.  Eg. it may contain any number of base types but only one function type or record type.  However, the function or record type contained my be only partially specified (eg. may contain type variables).
|#
(struct type-variable ([tvi #:mutable])
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (define innard (type-variable-tvi v))
     (match v
       [(type-variable (type-variable-innard _ t))
        (fprintf output-port
                 "#<type-variable ~a>"
                 t)]))])
(struct type-variable-innard ([handle-set #:mutable] [type #:mutable]))

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
  (when (<= 2 (length (filter sum-type? args))) (composite-error))
  (when (<= 2 (length (filter record-type? args))) (composite-error))
  ;; TODO - I probably only want to allow one of each kind of generic type
  (define type (if (null? args) #f args))
  (define handle-set (weak-seteq))
  (define tvi (type-variable-innard handle-set type))
  (define tv (type-variable tvi))
  (set-add! handle-set tv)
  tv)

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
    [else (can-or-do-unify-shared-code can-unify? t1 t2)]))

(define (unify! t1 t2)
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
          (set-union! s1 s2)
          (for ([handle s2])
            (set-type-variable-tvi! handle innard1))
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
             (unify! l r)])]))]
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
      [(type-variable
        (type-variable-innard _ (and maybe-options (or #f (list _ ...)))))
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
      [(type-variable (type-variable-innard _ non-list-type)) (r non-list-type)]
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


(define (at-least-as-concrete v constraint-type)
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
       [(type-variable-innard _ #f) #t]
       [(type-variable-innard _ (list cs ...))
        (match v
          [(type-variable (type-variable-innard _ #f)) #f]
          [(type-variable (type-variable-innard _ (list ts ...)))
           (for/and ([t ts])
             (for/or ([c cs])
               (match (list t c)
                 [(list (base-type x) (base-type y)) (equal? x y)]
                 [(list (? function-type?) (? function-type?))
                  (at-least-as-concrete t c)]
                 [(list (? product-type?) (? product-type?))
                  (at-least-as-concrete t c)]
                 [(list (? sum-type?) (? sum-type?))
                  (at-least-as-concrete t c)]
                 [(list (? record-type?) (? record-type?))
                  (at-least-as-concrete t c)]
                 [(list (? generic-type?) (? generic-type?))
                  (at-least-as-concrete t c)]
                 [else #f])))]
          [(type-variable (type-variable-innard _ t))
           (for/and ([c cs]) (at-least-as-concrete t c))]
          [else (for/and ([c cs]) (at-least-as-concrete v c))])]
       [(type-variable-innard _ c) (at-least-as-concrete v c)])]
    [(list (type-variable innard) _)
     (match innard
       [(type-variable-innard _ #f) #f]
       [(type-variable-innard _ (list t ...)) #f]
       [(type-variable-innard _ t) (at-least-as-concrete t constraint-type)])]
    ;; No more variables
    [(list (base-type _) (base-type _)) #t]
    [(list (function-type v-arg v-ret) (function-type c-arg c-ret))
     (and (at-least-as-concrete v-arg c-arg)
          (at-least-as-concrete v-ret c-ret))]
    [(list (product-type v-inner-list) (product-type c-inner-list))
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
    [(list (generic-type v-name v-inners) (generic-type c-name c-inners))
     (if (equal? v-name c-name)
         (andmap at-least-as-concrete v-inners c-inners)
         #t)]
    [else #t]))

(module+ test
  (check-true (at-least-as-concrete (fresh-type-variable) (fresh-type-variable)))
  (check-true (at-least-as-concrete (base-type 'foo) (fresh-type-variable)))
  (check-true (at-least-as-concrete (base-type 'foo)
                                    (fresh-type-variable (base-type 'foo)
                                                         (base-type 'bar))))
  (check-true (at-least-as-concrete (base-type 'foo)
                                    (fresh-type-variable (base-type 'foo)
                                                         (base-type 'bar))))
  (check-false (at-least-as-concrete (fresh-type-variable)
                                     (fresh-type-variable (base-type 'foo)
                                                          (base-type 'bar))))
  (check-false (at-least-as-concrete (function-type (fresh-type-variable)
                                                    (base-type 'foo))
                                     (function-type (base-type 'bar)
                                                    (base-type 'foo))))
  )


;;; True if any of the variables is anywhere in the type.
(define (contains-type-variables? t vs)
  (define innards (map type-variable-tvi vs))
  (contains-type-variable-innards? t innards))
(define (contains-type-variable-innards? t innards)
  (define (rec t) (contains-type-variable-innards? t innards))
  (match t
    [(base-type _) #f]
    [(function-type arg ret) (or (rec arg) (rec ret))]
    [(product-type inners)
     (match (unbox* inners)
       [#f #f]
       [(list ts ...) (ormap rec ts)])]
    ;[(sum-type)]
    ;[(record-type)]
    [(generic-type name inners)
     (ormap rec inners)]
    [(type-variable t-innard)
     (or (member t-innard innards)
         (match t-innard
           [(type-variable-innard _ #f) #f]
           [(type-variable-innard _ (list ts ...)) (ormap rec ts)]
           [(type-variable-innard _ inner-t) (rec inner-t)]))]))

;;; Returns a list of every type variable contained in a type.
(define (type->type-variable-list t)
  (define (rec t)
    (match t
      [(base-type _) '()]
      [(function-type arg ret) (append (rec arg) (rec ret))]
      [(product-type inners)
       (match (unbox* inners)
         [#f '()]
         [(list ts ...) (flatten (map rec ts))])]
      ;[(sum-type)]
      ;[(record-type)]
      [(generic-type name inners) (flatten (map rec inners))]
      [(type-variable innard)
       (match innard
         [(type-variable-innard _ (list its ...))
          (cons t (flatten (map rec its)))]
         [(type-variable-innard _ #f) (list t)]
         [(type-variable-innard _ it) (cons t (rec it))])]))
  (remove-duplicates (map type-variable->canonical-type-variable (rec t))))

(define (type-variable->canonical-type-variable tv)
  (match tv
    [(type-variable (type-variable-innard handles _)) (set-first handles)]))

(module+ test
  (define v1 (fresh-type-variable))
  (define v2 (fresh-type-variable))
  (define v3 (fresh-type-variable (base-type 'foo)
                                  (function-type v1 v2)))
  (check-true
   (set=? (type->type-variable-list v3)
          (list v1 v2 v3)))
  (unify! v1 v2)
  (check-true
   (or (set=? (type->type-variable-list v3)
              (list v3 v2))
       (set=? (type->type-variable-list v3)
              (list v3 v1))))

  )