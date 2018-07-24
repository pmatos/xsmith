#lang racket/base

(provide
 (all-defined-out)
 (all-from-out "../xsmith-utils.rkt")
 )

(require
 "../xsmith-utils.rkt"
 racr
 pprint
 racket/random
 racket/dict
 racket/match
 racket/math
 (prefix-in rt: rosette)
 (except-in racket/list empty)
 "../scope-graph.rkt"
 "../xsmith-options.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))


(define page-width      80)
(define nest-step       4)
(define nest-step-string (text (make-string nest-step #\space)))
(define lbrace          (char #\{))
(define rbrace          (char #\}))
(define lparen          (char #\())
(define rparen          (char #\)))
(define comma           (char #\,))
(define semi            (char #\;))
(define plus            (char #\+))
(define minus           (char #\-))
(define star            (char #\*))
(define slash           (char #\/))
(define percent         (char #\%))
(define eqsign          (char #\=))
(define greater         (char #\>))
(define less            (char #\<))
(define qmark           (char #\?))
(define colon           (char #\:))
(define comment-start   (text "/*"))
(define comment-end     (text "*/"))

(define do:             (text "do"))
(define else:           (text "else"))
(define for:            (text "for"))
(define if:             (text "if"))
(define return:         (text "return"))
(define while:          (text "while"))

(define (comment d)
  (if (eq? d empty)
      empty
      (hs-append comment-start d comment-end)))

#;(define (v-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            (group (h-append d line))
            (group (h-append (comment pre) line d line)))
        (if (eq? pre empty)
            (group (h-append d line (comment post) line))
            (group (h-append (comment pre) line d line (comment post) line))))))

(define (v-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            d
            (group (h-append (comment pre) line d)))
        (if (eq? pre empty)
            (group (h-append d line (comment post)))
            (group (h-append (comment pre) line d line (comment post)))))))

(define (h-comment n d)
  (let ((pre (ast-child 'precomment n))
        (post (ast-child 'postcomment n)))
    (if (eq? post empty)
        (if (eq? pre empty)
            d
            (hs-append (comment pre) d))
        (if (eq? pre empty)
            (hs-append d (comment post))
            (hs-append (comment pre) d (comment post))))))

(define (nest-if-not-block n)
  (if (equal? (node-type n) 'Block)
      (att-value 'pretty-print n)
      (nest nest-step (h-append nest-step-string
                                (att-value 'pretty-print n)))))


(define ident (λ(x)x))

(struct hint
  (weight-multiplier)
  #:transparent)

(define bool-hint (hint 8))
(define block-hint (hint 50))
(define assignment-hint (hint 70))
(define application-hint (hint 50))

#|
TYPES
-----

Types can be:
* #f for completely unconstrained
* (list '-> arg ... result) for function types
* basic-type for normal types
|#

(struct basic-type
  ;; Type name can be false or a type (eg int, float, ...),
  ;; constraints fields are lists.
  ;; Constraints are things like nonzero, constant, etc -- things that aren't part of the type, but that affect things like undefined behavior.
  ;; The constrain-type method should always account for every attribute in the list -- if something can't satisfy every attribute it should be out of the running.
  (name constraints)
  #:transparent)

(define (type-satisfies? given-t constraint-t)
  (match constraint-t
    ;; TODO - arrow types
    ;;        But I don't think I'm ever comparing two arrow types directly,
    ;;        just comparing return values and argument types...
    [(basic-type cname cconst)
     (match given-t
       [(basic-type gname gconst)
        (and (or (not cname) (equal? gname cname))
             (andmap (λ (c) (member c gconst)) cconst))]
       [else #f])]
    [(list-rest '-> c-args+ret)
     (match given-t
       [(list-rest '-> g-args+ret)
        (and (equal? (length c-args+ret) (length g-args+ret))
             (map type-satisfies? g-args+ret c-args+ret))]
       [else #f])]
    ;; if there is no constraint, anything goes
    [#f #t]))

(define (function-type? t)
  (and (list? t)
       (not (null? t))
       (eq? (car t) '->)))

(define empty-basic-type (basic-type #f (list)))
(define (specify-type t name)
  (cond [(not t) (basic-type name (list))]
        [(basic-type? t) (struct-copy basic-type t
                                      [name name])]
        [else (error 'specify-type "bad case")]))
(define (constrain-type t constraint)
  (cond [(not t) (basic-type #f (list constraint))]
        [(basic-type? t)
         (struct-copy basic-type t
                      [constraints (cons constraint (basic-type-constraints t))])]
        [else (error 'constrain-type "bad case")]))

(define int-type (specify-type empty-basic-type "int"))
(define (int-type? x) (and (basic-type? x) (equal? (basic-type-name x) "int")))
(define float-type (specify-type empty-basic-type "float"))
(define (float-type? x) (and (basic-type? x) (equal? (basic-type-name x) "float")))
(define nonzero-type (constrain-type empty-basic-type 'nonzero))
(define nonzero-int-type (constrain-type int-type 'nonzero))
(define nonzero-float-type (constrain-type float-type 'nonzero))

(define (print-debug-highlight pretty-print-node)
  ;; add terminal codes to print in magenta, then switch to default
  (h-append (text "\033[35m")
            pretty-print-node
            (text "\033[0m")))

;; TODO - I need one of these for each int type, when there are multiple
;; TODO - I should also use the actual values that these will take...
(define INT_MIN -10001)
(define INT_MAX 10000)

(struct abstract-value/range
  (low high)
  #:transparent)
(define abstract-value/range/top (abstract-value/range -inf.0 +inf.0))
(define range-store-top (hash))
(define (nan->+inf v)
  (if (nan? v) +inf.0 v))
(define (nan->-inf v)
  (if (nan? v) -inf.0 v))

(define (abstract-value-merge/range a b)
  (match-let ([(abstract-value/range a-l a-h) a]
              [(abstract-value/range b-l b-h) b])
    (abstract-value/range (min a-l b-l) (max a-h b-h))))
(define ({abstract-store-merge value-merger key->top-value} a b)
  (for/hash ([key (remove-duplicates (append (dict-keys a) (dict-keys b)))])
    (let ([top (key->top-value key)])
      (values key (value-merger (dict-ref a key top)
                                (dict-ref b key top))))))

(struct abstract-flow-control-return
  #|
  * maybes is a list of potential returns (corresponding to places a return
    statement may or may not execute)
  * must is a single return (corresponding to a place where a return always executes)
  |#
  (maybes must) #:transparent)
(define empty-abstract-flow-control-return (abstract-flow-control-return '() #f))
(define ({abstract-flow-control-return->val-store-list val-merge* store-merge*} r)
  (match-let* ([(abstract-flow-control-return maybes must) r]
               [(list (list v s) ...) (if must (cons must maybes) maybes)])
    (list (apply val-merge* v) (apply store-merge* s))))


(define (maybe-return returns val store)
  (match returns
    [(abstract-flow-control-return maybes must)
     (abstract-flow-control-return (cons (list val store) maybes) must)]))
(define (must-return returns val store)
  (match returns
    [(abstract-flow-control-return maybes must)
     (abstract-flow-control-return maybes (list val store))]))

(define (abstract-flow-control-return-merge a b)
  (match a
    [(abstract-flow-control-return a-maybes a-must)
     (match b
       [(abstract-flow-control-return b-maybes b-must)
        (if (and a-must b-must)
            (abstract-flow-control-return (append (list a-must) a-maybes b-maybes)
                                          b-must)
            (abstract-flow-control-return (append a-maybes b-maybes)
                                          (or a-must b-must)))])]))

(define (abstract-flow-control-return-only-maybe-ify r)
  (match r
    [(abstract-flow-control-return maybes must)
     (if must
         (abstract-flow-control-return (cons must maybes) #f)
         r)]))

(define ({merge-*-ify merge-func} . args)
  (cond [(empty? args) (error 'merge-*-ify "this shouldn't happen -- merge-*-ify client got no arguments")]
        [else (foldl merge-func (first args) (rest args))]))

(define {abstract-store-merge* value-merger key->top-value}
  {merge-*-ify {abstract-store-merge value-merger key->top-value}})

(define abstract-store-merge*/range
  {abstract-store-merge* abstract-value-merge/range
                         (λ (k) abstract-value/range/top)})
(define abstract-value-merge*/range {merge-*-ify abstract-value-merge/range})
(define abstract-flow-control-return-merge*
  {merge-*-ify abstract-flow-control-return-merge})

(define abstract-flow-control-return->val-store-list/range
  {abstract-flow-control-return->val-store-list
   abstract-value-merge*/range abstract-store-merge*/range})


(define current-abstract-interp-call-stack (make-parameter '()))

;;; wrapper for the ag-rule to prevent function cycles
(define (abstract-interp-wrap do-function
                              store-member-to-top-func
                              get-result-hash-func
                              make-top-return-for-recursion)
  (λ (n store . rest)
    (let* ([result
            (if (member (ast-node-type n) '(FunctionApplicationExpression
                                            FunctionDefinition))
                (let ([name (ast-child 'name n)])
                  (if (member name (current-abstract-interp-call-stack))
                      (let* ([assignments
                              (att-value 'find-transitive-assignments
                                         (if (node-subtype? n 'FunctionDefinition)
                                             n
                                             (let ([ref (resolve-variable-reference-node n)])
                                               (dict-ref (binding-bound ref) 'declaration-node))))]
                             [new-store (for/fold ([store store])
                                                  ([a assignments])
                                          (store-member-to-top-func store a))])
                        (apply make-top-return-for-recursion n store rest))
                      (parameterize ([current-abstract-interp-call-stack
                                      (cons name (current-abstract-interp-call-stack))])
                        (apply do-function n store rest))))
                (apply do-function n store rest))]
           [result-hash (get-result-hash-func n)])
      (hash-set! result-hash n (cons result (hash-ref result-hash n '())))
      result)))

(define abstract-interp-wrap/range
  (abstract-interp-wrap (λ (n store flow-returns)
                          (att-value 'abstract-interp-do/range n store flow-returns))
                        (λ (store key)
                          (dict-set store key abstract-value/range/top))
                        (λ (n)
                          (att-value 'abstract-interp-result-hash/range n))
                        (λ (n store flow-returns)
                          (list abstract-value/range/top store flow-returns))))
(define symbolic-interp-wrap
  (abstract-interp-wrap (λ (n store path-condition return-variable assertions)
                          (att-value 'symbolic-interp-do
                                     n store path-condition return-variable assertions))
                        (λ (store key)
                          (define t (dict-ref (binding-bound key) 'type))
                          (dict-set store key (fresh-symbolic-var t)))
                        (λ (n)
                          (att-value 'symbolic-interp-result-hash n))
                        (λ (n store path-condition return-variable assertions)
                          (define node-type
                            (dict-ref (binding-bound
                                       (resolve-variable-reference-node n))
                                      'type))
                          (define ret-type (first (reverse node-type)))
                          (define v (fresh-symbolic-var ret-type))
                          (list v store #f assertions))))

(define (fresh-symbolic-var type)
  (define type-pred (cond [(int-type? type) rt:integer?]
                          [(float-type? type) rt:real?]
                          [(member type (list rt:boolean? rt:real? rt:integer?)) type]
                          [else (error 'fresh-symbolic-var "can't handle type: ~a" type)]))
  (rt:define-symbolic* var type-pred)
  var)
(define symbolic-store-top (hash))

(define-syntax (values->list stx)
  (syntax-parse stx
    [(_ e ...)
     #'(call-with-values
        (λ () e ...)
        list)]))

(define (resolve-variable-reference-node n)
  (resolve-reference
   (reference (ast-child 'name n)
              (att-value 'scope-graph-scope n))))




(define ({binary-expression-print/infix op-sym} n)
  (h-comment
   n
   (h-append lparen
             (hs-append (att-value 'pretty-print (ast-child 'l n))
                        op-sym
                        (att-value 'pretty-print (ast-child 'r n)))
             rparen)))
(define ({binary-expression-print/function type->f-name} n)
  (h-comment
   n
   (h-append (type->f-name (att-value 'type-context n))
             lparen
             (hs-append (att-value 'pretty-print (ast-child 'l n))
                        comma
                        (att-value 'pretty-print (ast-child 'r n)))
             rparen)))

(define-syntax (def-type->print stx)
  (syntax-parse stx
    [(_ printer-name int-name float-name)
     #'(define (printer-name t)
         (cond [(int-type? t) (text int-name)]
               [(float-type? t) (text float-name)]
               [else (error 'printer-name "bad type")]))]))

(def-type->print type->print-add "safe_add_func_int32_t_s_s" "safe_add_func_float_f_f")
(def-type->print type->print-sub "safe_sub_func_int32_t_s_s" "safe_sub_func_float_f_f")
(def-type->print type->print-mul "safe_mul_func_int32_t_s_s" "safe_mul_func_float_f_f")
(def-type->print type->print-div "safe_div_func_int32_t_s_s" "safe_div_func_float_f_f")
(def-type->print type->print-mod "safe_mod_func_int32_t_s_s" "safe_mod_func_float_f_f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fresh-var-name [base "var_"])
  (let ((n (generator-state-fresh-name-counter (xsmith-state))))
    (set-generator-state-fresh-name-counter! (xsmith-state) (add1 n))
    (format "~a~a" base n)))
(define (fresh-var-type)
  (let ((disabled (xsmith-option 'features-disabled)))
    ;; XXX Obviously, the code below is not quite right.
    ;; What is both float and int are disabled?
    (cond [(dict-ref disabled 'float #f)
           int-type]
          [(dict-ref disabled 'int #f)
           float-type]
          [else
           (random-ref (list int-type float-type))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

