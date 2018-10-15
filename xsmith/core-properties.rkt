#lang racket/base

(provide
 may-be-generated
 depth-increase
 choice-weight
 fresh
 wont-over-deepen
 introduces-scope
 binder-info
 lift-predicate
 binding-structure
 choice-filters-to-apply
 )

(require
 "grammar-macros.rkt"
 "xsmith-utils.rkt"
 "xsmith-options.rkt"
 "scope-graph.rkt"
 racr
 racket/class
 racket/dict
 racket/list
 racket/match
 (for-syntax
  racket/base
  syntax/parse
  racket/dict
  racket/list
  ))

(define-non-inheriting-rule-property
  may-be-generated
  choice-rule
  #:rule-name xsmith_may-be-generated
  #:default #t
  #:transformer (syntax-parser [#t #'(λ () this)]
                               [#f #'(λ () #f)]))

(define-non-inheriting-rule-property
  depth-increase
  ag-rule
  #:rule-name ast-depth
  #:default (λ (n) 1)
  #:transformer (syntax-parser
                  [inc:expr
                   #'(λ (n)
                       (define increment (inc n))
                       (define parent-depth
                         ;; TODO - liftdepth should be xsmithliftdepth
                         (cond [(and (ast-has-child? 'liftdepth n)
                                     (number? (ast-child 'liftdepth n)))
                                (ast-child 'liftdepth n)]
                               [(ast-has-parent? n)
                                (att-value 'ast-depth (parent-node n))]
                               [else 0]))
                       (+ increment parent-depth))]))

(define-property choice-weight
  #:appends (choice-rule xsmith_choice-weight)
  #:transformer
  (λ (this-prop-info)
    (list
     (for/hash ([node-name (dict-keys this-prop-info)])
       (values node-name
               #`(λ () #,(dict-ref this-prop-info node-name)))))))

#|
The fresh property will take an expression (to be the body of a method
-- so `this` can be used to access the current choice method) that
must return a list of field specifications.

Each field specification is a dict mapping field names (as symbols) to values.

All other fields will receive the default value (by evaluating the
default value expression specified in the grammar), #f if no default
is specified and no type is known for the field, or an appropriate
hole for the type.
|#
(define-property fresh
  #:reads (grammar)
  #:appends (choice-rule xsmith_fresh)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))
    ;; I need to create a lambda (of zero args) that evaluates the given expression (if it exists), then calls a thunk to get the default value for any fields not specified in the list received.
    (define rule-info
      (for/hash ([node nodes])
        (define fields (dict-ref field-info-hash node))
        (define field-hash (for/hash ([field fields])
                             (values (grammar-node-field-struct-name field)
                                     field)))
        (define field-names (map grammar-node-field-struct-name fields))
        (define field-types (map grammar-node-field-struct-type fields))
        (define field-seq?s (map grammar-node-field-struct-kleene-star? fields))
        (define (sym->quoted-sym-stx s)
          #`(quote #,(datum->syntax #'here s)))
        (define prop-for-this-node
          (syntax->list (dict-ref this-prop-info node #'(hash))))
        (with-syntax ([fresh-expr prop-for-this-node]
                      [node-name node]
                      [(field-name ...) (map sym->quoted-sym-stx field-names)]
                      [(field-type ...) (map sym->quoted-sym-stx field-types)]
                      [(field-seq? ...) (map sym->quoted-sym-stx field-seq?s)])
          (values
           node
           #`(λ ([field-dict (hash)])
               (define thunk-hash
                 (hash
                  #,@(flatten
                      (map
                       (λ (fname)
                         (list
                          #`(quote #,fname)
                          #`(λ ()
                              #,(let* ([fstruct (dict-ref field-hash
                                                          fname)]
                                       [init-e (grammar-node-field-struct-init-expr
                                                fstruct)]
                                       [f-type (grammar-node-field-struct-type
                                                fstruct)]
                                       [seq? (grammar-node-field-struct-kleene-star?
                                              fstruct)])
                                  (cond
                                    [init-e init-e]
                                    [seq? #'(create-ast-list (list))]
                                    [f-type #`(make-hole
                                               '#,(datum->syntax #'here f-type))]
                                    [else #'#f])))))
                       field-names))))
               (define prop-given-values fresh-expr)
               (define all-values-hash
                 (for/hash ([f-name (list field-name ...)])
                   (values
                    f-name
                    (dict-ref field-dict
                              f-name
                              (dict-ref prop-given-values
                                        f-name
                                        ((dict-ref thunk-hash f-name)))))))
               (define all-values-hash/num-transformed
                 (for/hash ([f-name (list field-name ...)]
                            [f-type (list field-type ...)]
                            [f-seq? (list field-seq? ...)])
                   (values
                    f-name
                    (let ([v (dict-ref all-values-hash f-name)])
                      (if (and f-seq? (number? v))
                          ;; If the init value is a number and a list
                          ;; of hole nodes is required, make an appropriate
                          ;; list of that length.
                          (expr->ast-list v (and f-type (make-hole f-type)))
                          v)))))
               (define all-values-in-order
                 (map (λ (name) (dict-ref all-values-hash/num-transformed name))
                      (list field-name ...)))

               (create-ast (current-racr-spec)
                           '#,node
                           all-values-in-order))))))
    (list rule-info)))

(define-property wont-over-deepen
  #:reads (grammar)
  #:appends (choice-rule xsmith_wont-over-deepen)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))
    ;; If a node in the grammar has fields that are also nodes, it will make
    ;; the tree deeper.
    (define rule-info-defaults
      (for/hash ([node nodes])
        (values node
                (if (ormap (λ (x) (grammar-node-field-struct-type x))
                           (dict-ref field-info-hash node))
                    #'#f
                    #'#t))))
    ;; But we'll let the user override if they want.
    (define rule-info
      (for/hash ([node nodes])
        (values node
                #`(λ ()
                    (or (<= (att-value 'ast-depth current-hole)
                            (xsmith-option 'max-depth))
                        (current-force-deepen)
                        #,(dict-ref
                           this-prop-info
                           node
                           (dict-ref rule-info-defaults node)))))))
    (list rule-info)))


#|
Helper function for xsmith_scope-graph-child-scope-dict.
* cb-pairs is a list of (cons child-node binding), where binding is #f or a binding struct.
* parent scope is the scope that the parent node is in.
* serial/parallel/recursive-flag is a symbol
|#
(define (make-child-scope-dict cb-pairs parent-scope serial/parallel/recursive-flag)
  (define cb-no-bindings (filter (λ (cb) (not (cdr cb))) cb-pairs))
  (define cb-with-bindings (filter (λ (cb) (cdr cb)) cb-pairs))
  (match serial/parallel/recursive-flag
    ['serial
     (define-values (scope-for-non-binding-children
                     child-dict-with-binders)
       (for/fold ([incoming-scope parent-scope]
                  [child-dict (hash)])
                 ([cb-pair cb-with-bindings])
         (define new-scope (scope incoming-scope (list (cdr cb-pair)) '()))
         (values new-scope
                 (dict-set child-dict (car cb-pair) incoming-scope))))
     (for/fold ([child-dict child-dict-with-binders])
               ([cb-pair cb-no-bindings])
       (dict-set child-dict
                 (car cb-pair)
                 scope-for-non-binding-children))]
    ['parallel
     (define new-scope (scope parent-scope (map cdr cb-with-bindings) '()))
     (for/hash ([cb cb-pairs])
       (if (cdr cb)
           (values (car cb) parent-scope)
           (values (car cb) new-scope)))]
    ['recursive
     (define new-scope (scope parent-scope (map cdr cb-with-bindings) '()))
     (for/hash ([child (map car cb-pairs)])
       (values child new-scope))]))

#|
TODO - This property now takes NO arguments and isn't even checked!
       But it does a bunch of stuff by reading other properties...

The introduces-scope property generates RACR attributes for resolving bindings via scope graphs.
The scope-graph-descendant-bindings attribute returns a list of all bindings on descendant nodes that are not under a different scope.  In other words, you call it on a node that introduces a scope and it returns all bindings within that scope.  It does not return bindings in child scopes.
The scope-graph-scope attribute returns the scope that the node in question resides in.  For nodes that introduce a scope, it is their own.
The scope-graph-introduces-scope? predicate attribute is just used to know when to stop for the scope-graph-descendant-bindings attribute.
|#
(define-property introduces-scope
  #:reads
  (grammar)
  (property binder-info)
  (property lift-predicate)
  (property binding-structure)
  #:appends
  ;; TODO - I don't think introduces-scope? is used anywhere anymore...  should it be removed?
  (ag-rule xsmith_scope-graph-introduces-scope?)
  (ag-rule xsmith_scope-graph-child-scope-dict)
  (ag-rule xsmith_scope-graph-scope)
  (ag-rule xsmith_lift-predicate)
  #:transformer
  (λ (this-prop-info
      grammar-info
      binder-info-info
      lift-predicate-info
      binding-structure-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))
    (define (binder-or-supertype? node-name)
      (define n (if (syntax? node-name) (syntax->datum node-name) node-name))
      (syntax-parse (dict-ref binder-info-info n #'#f)
        ;; TODO - reading other nodes still a list...
        [((name-field-name type-method-name)) #t]
        ;; TODO - This should detect supertypes, but for now I'm not...
        [else-stx #f]))

    (define has-binder-child-hash
      (for/hash ([node nodes])
        (values node
                (for/or ([f (dict-ref field-info-hash node)])
                  (binder-or-supertype? (grammar-node-field-struct-type f))))))
    (define possible-lift-destination-hash
      (for/hash ([node nodes])
        (values node
                (and (dict-ref has-binder-child-hash node)
                     (for/or ([f (dict-ref field-info-hash node)])
                       (and (binder-or-supertype? (grammar-node-field-struct-type f))
                            (grammar-node-field-struct-kleene-star? f)))))))

    (define binding-structure-hash
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref binding-structure-info node #'(#f))
                  #:literals (quote)
                  ;; TODO - because this is not in the transformer of the
                  ;; original property I have to check duplicates by hand...
                  [(a b) (raise-syntax-error 'xsmith
                                             "duplicate property declaration"
                                             #'b)]
                  [((quote (~and flag:id (~or (~datum serial)
                                              (~datum parallel)
                                              (~datum recursive)))))
                   #'flag]
                  [(#f) #''serial]))))
    (define xsmith_lift-predicate-info
      (for/hash ([node nodes])
        (values node
                (or
                 (and (dict-ref possible-lift-destination-hash node)
                      (syntax-parse (dict-ref lift-predicate-info node #'(#t))
                        ;; TODO - manual duplicate checking because this in not
                        ;; in the original property's transformer...
                        [(a b) (raise-syntax-error 'xsmith
                                                   "duplicate property declaration"
                                                   #'b)]
                        [(#t) #'(λ (n type) #t)]
                        [(#f) #'(λ (n type) #f)]
                        [(predicate) #'predicate]))
                 #'(λ (n type) #f)))))

    (define scope-graph-introduces-scope?-info
      (for/fold ([rule-info (hash #f #'(λ (n) #f))])
                ([node nodes])
        (if (dict-ref has-binder-child-hash node)
            (dict-set rule-info node #'(λ (n) #t))
            rule-info)))

    (define scope-graph-scope-child-dict-info
      (for/fold ([rule-info (hash #f #'(λ (n)
                                         ;; If a node does not introduce a scope,
                                         ;; it just passes through its own.
                                         (define children (ast-children/flat n))
                                         (define scope
                                           (att-value 'xsmith_scope-graph-scope n))
                                         (for/hash ([c children])
                                           (values c scope))))])
                ([node nodes])
        (define binding-structure-for-node (dict-ref binding-structure-hash node))
        (if (dict-ref has-binder-child-hash node)
            rule-info
            (dict-set
             rule-info
             node
             #`(λ (n)
                 (define children (filter ast-node? (ast-children/flat n)))
                 (define children-bindings
                   (map (λ (c) (att-value 'xsmith_scope-graph-binding c))
                        children))
                 (define cb-pairs (map cons children children-bindings))
                 (define parent-scope
                   (att-value 'xsmith_scope-graph-scope n))
                 (make-child-scope-dict cb-pairs
                                        parent-scope
                                        #,binding-structure-for-node))))))
    (define scope-graph-scope-info
      (hash #f
            #'(λ (n) (if (ast-has-parent? n)
                         (let ([parent-dict (att-value
                                             'xsmith_scope-graph-child-scope-dict
                                             (parent-node n))])
                           (dict-ref parent-dict n))
                         ;; dummy program parent scope to simplify child-dict lookup
                         (scope #f '() '())))))

    (list scope-graph-introduces-scope?-info
          scope-graph-scope-child-dict-info
          scope-graph-scope-info
          xsmith_lift-predicate-info)))

(define-property binder-info
  #:reads (grammar)
  #:appends (ag-rule xsmith_scope-graph-binding)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define scope-graph-binding-info
      (for/fold ([rule-info (hash #f #'(λ (n) #f))])
                ([node nodes])
        (syntax-parse (dict-ref this-prop-info node #'#f)
          [#f rule-info]
          [(name-field-name:id type-method-name:id)
           (dict-set rule-info node
                     #'(λ (n)
                         (binding
                          (ast-child 'name-field-name n)
                          n
                          (att-value 'type-method-name n))))])))
    (list scope-graph-binding-info)))

;; These are declared separately, but are handled by the transformer of
;; the `introduces-scope` property.
(define-property lift-predicate)
(define-property binding-structure)

(define-property choice-filters-to-apply
  #:appends (choice-rule xsmith_apply-choice-filters)
  #:transformer
  (λ (this-prop-info)

    (define-syntax-class filtering-method
      (pattern method-name:id
               #:attr func #'(λ (o) (send o method-name)))
      (pattern (method-name:id arg:expr ...)
               #:attr func #'(λ (o) (send o method-name arg ...))))
    (define (get-filters node-name)
      (let ([user-filters (dict-ref this-prop-info node-name #'())])
        ;; Add user-specified filters to the core filters.
        #`(xsmith_may-be-generated
           xsmith_wont-over-deepen
           #,@user-filters)))
    (define (helper filter-method-stx filter-failure-set!-id)
      (syntax-parse filter-method-stx
        [() #'this]
        [(filt1:filtering-method filt:filtering-method ...)
         #`(let ([result (filt1.func this)])
             (if result
                 #,(helper #'(filt ...)
                           filter-failure-set!-id)
                 (begin
                   (set! #,filter-failure-set!-id 'filt1.method-name)
                   #f)))]))
    (define rule-info
      (for/hash ([node-name (dict-keys this-prop-info)])
        (values
         node-name
         (with-syntax ([failure-set!-id #'failed-on])
           #`(λ ()
               (define failure-set!-id #f)
               (define result #,(helper (get-filters node-name)
                                        #'failure-set!-id))
               (if result
                   this
                   (format "Choice ~a: filtered out by ~a method."
                           this%
                           failure-set!-id)))))))
    (list rule-info)))
