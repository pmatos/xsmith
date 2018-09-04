#lang racket/base

(provide
 may-be-generated
 depth-increase
 choice-weight
 fresh
 wont-over-deepen
 introduces-scope
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
The introduces-scope property generates RACR attributes for resolving bindings via scope graphs.
The scope-graph-descendant-bindings attribute returns a list of all bindings on descendant nodes that are not under a different scope.  In other words, you call it on a node that introduces a scope and it returns all bindings within that scope.  It does not return bindings in child scopes.
The scope-graph-scope attribute returns the scope that the node in question resides in.  For nodes that introduce a scope, it is their own.
The scope-graph-introduces-scope? predicate attribute is just used to know when to stop for the scope-graph-descendant-bindings attribute.
|#
(define-property introduces-scope
  #:reads (grammar)
  #:appends
  (ag-rule xsmith_scope-graph-introduces-scope?)
  (ag-rule xsmith_scope-graph-scope)
  (ag-rule xsmith_scope-graph-descendant-bindings)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))

    (define scope-graph-introduces-scope?-info
      (for/fold ([rule-info (hash #f #'(λ (n) #f))])
                ([node nodes])
        (syntax-parse (dict-ref this-prop-info node #'#f)
          [#f rule-info]
          [#t (dict-set rule-info node #'(λ (n) #t))])))

    (define scope-graph-scope-info
      (for/fold ([rule-info (hash #f #'(λ (n)
                                         ;; If a node does not introduce a scope,
                                         ;; its ag-rule should just check its parent
                                         (att-value 'xsmith_scope-graph-scope
                                                    (parent-node n))))])
                ([node nodes])
        (define prop-for-node (dict-ref this-prop-info node #'#f))
        (syntax-parse prop-for-node
          [#f rule-info]
          [#t
           (dict-set
            rule-info
            node
            #'(λ (n)
                (scope
                 ;; parent scope -- this should be #f for the top-level scope
                 (if (parent-node n)
                     (att-value 'xsmith_scope-graph-scope (parent-node n))
                     #f)
                 ;; bindings
                 (att-value 'xsmith_scope-graph-descendant-bindings n #t)
                 ;; imports -- this exists in the scope graphs impl, but is unused...
                 '())))])))

    (define scope-graph-descendant-bindings-info
      (for/hash ([node nodes])
        (define field-info (dict-ref field-info-hash node))
        (values
         node
         #`(λ (n [is-first-node? #f])
             (if (or (att-value 'is-hole? n)
                     (and (not is-first-node?)
                          (att-value 'xsmith_scope-graph-introduces-scope? n)))
                 '()
                 (let ([this-node-binding (att-value 'scope-graph-binding n)]
                       [children-binding-lists
                        (list
                         #,@(map (λ (fi)
                                   (cond
                                     [(not (grammar-node-field-struct-type fi))
                                      #'(list)]
                                     [(grammar-node-field-struct-kleene-star? fi)
                                      #`(map (λ (c) (att-value
                                                     'xsmith_scope-graph-descendant-bindings
                                                     c))
                                             (ast-children
                                              (ast-child
                                               '#,(datum->syntax
                                                   #f
                                                   (grammar-node-field-struct-name fi))
                                               n)))]
                                     [else
                                      #`(att-value
                                         'xsmith_scope-graph-descendant-bindings
                                         (ast-child
                                          '#,(datum->syntax
                                              #f
                                              (grammar-node-field-struct-name fi))
                                          n))]))
                                 field-info))])
                   (define children-bindings (flatten children-binding-lists))
                   (if this-node-binding
                       (cons this-node-binding children-bindings)
                       children-bindings)))))))
    (list scope-graph-introduces-scope?-info
          scope-graph-scope-info
          scope-graph-descendant-bindings-info)))

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
