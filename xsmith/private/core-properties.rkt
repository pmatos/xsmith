#lang racket/base

(provide
 may-be-generated
 depth-increase
 choice-weight
 fresh
 child-node-name-dict
 wont-over-deepen
 type-info
 introduces-scope
 binder-info
 reference-info
 strict-child-order?
 io
 lift-predicate
 lift-type->ast-binder-type
 binding-structure
 choice-filters-to-apply

 make-lift-reference-choice-proc
 )

(require
 "grammar-macros.rkt"
 "xsmith-utils.rkt"
 "xsmith-options.rkt"
 "scope-graph.rkt"
 "types.rkt"
 "effects.rkt"
 racr
 racket/random
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
                         (cond [(and (ast-has-child? 'xsmithliftdepth n)
                                     (number? (ast-child 'xsmithliftdepth n)))
                                (ast-child 'xsmithliftdepth n)]
                               [(ast-has-parent? n)
                                (att-value 'ast-depth (parent-node n))]
                               [else 0]))
                       (+ increment parent-depth))]))

(define-property choice-weight
  #:appends (choice-rule xsmith_choice-weight)
  #:transformer
  (λ (this-prop-info)
    (define this-prop/defaulted
      (if (dict-has-key? this-prop-info #f)
          this-prop-info
          (dict-set this-prop-info #f #'10)))
    (list
     (for/hash ([node-name (dict-keys this-prop/defaulted)])
       (values node-name
               #`(λ () #,(dict-ref this-prop/defaulted node-name)))))))

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
                    (let ([v (dict-ref
                              field-dict
                              f-name
                              (λ () (dict-ref prop-given-values
                                              f-name
                                              (dict-ref thunk-hash f-name))))])
                      (if (procedure? v) (v) v)))))
               (define all-values-hash/seq-transformed
                 (for/hash ([f-name (list field-name ...)]
                            [f-type (list field-type ...)]
                            [f-seq? (list field-seq? ...)])
                   (values
                    f-name
                    (let ([v (dict-ref all-values-hash f-name)])
                      (cond [(and f-seq? (list? v) (create-ast-list v))]
                            [(and f-seq? (number? v))
                             ;; If the init value is a number and a list
                             ;; of hole nodes is required, make an appropriate
                             ;; list of that length.
                             (expr->ast-list v (and f-type (make-hole f-type)))]
                            [else v])))))
               (define all-values-in-order
                 (map (λ (name) (dict-ref all-values-hash/seq-transformed name))
                      (list field-name ...)))
               (define all-values+xsmith-injected
                 ;; add xsmithliftdepth and xsmithlifterwrapped
                 (append (list #f #f)
                         all-values-in-order))

               (create-ast (current-racr-spec)
                           '#,node
                           all-values+xsmith-injected))))))
    (list rule-info)))

(define-property child-node-name-dict
  #:reads (grammar)
  #:appends (ag-rule xsmith_child-node-name-dict)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))
    (define node-typed-fields-dict
      (for/hash ([node nodes])
        (values node
                (map grammar-node-field-struct-name
                     (filter grammar-node-field-struct-type
                             (dict-ref field-info-hash node))))))
    (define child-node-name-dict-info
      (for/hash ([node nodes])
        (with-syntax ([(field-name ...)
                       (map (λ (x) (datum->syntax #f x))
                            (dict-ref node-typed-fields-dict node))])
          (values node
                  #`(λ (n) (make-immutable-hash
                            (list
                             (cons (ast-child 'field-name n) 'field-name)
                             ...)))))))
    (list child-node-name-dict-info)))

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
    (define node-typed-fields-dict
      (for/hash ([node nodes])
        (values node
                (filter (λ(x)x)
                        (map grammar-node-field-struct-type
                             (dict-ref field-info-hash node))))))
    ;; If a node in the grammar has fields that are also nodes, it will make
    ;; the tree deeper.
    (define wont-over-deepen-info-defaults
      (for/hash ([node nodes])
        (values node
                (if (ormap (λ (x) (grammar-node-field-struct-type x))
                           (dict-ref field-info-hash node))
                    #'#f
                    #'#t))))
    ;; But we'll let the user override if they want.
    (define wont-over-deepen-info
      (for/hash ([node nodes])
        (values node
                #`(λ ()
                    (or (<= (att-value 'ast-depth current-hole)
                            (xsmith-option 'max-depth))
                        (current-force-deepen)
                        #,(dict-ref
                           this-prop-info
                           node
                           (dict-ref wont-over-deepen-info-defaults node)))))))
    (list wont-over-deepen-info)))


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

(define (default-lift-destinations-impl n type lift-depth origin-hole)
  (if (ast-has-parent? n)
      (att-value
       'xsmith_lift-destinations
       (ast-parent n) type lift-depth origin-hole)
      '()))

;;; For use when choosing which visible binding to reference from some
;;; sort of reference node.
;;; Put the resulting thunk in your list of potential bindings, and
;;; if you choose the thunk, call it (once!) to get a result binding
;;; struct.
(define (make-lift-reference-choice-proc lift-origin-hole type)
  (λ ()
    (define depth (att-value 'ast-depth lift-origin-hole))
    (define destinations
      (att-value 'xsmith_lift-destinations
                 lift-origin-hole type depth lift-origin-hole))
    (when (equal? 0 (length destinations))
      (error 'xsmith
             "internal error -- no destinations for lift from: ~a, type: ~a, depth: ~a\n"
             (ast-node-type lift-origin-hole) type depth))

    (define lift-name ((random-ref destinations)))
    ;; TODO - the binding struct is incomplete because
    ;; there is no node yet...
    (binding lift-name #f type 'definition)))

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
  (ag-rule xsmith_lift-destinations)
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

    (define binder-nodes
      (filter (λ (node) (dict-ref binder-info-info node #f))
              nodes))


    (define (ast-subtype? subtype-node-name supertype-node-name)
      (define subtype-inheritance-chain
        (cons subtype-node-name
              (grammar-clause->parent-chain (dict-ref grammar-info
                                                      subtype-node-name)
                                            grammar-info)))
      (member supertype-node-name subtype-inheritance-chain))

    (define has-potential-binder-child-hash
      (for/hash ([node nodes])
        (values node
                (for/or ([f (map
                             (λ (x)
                               (let ([type (grammar-node-field-struct-type x)])
                                 (if (syntax? type)
                                     (syntax->datum type)
                                     type)))
                             (dict-ref field-info-hash node))])
                  (and f
                       (for/or ([binder-node binder-nodes])
                         (ast-subtype? binder-node f)))))))

    (define node->liftee-node->field
      (for/hash ([node nodes])
        (values
         node
         (for/hash ([binder-node binder-nodes])
           (values binder-node
                   (for/or ([f (dict-ref field-info-hash node)])
                     (and
                      (grammar-node-field-struct-kleene-star? f)
                      (ast-subtype? binder-node
                                    (syntax->datum
                                     (grammar-node-field-struct-type f)))
                      (grammar-node-field-struct-name f))))))))

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
                (syntax-parse (dict-ref lift-predicate-info node #'(#t))
                  ;; TODO - manual duplicate checking because this in not
                  ;; in the original property's transformer...
                  [(a b) (raise-syntax-error 'xsmith
                                             "duplicate property declaration"
                                             #'b)]
                  [(#t) #'(λ (n type) #t)]
                  [(#f) #'(λ (n type) #f)]
                  [(predicate) #'predicate]))))

    (define xsmith_lift-destinations-info
      (for/fold ([rule-info (hash #f #'default-lift-destinations-impl)])
                ([node nodes])
        (define has-binder (dict-ref has-potential-binder-child-hash node))
        (define liftee-node->field (dict-ref node->liftee-node->field node))
        (if has-binder
            (dict-set
             rule-info
             node
             #`(λ (n type lift-depth lifting-hole-node)
                 (define ast-type
                   ((att-value 'xsmith_lift-type-to-ast-binder-type
                               n)
                    type))

                 ;; The field within the lift destination that a lift
                 ;; should be placed, if possible.
                 (define field
                   (match ast-type
                     #,@(filter
                         (λ(x)x)
                         (map (λ (n)
                                (define f (dict-ref liftee-node->field n))
                                (if f
                                    #`['#,n '#,f]
                                    #f))
                              binder-nodes))
                     [else #f]))
                 (if (and field
                          (att-value 'xsmith_lift-predicate n type))
                     (cons (att-value 'xsmith_make-lift-do-proc
                                      n
                                      field
                                      type
                                      lift-depth
                                      ast-type
                                      lifting-hole-node)
                           (default-lift-destinations-impl
                             n type lift-depth lifting-hole-node))
                     (default-lift-destinations-impl
                       n type lift-depth lifting-hole-node))))
            rule-info)))

    (define scope-graph-introduces-scope?-info
      (for/fold ([rule-info (hash #f #'(λ (n) #f))])
                ([node nodes])
        (if (dict-ref has-potential-binder-child-hash node)
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
        (if (dict-ref has-potential-binder-child-hash node)
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
          xsmith_lift-predicate-info
          xsmith_lift-destinations-info)))

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
          [(name-field-name:id
            type-field-name:id
            (~and def-or-param (~or (~datum definition) (~datum parameter))))
           (dict-set rule-info node
                     #'(λ (n)
                         (let ([name (ast-child 'name-field-name n)]
                               [type (ast-child 'type-field-name n)])
                           (if (or (and (ast-node? type) (ast-bud-node? type))
                                   (and (ast-node? name) (ast-bud-node? name)))
                               #f
                               (binding
                                (ast-child 'name-field-name n)
                                n
                                (ast-child 'type-field-name n)
                                'def-or-param)))))])))
    (list scope-graph-binding-info)))

;; This property should be a list containing:
;; the identifier `read` or the identifier `write`,
;; the field name that references use (as an identifier)
(define-property reference-info)

;; TODO - this is not a great design, but I need the user to specify
;; one function for this and make it available to the xsmith machinery.
;; The function given to this should be something like:
;; (λ (type) 'Declaration)
;; Where you might actually look at the type to determine what kind of
;; ast node a lifted definition should be.
(define-property lift-type->ast-binder-type
  #:appends (ag-rule xsmith_lift-type-to-ast-binder-type)
  #:transformer
  (λ (this-prop-info)
    (unless (and (dict-has-key? this-prop-info #f)
                 (equal? 1
                         (length (dict-keys this-prop-info))))
      (raise-syntax-error 'lift-type->ast-binder-type
                          "you need to specify exactly one function under #f"))
    (list (hash #f #`(λ (n) #,(dict-ref this-prop-info #f))))))

;; These are declared separately, but are handled by the transformer of
;; the `introduces-scope` property.
(define-property lift-predicate)
(define-property binding-structure)

(define-property choice-filters-to-apply
  #:appends (choice-rule xsmith_apply-choice-filters)
  #:transformer
  (λ (this-prop-info)

    (define this-info/defaults
      (if (dict-has-key? this-prop-info #f)
          this-prop-info
          (dict-set this-prop-info #f #'())))

    (define-syntax-class filtering-method
      (pattern method-name:id
               #:attr func #'(λ (o) (send o method-name)))
      (pattern (method-name:id arg:expr ...)
               #:attr func #'(λ (o) (send o method-name arg ...))))
    (define (get-filters node-name)
      (let ([user-filters (dict-ref this-info/defaults node-name #'())])
        ;; Add user-specified filters to the core filters.
        #`(xsmith_may-be-generated
           xsmith_wont-over-deepen
           xsmith_satisfies-type-constraint?
           xsmith_no-io-conflict?
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
      (for/hash ([node-name (dict-keys this-info/defaults)])
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

#|
ref-choices-filtered-hash is for xsmith_constrain-type.
Apparently class definitions don't let public methods be defined with
let-over-lambda (maybe the class macro rewrites the lambdas...).
So let's have a weak hash table store the mutable state we need in a
few of these methods.
|#
(define ref-choices-filtered-hash (make-weak-hasheq))

#|
The type-info property is two-armed.
The first arm is an expression that must return a type (which should be fresh if it is a [maybe constrained] variable) that the AST node can fulfill.
The second arm is a function that takes the type that the node has been assigned and must return a dictionary mapping the node's child fields to either:
• its type
• A function that takes the AST node for the child and returns its type. (This must be used for list children, IE those with a kleene star)
|#
(define-property type-info
  #:reads
  (grammar)
  (property reference-info)
  #:appends
  (ag-rule xsmith_my-type-constraint)
  (choice-rule xsmith_my-type-constraint)
  (ag-rule xsmith_children-type-dict)
  (ag-rule xsmith_type)
  (choice-rule xsmith_satisfies-type-constraint?)
  (choice-rule xsmith_reference-options!)
  #:transformer
  (λ (this-prop-info grammar-info reference-info-info)
    (define nodes (dict-keys grammar-info))
    (define default-prop-info #'#f)
    (define node-type-constraints
      (for/fold ([h (hash)])
                ([n nodes])
        (define c (syntax-parse (dict-ref this-prop-info n default-prop-info)
                    [(constraint:expr _) #'constraint]
                    [else #f]))
        (if c (hash-set h n c) h)))

    (define constraints-checked
      (for/hash ([n (dict-keys node-type-constraints)])
        (values n #`(let ([t #,(dict-ref node-type-constraints n)])
                      (if (type? t)
                          t
                          (error 'type-info
                                 "Type constraint returned for node of AST type ~a was not a type: ~a\n"
                                 (quote #,n)
                                 t))))))
    (define xsmith_my-type-constraint-info/ag-rule
      (for/hash ([n (dict-keys constraints-checked)])
        (values n #`(λ (arg-ignored) #,(dict-ref constraints-checked n)))))
    (define xsmith_my-type-constraint-info/choice-rule
      (for/hash ([n (dict-keys constraints-checked)])
        (values n #`(λ () #,(dict-ref constraints-checked n)))))

    (define node-child-dict-funcs
      (for/fold ([h (hash)])
                ([n nodes])
        (define f (syntax-parse (dict-ref this-prop-info n default-prop-info)
                    [(_ f:expr) #'f]
                    [else #f]))
        (if f (hash-set h n f) h)))

    (define node-reference-info-cleansed
      (for/list ([n nodes])
        (syntax-parse (dict-ref reference-info-info n #'#f)
          [#f (list #'#f #'#f)]
          ;; TODO - because this is not in the transformer of the
          ;; original property I have to check duplicates by hand...
          [(((~and r/w-type:id
                   (~or (~datum read) (~datum write)))
             field:id))
           (list #'(quote r/w-type) #'(quote field))])))
    (define node-r/w-type (for/hash ([n nodes]
                                     [i node-reference-info-cleansed])
                            (values n (first i))))

    (define xsmith_children-type-dict-info
      (for/hash ([n (dict-keys node-child-dict-funcs)])
        (values
         n
         #`(λ (node)
             (define my-type (att-value 'xsmith_type node))
             (define my-type->child-type-dict
               #,(dict-ref node-child-dict-funcs n))
             (my-type->child-type-dict node my-type)))))
    (define xsmith_type-info
      (for/hash ([n nodes])
        (values
         n
         #`(λ (node)
             (define parent-child-type-dict
               (if (ast-has-parent? node)
                   (att-value 'xsmith_children-type-dict (ast-parent node))
                   (hash node (fresh-type-variable))))
             (define (parent-node-type)
               (and (ast-has-parent? node)
                    (ast-node-type (ast-parent node))))
             (define my-type-from-parent/func
               (dict-ref parent-child-type-dict
                         node
                         (λ () (dict-ref
                                parent-child-type-dict
                                (att-value 'xsmith_node-field-name-in-parent node)
                                (λ ()
                                  (error
                                   'type-info
                                   (string-append
                                    "No type info available for node "
                                    "(of AST type ~a with parent of AST type ~a).")
                                   (quote #,n)
                                   (parent-node-type)))))))
             (define my-type-from-parent (if (procedure? my-type-from-parent/func)
                                             (my-type-from-parent/func node)
                                             my-type-from-parent/func))
             (when (not (type? my-type-from-parent))
               (error
                'type-info
                "Got a value that was not a type: ~a, while typechecking node of AST type ~a and parent of AST type ~a"
                my-type-from-parent
                (quote #,n)
                (parent-node-type)))
             (define my-type-constraint
               (if (att-value 'is-hole? node)
                   #f
                   (att-value 'xsmith_my-type-constraint node)))
             (when my-type-constraint
               (with-handlers
                 ([(λ(x)#t)
                   (λ (e)
                     (eprintf "error while unifying types: ~a and ~a\n"
                              my-type-from-parent my-type-constraint)
                     (eprintf "for node of AST type: ~a\n" (ast-node-type node))
                     (eprintf "with parent of AST type: ~a\n" (ast-node-type
                                                               (parent-node node)))
                     (raise e))])
                 (unify! my-type-from-parent my-type-constraint)))
             my-type-from-parent))))
    (define xsmith_satisfies-type-constraint?-info
      (hash #f #'(λ ()
                   (satisfies-type-constraint?
                    current-hole

                    (send this xsmith_my-type-constraint)))))
    (define xsmith_reference-options!-info
      (hash-set
       (for/hash ([n nodes])
         (values
          n
          #`(λ ()
              (define type-needed (att-value 'xsmith_type (current-hole)))
              (define my-type-constraint (send this xsmith_my-type-constraint))
              (unify! type-needed my-type-constraint)
              (let ([ref-choices-filtered
                     (hash-ref ref-choices-filtered-hash this #f)])
                (if ref-choices-filtered
                    ref-choices-filtered
                    (let ()
                      (define write? (equal? 'write #,(dict-ref node-r/w-type n)))
                      (define effects-to-avoid
                        (filter (if write?
                                    (λ (x) (not (effect-io? x)))
                                    effect-write-variable?)
                                (att-value 'xsmith_effect-constraints (current-hole))))
                      (define effect-variable-names
                        (map effect-variable effects-to-avoid))

                      (define visibles
                        ;; TODO - maybe this should be xsmith_visible-bindings
                        (att-value 'visible-bindings current-hole))
                      (define visibles-with-type
                        (filter (λ (b) (and b (can-unify? type-needed
                                                          (binding-type b))))
                                visibles))
                      (define effect-filtered
                        (filter
                         (λ (x) (not (member (binding-name x) effect-variable-names)))
                         visibles-with-type))

                      (define lift-type (concretize-type type-needed))
                      ;; TODO - I should check if the type contains a function, not merely IS a function.  And for higher order effects I should check this before concretizing.
                      (define function? (function-type? lift-type))

                      ;; Higher order functions could have any effect!
                      (define higher-order-effect-filtered
                        (if (and function?
                                 (not (null? effects-to-avoid)))
                            (filter
                             ;; Filter out function parameters
                             (λ (x) (eq? (binding-def-or-param x) 'definition))
                             effect-filtered)
                            effect-filtered))

                      ;; TODO - for functions there was a filter here to not get main
                      (when (and write? function?)
                        ;; Assigning to functions destroys language-agnostic effect tracking.
                        (error 'xsmith "Got a function type as a type to assign to.  Xsmith's effect tracking requires that assignment can never have a function type."))
                      (define legal+lift
                        ;; TODO - lift effect constraints...
                        (cons (make-lift-reference-choice-proc
                               current-hole
                               lift-type)
                              higher-order-effect-filtered))
                      (hash-set! ref-choices-filtered-hash this legal+lift)
                      legal+lift))))))
       #f #'(λ () (error 'xsmith_reference-options!
                         "Only defined for nodes with reference-info property"))))

    (list
     xsmith_my-type-constraint-info/ag-rule
     xsmith_my-type-constraint-info/choice-rule
     xsmith_children-type-dict-info
     xsmith_type-info
     xsmith_satisfies-type-constraint?-info
     xsmith_reference-options!-info)))

(define (satisfies-type-constraint? hole type-constraint)
  #|
  We need to call `can-unify?`, but we do type checking lazily.
  This means that the hole type may need to unify with a cousin node's type
  to get all of its constraints, and `can-unify` may give us the wrong answer
  if we haven't done that unification.

  So we need to walk some of the tree to unify.  But we don't want to walk the
  whole tree.  So we check as we go whether the type is sufficiently concrete
  to always give a correct answer, and break the loop when it is.

  We start by going to sibling nodes, and when any type shares variables with
  the hole type, we recur down its subtree as far as variables are shared.
  After each sibling we go up the parent chain and repeat.
  |#
  (define hole-type (att-value 'xsmith_type hole))

  ;;; Begin traversal
  (let/cc break!!
    (define variables '())
    (define (break?!)
      (when (at-least-as-concrete hole-type type-constraint)
        (break!! #t))
      ;; Even if we're not done yet, when we make progress we should update this list.
      (set! variables (type->type-variable-list hole-type)))
    (break?!)
    (let parent-loop ([p (ast-parent hole)]
                      [child hole])
      (define (loop-over-viable-nodes kernel nodes)
        (define (rec nodes) (loop-over-viable-nodes kernel nodes))
        (if (null? nodes)
            (void)
            (let ([n (car nodes)])
              (if (ast-node? n)
                  (cond [(eq? n child)
                         (rec (cdr nodes))]
                        [(ast-list-node? n)
                         (rec (append (ast-children n)
                                      (cdr nodes)))]
                        [(or (ast-bud-node? n)
                             (att-value 'is-hole? n))
                         (rec (cdr nodes))]
                        [else
                         (kernel n)
                         (rec (cdr nodes))])
                  (void)))))
      (define (sibling-loop siblings)
        (loop-over-viable-nodes
         (λ (sibling)
           (define s-type (att-value 'xsmith_type sibling))
           ;; When we check the type of a new thing it may unify variables,
           ;; so we've maybe made progress.
           (break?!)
           (when (contains-type-variables? s-type variables)
             (sibling-loop (ast-children sibling))))
         siblings))
      (sibling-loop (ast-children p))
      (when (and (ast-has-parent? p)
                 ;; If the parent type doesn't include the variable,
                 ;; it was fresh for its children, and we don't need
                 ;; to climb the tree anymore.
                 (contains-type-variables? (att-value 'xsmith_type p)
                                           variables))
        (parent-loop (parent-node p) p))))
  ;;; End traversal

  ;; The hole type is now either maximally unified or sufficiently concrete
  ;; that no more unification can change the result of this predicate.
  (can-unify? hole-type
              type-constraint))



(define-property strict-child-order?
  #:appends (ag-rule xsmith_strict-child-order?)
  #:transformer
  (λ (this-prop-info)
    (define xsmith_strict-child-order?-info
      (for/fold ([out-info (hash #f #'(λ (n) #f))])
                ([n (dict-keys this-prop-info)])
        (dict-set out-info n (syntax-parse (dict-ref this-prop-info n)
                               [b:boolean #'(λ (n) b)]))))
    (list xsmith_strict-child-order?-info)))

(define (non-hole-node? x)
  (and (ast-node? x) (not (att-value 'is-hole? x))))

(define-property io
  #:reads
  (grammar)
  (property reference-info)
  #:appends
  (ag-rule xsmith_effects/no-children) ;; effects directly caused by a node
  (ag-rule xsmith_effects) ;; effects caused by a node and its children
  (ag-rule xsmith_effect-constraints-for-child)
  (choice-rule xsmith_no-io-conflict?)
  #:transformer
  (λ (this-prop-info grammar-info reference-info)
    (define nodes (dict-keys grammar-info))
    (define io-info (for/hash ([node nodes])
                      (values node
                              (syntax-parse (dict-ref this-prop-info node #'#f)
                                [b:boolean #'b]))))
    (define xsmith_effects/no-children-info
      (for/hash ([n nodes])
        (define-values (read-or-write varname)
          (syntax-parse (dict-ref reference-info n #'(#f))
            ;; Because I'm reading it outside of its own property
            ;; I have to check duplicates manually...
            [(((~datum read) field-name:id))
             (values #'effect-read-variable #'field-name)]
            [(((~datum write) field-name:id))
             (values #'effect-write-variable #'field-name)]
            [(#f) (values #f #f)]))
        (values
         n
         #`(λ (n) (filter (λ(x)x)
                          (list #,(dict-ref io-info n)
                                #,(if read-or-write
                                      #`(#,read-or-write
                                         (att-value
                                          'resolve-reference-name
                                          n
                                          (ast-child '#,varname n)))
                                      #'#f)
                                ;; This is an over-approximation.
                                ;; For function application, I need the effects of
                                ;; the function body.
                                ;; If I can tell when a reference is for a function
                                ;; specifically I can limit this to only function
                                ;; lookup.
                                ;; However, even then it is an over-approximation
                                ;; because a function definition in some languages
                                ;; can have arbitrary expressions around a lambda,
                                ;; or even different lambdas behind conditionals.
                                (and (equal? #,read-or-write effect-read-variable)
                                     (att-value 'xsmith_effects
                                                (binding-ast-node
                                                 (att-value
                                                  'resolve-reference-name
                                                  n
                                                  (ast-child '#,varname n)))))))))))
    (define xsmith_effects-info
      ;; TODO - this is not node specific, but I think I want ag-rule caching on it...
      (hash
       #f
       #`(λ (n)
           (remove-duplicates
            (flatten
             (cons
              (att-value 'xsmith_effects/no-children n)
              (for/list ([child (filter non-hole-node? (ast-children/flat n))])
                (att-value 'xsmith_effects child))))))))
    (define xsmith_effect-constraints-for-child-info
      ;; TODO - this is not node specific, but I think I want ag-rule caching on it...
      (hash
       #f
       #`(λ (n c)
           (define extended-family-constraints
             (if (ast-has-parent? n)
                 (att-value 'xsmith_effect-constraints-for-child (ast-parent n) n)
                 '()))
           (define lift-constraints
             (if (ast-has-child? 'xsmithlifterwrapped n)
                 (let ([lifter (ast-child 'xsmithlifterwrapped n)])
                   (if lifter
                       (att-value 'xsmith_effects (unbox lifter))
                       '()))
                 '()))
           (define direct-constraints
             (if (att-value 'xsmith_strict-child-order? n)
                 '()
                 (for/list ([sibling (filter non-hole-node? (ast-children/flat n))])
                   (if (eq? c sibling)
                       '()
                       (att-value 'xsmith_effects sibling)))))
           (remove-duplicates
            (flatten (cons lift-constraints
                           (cons extended-family-constraints
                                 direct-constraints)))))))
    (define xsmith_no-io-conflict?-info
      (for/hash ([n nodes])
        (values
         n
         (syntax-parse (dict-ref io-info n)
           [#t #'(λ () (or (not (ast-has-parent? (current-hole)))
                           (not (memf effect-io?
                                      (att-value 'xsmith_effect-constraints-for-child
                                                 (ast-parent (current-hole))
                                                 (current-hole))))))]
           [#f #'(λ () #t)]))))
    (list xsmith_effects/no-children-info
          xsmith_effects-info
          xsmith_effect-constraints-for-child-info
          xsmith_no-io-conflict?-info)))

