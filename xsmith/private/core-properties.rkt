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

(provide
 may-be-generated
 depth-increase
 choice-weight
 fresh
 child-node-name-dict
 wont-over-deepen
 type-info
 binder-info
 reference-info
 strict-child-order?
 io
 lift-predicate
 lift-type->ast-binder-type
 binding-structure
 choice-filters-to-apply
 print-node
 print-node-info

 make-lift-reference-choice-proc
 )

(module+ for-private
  (provide introduces-scope))

(require
 "grammar-macros.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
 "xsmith-command-line.rkt"
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
  #:rule-name _xsmith_may-be-generated
  #:default #t
  #:transformer (syntax-parser [#t #'(λ () this)]
                               [#f #'(λ () #f)]))

(define-non-inheriting-rule-property
  depth-increase
  att-rule
  #:rule-name xsmith_ast-depth
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
                                (att-value 'xsmith_ast-depth (parent-node n))]
                               [else 0]))
                       (+ increment parent-depth))]))

(define-property choice-weight
  #:appends
  (choice-rule _xsmith_choice-weight)
  (choice-rule _xsmith_nonzero-weight?)
  #:transformer
  (λ (this-prop-info)
    (define this-prop/defaulted
      (if (dict-has-key? this-prop-info #f)
          this-prop-info
          (dict-set this-prop-info #f #'10)))
    (list
     (for/hash ([node-name (dict-keys this-prop/defaulted)])
       (values node-name
               #`(λ () (let ([node-val #,(dict-ref this-prop/defaulted node-name)])
                         (cond
                           [(procedure? node-val) (node-val)]
                           [(number? node-val) node-val]
                           [else (error 'choice-weight "Invalid weight given: ~a. Expected number or procedure." node-val)])))))
     (hash #f #'(λ () (not (zero? (send this _xsmith_choice-weight))))))))
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
  #:reads
  (grammar)
  (property binder-info)
  (property reference-info)
  #:appends (choice-rule _xsmith_fresh)
  #:transformer
  (λ (this-prop-info grammar-info binder-info-info reference-info-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info-list node-name grammar-info))))

    (define binder-field-names-dict
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref binder-info-info node #'#f)
                  [(name-field-name:id type-field-name:id def/param)
                   (list #''name-field-name
                         #''type-field-name)]
                  [else (list #'#f #'#f)]))))

    (define reference-field-names-dict
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref reference-info-info node #'#f)
                  [(_ name:id (~optional (~seq #:unifies target))) (syntax->datum #'name)]
                  [else #f]))))

    ;; I need to create a lambda (of zero args) that evaluates the given expression (if it exists), then calls a thunk to get the default value for any fields not specified in the list received.
    (define _xsmith_fresh-info
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
                                              fstruct)]
                                       [reference? (equal? fname
                                                           (dict-ref
                                                            reference-field-names-dict
                                                            node
                                                            #f))])
                                  (cond
                                    [init-e init-e]
                                    [seq? #'(create-ast-list (list))]
                                    [f-type #`(make-hole
                                               '#,(datum->syntax #'here f-type))]
                                    [reference? #'(binding-name
                                                   (send this xsmith_get-reference!))]
                                    [else #'#f])))))
                       field-names))))
               (define binder-name-field
                 #,(first (hash-ref binder-field-names-dict node)))
               (define binder-type-field
                 #,(second (hash-ref binder-field-names-dict node)))
               ;; TODO - get name and type directly for lift nodes
               (define binder-hash
                 (if binder-name-field
                     (let* (;; the hole name and type are only there for lifts,
                            ;; in which case they are a hole of the type that is
                            ;; lifted and never a supertype.  But generally
                            ;; a hole may be of a supertype of a definition node,
                            ;; so we need to check that the name and type fields
                            ;; are there for the current hole.
                            [hole-name (and (ast-has-child? binder-name-field
                                                            (current-hole))
                                            (ast-child binder-name-field
                                                       (current-hole)))]
                            [hole-type (and (ast-has-child? binder-type-field
                                                            (current-hole))
                                            (ast-child binder-type-field
                                                       (current-hole)))]
                            [hole-type (if (and hole-type
                                                (not
                                                 (and
                                                  (ast-node? hole-type)
                                                  (ast-bud-node? hole-type))))
                                           hole-type
                                           #f)])
                       (if hole-type
                           (hash binder-name-field hole-name
                                 binder-type-field hole-type)
                           (hash)))
                     (hash)))
               (define fresh-expr-result fresh-expr)
               (define prop-given-values (if (procedure? fresh-expr-result)
                                             (fresh-expr-result binder-hash)
                                             fresh-expr-result))
               (define all-values-hash
                 (for/hash ([f-name (list field-name ...)])
                   (values
                    f-name
                    (let ([v (dict-ref
                              binder-hash
                              f-name
                              (λ ()
                                (dict-ref
                                 field-dict
                                 f-name
                                 (λ ()
                                   (dict-ref prop-given-values
                                             f-name
                                             (dict-ref thunk-hash f-name))))))])
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
                 (append (map (λ (name) (dict-ref field-dict name #f))
                              (list 'xsmithliftdepth
                                    'xsmithlifterwrapped))
                         all-values-in-order))

               (create-ast (current-racr-spec)
                           '#,node
                           all-values+xsmith-injected))))))
    (list _xsmith_fresh-info)))

(define-property child-node-name-dict
  #:reads (grammar)
  #:appends (att-rule _xsmith_child-node-name-dict)
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
  #:appends (choice-rule _xsmith_wont-over-deepen)
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
                    (let ([ok? (<= (att-value 'xsmith_ast-depth current-hole)
                                   (xsmith-max-depth))]
                          [override-ok? #,(dict-ref
                                           this-prop-info
                                           node
                                           (dict-ref wont-over-deepen-info-defaults
                                                     node))]
                          [ref-in-lift? (and (att-value '_xsmith_in-lift-branch
                                                        current-hole)
                                             (send
                                              this
                                              _xsmith_is-read-reference-choice?))])
                      ;; TODO - I should prevent circles of lifting where a lift
                      ;; variable is defined as another variable reference that
                      ;; gets lifted, etc.
                      ;; But my basic heuristic that I first used is bad in the face
                      ;; of nominal record variable reference.
                      (or ok? override-ok? (current-force-deepen)))))))
    (list wont-over-deepen-info)))


#|
Helper function for _xsmith_scope-graph-child-scope-dict.
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
       '_xsmith_lift-destinations
       (ast-parent n) type lift-depth origin-hole)
      '()))

;;; For use when choosing which visible binding to reference from some
;;; sort of reference node.
;;; Put the resulting thunk in your list of potential bindings, and
;;; if you choose the thunk, call it (once!) to get a result binding
;;; struct.
(define (make-lift-reference-choice-proc lift-origin-hole type)
  (λ ()
    (define depth (att-value 'xsmith_ast-depth lift-origin-hole))
    (define destinations
      (att-value '_xsmith_lift-destinations
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
The introduces-scope property generates RACR attributes for resolving bindings via scope graphs.
The scope-graph-descendant-bindings attribute returns a list of all bindings on descendant nodes that are not under a different scope.  In other words, you call it on a node that introduces a scope and it returns all bindings within that scope.  It does not return bindings in child scopes.
The scope-graph-scope attribute returns the scope that the node in question resides in.  For nodes that introduce a scope, it is their own.

Note that this property doesn't have arguments and isn't public so users can't even set a value for it.
It just reads the values of several other properties and produces the results for them.
|#
(define-property introduces-scope
  #:reads
  (grammar)
  (property binder-info)
  (property lift-predicate)
  (property binding-structure)
  #:appends
  (att-rule _xsmith_scope-graph-child-scope-dict)
  (att-rule _xsmith_scope-graph-scope)
  (att-rule _xsmith_lift-predicate)
  (att-rule _xsmith_lift-destinations)
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

    (define node-binder-types
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref binder-info-info node #'#f)
                  [(name-field-name type-field-name (~and def/param
                                                          (~or (~datum definition)
                                                               (~datum parameter))))
                   (syntax->datum #'def/param)]
                  [else #f]))))
    (define binder-nodes (filter (λ (n) (dict-ref node-binder-types n)) nodes))
    (define definition-nodes (filter (λ (n) (equal? (dict-ref node-binder-types n)
                                                    'definition))
                                     nodes))

    (define (ast-subtype? subtype-node-name supertype-node-name)
      (define subtype-inheritance-chain
        (cons subtype-node-name
              (grammar-clause->parent-chain (dict-ref grammar-info
                                                      subtype-node-name)
                                            grammar-info)))
      (member supertype-node-name subtype-inheritance-chain))

    (define field-types-hash
      ;; get a list of field types that a node contains
      (for/hash ([node nodes])
        (values node
                (map (λ (x) (let ([type (grammar-node-field-struct-type x)])
                              (if (syntax? type)
                                  (syntax->datum type)
                                  type)))
                     (dict-ref field-info-hash node)))))

    (define has-potential-binder-child-hash
      (for/hash ([node nodes])
        (values node
                (for/or ([f (dict-ref field-types-hash node)])
                  (and f (for/or ([binder-node binder-nodes])
                           (ast-subtype? binder-node f)))))))

    (define has-potential-definition-child-hash
      ;; Like binder hash, but only definitions, NOT parameters.
      ;; For figuring out lift destinations.
      (for/hash ([node nodes])
        (values node
                (for/or ([f (dict-ref field-types-hash node)])
                  (and f (for/or ([definition-node definition-nodes])
                           (ast-subtype? definition-node f)))))))

    (define node->liftee-node->field
      ;; For each node, what field should a lifted definition use
      (for/hash ([node nodes])
        (values
         node
         (for/hash ([definition-node definition-nodes])
           (values definition-node
                   (for/or ([f (dict-ref field-info-hash node)])
                     (and
                      (grammar-node-field-struct-kleene-star? f)
                      (ast-subtype? definition-node
                                    (syntax->datum
                                     (grammar-node-field-struct-type f)))
                      (grammar-node-field-struct-name f))))))))

    (define binding-structure-hash
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref binding-structure-info node #'#f)
                  #:literals (quote)
                  [(quote (~and flag:id (~or (~datum serial)
                                             (~datum parallel)
                                             (~datum recursive))))
                   #''flag]
                  [#f #''serial]))))
    (define _xsmith_lift-predicate-info
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref lift-predicate-info node #'#t)
                  [#t #'(λ (n type) #t)]
                  [#f #'(λ (n type) #f)]
                  [predicate #'predicate]))))

    (define _xsmith_lift-destinations-info
      (for/fold ([rule-info (hash #f #'default-lift-destinations-impl)])
                ([node nodes])
        (define has-definition (dict-ref has-potential-definition-child-hash node))
        (define liftee-node->field (dict-ref node->liftee-node->field node))
        (if has-definition
            (dict-set
             rule-info
             node
             #`(λ (n type lift-depth lifting-hole-node)
                 (define ast-type
                   ((att-value '_xsmith_lift-type-to-ast-binder-type
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
                              definition-nodes))
                     [else #f]))
                 (define parent-destinations
                   (default-lift-destinations-impl
                     n type lift-depth lifting-hole-node))
                 (if (and field
                          (att-value '_xsmith_lift-predicate n type))
                     (cons (att-value '_xsmith_make-lift-do-proc
                                      n
                                      field
                                      type
                                      lift-depth
                                      ast-type
                                      lifting-hole-node)
                           parent-destinations)
                     parent-destinations)))
            rule-info)))


    (define _xsmith_scope-graph-scope-child-dict-info
      (for/fold ([rule-info (hash #f #'(λ (n)
                                         ;; If a node does not introduce a scope,
                                         ;; it just passes through its own.
                                         (define children (ast-children/flat n))
                                         (define scope
                                           (att-value '_xsmith_scope-graph-scope n))
                                         (for/hash ([c children])
                                           (values c scope))))])
                ([node nodes])
        (define binding-structure-for-node (dict-ref binding-structure-hash node))
        (if (dict-ref has-potential-binder-child-hash node)
            (dict-set
             rule-info
             node
             #`(λ (n)
                 (define children (filter ast-node? (ast-children/flat n)))
                 (define children-bindings
                   (map (λ (c) (att-value 'xsmith_definition-binding c))
                        children))
                 (define cb-pairs (map cons children children-bindings))
                 (define parent-scope
                   (att-value '_xsmith_scope-graph-scope n))
                 (make-child-scope-dict cb-pairs
                                        parent-scope
                                        #,binding-structure-for-node)))
            rule-info)))
    (define _xsmith_scope-graph-scope-info
      (hash #f
            #'(λ (n) (if (ast-has-parent? n)
                         (let ([parent-dict (att-value
                                             '_xsmith_scope-graph-child-scope-dict
                                             (parent-node n))])
                           (dict-ref parent-dict n))
                         ;; dummy program parent scope to simplify child-dict lookup
                         (scope #f '() '())))))

    (list ;scope-graph-introduces-scope?-info
     _xsmith_scope-graph-scope-child-dict-info
     _xsmith_scope-graph-scope-info
     _xsmith_lift-predicate-info
     _xsmith_lift-destinations-info)))

(define-property binder-info
  #:reads (grammar)
  #:appends (att-rule xsmith_definition-binding)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define xsmith_definition-binding-info
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
    (list xsmith_definition-binding-info)))

;; This property should be a list containing:
;; the identifier `read` or the identifier `write`,
;; the field name that references use (as an identifier)
(begin-for-syntax
  (define-syntax-class reference-info-class
    (pattern ((~and ref-type (~or (~datum write) (~datum read)))
              field-name:id
              (~optional (~seq #:unifies (~or target:id #f))))
             #:with unify-target (or (and (attribute target) #''target)
                                     #'#t)
             #:with is-read? (eq? (syntax->datum #'ref-type) 'read))))

(define-property reference-info
  #:reads (grammar)
  #:appends
  (choice-rule _xsmith_is-read-reference-choice?)
  (att-rule _xsmith_is-read-reference-node?)
  (att-rule _xsmith_is-reference-node?)
  (att-rule _xsmith_resolve-reference)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define _xsmith_is-read-reference-info
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref this-prop-info
                                        node
                                        #'#f)
                  [prop:reference-info-class #:when (attribute prop.is-read?) #''prop.field-name]
                  [else #'#f]))))
    (define _xsmith_is-reference-info
      (for/hash ([node nodes])
        (values node
                (syntax-parse (dict-ref this-prop-info
                                        node
                                        #'#f)
                  [prop:reference-info-class #''prop.field-name]
                  [else #'#f]))))
    (define _xsmith_is-read-reference-choice?-info
      (for/hash ([node nodes])
        (values node #`(λ () #,(dict-ref _xsmith_is-read-reference-info node)))))
    (define _xsmith_is-read-reference-node?-info
      (for/hash ([node nodes])
        (values node #`(λ (n) #,(dict-ref _xsmith_is-read-reference-info node)))))
    (define _xsmith_is-reference-node?-info
      (for/hash ([node nodes])
        (values node #`(λ (n) #,(dict-ref _xsmith_is-reference-info node)))))
    (define _xsmith_resolve-reference
      (for/hash ([node nodes])
        (values node
                #`(λ (n)
                    (define field #,(dict-ref _xsmith_is-reference-info node))
                    (when (not field) (error '_xsmith_resolve-reference
                                             "not a reference node: ~a"
                                             (node-type n)))
                    (att-value '_xsmith_resolve-reference-name
                               n (ast-child field n))))))
    (list _xsmith_is-read-reference-choice?-info
          _xsmith_is-read-reference-node?-info
          _xsmith_is-reference-node?-info
          _xsmith_resolve-reference)))

;; TODO - this is not a great design, but I need the user to specify
;; one function for this and make it available to the xsmith machinery.
;; The function given to this should be something like:
;; (λ (type) 'Declaration)
;; Where you might actually look at the type to determine what kind of
;; ast node a lifted definition should be.
(define-property lift-type->ast-binder-type
  #:reads (property binder-info)
  #:appends (att-rule _xsmith_lift-type-to-ast-binder-type)
  #:transformer
  (λ (this-prop-info binder-info)
    (define definitions (filter (λ (n) (syntax-parse (dict-ref binder-info n)
                                         [(name-f type-f (~datum definition)) #t]
                                         [else #f]))
                                (dict-keys binder-info)))
    (define single-definition (and (equal? 1 (length definitions)) (car definitions)))

    (define this-prop-defaulted
      (if (dict-has-key? this-prop-info #f)
          this-prop-info
          (dict-set
           this-prop-info
           #f
           (if single-definition
               #`(λ (type) '#,(datum->syntax #f single-definition))
               #'(λ (type) (error 'lift-type->ast-binder-type
                                  "You must specify a #f value for the lift-type->ast-binder-type property if your language has more than one binding form."))))))

    (unless (equal? 1 (length (dict-keys this-prop-defaulted)))
      (raise-syntax-error 'lift-type->ast-binder-type
                          "you need to specify exactly one function under #f"))
    (list (hash #f #`(λ (n) #,(dict-ref this-prop-defaulted #f))))))

;; These are declared separately, but are handled by the transformer of
;; the `introduces-scope` property.
(define-property lift-predicate)
(define-property binding-structure)

(define-property choice-filters-to-apply
  #:appends (choice-rule _xsmith_apply-choice-filters)
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
        #`(_xsmith_may-be-generated
           _xsmith_wont-over-deepen
           _xsmith_satisfies-type-constraint?
           _xsmith_no-io-conflict?
           _xsmith_nonzero-weight?
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
ref-choices-filtered-hash is for _xsmith_reference-options!
Apparently class definitions don't let public methods be defined with
let-over-lambda (maybe the class macro rewrites the lambdas...).
So let's have a weak hash table store the mutable state we need in a
few of these methods.
|#
(define ref-choices-filtered-hash (make-weak-hasheq))

(define (xsmith_get-reference!-func self lift-probability)
  (let* ([options/all (send self _xsmith_reference-options!)]
         [options/lift (if (<= (random) lift-probability)
                           options/all
                           (filter (λ (x) (not (procedure? x))) options/all))]
         [options (if (null? options/lift) options/all options/lift)]
         ;; TODO - add some tunable heuristics to make good choices here.
         [choice/proc (random-ref options)]
         [choice (if (procedure? choice/proc) (choice/proc) choice/proc)])
    choice))

(define (_xsmith_reference-options!-func self hole node-r/w-type)
  (define type-needed (att-value 'xsmith_type hole))
  (let ([ref-choices-filtered
         (hash-ref ref-choices-filtered-hash self #f)])
    (if ref-choices-filtered
        ref-choices-filtered
        (let ()
          (define write? (equal? 'write node-r/w-type))
          (define effects-to-avoid
            (filter (if write?
                        (λ (x) (not (effect-io? x)))
                        effect-write-variable?)
                    (att-value '_xsmith_effect-constraints hole)))
          (define effect-variable-names
            (map effect-variable effects-to-avoid))

          (define visibles
            (att-value '_xsmith_visible-bindings hole))
          (define visibles-with-type
            (filter (λ (b) (and b
                                (concrete-type? (binding-type b))
                                (can-unify? type-needed
                                            (binding-type b))))
                    visibles))
          (define visibles/no-func-for-write
            (if (not write?)
                visibles-with-type
                (filter
                 (λ (b)
                   (and (not (can-unify? (binding-type b)
                                         (function-type
                                          (fresh-type-variable)
                                          (fresh-type-variable))))
                        (not (can-unify? (binding-type b)
                                         (nominal-record-definition-type
                                          (fresh-type-variable))))))
                 visibles-with-type)))

          (define effect-filtered
            (filter
             (λ (x) (not (member (binding-name x) effect-variable-names)))
             visibles/no-func-for-write))

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
            (if (and (nominal-record-definition-type? lift-type)
                     (not (null? higher-order-effect-filtered)))
                higher-order-effect-filtered
                (cons (make-lift-reference-choice-proc
                       hole
                       lift-type)
                      higher-order-effect-filtered)))
          (hash-set! ref-choices-filtered-hash self legal+lift)
          legal+lift))))

(define (get-value-from-parent-dict parent-child-dict child default)
  (dict-ref parent-child-dict
            child
            (λ () (dict-ref
                   parent-child-dict
                   (att-value '_xsmith_node-field-name-in-parent child)
                   default))))

(define (_xsmith_type-constraint-from-parent-func node node-type-name)
  (define (parent-node-type)
    (and (ast-has-parent? node)
         (ast-node-type (parent-node node))))
  (define parent-child-type-dict
    (if (ast-has-parent? node)
        (att-value '_xsmith_children-type-dict (ast-parent node))
        (hash node (fresh-type-variable))))
  (define my-type-from-parent/func
    (get-value-from-parent-dict parent-child-type-dict node
                                (λ ()
                                  (error
                                   'type-info
                                   (string-append
                                    "No type info provided by parent for node "
                                    "(of AST type ~a, with parent of AST type ~a, "
                                    "and field name ~a).")
                                   node-type-name
                                   (parent-node-type)
                                   (att-value '_xsmith_node-field-name-in-parent
                                              node)))))
  (define my-type-from-parent (if (procedure? my-type-from-parent/func)
                                  (my-type-from-parent/func node)
                                  my-type-from-parent/func))
  (when (not (type? my-type-from-parent))
    (error
     'type-info
     "Got a value that was not a type: ~a, while typechecking node of AST type ~a and parent of AST type ~a"
     my-type-from-parent
     node-type-name
     (parent-node-type)))
  my-type-from-parent)

(define (xsmith_type-info-func node reference-unify-target reference-field definition-type-field)
  (define my-type-constraint
    (if (att-value 'xsmith_is-hole? node)
        #f
        (att-value '_xsmith_my-type-constraint node)))
  (define my-type-from-parent
    (att-value '_xsmith_type-constraint-from-parent node))
  (when my-type-constraint
    (with-handlers
      ([(λ(x)#t)
        (λ (e)
          (xd-printf "error while unifying types: ~a and ~a\n"
                     my-type-from-parent my-type-constraint)
          (xd-printf "for node of AST type: ~a\n" (ast-node-type node))
          (xd-printf "with parent of AST type: ~a\n" (ast-node-type
                                                      (parent-node node)))
          (raise e))])
      (unify! my-type-from-parent my-type-constraint)))
  (when (and reference-field (not (att-value 'xsmith_is-hole? node)))
    (let* ([binding (att-value '_xsmith_resolve-reference-name
                               node
                               (ast-child reference-field node))]
           [binding-node (binding-ast-node binding)]
           [binding-node-type (att-value 'xsmith_type binding-node)]
           [var-type (binding-type binding)])
      (with-handlers
        ([(λ(x)#t)
          (λ (e)
            (xd-printf "Error unifying types for reference of AST type: ~a\n"
                       (ast-node-type node))
            (xd-printf "Type received from parent AST node: ~a\n"
                       my-type-from-parent)
            (xd-printf "Type annotated at variable definition: ~a\n"
                       var-type)
            (raise e))])
        (match reference-unify-target
          ;; If the reference-unify-target is not #t or #f, it still needs to be
          ;; unified. However, unifying here will cause a cycle. Instead, this
          ;; is handled in the type-info property definition.
          [#t (unify! my-type-from-parent var-type)]
          [else (void)]))
      ;; This shouldn't be necessary, but something is going wrong,
      ;; so I'll give a chance to get this error message.
      (with-handlers
        ([(λ(x)#t)
          (λ (e)
            (xd-printf "Error unifying types for reference of AST type: ~a\n"
                       (ast-node-type node))
            (xd-printf "Type annotated at variable definition: ~a\n"
                       binding-node-type)
            (xd-printf "Type that was recorded in scope graph: ~a\n"
                       var-type)
            (raise e))])
        (unify! binding-node-type var-type))))
  (when (and definition-type-field (not (att-value 'xsmith_is-hole? node)))
    (let ([def-type (ast-child definition-type-field node)])
      (when (type? def-type)
        ;; TODO - in my existing fuzzers this is sometimes not set for parameters, but it should be... I'm just not sure about the timing of setting it all up right now...
        (unify! my-type-from-parent def-type))))
  ;; Now unified, return the one from the parent since it likely has
  ;; the most direct info.
  my-type-from-parent)

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
  (property binder-info)
  #:appends
  (att-rule _xsmith_my-type-constraint)
  (choice-rule _xsmith_my-type-constraint)
  ;_xsmith_my-type-constraint -- returns the type that a node must fulfill (the first half of the type info property), both in att-rule and choice-rule form.
  (att-rule _xsmith_children-type-dict)
  ;_xsmith_children-type-dict -- returns a dict mapping nodes (or node field names) to types
  (att-rule _xsmith_type-constraint-from-parent)
  (att-rule xsmith_type)
  (choice-rule _xsmith_satisfies-type-constraint?)
  ;_xsmith_satisfies-type-constraint? -- choice predicate -- tests if a hole's type and a choice object are compatible
  (choice-rule _xsmith_reference-options!)
  ;_xsmith_reference-options! -- returns a list of options for a variable to reference that are type compatible.  BUT - it unifies the type of the reference with a fully concrete version.  One of the list members is a thunk that can be applied to get a lifted binding.
  (choice-rule xsmith_get-reference!)
  ;xsmith_get-reference! -- like xsmith_reference-options! but it just returns one (pre-called in the case of lifts).
  #:transformer
  (λ (this-prop-info grammar-info reference-info-info binder-info-info)
    (define nodes (cons #f (dict-keys grammar-info)))
    (define default-prop-info #'#f)
    (define node-type-constraints
      (for/fold ([h (hash)])
                ([n nodes])
        (define c (syntax-parse (dict-ref this-prop-info n default-prop-info)
                    [(constraint:expr _) #'constraint]
                    [#f #f]))
        (if c (hash-set h n c) h)))

    (define constraints-checked
      ;; TODO - I need to put a default implementation that says what node it is, but ties it to `type-info` rather than a private method.
      (for/hash ([n (dict-keys node-type-constraints)])
        (values
         n
         #`(let ([t #,(dict-ref node-type-constraints n)])
             (if (type? t)
                 t
                 (error 'type-info
                        "Type constraint returned for node of AST type ~a was not a type: ~a\n"
                        (quote #,n)
                        t))))))
    (define _xsmith_my-type-constraint-info/att-rule
      (for/hash ([n (dict-keys constraints-checked)])
        (values n #`(λ (arg-ignored) #,(dict-ref constraints-checked n)))))
    (define _xsmith_my-type-constraint-info/choice-rule
      (for/hash ([n (dict-keys constraints-checked)])
        (values n #`(λ () #,(dict-ref constraints-checked n)))))

    (define node-child-dict-funcs
      (hash-set
       (for/fold ([h (hash)])
                 ([n nodes])
         (define f (syntax-parse (dict-ref this-prop-info n default-prop-info)
                     [(_ f:expr) #'f]
                     [else #f]))
         (if f (hash-set h n f) h))
       #f #'(λ (n t) (error 'type-info "Missing parent-child type relation."))))

    (define node-reference-info-cleansed
      (for/list ([n nodes])
        (syntax-parse (dict-ref reference-info-info n #'#f)
          [#f (list #'#f #'#f #'#t)]
          [prop:reference-info-class
           (list #''prop.ref-type #''prop.field-name #'prop.unify-target)])))
    (define node-r/w-type (for/hash ([n nodes]
                                     [i node-reference-info-cleansed])
                            (values n (first i))))
    (define node-reference-unify-target (for/hash ([n nodes]
                                                   [i node-reference-info-cleansed])
                                          (values n (third i))))
    (define node-reference-field (for/hash ([n nodes]
                                            [i node-reference-info-cleansed])
                                   (values n (second i))))
    (define binder-type-field
      (for/hash ([n nodes])
        (values n (syntax-parse (dict-ref binder-info-info n #'#f)
                    [(name-field-name type-field-name def/param)
                     #''type-field-name]
                    [else #'#f]))))

    (define _xsmith_children-type-dict-info
      (for/hash ([n (dict-keys node-child-dict-funcs)])
        (values n #`(λ (node)
                      (define my-type (att-value 'xsmith_type node))
                      (define my-type->child-type-dict
                        #,(dict-ref node-child-dict-funcs n))
                      (define child-types
                        (my-type->child-type-dict node my-type))
                      (define reference-unify-target
                        #,(dict-ref node-reference-unify-target n))
                      (when (and reference-unify-target (not (eq? #t reference-unify-target)))
                        (unify!
                         (get-value-from-parent-dict child-types reference-unify-target
                                                     (λ () (error 'type-info
                                                                  "No type given for field ~a"
                                                                  reference-unify-target)))
                         (binding-type (att-value '_xsmith_resolve-reference-name
                                                  node
                                                  (ast-child #,(dict-ref node-reference-field n) node)))))
                      child-types))))
    (define _xsmith_type-constraint-from-parent-info
      (for/hash ([n nodes])
        (values n #`(λ (node) (_xsmith_type-constraint-from-parent-func
                               node
                               (quote #,n))))))
    (define xsmith_type-info
      (for/hash ([n nodes])
        (values n #`(λ (node)
                      (xsmith_type-info-func node
                                             #,(dict-ref node-reference-unify-target n)
                                             #,(dict-ref node-reference-field n)
                                             #,(dict-ref binder-type-field n))))))
    (define _xsmith_satisfies-type-constraint?-info
      (hash #f #'(λ ()
                   (satisfies-type-constraint?
                    (current-hole)
                    (send this _xsmith_my-type-constraint)))))
    (define _xsmith_reference-options!-info
      (hash-set
       (for/hash ([n nodes])
         (values n #`(λ () (_xsmith_reference-options!-func
                            this
                            (current-hole)
                            #,(dict-ref node-r/w-type n)))))
       #f #'(λ () (error '_xsmith_reference-options!
                         "Only defined for nodes with reference-info property"))))
    (define xsmith_get-reference!-info
      (for/hash ([n nodes])
        (values n #`(λ (#:lift-probability [lift-probability 0])
                      (xsmith_get-reference!-func this lift-probability)))))

    (list
     _xsmith_my-type-constraint-info/att-rule
     _xsmith_my-type-constraint-info/choice-rule
     _xsmith_children-type-dict-info
     _xsmith_type-constraint-from-parent-info
     xsmith_type-info
     _xsmith_satisfies-type-constraint?-info
     _xsmith_reference-options!-info
     xsmith_get-reference!-info
     )))

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
    (define binding-nodes-started '())
    (define binding-nodes-finished '())

    (define (break?!)
      (when (concrete-type? hole-type)
        (break!! #t))
      (when (at-least-as-concrete hole-type type-constraint)
        #|
        TODO This is currently broken.  I'm sure at-least-as-concrete is broken.
        I need to make at-least-as-concrete more conservative, probably, but also
        I can pair it here with `can-unify?` to break off in the case that there
        are no common cases instead of trying to cram a second version of that into
        `at-least-as-concrete`.
        |#
        ;;(break!! #t)
        (void)
        )
      ;; Even if we're not done yet, when we make progress we should update this list.
      (set! variables (type->type-variable-list hole-type)))
    (break?!)
    (let parent-loop ([p (ast-parent hole)]
                      [child hole])
      (define (sibling-loop nodes)
        (define rec sibling-loop)
        (if (null? nodes)
            (void)
            (let ([n (car nodes)]
                  [ns (cdr nodes)])
              (cond [(not (ast-node? n))
                     (rec ns)]
                    [(eq? n child)
                     (rec ns)]
                    [(ast-list-node? n)
                     (rec (append (ast-children n)
                                  ns))]
                    [(ast-bud-node? n)
                     (rec ns)]
                    [(att-value 'xsmith_is-hole? n)
                     (rec ns)]
                    [(memq n binding-nodes-finished)
                     (rec ns)]
                    [else
                     (define n-type (att-value 'xsmith_type n))
                     ;; When we check the type of a new thing it may unify variables,
                     ;; so we've maybe made progress.
                     (break?!)

                     ;; If the node is a binder, mark it so we don't look at it
                     ;; repeatedly when we hit references to it.
                     (when (att-value 'xsmith_definition-binding n)
                       (set! binding-nodes-finished
                             (cons n binding-nodes-finished)))

                     ;; If the node is a reference, the definition site
                     ;; may have nodes that will affect the type.
                     (when (att-value '_xsmith_is-read-reference-node? n)
                       (let ([binding-node (binding-ast-node
                                            (att-value
                                             '_xsmith_resolve-reference n))])
                         (when (not (memq binding-node binding-nodes-started))
                           (set! binding-nodes-started
                                 (cons binding-node binding-nodes-started))
                           (sibling-loop (list binding-node)))))

                     ;; Check children nodes if they are relevant
                     (when (contains-type-variables? n-type variables)
                       (sibling-loop (ast-children n)))
                     (rec ns)]))))
      (sibling-loop (ast-children p))
      (when (and (ast-has-parent? p)
                 (or
                  ;; If the current node (child) includes relevant variables,
                  ;; its siblings may too even if the parent doesn't.
                  (contains-type-variables? (att-value 'xsmith_type child)
                                            variables)
                  ;; If the parent includes relevant variables its siblings
                  ;; or ancestors might as well.
                  (contains-type-variables? (att-value 'xsmith_type p)
                                            variables)))
        (parent-loop (parent-node p) p))))
  ;;; End traversal

  ;; The hole type is now either maximally unified or sufficiently concrete
  ;; that no more unification can change the result of this predicate.
  (can-unify? hole-type
              type-constraint))


(define-property strict-child-order?
  #:appends (att-rule _xsmith_strict-child-order?)
  #:transformer
  (λ (this-prop-info)
    (define _xsmith_strict-child-order?-info
      (hash-set
       (for/hash ([(n v) (in-dict this-prop-info)])
         (values n (syntax-parse v [b:boolean #'(λ (n) b)])))
       #f #'(λ (n) #f)))
    (list _xsmith_strict-child-order?-info)))

(define (non-hole-node? x)
  (and (ast-node? x) (not (att-value 'xsmith_is-hole? x))))

(define-property io
  #:reads
  (grammar)
  (property reference-info)
  #:appends
  (att-rule _xsmith_effects/no-children) ;; effects directly caused by a node
  (att-rule _xsmith_effects) ;; effects caused by a node and its children
  (att-rule _xsmith_effect-constraints-for-child)
  (choice-rule _xsmith_no-io-conflict?)
  #:transformer
  (λ (this-prop-info grammar-info reference-info)
    (define nodes (dict-keys grammar-info))
    (define io-info (for/hash ([node nodes])
                      (values node
                              (syntax-parse (dict-ref this-prop-info node #'#f)
                                [b:boolean #'b]))))
    (define _xsmith_effects/no-children-info
      (for/hash ([n nodes])
        (define-values (read-or-write varname)
          (syntax-parse (dict-ref reference-info n #'#f)
            [prop:reference-info-class #:when (attribute prop.is-read?)
                                       (values #'effect-read-variable #'prop.field-name)]
            [prop:reference-info-class #:when (not (attribute prop.is-read?))
                                       (values #'effect-write-variable #'prop.field-name)]
            [#f (values #f #f)]))
        (values
         n
         #`(λ (n) (filter (λ(x)x)
                          (list (and #,(dict-ref io-info n) (effect-io))
                                #,(if read-or-write
                                      #`(#,read-or-write
                                         (att-value
                                          '_xsmith_resolve-reference-name
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
                                     (att-value '_xsmith_effects
                                                (binding-ast-node
                                                 (att-value
                                                  '_xsmith_resolve-reference-name
                                                  n
                                                  (ast-child '#,varname n)))))))))))
    (define _xsmith_effects-info
      ;; TODO - this is not node specific, but I think I want att-rule caching on it...
      (hash
       #f
       #`(λ (n)
           (remove-duplicates
            (flatten
             (cons
              (att-value '_xsmith_effects/no-children n)
              (for/list ([child (filter non-hole-node? (ast-children/flat n))])
                (att-value '_xsmith_effects child))))))))
    (define _xsmith_effect-constraints-for-child-info
      ;; TODO - this is not node specific, but I think I want att-rule caching on it...
      (hash
       #f
       #`(λ (n c)
           (define extended-family-constraints
             (if (ast-has-parent? n)
                 (att-value '_xsmith_effect-constraints-for-child (ast-parent n) n)
                 '()))
           (define lift-constraints
             (if (ast-has-child? 'xsmithlifterwrapped n)
                 (let ([lifter (ast-child 'xsmithlifterwrapped n)])
                   (if lifter
                       (att-value '_xsmith_effects (unbox lifter))
                       '()))
                 '()))
           (define direct-constraints
             (if (att-value '_xsmith_strict-child-order? n)
                 '()
                 (for/list ([sibling (filter non-hole-node? (ast-children/flat n))])
                   (if (eq? c sibling)
                       '()
                       (att-value '_xsmith_effects sibling)))))
           (remove-duplicates
            (flatten (cons lift-constraints
                           (cons extended-family-constraints
                                 direct-constraints)))))))
    (define _xsmith_no-io-conflict?-info
      (for/hash ([n nodes])
        (values
         n
         (syntax-parse (dict-ref io-info n)
           [#t #'(λ () (or (not (ast-has-parent? (current-hole)))
                           (not (memf effect-io?
                                      (att-value '_xsmith_effect-constraints-for-child
                                                 (ast-parent (current-hole))
                                                 (current-hole))))))]
           [#f #'(λ () #t)]))))
    (list _xsmith_effects/no-children-info
          _xsmith_effects-info
          _xsmith_effect-constraints-for-child-info
          _xsmith_no-io-conflict?-info)))

#|
There are two properties involved in pretty-printing:
 - print-node
 - print-hole

The `print-node` property allows users to specify functions for printing each type
of node. They may also give a default print function via #f.

Functions specified this way will be wrapped with a test to determine whether
the supplied argument is actually a hole. If it is, then `print-hole` will be
called instead.

These properties will be combined into a user-accessible function which can also
be used inside these function definitions, allowing the user to easily print
over recursively-specified data.
|#
(define-syntax-rule (print-node args ...)
  (att-value '_xsmith_print-node args ...))

(define-property print-node-info
  #:appends
  (att-rule _xsmith_print-node)
  #:transformer
  (λ (this-prop-info)
    (define _xsmith-print-node-info
      (for/hash ([(n v) (in-dict this-prop-info)])
        (values n
                v)))
    (list _xsmith-print-node-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
