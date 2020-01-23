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
 render-node
 render-node-info
 render-hole
 render-hole-info
 render-bud
 render-bud-info

 make-lift-reference-choice-proc

 force-type-exploration-for-node!
 )

(module+ for-private
  (provide introduces-scope))

(require
 "grammar-macros.rkt"
 "xsmith-parameters.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
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
  #:appends (choice-rule _xsmith_fresh) (att-rule _xsmith_field-names)
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

    (define _xsmith_field-names-info
      (for/hash ([node nodes])
        (define fields (dict-ref field-info-hash node))
        (define field-names (map grammar-node-field-struct-name fields))
        (values
         node
         #`(λ (n) '(#,@(map (λ (name) (datum->syntax #f name)) field-names))))))

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
               (define all-values-hash/binder-sanitized
                 (if binder-type-field
                     (let* ([t (hash-ref all-values-hash
                                         binder-type-field)]
                            [concretized
                             (if (concrete-type? t)
                                 t
                                 (let ()
                                   (xd-printf
                                    "Concretizing binding ~a.  Type: ~v, "
                                    (hash-ref all-values-hash binder-name-field)
                                    t)
                                   (force-type-exploration-for-node!
                                    current-hole)
                                   (define ct
                                     (concretize-type t #:at-node current-hole))
                                   (xd-printf "concretized to: ~v\n" ct)
                                   (unify! ct t)
                                   ct))])
                       (hash-set all-values-hash
                                 binder-type-field
                                 concretized))
                     all-values-hash))
               (define all-values-hash/seq-transformed
                 (for/hash ([f-name (list field-name ...)]
                            [f-type (list field-type ...)]
                            [f-seq? (list field-seq? ...)])
                   (values
                    f-name
                    (let ([v (dict-ref all-values-hash/binder-sanitized f-name)])
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
    (list _xsmith_fresh-info _xsmith_field-names-info)))

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
    (xd-printf "lifting binding: ~v with type: ~v\n"
               lift-name
               type)
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
  #:appends
  (att-rule _xsmith_binder-type-field)
  (att-rule xsmith_definition-binding)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define name+type+d/p-hash
      (for/hash ([node (cons #f nodes)])
        (values node
                (syntax-parse (dict-ref this-prop-info node #f)
                  [#f #f]
                  [(name-field-name:id
                    type-field-name:id
                    (~and def-or-param (~or (~datum definition) (~datum parameter))))
                   (list #'name-field-name #'type-field-name #'def-or-param)]))))
    (define _xsmith_binder-type-field
      (for/hash ([node (cons #f nodes)])
        (cond [(dict-ref name+type+d/p-hash node #f)
               =>
               (λ (l) (values node #`(λ (n) '#,(second l))))]
              [else (values node #'(λ (n) #f))])))
    (define xsmith_definition-binding-info
      (for/fold ([rule-info (hash #f #'(λ (n) #f))])
                ([node nodes])
        (syntax-parse (dict-ref name+type+d/p-hash node #f)
          [#f rule-info]
          [(name-field-name type-field-name def-or-param)
           (dict-set rule-info node
                     #'(λ (n)
                         (let ([name (ast-child 'name-field-name n)]
                               [type (ast-child 'type-field-name n)])
                           (if (or (and (ast-node? type) (ast-bud-node? type))
                                   (and (ast-node? name) (ast-bud-node? name)))
                               #f
                               (begin
                                 (unify! type (att-value 'xsmith_type n))
                                 (binding name n type 'def-or-param))))))])))
    (list _xsmith_binder-type-field xsmith_definition-binding-info)))

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

(define (type-satisfaction-loop node-to-satisfy
                                build-type-thunk
                                type->use-type
                                failure-thunk
                                max-tries)
  (let loop ([count 0]
             [t (build-type-thunk)])
    (define satisfies? (can-unify-node-type-with-type?! node-to-satisfy t))
    (cond [satisfies? t]
          [(< max-tries count) (failure-thunk)]
          [else (loop (add1 count)
                      (build-type-thunk))])))

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
          (define my-choice-type-constraint (send self _xsmith_my-type-constraint))
          (define visibles-with-type
            (filter (λ (b) (and b
                                (concrete-type? (binding-type b))
                                ;; Sometimes a reference choice may have a stricter
                                ;; type requirement than the hole node.  So we ask
                                ;; if it can unify with the choice type constraint
                                ;; AND the actual hole.
                                (can-unify? (binding-type b)
                                            my-choice-type-constraint)
                                (can-unify-node-type-with-type?!
                                 hole
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

          (define lift-type
            (if (and (nominal-record-type? type-needed)
                     (not (nominal-record-type-name type-needed))
                     (let ([keys (dict-keys (nominal-record-type-inners type-needed))])
                       (and (not (null? keys)) (->bool (car keys)))))
                ;; In this case we have a defined nominal-record-type that we need to look up, rather than concretizing.
                ;; TODO - it would be nice if concretizing did this, but then I would have to change concretize-type to know about the hole and tree and whatnot.
                (let* ([d (nominal-record-definition-type type-needed)]
                       [def-filtered
                         (filter (λ (b) (and b
                                             (nominal-record-definition-type?
                                              (binding-type b))
                                             (can-unify? d (binding-type b))))
                                 visibles)])
                  (match def-filtered
                    [(list def)
                     (nominal-record-definition-type-type
                                 (binding-type def))]
                    [else
                     (error 'xsmith
                            "can't find a matching definition for nominal record type: ~v\n"
                            type-needed)]))
                (type-satisfaction-loop
                 hole
                 (λ () (concretize-type type-needed
                                        #:at-node hole))
                 (λ(x)x)
                 (λ() #f)
                 ;; TODO
                 ;; Right now we give up after some number of loops.
                 ;; Generally, this should just be an error.
                 ;; But for now there are cases (nominal-record-types) where there can be a valid reference but that we can't create a valid lift-type.
                 100)))
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
            (cond [(and (or (not lift-type)
                            (nominal-record-definition-type? lift-type))
                        (not (null? higher-order-effect-filtered)))
                   higher-order-effect-filtered]
                  [lift-type (cons (make-lift-reference-choice-proc
                                    hole
                                    lift-type)
                                   higher-order-effect-filtered)]
                  [else (error 'xsmith "When trying to generate a reference, there were no legal choices and xsmith couldn't generate a lifted reference for type: ~v\n (visibles: ~v\n)"
                               type-needed
                               visibles)]))
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

(define (xsmith_type-info-func node
                               reference-unify-target
                               reference-field
                               definition-type-field
                               definition-name-field
                               parameter?)
  #|
  Here we unify types we get from the various sources of typing info:
  * The type that a grammar node claims for itself
  * The type assigned by the parent node
  * The type annotated in the node for a definition

  Since we support subtypes, the unification must be a subtype unification.
  In particular, at any point in the tree, a subtype may be used for its supertype.

  Doing subtype unification here roughly corresponds to having a separate
  subtype rule (besides the user-supplied type rules) of the form:

  Γ ⊢ e : T_sub,   T_sub <: T_sup
  ———————————————————————————————
       Γ ⊢ e : T_sup

  Note that some types have different variances in their subtype relations.
  These variance rules are encoded in the `subtype-unify!` function.
  Eg. functions are subtypes when they have covariant return types and contravariant argument types, while boxes are invariant.

  As an example, imagine this subtree:
  (application (lambda ...) (argument ...))
  The application node must fulfil some type t1.
  The application then gives its children types: t2->t1 for the function and t2 for the argument, where t2 is a fresh type variable.
  However, the lambda node is free to be a subtype of t2->t1, say t2p->t1p, so we do subtype-unify here rather than symmetric-unify.
  The subtype relation machinery enforces that the t2p type is a SUPERTYPE of t2, while t1p is a subtype of t1.
  Similarly, the argument node is allowed to be a subtype of t2, t2q.

  An interesting case for this machinery is write references.
  For writes, the right-hand-side expression needs to be a subtype of the variable's type, and its relation to the return type is irrelevant.
  If the return is the variable itself (default behavior -- as in C assignment expressions), the variable needs to be a subtype of the return type.

  TODO - maybe allow users to set a flag to enable/disable subtyping in this way.
  |#
  (define binder-type-field (att-value '_xsmith_binder-type-field node))
  (define my-type-constraint
    (if (att-value 'xsmith_is-hole? node)
        (and binder-type-field
             (not (bud-node? (ast-child binder-type-field node)))
             (ast-child binder-type-field node))
        (att-value '_xsmith_my-type-constraint node)))
  (define my-type (or my-type-constraint (fresh-type-variable)))
  (define my-type-from-parent
    (att-value '_xsmith_type-constraint-from-parent node))
  (define (debug-print-1 t1 t2)
    (xd-printf "\n\n")
    (xd-printf "error while unifying types:\n~a\nand\n~a\n" t1 t2)
    (xd-printf "for node of AST type: ~a\n" (ast-node-type node))
    (xd-printf "with parent chain of AST types: ~v\n" (map ast-node-type
                                                           (ancestor-nodes node)))
    (xd-printf "(Note that type variables may have already been unified)\n"))
  (with-handlers
    ([(λ(x)#t)
      (λ (e)
        (debug-print-1 my-type-constraint my-type-from-parent)
        ;(xd-printf "error unifying my-type with my-type-constraint\n")
        (xd-printf "error subtype-unifying my-type to my-type-from-parent\n")
        (xd-printf "type-from-parent: ~v\n" my-type-from-parent)
        (xd-printf "my-type ~v\n" my-type)
        (xd-printf "my-type-constraint ~v\n" my-type-constraint)
        (raise e))])
    (if (and definition-type-field
             parameter?)
        ;; Parameters are a special case.  These aren't meant to be subtypes,
        ;; rather, they reflect the type annotation of the lambda term.
        (unify! my-type my-type-from-parent)
        (subtype-unify! my-type my-type-from-parent)))
  (when (and reference-field (not (att-value 'xsmith_is-hole? node)))
    (let* ([var-name (ast-child reference-field node)]
           [binding (att-value '_xsmith_resolve-reference-name
                               node
                               var-name)]
           [binding-node (binding-ast-node binding)]
           [binding-node-type (att-value 'xsmith_type binding-node)]
           [var-type (binding-type binding)])
      (with-handlers
        ([(λ(x)#t)
          (λ (e)
            (debug-print-1 var-type my-type-from-parent)
            (xd-printf "Error unifying types for reference of AST type: ~a\n"
                       (ast-node-type node))
            (xd-printf "Type constraint for this node: ~a\n"
                       my-type-constraint)
            (xd-printf "Type received from parent AST node: ~a\n"
                       my-type-from-parent)
            (xd-printf "Type annotated at variable definition: ~a\n"
                       var-type)
            (xd-printf "Variable name: ~a\n" var-name)
            (raise e))])
        (match reference-unify-target
          ;; If the reference-unify-target is not #t or #f, it still needs to be
          ;; unified. However, unifying here will cause a cycle. Instead, this
          ;; is handled in the type-info property definition.
          ;; Note that we symmetrically unify here.
          ;; We do this to ensure that for writes, the RHS doesn't end up being a
          ;; different, incompatible subtype from the variable subtype.
          ;; The reference node is already related by subtyping to the type
          ;; assigned by its parent.  So we already have the flexibility of subtyping.
          [#t (unify! var-type my-type)]
          [else (void)]))
      ;; This shouldn't be necessary, but something is going wrong,
      ;; so I'll give a chance to get this error message.
      (with-handlers
        ([(λ(x)#t)
          (λ (e)
            (debug-print-1 binding-node-type my-type-from-parent)
            (xd-printf "Error unifying types for reference of AST type: ~a\n"
                       (ast-node-type node))
            (xd-printf "Type in scope graph and type annotated at variable definition differ.  This shouldn't happen.\n")
            (xd-printf "Type annotated at variable definition: ~a\n"
                       binding-node-type)
            (xd-printf "Type that was recorded in scope graph: ~a\n"
                       var-type)
            (xd-printf "Variable name: ~a\n" var-name)
            (raise e))])
        (unify! var-type binding-node-type)
        )))
  (when definition-type-field
    (let ([def-type (ast-child definition-type-field node)])
      (when (and (not (att-value 'xsmith_is-hole? node))
                 (not (type? def-type)))
        (xd-printf "WARNING: definition node type field has non-type value: ~v\n"
                   def-type))
      (when (type? def-type)
        ;; TODO - in my existing fuzzers this is sometimes not set for parameters, but it should be... I'm just not sure about the timing of setting it all up right now...
        (with-handlers
          ([(λ(x)#t)
            (λ (e)
              (debug-print-1 def-type my-type-from-parent)
              (xd-printf "Error unifying definition type recorded in definition field.\n")
              (xd-printf "binding name: ~a\n" (ast-child definition-name-field node))
              (xd-printf "Type of this node: ~v\n" my-type)
              (xd-printf "Type constraint on this node: ~v\n" my-type-constraint)
              (xd-printf "Type from parent: ~v\n" my-type-from-parent)
              (xd-printf "Recorded definition type ~v\n" def-type)
              (xd-printf "Parent type: ~v\n\n"
                         (and (parent-node node)
                              (att-value 'xsmith_type (parent-node node))))
              (raise e))])
          (unify! def-type my-type)))))
  my-type)

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
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ () default-base-type))
          (for/hash ([n (dict-keys constraints-checked)])
            (values n #`(λ (arg-ignored) #,(dict-ref constraints-checked n))))))
    (define _xsmith_my-type-constraint-info/choice-rule
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ () default-base-type))
          (for/hash ([n (dict-keys constraints-checked)])
            (values n #`(λ () #,(dict-ref constraints-checked n))))))

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

    ;; TODO - I should clean this up by making a syntax class to parse the binder-info property and get this info more easily.
    (define binder-type-field
      (for/hash ([n nodes])
        (values n (syntax-parse (dict-ref binder-info-info n #'#f)
                    [(name-field-name type-field-name def/param)
                     #''type-field-name]
                    [else #'#f]))))
    (define binder-name-field
      (for/hash ([n nodes])
        (values n (syntax-parse (dict-ref binder-info-info n #'#f)
                    [(name-field-name type-field-name def/param)
                     #''name-field-name]
                    [else #'#f]))))
    (define parameter?-hash
      (for/hash ([n nodes])
        (values n (syntax-parse (dict-ref binder-info-info n #'#f)
                    [(name-field-name type-field-name (~datum parameter))
                     #'#t]
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
                        ;; This is the case that we can't handle in
                        ;; xsmith_type-info-func to avoid a cycle.
                        ;; Note that we symmetrically unify, to ensure that
                        ;; the RHS (which can be a subtype of the parent assignment)
                        ;; isn't a different, incompatible subtype than the LHS.
                        (unify!
                         (binding-type (att-value '_xsmith_resolve-reference-name
                                                  node
                                                  (ast-child #,(dict-ref node-reference-field n) node)))
                         (get-value-from-parent-dict child-types reference-unify-target
                                                     (λ () (error 'type-info
                                                                  "No type given for field ~a"
                                                                  reference-unify-target)))
                         ))
                      child-types))))
    (define _xsmith_type-constraint-from-parent-info
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ (node) default-base-type))
          (for/hash ([n nodes])
            (values n #`(λ (node) (_xsmith_type-constraint-from-parent-func
                                   node
                                   (quote #,n)))))))
    (define xsmith_type-info
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ (node) default-base-type))
          (for/hash ([n nodes])
            (values n #`(λ (node)
                          (xsmith_type-info-func
                           node
                           #,(dict-ref node-reference-unify-target n)
                           #,(dict-ref node-reference-field n)
                           #,(dict-ref binder-type-field n)
                           #,(dict-ref binder-name-field n)
                           #,(dict-ref parameter?-hash n)))))))
    (define _xsmith_satisfies-type-constraint?-info
      (hash #f #'(λ ()
                   #;(eprintf "testing type for ~a\n" this)
                   (can-unify-node-type-with-type?!
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

(define (can-unify-node-type-with-type?! node-in-question type-constraint
                                         #:break-when-more-concrete?
                                         [break-when-more-concrete? #t])
  #|
  We need to call `can-unify?`, but we do type checking lazily.
  This means that the node type may need to unify with a cousin node's type
  to get all of its constraints, and `can-unify` may give us the wrong answer
  if we haven't done that unification.

  So we need to walk some of the tree to unify.  But we don't want to walk the
  whole tree.  So we check as we go whether the type is sufficiently concrete
  to always give a correct answer, and break the loop when it is.

  We start by going to sibling nodes, and when any type shares variables with
  the node-in-question type, we recur down its subtree as far as variables are shared.
  After each sibling we go up the parent chain and repeat.
  |#
  (when (not (ast-node? node-in-question))
    (error 'can-unify-node-type-with-type?!
           "given non-node value: ~v" node-in-question))
  (when (not (type? type-constraint))
    (error 'can-unify-node-type-with-type?!
           "given non-type value: ~v" type-constraint))
  ;; The name hole-type is now wrong, given that it's now a predicate for arbitrary nodes.  But I'm leaving it.
  (define hole-type (att-value 'xsmith_type node-in-question))
  (define hole? (att-value 'xsmith_is-hole? node-in-question))

  ;;; Begin traversal
  (define maybe-can-unify?
    (let/cc break!!
      (define binding-nodes-started '())
      (define binding-nodes-finished '())
      (define parent-nodes-done '())
      (define (relevant? other-type)
        (contains-type-variables? other-type
                                  (type->type-variable-list hole-type)))

      (define (break?!)
        ;; TODO - right now removing breaks fixes issues.  Probably I need to re-think the conditions under which I can terminate early with subtype unification rather than symmetric unification.
        (when (concrete-type? hole-type)
          (break!! #t))
        (when (not (can-unify? hole-type type-constraint))
          (break!! #f))
        (when (and break-when-more-concrete?
                   (at-least-as-concrete hole-type type-constraint))
            (break!! #t)))
      (break?!)
      (let parent-loop ([p (parent-node node-in-question)]
                        [child node-in-question])
        (define (resolve-types node)
          (match node
            [(? (λ (n) (not (ast-node? n)))) (void)]
            [(? ast-list-node?) (for-each resolve-types (ast-children node))]
            [(? ast-bud-node?) (void)]
            [else (att-value 'xsmith_type node)]))
        (define (sibling-loop nodes)
          (for ([n nodes]) (resolve-types n))
          ;; When we check the type of a new thing it may unify variables,
          ;; so we've maybe made progress.
          (break?!)
          (define (rec nodes)
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
                         ;; If the node is a binder, mark it so we don't look at it
                         ;; repeatedly when we hit references to it.
                         (when (att-value 'xsmith_definition-binding n)
                           (set! binding-nodes-finished
                                 (cons n binding-nodes-finished))
                           (parent-loop (ast-parent n) n))

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
                         (when (relevant? n-type)
                           (sibling-loop (ast-children n)))
                         (rec ns)]))))
          (rec nodes))
        (when (not (memq p parent-nodes-done))
          (set! parent-nodes-done (cons p parent-nodes-done))
          (and p (att-value 'xsmith_type p))
          (when (and (eq? node-in-question child) (not hole?))
            ;; IE this is the first iteration.
            ;; The children of the original node may have relevant data that they
            ;; add to the parent.
            (sibling-loop (ast-children node-in-question)))
          (and p (sibling-loop (ast-children p)))
          (when (and p
                     (ast-has-parent? p)
                     (or
                      ;; If the current node (child) includes relevant variables,
                      ;; its siblings may too even if the parent doesn't.
                      (relevant? (att-value 'xsmith_type child))
                      ;; If the parent includes relevant variables its siblings
                      ;; or ancestors might as well.
                      (relevant? (att-value 'xsmith_type p))))
            (parent-loop (parent-node p) p))))))
  ;;; End traversal

  ;; The hole type is now either maximally unified or sufficiently concrete
  ;; that no more unification can change the result of this predicate.
  (and maybe-can-unify?
       (can-unify? hole-type type-constraint)))

(define (force-type-exploration-for-node! node)
  (can-unify-node-type-with-type?! node (fresh-type-variable)
                                   #:break-when-more-concrete? #f))


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
There are a few properties involved in rendering ASTs for pretty-printing:
 - render-node
 - render-hole
 - render-bud

The `render-node` property allows users to specify functions for rendering each
type of node. They may also give a default render function via #f. These can
return any type, but if the type is not a string then the user should specify
the `#:format-render` argument in the `xsmith-command-line` function to handle
converting the rendered output to a string for printing.

Functions specified this way will be wrapped with a test to determine the type
of the supplied argument. If it's actually a hole, then `render-hole` will be
called instead. If it's an AST bud node, then `render-bud` will be called.

Users can call `(render-node <node>)`, `(render-hole <hole>)`, or
`(render-bud <bud>)` instead of the longer-winded
`(att-value 'render-node-info <node>)`-style calls.
|#
(define (render-node node)
  (when (not (ast-node? node))
    (error "render-node received object which is not a RACR AST node:" node))
  (cond
    [(ast-bud-node? node)
     (render-bud node)]
    [(att-value 'xsmith_is-hole? node)
     (render-hole node)]
    [else
     (att-value '_xsmith_render-node node)]))

(define-property render-node-info
  #:appends
  (att-rule _xsmith_render-node)
  #:transformer
  (λ (this-prop-info)
    (define _xsmith_render-node-info
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ (n) (symbol->string (ast-node-type n))))
          (for/hash ([(n v) (in-dict this-prop-info)])
            (values n
                    v))))
    (list _xsmith_render-node-info)))

(define (render-hole hole)
  (att-value '_xsmith_render-hole hole))

(define-property render-hole-info
  #:appends
  (att-rule _xsmith_render-hole)
  #:transformer
  (λ (this-prop-info)
    (define _xsmith_render-hole-info
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ (h) (format "<~a>"
                                    (symbol->string (ast-node-type h)))))
          (for/hash ([(n v) (in-dict this-prop-info)])
            (values n
                    v))))
    (list _xsmith_render-hole-info)))

(define (render-bud bud)
  (att-value '_xsmith_render-bud bud))

(define-property render-bud-info
  #:appends
  (att-rule _xsmith_render-bud)
  #:transformer
  (λ (this-prop-info)
    (define _xsmith_render-bud-info
      (if (dict-empty? this-prop-info)
          (hash #f #'(λ (b) "[[BUD]]"))
          (for/hash ([(n v) (in-dict this-prop-info)])
            (values n
                    v))))
    (list _xsmith_render-bud-info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
