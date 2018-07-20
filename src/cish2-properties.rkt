#lang racket/base

(provide
 may-be-generated
 depth-increase-predicate
 fresh
 wont-over-deepen
 )

(require
 "grammar-macros.rkt"
 "choice.rkt"
 "cish2-utils.rkt"
 "xsmith-options.rkt"
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

(define-syntax (define-non-inheriting-rule-property stx)
  (define-syntax-class rule-type
    (pattern (~or (~datum choice-rule) (~datum ag-rule))))
  (syntax-parse stx
    [(_ property-name:id
        rt:rule-type
        (~or
         (~optional (~seq #:rule-name rule-name:id))
         (~once (~seq #:default default-value:expr))
         (~optional (~seq #:transformer value-transformer:expr))
         )
        ...)
     (with-syntax ([transformer (or (attribute value-transformer) #'(λ (x) x))]
                   [rule-name (or (attribute rule-name) #'property-name)])
       #'(define-property property-name
           #:reads (grammar)
           #:appends (rt rule-name)
           #:transformer
           (λ (this-prop-info grammar-info)
             (define rule-info
               (for/hash ([node-name (dict-keys grammar-info)])
                 (define prop-vals
                   (dict-ref this-prop-info node-name #f))
                 (values node-name
                         (transformer
                          (if prop-vals
                              (syntax-parse prop-vals
                                [(a) #'a]
                                [(a b ...+)
                                 (raise-syntax-error
                                  #f
                                  (format
                                   "duplicate definition of ~a property for node ~a."
                                   'property-name
                                   node-name)
                                  #'a)])
                              (quote-syntax default-value))))))
             (list rule-info))))]))

(define-non-inheriting-rule-property
  may-be-generated
  choice-rule
  #:rule-name may-be-generated-method
  #:default #t
  #:transformer (syntax-parser [#t #'(λ () this)]
                               [#f #'(λ () #f)]))

(define-non-inheriting-rule-property
  depth-increase-predicate
  ag-rule
  #:rule-name ast-depth
  #:default (λ (n) #t)
  #:transformer (syntax-parser
                  [pred:expr
                   #'(λ (n)
                       (cond [(pred n) (let ([p (parent-node n)])
                                         (if p
                                             (add1 (att-value 'ast-depth p))
                                             0))]
                             [else (att-value 'ast-depth (parent-node n))]))]))


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
  #:appends (choice-rule fresh)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info node-name grammar-info))))
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
          (syntax->list (dict-ref this-prop-info node #'((hash)))))
        (when (> (length prop-for-this-node) 1)
          (raise-syntax-error 'fresh
                              (format
                               "duplicate definition of fresh property for node ~a"
                               node)
                              (car prop-for-this-node)))
        (with-syntax ([fresh-expr (car prop-for-this-node)]
                      [node-name node]
                      [(field-name ...) (map sym->quoted-sym-stx field-names)]
                      [(field-type ...) (map sym->quoted-sym-stx field-types)]
                      [(field-seq? ...) (map sym->quoted-sym-stx field-seq?s)])
          (values
           node
           #`(λ ()
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
               (define given-values fresh-expr)
               (define all-values-hash
                 (for/hash ([f-name (list field-name ...)])
                   (values
                    f-name
                    (dict-ref given-values
                              f-name
                              ((dict-ref thunk-hash f-name))))))
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
                          (create-ast-list
                           (map (if f-type
                                    (λ (x) (make-hole f-type))
                                    (λ (x) x))
                                (make-list v #f)))
                          v)))))
               (define all-values-in-order
                 (map (λ (name) (dict-ref all-values-hash/num-transformed name))
                      (list field-name ...)))

               (create-ast (current-xsmith-grammar)
                           '#,node
                           all-values-in-order))))))
    (list rule-info)))

(define-property wont-over-deepen
  #:reads (grammar)
  #:appends (choice-rule wont-over-deepen)
  #:transformer
  (λ (this-prop-info grammar-info)
    (define nodes (dict-keys grammar-info))
    (define field-info-hash
      (for/hash ([node-name nodes])
        (values node-name
                (grammar-node-name->field-info node-name grammar-info))))
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
                        #,(syntax-parse (dict-ref
                                         this-prop-info
                                         node
                                         #`(#,(dict-ref rule-info-defaults node)))
                            [(a) #'a]
                            [(a b ...) (raise-syntax-error
                                        #f
                                        (format
                                         "Multiple definitions of property for node ~a"
                                         node)
                                        #'a)]))))))
    (list rule-info)))
