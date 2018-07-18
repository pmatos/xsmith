#lang racket/base

(provide
 may-be-generated
 depth-increase-predicate
 )

(require
 "grammar-macros.rkt"
 "cish2-utils.rkt"
 racr
 racket/class
 (for-syntax
  racket/base
  syntax/parse
  racket/dict
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
                             (values (field-info-struct-name field)
                                     field)))
        (define field-names (map (λ (f) (field-info-struct-name f))
                                 fields))
        (with-syntax ([fresh-expr (dict-ref this-prop-info node #'(hash))]
                      [(field-name ...) (map (λ (fn) #`(quote #,fn)) field-names)])
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
                                       [init-e (field-info-struct-init-expr
                                                fstruct)]
                                       [field-type (field-info-struct-type
                                                    fstruct)]
                                       [seq? (field-info-struct-kleene-star?
                                              fstruct)])
                                  (cond
                                    [init-e
                                     ;; If the init value is a number and a list
                                     ;; of hole nodes is required, make an appropriate
                                     ;; list of that length.
                                     (with-syntax ([f-type
                                                    (datum->syntax #f field-type)])
                                       #`(let ([init-v #,init-e])
                                           (if (and 'f-type
                                                    (number? init-v))
                                               (create-ast-list
                                                (map (λ (x) (make-hole f-type))
                                                     (make-list init-v #f)))
                                               init-v)))]
                                    [seq? #'(create-ast-list (list))]
                                    [field-type #'(make-hole
                                                   #,(datum->syntax #f field-type))]
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
               (define all-values-in-order
                 (map (λ (name) (dict-ref all-values-hash name))
                      (list field-name ...)))

               (create-ast current-xsmith-grammar
                           '#,node
                           all-values-in-order))))))
    (list rule-info)))
