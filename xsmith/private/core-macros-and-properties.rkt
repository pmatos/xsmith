#lang racket/base
(provide
 assemble-spec-components
 (all-from-out "core-properties.rkt")
 (except-out (all-from-out "grammar-macros.rkt")
             assemble-spec-components/core)
 )

(require
 "grammar-macros.rkt"
 "core-properties.rkt"
 syntax/parse/define
 (for-syntax
  racket/base
  ))

(define-syntax-parser assemble-spec-components
  [(_ spec:id
      (~optional (~seq #:properties (prop-name:id ...)))
      component ...)
   (with-syntax ([(prop-name ...) (or (attribute prop-name)
                                      '())])
     #'(assemble-spec-components/core
        spec
        (
         ;; Default properties
         depth-increase
         fresh
         choice-filters-to-apply
         may-be-generated
         choice-weight
         child-node-name-dict
         wont-over-deepen
         introduces-scope
         binder-info
         lift-predicate
         lift-type->ast-binder-type
         binding-structure
         io
         ;; user-added props
         prop-name ...
         )
        component ...))])

