#lang racket/base

(provide
 declare-spec
 add-to-grammar
 add-ag
 add-cm
 assemble-spec-parts
 )

(require
 syntax/parse/define
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  racket/list
  racket/string
  ))


(define-for-syntax (spec->export-name spec-name-stx)
  (format-id spec-name-stx "%%~a-grammar-info-hash" spec-name-stx))

;; This will export a grammar-info-hash, and the other macros in this module will edit the exported hash.
(define-syntax-parser declare-spec
  [(_ spec-name:identifier)
   (with-syntax ([export-name (spec->export-name #'spec-name)])
     #'(begin
         ;; TODO - should the provide be in a submodule?
         (provide (for-syntax export-name))
         (begin-for-syntax
           (define export-name (hash 'grammar-info (hash)
                                     'ag-info (hash)
                                     'cm-info (hash)
                                     'prop-infos (hash))))))])


(define-for-syntax (stuff-export-hash export-hash-name subhash-key keys-stx infos-stx)
  (syntax-parse #`(#,keys-stx #,infos-stx #,export-hash-name #,subhash-key)
    [((key ...) (info ...) export-hash-name subhash-key)
     #'(begin-for-syntax
         (let* ([new-sub-hash
                 (for/fold ([phash (hash-ref export-hash-name subhash-key (hash))])
                           ([k '(key ...)]
                            [ifo (list (quote-syntax (info ...)))])
                   ;; TODO - check duplicates
                   (hash-set phash k ifo))])
           (set! export-hash-name
                 (hash-set export-hash-name subhash-key new-sub-hash))))]))


(begin-for-syntax
  (define-syntax-class grammar-component
    (pattern
     (~or name:id
          [name:id (~optional (~seq (~datum :) type:id))
                   (~optional (~and (~datum *) kleene-star))
                   (~optional (~seq (~datum =) init-val:expr))])))
  (define-syntax-class grammar-clause
    (pattern
     [node-name:id (~and parent (~or parent-name:id #f))
                   (component:grammar-component ...)]))
  (define-syntax-class prop-clause
    (pattern
     (node-name:id prop-name:id prop-val:expr)))
  )

(define-for-syntax (grammar-component->ast-rule-component-part gcomp-stx)
  (syntax-parse gcomp-stx
    [gc:grammar-component
     (symbol->string
      (syntax->datum
       (if (attribute gc.type)
           (format-id #f "~a~a<~a"
                      #'gc.type
                      (or (attribute gc.kleene-star) "")
                      #'gc.name)
           (format-id #f "~a~a"
                      #'gc.name
                      (or (attribute gc.kleene-star) "")))))]))

(define-for-syntax ({make-ast-rule-id base-node-name-stx} grammar-part-stx)
  ;; make the (ast-rule 'NAME:PARENT->field<name-field*<name...) symbols
  (syntax-parse grammar-part-stx
    [gc:grammar-clause
     (define base-name
       (format "~a:~a" #'gc.node-name (or (attribute gc.parent-name)
                                          (syntax->datum base-node-name-stx))))
     (define fields (map grammar-component->ast-rule-component-part
                         (syntax->list #'(gc.component ...))))
     (format-id #f "~a->~a"
                base-name
                (string-join fields "-"))]))

(define-syntax-parser add-to-grammar
  [(_ grammar-name:id
      clause:grammar-clause
      ;; TODO - each node in the grammar should be able to add in-line properties here
      ...)
   (stuff-export-hash (spec->export-name #'grammar-name)
                      #''grammar-info
                      #'((clause.node-name grammar-name) ...)
                      #'(clause ...))])

(define-syntax-parser add-prop-generic
  [(_ prop/ag/cm-type
      grammar-name:id
      prop/ag/cm-name:id
      [node-name:id prop:expr] ...+)
   (stuff-export-hash (spec->export-name #'grammar-name)
                      #'prop/ag/cm-type
                      #'((node-name prop/ag/cm-name) ...)
                      #'([node-name prop/ag/cm-name prop] ...))])

(define-syntax-parser add-ag
  [(_ arg ...) #'(add-prop-generic 'ag-info arg ...)])
(define-syntax-parser add-cm
  [(_ arg ...) #'(add-prop-generic 'cm-info arg ...)])

;; TODO - maybe add-prop should be different -- I could have hygienic names for properties, for example...
#;(define-syntax-parser add-prop
  [(_ arg ...) #'(add-prop-generic 'prop-infos arg ...)])


(define-syntax-parser assemble-spec-parts
  [(_ spec require-path ...)
   (eprintf "starting stage 1 ...\n")
   (with-syntax ([(req-name ...) (map (λ (rp-stx)
                                         (format-id #'spec
                                                    "~a__~a"
                                                    (spec->export-name #'spec)
                                                    rp-stx))
                                       (syntax->datum #'(require-path ...)))]
                 [export-name-original (spec->export-name #'spec)]
                 [next-macro (format-id #'spec "assemble-spec-parts-next_~a" #'spec)])
     #'(begin
         (require (rename-in (only-in require-path export-name-original)
                             [export-name-original req-name]))
         ...
         ;; define a new macro to use the req-names...
         (define-syntax-parser assemble-spec-parts_stage2
           [(_)
            (eprintf "starting stage 2 ...\n")
            (define parts (list req-name ...))
            (define combined (spec-hash-merge parts))
            (define (parts->stx key)
              (let ([phash (hash-ref combined key)])
                (datum->syntax #f (map (λ (k) (hash-ref phash k))
                                       (hash-keys phash)))))
            (define g-parts (parts->stx 'grammar-info))
            (define ag-parts (parts->stx 'ag-info))
            (define cm-parts (parts->stx 'cm-info))

            #`(assemble-spec-parts_stage3
               spec
               #,g-parts
               #,ag-parts
               #,cm-parts)])
         (assemble-spec-parts_stage2)))])

(define-syntax-parser assemble-spec-parts_stage3
  [(_ spec
      (g-part:grammar-clause ...)
      (ag-clause:prop-clause ...)
      (cm-clause:prop-clause ...))
   (eprintf "starting stage 3 ...\n")
   (define (node->choice node-name-stx)
     (format-id node-name-stx "~aChoice%" node-name-stx))
   (define base-node-name (format-id #'spec "BaseNode~a"))
   (with-syntax* ([(ast-rule-sym ...) (map {make-ast-rule-id base-node-name}
                                           (syntax->list #'(g-part ...)))]
                  [base-node-spec (format-id #'spec "~a->" base-node-name)]
                  [base-node-choice (node->choice base-node-name)]
                  [(choice-name ...) (map node->choice
                                          (syntax->list #'(g-part.node-name ...)))]
                  [(choice-parent ...)
                   (map (syntax-parser [#f #'base-node-choice]
                                       [p (node->choice #'p)])
                        (syntax->list #'(g-part.parent ...)))]
                  [(ag-rule-name ...) (remove-duplicates
                                       (syntax->datum #'(ag-clause.prop-name ...)))]
                  )
     ;; capture a couple names with syntax-parse (to have stx classes/attributes)
     (syntax-parse (map (λ (rule-name)
                          (filter (syntax-parser
                                    [c:prop-clause
                                     (equal? (syntax->datum #'c.prop-name)
                                             rule-name)])
                                  (syntax->list #'(ag-clause ...))))
                        (syntax->datum #'(ag-rule-name ...)))
       [((ag-rule-node:prop-clause ...) ...)
        (syntax-parse (map (λ (node-name)
                             (filter (syntax-parser
                                       [c:prop-clause
                                        (equal? (syntax->datum #'c.node-name)
                                                (syntax->datum node-name))])
                                     (syntax->list #'(cm-clause ...))))
                           (syntax->list #'(g-part.node-name ...)))
          [((c-method:prop-clause ...) ...)
           #`(begin
               ;; Define RACR spec and ag-rules
               (define spec (create-specification))
               (with-specification spec
                 (ast-rule 'base-node-spec)
                 (ast-rule 'ast-rule-sym)
                 ...
                 ;; TODO - define hole nodes
                 (compile-ast-specifications 'Node)
                 (ag-rule ag-rule-name
                          [ag-rule-node.node-name ag-rule-node.prop-val]
                          ...)
                 ...
                 (compile-ag-specifications))

               ;; Define choice objects mirroring grammar
               (define base-node-choice
                 (class ast-choice%
                   (super-new)))
               (define choice-name
                 (class choice-parent
                   (define c-method.prop-name
                     c-method.prop-val)
                   ...
                   (super-new)))
               ...

               ;; TODO - fresh method for free -- use default init values and types from grammar definition
               ;; TODO - other ag-rules and choice-methods for free

               ;; TODO - define choice% lists (IE for a hole of type X make a list of ponential choice objects)

               ;; TODO - add default erroring case to every ag-rule (on the base node)
               )])]))])
