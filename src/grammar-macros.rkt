#lang racket/base

(provide
 declare-spec
 add-to-grammar
 add-ag-rule
 add-choice-rule
 add-prop
 assemble-spec-parts
 current-xsmith-grammar
 define-property

 make-hole
 make-fresh-node

 (for-syntax
  prop-clause
  grammar-component
  grammar-clause

  grammar-clause->parent-chain
  grammar-node-name->field-info

  grammar-node-field-struct
  grammar-node-field-struct-name
  grammar-node-field-struct-type
  grammar-node-field-struct-kleene-star?
  grammar-node-field-struct-init-expr
  ))

(require
 syntax/parse/define
 racr
 racket/class
 racket/dict
 racket/list
 racket/string
 racket/stxparam
 racket/splicing
 "choice.rkt"
 "define-grammar-property.rkt"
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  racket/list
  racket/string
  racket/dict
  racket/match
  "grammar-properties.rkt"
  ))


(define-syntax-parameter current-xsmith-grammar
  (syntax-parser [stx (raise-syntax-error
                       'current-xsmith-grammar
                       "current-xsmith-grammar used without being parameterized"
                       #'stx)]))
(define-syntax-parameter current-xsmith-grammar-clauses
  (syntax-parser [stx (raise-syntax-error
                       'xsmith
                       "current-xsmith-grammar-clauses used without parameterization"
                       #'stx)]))

(define-syntax-parameter make-hole
  (syntax-parser [stx (raise-syntax-error
                       'make-hole
                       "Not in a context where make-hole is parameterized"
                       #'stx)]))
(define-syntax-parameter make-fresh-node
  (syntax-parser [stx (raise-syntax-error
                       'make-fresh-node
                       "Not in a context where make-fresh-node is parameterized"
                       #'stx)]))


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
                                     'props-info (hash))))))])



(begin-for-syntax
  (define-syntax-class prop-clause
    (pattern
     (prop-name:id (~and node-name (~or node-name-id:id #f)) prop-val:expr)))
  (define-syntax-class grammar-component
    (pattern
     (~or name:id
          [name:id (~optional (~seq (~datum :) type:id))
                   (~optional (~and (~datum *) kleene-star))
                   (~optional (~seq (~datum =) init-expr:expr))])))
  (define-syntax-class grammar-clause
    (pattern
     [node-name:id (~and parent (~or parent-name:id #f))
                   (component:grammar-component ...)]))

  (define (grammar-clause->parent-chain clause clause-hash)
    (syntax-parse clause
      [c:grammar-clause
       (define parent (and (attribute c.parent-name)
                           (syntax->datum (attribute c.parent-name))))
       (if parent
           (cons parent
                 (grammar-clause->parent-chain (dict-ref clause-hash parent)
                                               clause-hash))
           '())]))

  (struct grammar-node-field-struct
    (name type kleene-star? init-expr)
    #:transparent)

  (define (grammar-node-name->field-info name grammar-clause-hash)
    (define (name->field-info/direct name)
      (syntax-parse (dict-ref grammar-clause-hash name
                              (λ () (error 'xsmith
                                           "internal error - no field name in hash")))
        [gcl:grammar-clause
         (map (syntax-parser
                [gco:grammar-component
                 (define n (syntax->datum #'gco.name))
                 (define type (or (attribute gco.type)
                                  (and (dict-ref grammar-clause-hash n #f)
                                       n)))
                 (define kstar? (and (attribute gco.kleene-star) #t))
                 (when (and (equal? n type) kstar?)
                   (raise-syntax-error
                    #f
                    (format
                     "Nonterminal ~a in grammar includes field ~a that includes a kleene star but no name besides its type.  When the Kleene star is used, an explicit name is required."
                     name
                     n)
                    #'gco.name))
                 (grammar-node-field-struct
                  n
                  type
                  kstar?
                  (attribute gco.init-expr))])
              (syntax->list #'(gcl.component ...)))]))
    (define parent-list
      (grammar-clause->parent-chain (dict-ref grammar-clause-hash name)
                                    grammar-clause-hash))
    (define field-info-lists
      (map name->field-info/direct (append (reverse parent-list) (list name))))
    (define field-info
      (apply append field-info-lists))

    field-info)

  (define grammar-clauses-stx->clause-hash
    (syntax-parser [(c:grammar-clause ...)
                    (for/hash ([name (syntax->datum #'(c.node-name ...))]
                               [clause (syntax->list #'(c ...))])
                      (values name clause))]))

  (define (ag/cm-list->hash xs)
    ;; Makes a tiered hash from rule->node->val-stx
    (for/fold ([h (hash)])
              ([x xs])
      (syntax-parse x
        [pc:prop-clause
         (define new-rule-hash (dict-set
                                (dict-ref h (syntax->datum #'pc.prop-name) (hash))
                                (syntax->datum #'pc.node-name)
                                #'pc.prop-val))
         (dict-set h (syntax->datum #'pc.prop-name) new-rule-hash)])))

  (define ((prop-clause-false-to-default base-node-name) prop-clause-stx)
    (syntax-parse prop-clause-stx
      [pc:prop-clause
       (syntax-parse #'pc.node-name
         [#f #`(pc.prop-name #,base-node-name pc.prop-val)]
         [_ prop-clause-stx])]))

  (define (ast-node-name-stx->hole-name-stx n)
    ;; It would be nice to make the hole nodes hygienically named,
    ;; but they have to be passed to `make-ast-rule` encoded with
    ;; their parent and such, and the name will basically be taken
    ;; from the string representation of the symbol.
    ;; But it could be a gensym if all of the machinery for
    ;; creating holes or dealing with them is handled automatically
    ;; by macros from this file.
    (format-id n "~aHole" n))

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
       (format-id #f "~a:~a" #'gc.node-name (or (attribute gc.parent-name)
                                                (syntax->datum base-node-name-stx))))
     (define fields (map grammar-component->ast-rule-component-part
                         (syntax->list #'(gc.component ...))))
     (format-id #f "~a->~a"
                base-name
                (string-join fields "-"))]))

(define-for-syntax (get-ast-immediate-subtypes name-stx grammar-parts-stx)
  (syntax-parse grammar-parts-stx
    [(gc:grammar-clause ...)
     (for/fold ([subs '()])
               ([name+parent (map syntax->list
                                  (syntax->list #'([gc.node-name gc.parent] ...)))])
       (if (and (syntax->datum (cadr name+parent))
                (free-identifier=? (cadr name+parent)
                                   name-stx))
           (cons (car name+parent) subs)
           subs))]))

(define-for-syntax ({get-non-abstract-ast-subtypes grammar-parts-stx} name-stx)
  (define (get-subs/work done-subs work-subs)
    (if (null? work-subs)
        done-subs
        (get-subs/work (cons (car work-subs) done-subs)
                       (append (get-ast-immediate-subtypes (car work-subs)
                                                           grammar-parts-stx)
                               (cdr work-subs)))))
  (define (get-subs node)
    (get-subs/work '() (list node)))
  ;; TODO - filter abstracts
  (get-subs name-stx))

(define-for-syntax (stuff-export-hash export-hash-name subhash-key keys-stx infos-stx)
  (syntax-parse #`(#,keys-stx #,infos-stx #,export-hash-name #,subhash-key)
    [((key ...) (info ...) export-hash-name subhash-key)
     #'(begin-for-syntax
         (let* ([new-sub-hash
                 (for/fold ([phash (hash-ref export-hash-name subhash-key (hash))])
                           ([k '(key ...)]
                            [ifo (list (quote-syntax info) ...)])
                   (hash-set phash k (cons ifo (hash-ref phash k '()))))])
           (set! export-hash-name
                 (hash-set export-hash-name subhash-key new-sub-hash))))]))

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
      [(~and node-name (~or node-name-id:id #f)) prop:expr] ...+)
   (stuff-export-hash (spec->export-name #'grammar-name)
                      #'prop/ag/cm-type
                      #'((prop/ag/cm-name node-name) ...)
                      #'([prop/ag/cm-name node-name prop] ...))])

(define-syntax-parser add-ag-rule
  [(_ arg ...) #'(add-prop-generic 'ag-info arg ...)])
(define-syntax-parser add-choice-rule
  [(_ arg ...) #'(add-prop-generic 'cm-info arg ...)])

(define-syntax-parser add-prop
  [(_ arg ...) #'(add-prop-generic 'props-info arg ...)])

(define-for-syntax (spec-hash-merge part-hashes)
  (for/fold ([bighash (hash)])
            ([subhash-key '(grammar-info ag-info cm-info props-info)])
    (define subhash
      (for/fold ([h (hash)])
                ([p (map (λ (x) (hash-ref x subhash-key))
                         part-hashes)])
        (for/fold ([h h])
                  ([k (hash-keys p)])
          (hash-set h k (append (hash-ref p k)
                                (hash-ref h k '()))))))
    (hash-set bighash subhash-key subhash)))

(define-syntax-parser assemble-spec-parts
  [(_ spec
      (~optional (~seq #:properties (~and extra-props (prop-name:id ...))))
      require-path ...)
   (with-syntax ([(req-name ...) (map (λ (rp-stx)
                                         (format-id #'spec
                                                    "~a__~a"
                                                    (spec->export-name #'spec)
                                                    rp-stx))
                                       (syntax->datum #'(require-path ...)))]
                 [export-name-original (spec->export-name #'spec)]
                 [extra-props (or (attribute extra-props) #'())]
                 [next-macro (format-id #'spec "assemble-spec-parts-next_~a" #'spec)])
     ;; We now have the require specifications for the grammar parts as syntax,
     ;; and the easiest way to retrieve and use them is by having the output
     ;; of this macro include the require specifications as well as the
     ;; definition of a new macro that uses the new names required.
     ;; So the rest of the processing is done by the macro we define (and call)
     ;; in the output here.
     #'(begin
         (require (rename-in (only-in require-path export-name-original)
                             [export-name-original req-name]))
         ...
         ;; define a new macro to use the req-names...
         (define-syntax-parser assemble-spec-parts_stage2
           [(_ spec-name)
            (define parts (list req-name ...))
            (define combined (spec-hash-merge parts))
            (define (parts->stx key)
              (let ([phash (hash-ref combined key)])
                (datum->syntax #f (map (λ (k) (hash-ref phash k))
                                       (hash-keys phash)))))
            (define g-parts (parts->stx 'grammar-info))
            (define ag-parts (parts->stx 'ag-info))
            (define cm-parts (parts->stx 'cm-info))
            (define props-parts (parts->stx 'props-info))

            #`(assemble-spec-parts_stage3
               spec-name
               extra-props
               #,g-parts
               #,ag-parts
               #,cm-parts
               #,props-parts)])
         (assemble-spec-parts_stage2 spec)))])

(define-syntax-parser assemble-spec-parts_stage3
  ;; Check for duplicates, then run transformers
  [(_ spec
      extra-props
      (pre ... (g-part1:grammar-clause g-part2:grammar-clause c ...) post ...)
      ag-clauses
      cm-clauses
      prop-clauses)
   (raise-syntax-error #f "duplicate definitions for grammar clause"
                       #'g-part1 #f #'g-part2)]
  [(_ spec
      extra-props
      grammar-clauses
      (pre ... (ag1:prop-clause ag2:prop-clause c ...) post ...)
      cm-clauses
      prop-clauses)
   (raise-syntax-error #f "duplicate definitions for ag-rule"
                       #'ag1 #f #'ag2)]
  [(_ spec
      extra-props
      grammar-clauses
      ag-clauses
      (pre ... (cm1:prop-clause cm2:prop-clause c ...) post ...)
      prop-clauses)
   (raise-syntax-error #f "duplicate definitions for choice rule"
                       #'ag1 #f #'ag2)]
  [(_ spec
      extra-props
      ((g-part:grammar-clause) ...)
      ((ag-clause:prop-clause) ...)
      ((cm-clause:prop-clause) ...)
      ((p-clause+:prop-clause ...) ...))
   (define p-clauses (flatten (map syntax->list
                                   (syntax->list #'((p-clause+ ...) ...)))))
   (define (clause->list p-c-stx)
     (syntax-parse p-c-stx
       [p:prop-clause (list #'p.prop-name
                            #'p.node-name
                            #'p.prop-val)]))
   (define p-lists (map clause->list p-clauses))
   ;; I want one syntax object to point to for each property object.
   (define prop->prop-stx
     (for/fold ([h (for/fold ([h (hash)])
                             ([pl p-lists])
                     (dict-set h
                               (syntax-local-value
                                (car pl)
                                (λ ()
                                  (raise-syntax-error
                                   #f
                                   "Identifier not defined as a property."
                                   (car pl))))
                               (car pl)))])
               ([prop (syntax->list #'extra-props)])
       (dict-set h (syntax-local-value prop) prop)))
   (define starter-prop-hash (for/hash ([k (dict-keys prop->prop-stx)])
                               (values k (hash))))
   (define prop-hash-with-lists
     ;; a tiered hash from prop-struct->node-name->val-stx-list
     (for/fold ([h starter-prop-hash])
               ([pl p-lists])
       (match pl
         [(list prop-stx node-name-stx val-stx)
          (let* ([prop (syntax-local-value prop-stx)]
                 [node-hash (dict-ref h prop (hash))]
                 [node-name (syntax->datum node-name-stx)]
                 [current-node-name-list (dict-ref node-hash node-name '())])
            (dict-set h
                      prop
                      (dict-set node-hash
                                node-name
                                (cons val-stx current-node-name-list))))])))
   ;; Switch from a list of syntax objects to a syntax object wrapping a list.
   ;; For easier parsing on the receiving side.
   (define prop-hash
     (for/hash ([pk (dict-keys prop-hash-with-lists)])
       (define subhash (dict-ref prop-hash-with-lists pk))
       (values pk (for/hash ([nk (dict-keys subhash)])
                    (values nk (datum->syntax #f (dict-ref subhash nk)))))))

   (define g-parts (syntax->list #'(g-part ...)))
   ;; g-hash is a single-level hash node-name->node-spec-stx
   (define g-hash (for/hash ([g g-parts])
                    (syntax-parse g
                      [gc:grammar-clause (values (syntax->datum #'gc.node-name) g)])))

   (define ag-hash (ag/cm-list->hash (syntax->list #'(ag-clause ...))))
   (define cm-hash (ag/cm-list->hash (syntax->list #'(cm-clause ...))))

   (define prop-structs (sort (dict-keys prop-hash) grammar-property-less-than))
   (define pre-transform-infos-hash
     (hash 'ag-info ag-hash
           'cm-info cm-hash
           'grammar-info g-hash
           'props-info prop-hash))
   (define infos-hash
     (for/fold ([ih pre-transform-infos-hash])
               ([prop-struct prop-structs])
       (grammar-property-transform (hash-ref prop->prop-stx prop-struct prop-struct)
                                   ih)))
   ;; TODO - check duplicates again?  Other checks?
   (define (rule-hash->clause-list rules-hash)
     (for/fold ([clauses '()])
               ([rule-name (dict-keys rules-hash)])
       (define nodes-hash (dict-ref rules-hash rule-name))
       (append (for/list ([node-name (dict-keys nodes-hash)])
                 #`(#,(datum->syntax #'here rule-name)
                    #,(datum->syntax #'here node-name)
                    #,(dict-ref nodes-hash node-name)))
               clauses)))
   (define ag-prop-clauses
     (rule-hash->clause-list (dict-ref infos-hash 'ag-info)))
   (with-syntax ([(n-g-part ...) (dict-values (dict-ref infos-hash 'grammar-info))]
                 [(n-ag-clause ...) (rule-hash->clause-list
                                     (dict-ref infos-hash 'ag-info))]
                 [(n-cm-clause ...) (rule-hash->clause-list
                                     (dict-ref infos-hash 'cm-info))])
     #'(assemble-spec-parts_stage4
        spec
        (n-g-part ...)
        (n-ag-clause ...)
        (n-cm-clause ...)))])

(define-syntax-parser assemble-spec-parts_stage4
  ;; Sort the grammar clauses
  [(_ spec
      (g-part:grammar-clause ...)
      (ag-clause:prop-clause ...)
      (cm-clause:prop-clause ...))
   (define all-g-part-hash (grammar-clauses-stx->clause-hash #'(g-part ...)))
   (define (grammar-part-n-parents gp)
     (length (grammar-clause->parent-chain gp all-g-part-hash)))
   (with-syntax ([(g-part-sorted ...)
                  (sort (syntax->list #'(g-part ...))
                        <
                        #:key grammar-part-n-parents
                        #:cache-keys? #t)])
     #'(assemble-spec-parts_stage5
        spec
        (g-part-sorted ...)
        (ag-clause ...)
        (cm-clause ...)))])

(define-syntax-parser assemble-spec-parts_stage5
  ;; Assemble everything!
  ;; First we use with-syntax and syntax-parse to bind a bunch of names
  ;; that are needed in the template.  Then there is a giant template.
  [(_ spec
      (g-part:grammar-clause ...)
      (ag-clause:prop-clause ...)
      (cm-clause:prop-clause ...))
   (define (node->choice node-name-stx)
     (format-id #'here "~aChoice%" node-name-stx))

   (define grammar-hash
     (grammar-clauses-stx->clause-hash #'(g-part ...)))
   (define node-names
     (syntax->datum #'(g-part.node-name ...)))
   (define node-attribute-length-hash
     (for/hash ([node-name node-names])
       (values node-name
               (length (grammar-node-name->field-info
                        node-name
                        grammar-hash)))))
   (define choice-rule-name->node-name->rule-body
     (ag/cm-list->hash (syntax->list #'(cm-clause ...))))

   (with-syntax* ([base-node-name (format-id #'spec "BaseNode~a" #'spec)]
                  [base-node-choice (node->choice #'base-node-name)]
                  ;; Replace the ag-clauses with versions where
                  ;; #f node names are replaced with base-node-name
                  [(ag-clause ...)
                   (map (prop-clause-false-to-default #'base-node-name)
                        (syntax->list #'(ag-clause ...)))]
                  [fresh-node-func (format-id #'spec "~a-fresh-node" #'spec)]
                  [([subtype-name ...] ...)
                   (map {get-non-abstract-ast-subtypes #'(g-part ...)}
                        (syntax->list #'(g-part.node-name ...)))]
                  [([subtype-choice-name ...] ...)
                   (map (λ (subtypes-stx)
                          (map node->choice (syntax->list subtypes-stx)))
                        (syntax->list #'([subtype-name ...] ...)))]
                  [(ast-rule-sym ...) (map {make-ast-rule-id #'base-node-name}
                                           (syntax->list #'(g-part ...)))]
                  [(ast-hole-name ...)
                   (map ast-node-name-stx->hole-name-stx
                        (syntax->list #'(g-part.node-name ...)))]
                  [(ast-hole-rule-sym ...)
                   (map (syntax-parser
                          [(hole parent) (format-id #f "~a:~a->"
                                                    #'hole #'parent)])
                        (syntax->list #'([ast-hole-name g-part.node-name] ...)))]
                  [(node-attr-length ...)
                   (datum->syntax
                    #'here
                    (map (λ (x) (dict-ref node-attribute-length-hash x))
                         node-names))]
                  [base-node-spec (format-id #'spec "~a->" #'base-node-name)]
                  [(choice-name ...) (map node->choice
                                          (syntax->list #'(g-part.node-name ...)))]
                  [(choice-method-name ...) (remove-duplicates
                                             (syntax->datum
                                              #'(cm-clause.prop-name ...)))]
                  ;; When defining methods to the base choice for the grammar,
                  ;; I need to define them as public, *except* the ones in the
                  ;; choice base class, which I have to override.
                  [(cdef-pub-or-override-for-base ...)
                   (map (λ (name) (if (member (syntax->datum name)
                                              '(fresh
                                                choice-weight
                                                features))
                                      #'define/override
                                      #'define/public))
                        (syntax->list #'(choice-method-name ...)))]
                  [(cdef-body-for-base/default ...)
                   (map (λ (name)
                          (if (equal? (syntax->datum name) 'features)
                              #'(λ () (super features))
                              #`(λ args (error
                                         '#,name
                                         "no default implementation (called on ~a)"
                                         this))))
                        (syntax->list #'(choice-method-name ...)))]
                  [(choice-parent ...)
                   (map (syntax-parser [#f #'base-node-choice]
                                       [p (node->choice #'p)])
                        (syntax->list #'(g-part.parent ...)))]
                  [(ag-rule-name ...) (remove-duplicates
                                       (syntax->datum #'(ag-clause.prop-name ...)))]
                  [choice-hash-name (format-id #'spec "~a-choice-hash" #'spec)]
                  )
     ;; capture a couple names with syntax-parse (to have stx classes/attributes)
     (syntax-parse (map (λ (rule-name)
                          (filter (syntax-parser
                                    [c:prop-clause
                                     (equal? (syntax->datum #'c.prop-name)
                                             rule-name)])
                                  (syntax->list #'(ag-clause ...))))
                        (syntax->datum #'(ag-rule-name ...)))
       ;; ag-rule-node is now grouped by rule name
       [((ag-rule-node:prop-clause ...) ...)
        (syntax-parse (map (λ (node-name)
                             (filter (syntax-parser
                                       [c:prop-clause
                                        (equal? (syntax->datum #'c.node-name)
                                                (syntax->datum node-name))])
                                     (syntax->list #'(cm-clause ...))))
                           (syntax->list #'(base-node-name g-part.node-name ...)))
          ;; c-method is now grouped by node name
          [((base-node-c-method:prop-clause ...) (c-method:prop-clause ...) ...)
           (with-syntax*
             ([(cdef-body-for-base ...)
               ;; choice rule methods for the implicit parent node can be
               ;; specified but otherwise need to inherit a default.
               (map (λ (rule-name rule-default-impl)
                      (dict-ref (dict-ref choice-rule-name->node-name->rule-body
                                          rule-name)
                                #f
                                rule-default-impl))
                    (syntax->datum #'(choice-method-name ...))
                    (syntax->list #'(cdef-body-for-base/default ...)))])
             ;; Here finally is the master template
             #`(begin
                 (define spec (create-specification))
                 ;; This is set!-ed later, since it is defined inside the
                 ;; `with-specification` to see things there, but definitions
                 ;; in that scope are not available.
                 (define fresh-node-func #f)
                 (splicing-letrec
                     ;; The bindings in this splicing-letrec are needed for
                     ;; the `make-hole` macro
                     ([node-attr-length-hash
                       (make-immutable-hash
                        (list
                         (cons 'g-part.node-name 'node-attr-length)
                         ...))]
                      [hole-name-hash
                       (make-immutable-hash
                        (list
                         (cons 'g-part.node-name 'ast-hole-name)
                         ...))]
                      [make-hole-function
                       (λ (node-type)
                         ;; do a dict-ref here just for error checking.
                         (dict-ref hole-name-hash node-type
                                   (λ ()
                                     (error
                                      'make-hole
                                      "Not in the defined grammar: ~a, expected one of: ~a"
                                      node-type
                                      (dict-keys hole-name-hash))))
                         (create-ast
                          spec
                          (dict-ref hole-name-hash node-type)
                          (map (λ (x) (create-ast-bud))
                               (make-list (dict-ref node-attr-length-hash
                                                    node-type)
                                          #f))))])
                   (splicing-syntax-parameterize
                       ([current-xsmith-grammar (syntax-rules () [(_) spec])]
                        [make-hole (syntax-parser
                                     [(_ node-type-sym:expr)
                                      #'(make-hole-function node-type-sym)])])
                     (with-specification spec
                       ;; Define the grammar nodes
                       (ast-rule 'base-node-spec)
                       (ast-rule 'ast-rule-sym)
                       ...
                       (ast-rule 'ast-hole-rule-sym)
                       ...
                       (compile-ast-specifications 'base-node-name)

                       ;; Define the ag-rules for the grammar nodes
                       (splicing-syntax-parameterize
                           ([make-fresh-node
                             (syntax-parser [(_ node-sym:expr (~optional dict-expr:expr))
                                             #`(fresh-node-func
                                                node-sym
                                                #,(or (attribute dict-expr)
                                                      #'(hash)))])])
                         (ag-rule ag-rule-name
                                  [ag-rule-node.node-name ag-rule-node.prop-val]
                                  ...)
                         ...

                         ;; Define choice objects mirroring grammar.
                         ;; Choice rules are methods within the choice objects.
                         (define base-node-choice
                           (class ast-choice%
                             (cdef-pub-or-override-for-base
                              choice-method-name
                              cdef-body-for-base)
                             ...
                             (super-new)))
                         (define choice-name
                           (class choice-parent
                             (define c-method.prop-name
                               c-method.prop-val)
                             ...
                             (override c-method.prop-name)
                             ...
                             (super-new)))
                         ...)

                       (define choice-object-hash
                         (make-immutable-hash
                          (list
                           (cons 'g-part.node-name choice-name)
                           ...)))
                       (define (fresh-node-func-impl node-type [field-dict (hash)])
                         (dict-ref hole-name-hash node-type
                                   (λ ()
                                     (error
                                      'fresh-node-func
                                      "Not in the defined grammar: ~a, expected one of: ~a"
                                      node-type
                                      (dict-keys hole-name-hash))))
                         (send (new (dict-ref choice-object-hash node-type)
                                    [hole (make-hole node-type)])
                               fresh
                               field-dict))
                       ;; Since with-specification creates a new scope,
                       ;; fresh-node-func can't be defined here and visible
                       ;; outside.  So we `set!` it in place.
                       (set! fresh-node-func fresh-node-func-impl)

                       ;; define some core ag-rules
                       (ag-rule hole->choice-list
                                [base-node-name
                                 (λ (n) (error 'hole->choice-list
                                               "only implemented for grammar hole nodes"))]
                                [ast-hole-name
                                 (λ (n) (list (new subtype-choice-name [hole n]) ...))]
                                ...)
                       (ag-rule is-hole?
                                [base-node-name (λ (n) #f)]
                                [ast-hole-name (λ (n) #t)]
                                ...)
                       (ag-rule
                        hole->replacement
                        [base-node-name
                         (λ (n)
                           (if (att-value 'is-hole? n)
                               (let* ([choices (att-value 'hole->choice-list n)]
                                      [choices-or-reasons
                                       (map (λ (c) (send c apply-choice-filters))
                                            choices)]
                                      [filtered (filter (λ (x) (is-a? x ast-choice%))
                                                        choices-or-reasons)])
                                 (if (null? filtered)
                                     (error 'replace-hole
                                            (string-append
                                             "All choices for filling in a "
                                             (symbol->string (ast-node-type n))
                                             " hole were filtered out.\n"
                                             (string-join choices-or-reasons
                                                          "\n")))
                                     (send (choose-ast filtered) fresh)))
                               (error 'hole->replacement
                                      "called on non-hole node")))])
                       (compile-ag-specifications))))))])]))])


