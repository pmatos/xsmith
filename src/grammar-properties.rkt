#lang racket/base

;; struct properties and syntax classes for defining and detecting property transformers

(provide
 (struct-out grammar-property)
 grammar-property-less-than
 grammar-property-transform

 ;; syntax classes
 grammar-property-stx
 property-arg
 property-arg-property
 property-arg-ag-rule
 property-arg-choice-rule
 property-arg-grammar

 )

(require
 syntax/parse
 racket/dict
 racket/list
 )


(define-syntax-class grammar-property-stx
  (pattern gp:id
           #:when (grammar-property? (syntax-local-value #'gp (λ () #f)))))

(define-syntax-class property-arg-property
  (pattern ((~datum property) name:id)))
(define-syntax-class property-arg-ag-rule
  (pattern ((~datum ag-rule) name:id)))
(define-syntax-class property-arg-choice-rule
  (pattern ((~datum choice-rule) name:id)))
(define-syntax-class property-arg-grammar
  (pattern (~and ((~datum grammar)) grammar-flag)))
(define-syntax-class property-arg
  #:datum-literals (property ag-rule choice-rule grammar)
  (pattern (~or prop:property-arg-property
                ag:property-arg-ag-rule
                cm:property-arg-choice-rule
                gram:property-arg-grammar)))

;;; For sorting properties to run in an appropriate order.
;;; A p1 is less than p2 if p1 rewrites/appends to p2, or if
;;; p2 reads (but does not rewrite) p1.
(define (grammar-property-less-than p1 p2)
  (or
   ;; writes?
   (for/or ([write-target (append (grammar-property-rewrites p1)
                                  (grammar-property-appends p1))])
     (syntax-parse write-target
       [p:property-arg-property
        (equal? p2 (syntax-local-value #'p.name))]
       [_ #f]))
   ;; is-read?
   (for/or ([read-target (grammar-property-reads p2)])
     (syntax-parse read-target
       [p:property-arg-property
        (equal? p1 (syntax-local-value #'p.name))]
       [_ #f]))))


;;; Run the transformer for a grammar property.
(define (grammar-property-transform grammar-prop-name-stx
                                    infos-hash)
  (define (infos->section infos-hash pa)
    (syntax-parse pa
      [p:property-arg-property (hash-ref (hash-ref infos-hash 'props-info)
                                         (syntax-local-value #'p.name)
                                         (hash))]
      [p:property-arg-ag-rule (hash-ref (hash-ref infos-hash 'ag-info)
                                        (syntax->datum #'p.name)
                                        (hash))]
      [p:property-arg-choice-rule (hash-ref (hash-ref infos-hash 'cm-info)
                                            (syntax->datum #'p.name)
                                            (hash))]
      [p:property-arg-grammar (hash-ref infos-hash 'grammar-info)]))

  (define (section->infos prop-arg new-hash infos append?)
    (define (ag/cm-branch ag/cm-name-stx ag/cm-flag)
      (define props-hash (hash-ref infos ag/cm-flag))
      (define this-prop-hash
        (hash-ref props-hash (syntax->datum ag/cm-name-stx) (hash)))
      (if append?
          (hash-set infos ag/cm-flag
                    (hash-set props-hash
                              (syntax->datum ag/cm-name-stx)
                              (for/fold ([combined this-prop-hash])
                                        ([k (dict-keys new-hash)])
                                (when (dict-ref combined k #f)
                                  (raise-syntax-error 'grammar-property-transform
                                                      "duplicate rule"
                                                      ag/cm-name-stx))
                                (define new-val (dict-ref new-hash k))
                                ;; TODO - Do some error checking on new-val.
                                ;;        It should be a valid syntax object
                                ;;        for a rule right-hand-side.
                                (hash-set combined k new-val))))
          (raise-syntax-error 'grammar-property-transform
                              "rewrite is not supported for ag-rules or choice-rules"
                              grammar-prop-name-stx)))
    (syntax-parse prop-arg
      [p:property-arg-property
       (define props-hash (hash-ref infos 'props-info))
       (define this-prop-hash
         (hash-ref props-hash (syntax-local-value #'p.name) (hash)))
       (hash-set infos 'props-info
                 (hash-set props-hash
                           (syntax-local-value #'p.name)
                           (if append?
                               (for/fold ([combined this-prop-hash])
                                         ([k (dict-keys new-hash)])
                                 (define old-val (dict-ref combined k '()))
                                 (define new-val
                                   (syntax-parse (dict-ref new-hash k)
                                     [(nv ...) (syntax->list #'(nv ...))]
                                     [bad-stx (raise-syntax-error
                                               (syntax->datum #'p.name)
                                               "bad return from property transformer"
                                               #'bad-stx
                                               #'p.name)]))
                                 (hash-set combined k (append old-val
                                                              new-val)))
                               new-hash)))]
      [p:property-arg-ag-rule
       (ag/cm-branch #'p.name 'ag-info)]
      [p:property-arg-choice-rule
       (ag/cm-branch #'p.name 'cm-info)]
      [p:property-arg-grammar
       (if append?
           (raise-syntax-error 'grammar-property-transform
                               "grammar appending not yet supported"
                               #'p)
           (dict-set infos 'grammar-info new-hash))]))

  (syntax-parse grammar-prop-name-stx
    [gp:grammar-property-stx
     (let* ([slv (syntax-local-value #'gp)]
            [transform (grammar-property-transformer slv)])
       (cond
         [(procedure? transform)
          (define reads (syntax->list (grammar-property-reads slv)))
          (define rewrites (syntax->list (grammar-property-rewrites slv)))
          (define appends (syntax->list (grammar-property-appends slv)))
          (define (i->s pa)
            (infos->section infos-hash pa))
          ;; TODO - do double local-intro+custom-intro for hygiene
          (define ret-list (apply transform (append (list (i->s #'(property gp)))
                                                    (map i->s rewrites)
                                                    (map i->s reads))))
          ;; Re-combine infos-hash.
          ;; The return list should be a hash for each rewrite
          ;; then a hash for each append.
          ;; TODO - verify the returns to be sure they are proper.
          (define post-rewrites
            (for/fold ([i infos-hash])
                      ([rw rewrites]
                       [ret (take ret-list (length rewrites))])
              (section->infos rw ret i #f)))
          (define post-appends
            (for/fold ([i post-rewrites])
                      ([a appends]
                       [ret (list-tail ret-list (length rewrites))])
              (section->infos a ret i #t)))
          post-appends]
         [else infos-hash]))]))


(struct grammar-property
  #|
  * reads, rewrites, and appends are all lists of property-args, specifically
    they refer to grammar-property instances, the grammar, or ag-rule
    or choice-rule names.
  * transformer is a function that receives as arguments a hash for the
    property values of the property it is a transformer for, a hash for each
    property in the rewrite list, then a hash for each property in the read
    list.  It must return a list of hashes, one for each property in the
    rewrite list, then one for each property in the appends list.

    The hashes that are returned for ag-rules or choice-rules must be
    hashes of grammar-node-name->rule-stx (IE syntax for a lambda),
    the hashes that are returned for properties must be
    grammar-node-name->(val-stx-list-as-stx) (IE a syntax object encapsulating
    a list of property values), and the hash returned for the grammar
    must be grammar-node-name->grammar-clause (where grammar-clause is
    the syntax class).
  |#
  (transformer reads rewrites appends)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-property
                                      "Can't be used directly as a macro."
                                      stx)))


