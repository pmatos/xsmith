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

(require syntax/parse)


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
        (free-identifier=? #'p.name
                           (grammar-property-name p2))]
       [_ #f]))
   ;; is-read?
   (for/or ([read-target (grammar-property-reads p2)])
     (syntax-parse read-target
       [p:property-arg-property
        (free-identifier=? #'p.name
                           (grammar-property-name p1))]
       [_ #f]))))


;;; Run the transformer for a grammar property.
(define (grammar-property-transform grammar-prop-name-stx
                                    infos-hash
                                    ;; because I don't have a hash-free-identifier=?
                                    canonicalize-prop-name)
  (define (infos->section infos-hash pa)
    (syntax-parse pa
      [p:property-arg-property (hash-ref (hash-ref infos-hash 'props-info)
                                         (canonicalize-prop-name #'p.name)
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
                                (hash-set combined k (dict-ref new-hash k)))))
          (raise-syntax-error 'grammar-property-transform
                              "rewrite is not supported for ag-rules or choice-rules"
                              grammar-prop-name-stx)))
    (syntax-parse prop-arg
      [p:property-arg-property
       (define props-hash (hash-ref infos 'props-info))
       (define this-prop-hash
         (hash-ref props-hash (canonicalize-prop-name #'p.name) (hash)))
       (hash-set infos 'props-info
                 (hash-set props-hash
                           (canonicalize-prop-name #'p.name)
                           (if append?
                               (for/fold ([combined this-prop-hash])
                                         ([k (dict-keys new-hash)])
                                 (define old-val (dict-ref combined k '()))
                                 (hash-set combined k (append old-val
                                                              (dict-ref new-hash k))))
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
          (define ret-list (apply transform (append (map i->s rewrites)
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
  (transformer reads rewrites appends)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-property
                                      "Can't be used directly as a macro."
                                      stx)))


