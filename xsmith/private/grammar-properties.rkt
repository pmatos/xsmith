#lang clotho/racket/base
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

#|
This file contains the grammar-property struct definition as well as the procedure to apply their transformers.

Grammar property names are bound with define-syntax to grammar-property structs (IE the syntax-local-value of the bound identifier is the grammar-property struct).
|#


(provide
 (struct-out grammar-property)
 grammar-property-less-than
 grammar-property-transform
 grammar-property-name
 grammar-property-allow-duplicates?

 ;; syntax classes
 grammar-property-stx
 property-arg
 property-arg-property
 property-arg-att-rule
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

;;; These are syntax classes for specifying the argument/return types
;;; of property transformers.
(define-syntax-class property-arg-property
  (pattern ((~datum property) name:id)))
(define-syntax-class property-arg-att-rule
  (pattern ((~datum att-rule) name:id)))
(define-syntax-class property-arg-choice-rule
  (pattern ((~datum choice-rule) name:id)))
(define-syntax-class property-arg-grammar
  (pattern (~and ((~datum grammar)) grammar-flag)))
(define-syntax-class property-arg
  #:datum-literals (property att-rule choice-rule grammar)
  (pattern (~or prop:property-arg-property
                ag:property-arg-att-rule
                cm:property-arg-choice-rule
                gram:property-arg-grammar)))

;;; For sorting properties to run in an appropriate order.
;;; A p1 is less than p2 if p1 rewrites/appends to p2, or if
;;; p2 reads (but does not rewrite) p1.
(define (grammar-property-less-than p1 p2)
  (or
   ;; writes?
   (for/or ([write-target (append (syntax->list (grammar-property-rewrites p1))
                                  (syntax->list (grammar-property-appends p1)))])
     (syntax-parse write-target
       [p:property-arg-property
        (equal? p2 (syntax-local-value #'p.name))]
       [_ #f]))
   ;; is-read?
   (for/or ([read-target (syntax->list (grammar-property-reads p2))])
     (syntax-parse read-target
       [p:property-arg-property
        (equal? p1 (syntax-local-value #'p.name))]
       [_ #f]))))


#|
Run the transformer for a grammar property.

* grammar-prop-name-stx is an identifier whose syntax-local-value is a grammar-property struct.
* infos-hash is a hash which contains the keys 'props-info, 'ag-info, 'cm-info, and 'grammar-info
The 'grammar-info key maps to a hash of node-name->node-spec-stx
The other three keys map to hashes of rule-name -> node-name -> val-stx

This function is only used in one place, so its interface is tightly bound with that use.  Maybe it ought to be improved.
|#
(define (grammar-property-transform grammar-prop-name-stx
                                    infos-hash
                                    spell-check-grammar-name)

  ;; Helper for getting the appropriate part out of the infos hash based on what
  ;; kind of arguments the property transformer needs.
  (define (infos->section infos-hash pa)
    (syntax-parse pa
      [p:property-arg-property (hash-ref (hash-ref infos-hash 'props-info)
                                         (syntax-local-value #'p.name)
                                         (hash))]
      [p:property-arg-att-rule (hash-ref (hash-ref infos-hash 'ag-info)
                                         (syntax->datum #'p.name)
                                         (hash))]
      [p:property-arg-choice-rule (hash-ref (hash-ref infos-hash 'cm-info)
                                            (syntax->datum #'p.name)
                                            (hash))]
      [p:property-arg-grammar (hash-ref infos-hash 'grammar-info)]))

  ;; Helper to merge hashes returned by property transformer functions back into
  ;; the master infos-hash.
  (define (section->infos prop-arg new-hash infos append?)
    ;; Sub-helper for att-rule/choice-method cases because they are basically the same.
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
                              "rewrite is not supported for att-rules or choice-rules"
                              grammar-prop-name-stx)))
    ;; Parse the prop-arg (which tells what type the hash is) to merge it back in
    ;; to the right section of the infos-hash.
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
      [p:property-arg-att-rule
       (ag/cm-branch #'p.name 'ag-info)]
      [p:property-arg-choice-rule
       (ag/cm-branch #'p.name 'cm-info)]
      [p:property-arg-grammar
       (if append?
           (raise-syntax-error 'grammar-property-transform
                               "grammar appending not yet supported"
                               #'p)
           (dict-set infos 'grammar-info new-hash))]))

  ;; In this form we do the actual transformation, then use the helpers to merge
  ;; the transformed/appended info about grammar/properties/attributes/choice-methods
  ;; back into the master infos-hash.
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
          (define dicts-to-send (append (list (i->s #'(property gp)))
                                        (map i->s rewrites)
                                        (map i->s reads)))
          (for ([d dicts-to-send])
            (for ([k (dict-keys d)])
              (spell-check-grammar-name k (dict-ref d k))))
          ;; TODO - do double local-intro+custom-intro for hygiene
          ;;        OR use the new local-apply-transformer procedure (in Racket v7).
          (define ret-list (apply transform dicts-to-send))
          (when (not (equal? (length ret-list)
                             (length (append rewrites appends))))
            (raise-syntax-error
             'xsmith
             "Too few hashes in return list from property transformer"
             grammar-prop-name-stx))
          ;; Re-combine infos-hash.
          ;; The return list should be a hash for each rewrite
          ;; then a hash for each append.
          ;; TODO - verify the returns to be sure they are proper.
          ;;        Eg. for rewritten/appended properties, check that they
          ;;        adhere to the `allow-duplicates?` flag.
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
    they refer to grammar-property instances, the grammar, or att-rule
    or choice-rule names.  They specify the argument and return types
    of the property transformer.
  * transformer is a function that receives as arguments a hash for the
    property values of the property it is a transformer for, a hash for each
    property in the rewrite list, then a hash for each property in the read
    list.  It must return a list of hashes, one for each property in the
    rewrite list, then one for each property in the appends list.

    The hashes that are returned for att-rules or choice-rules must be
    hashes of grammar-node-name->rule-stx (IE syntax for a lambda),
    the hashes that are returned for properties must be
    grammar-node-name->(val-stx-list-as-stx) (IE a syntax object encapsulating
    a list of property values), and the hash returned for the grammar
    must be grammar-node-name->grammar-clause (where grammar-clause is
    the syntax class).
  |#
  (name transformer reads rewrites appends allow-duplicates?)
  #:property prop:procedure (λ (stx) (raise-syntax-error
                                      'grammar-property
                                      "Can't be used directly as a macro."
                                      stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
