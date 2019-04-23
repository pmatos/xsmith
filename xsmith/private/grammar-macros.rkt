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
 define-spec-component
 add-to-grammar
 add-ag-rule
 add-choice-rule
 add-prop
 assemble-spec-components/core
 current-racr-spec
 (all-from-out "define-grammar-property.rkt")

 current-hole

 make-hole
 make-fresh-node

 ;; these should not be public, but are needed by other xsmith modules
 current-force-deepen
 ;; This should probably not be public, and so far is only used for lifts.
 ;; In principle, any number of extra transformations could be queued up to
 ;; be performed between choice transformations.  But that's probably a bad idea.
 enqueue-inter-choice-transform

 (all-from-out "types.rkt")

 (for-syntax
  grammar-component
  grammar-clause

  grammar-clause->parent-chain
  grammar-node-name->field-info-list

  grammar-node-field-struct
  grammar-node-field-struct?
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
 "xsmith-utils.rkt"
 "scope-graph.rkt"
 ;; for re-provide
 "types.rkt"
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  racket/list
  racket/string
  racket/dict
  racket/match
  "grammar-properties.rkt"
  "spec-component-struct.rkt"
  ))


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

;;; This is parameterized for the `fresh` property implementation.
(define-syntax-parameter current-racr-spec
  (syntax-parser [stx (raise-syntax-error
                       'current-racr-spec
                       "current-racr-spec used without being parameterized"
                       #'stx)]))

;; If you try to add a lift node during choice transformation racr will complain,
;; so this inter-choice-transform-queue allows the lift to be queued for completion
;; before the next choice is started.
(define inter-choice-transform-queue-box (box '()))
(define (enqueue-inter-choice-transform transform-thunk)
  (set-box! inter-choice-transform-queue-box
            (cons transform-thunk
                  (unbox inter-choice-transform-queue-box))))
(define (execute-inter-choice-transform-queue)
  (define transforms (reverse (unbox inter-choice-transform-queue-box)))
  (set-box! inter-choice-transform-queue-box '())
  (for ([transform transforms])
    (transform)))

(define current-force-deepen (make-parameter #f))


(begin-for-syntax

  ;;;; This begin-for-syntax includes a few syntax classes for parsing
  ;;;; add-ag-rule, add-to-grammar, etc,
  ;;;; then it has a bunch of helper functions for the spec assembly macro.

  (define-syntax-class prop-clause
    (pattern
     (prop-name:id (~and node-name (~or node-name-id:id #f)) prop-val:expr)))
  (define-syntax-class grammar-component
    (pattern
     (~or name:id
          [name:id (~optional (~seq (~datum :) type:id))
                   (~optional (~and (~datum *) kleene-star))
                   (~optional (~seq (~datum =) init-expr:expr))])))
  (define-splicing-syntax-class grammar-inline-prop-clause
    (pattern (#:prop name:id val:expr)))
  (define-syntax-class grammar-clause
    ;; TODO - validate node-name: it should not have hyphens or other RACR-special characters
    (pattern
     [node-name:id (~and parent (~or parent-name:id #f))
                   (component:grammar-component ...)]))
  (define-syntax-class grammar-clause-with-inline-props
    (pattern
     [node-name:id (~and parent (~or parent-name:id #f))
                   (component:grammar-component ...)
                   prop:grammar-inline-prop-clause ...]
     #:attr grammar-clause #'(node-name parent (component ...))))

  (struct grammar-node-field-struct
    (name type kleene-star? init-expr)
    #:transparent)

  ;;; Takes a node name and a grammar-clause-hash
  ;;; (made by grammar-clauses-stx->clause-hash),
  ;;; Returns a list of parent names (as identifiers)
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

  ;;; Takes a node name and a grammar-clause-hash
  ;;; (made by grammar-clauses-stx->clause-hash),
  ;;; Returns a list of field-info structs for that grammar clause.
  (define (grammar-node-name->field-info-list name grammar-clause-hash)
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

  ;;; (grammar-clause ...) -> (hashof node-name:id -> grammar-clause)
  ;;; To reference the list by node name instead of searching through it.
  (define grammar-clauses-stx->clause-hash
    (syntax-parser [(c:grammar-clause ...)
                    (for/hash ([name (syntax->datum #'(c.node-name ...))]
                               [clause (syntax->list #'(c ...))])
                      (values name clause))]))

  (define (ag/cm-list->hash xs)
    ;; Accepts a list of prop-clause,
    ;; Makes a tiered hash from rule -> node -> val-stx
    (for/fold ([h (hash)])
              ([x xs])
      (syntax-parse x
        [pc:prop-clause
         (define new-rule-hash (dict-set
                                (dict-ref h (syntax->datum #'pc.prop-name) (hash))
                                (syntax->datum #'pc.node-name)
                                #'pc.prop-val))
         (dict-set h (syntax->datum #'pc.prop-name) new-rule-hash)])))

  ;; Helper for defining properties on #f (instead of an AST node type name).
  ;; Basically it turns a prop-clause with #f as the node name into one with
  ;; the (generated) base node name.
  (define ((prop-clause-false-to-default base-node-name) prop-clause-stx)
    (syntax-parse prop-clause-stx
      [pc:prop-clause
       (syntax-parse #'pc.node-name
         [#f #`(pc.prop-name #,base-node-name pc.prop-val)]
         [_ prop-clause-stx])]))


  (define (ast-node-name-stx->hole-name-stx n)
    ;; Because they have to be passed to `make-ast-rule`, hole
    ;; names can't be hygienic.  So this name is meant to be
    ;; long and not likely to conflict with anything a user would
    ;; actually use.
    ;; Also, more importantly, it fits the now-documented form for
    ;; Xsmith-private names that can't be hygienic.
    (format-id n "XsmithAstHole~a" n))


  ;;; Synthesize the symbols needed for RACR's `ast-rule` form.
  ;;; They are in the form 'NAME:PARENT->field<name-field*<name...
  ;;; identifier -> (grammar-clause -> identifier)
  (define ({make-ast-rule-id base-node-name-stx} grammar-part-stx)
    (define (grammar-component->ast-rule-component-part gcomp-stx)
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


  ;;; Helper for assemble-part-specs.
  ;;; For creating choice lists.
  ;;; Given the spec grammar (as a syntax list of grammar-clause) and the name
  ;;; of an AST type, return all subtypes (as identifiers).
  ;;; (grammar-clause ...) -> (identifier -> (listof identifier))
  (define ({get-ast-subtypes grammar-parts-stx} name-stx)
    (define (get-ast-immediate-subtypes name-stx grammar-parts-stx)
      (syntax-parse grammar-parts-stx
        [(gc:grammar-clause ...)
         (for/fold ([subs '()])
                   ([name+parent (map syntax->list
                                      (syntax->list
                                       #'([gc.node-name gc.parent] ...)))])
           (if (and (syntax->datum (cadr name+parent))
                    (free-identifier=? (cadr name+parent)
                                       name-stx))
               (cons (car name+parent) subs)
               subs))]))
    (define (get-subs/work done-subs work-subs)
      (if (null? work-subs)
          done-subs
          (get-subs/work (cons (car work-subs) done-subs)
                         (append (get-ast-immediate-subtypes (car work-subs)
                                                             grammar-parts-stx)
                                 (cdr work-subs)))))
    (define (get-subs node)
      (get-subs/work '() (list node)))
    (get-subs name-stx))


  ;;;; Helpers for spec component manipulation

  (define (stuff-spec-component spec-component-name
                                subhash-getter-stx
                                subhash-setter-stx
                                keys-stx
                                infos-stx)
    (syntax-parse #`(#,keys-stx #,infos-stx #,spec-component-name)
      [((key ...) (info ...) component-name:spec-component)
       (with-syntax ([sc-ref (spec-component-struct-ref-ref
                              (syntax-local-value #'component-name))])
         #`(begin-for-syntax
             (let* ([new-sub-hash
                     (for/fold ([phash (#,subhash-getter-stx sc-ref)])
                               ([k '(key ...)]
                                [ifo (list (quote-syntax info) ...)])
                       (hash-set phash k (cons ifo (hash-ref phash k '()))))])
               (set! sc-ref (#,subhash-setter-stx sc-ref new-sub-hash)))))]))


  (define (spec-component-merge spec-components)
    (for/fold ([bighash (hash)])
              ([component-project (list spec-component-struct-grammar-info
                                        spec-component-struct-ag-rule-info
                                        spec-component-struct-choice-rule-info
                                        spec-component-struct-property-info)]
               [subhash-key '(grammar-info ag-info cm-info props-info)])
      (define subhash
        (for/fold ([h (hash)])
                  ([p (map (λ (x) (component-project x))
                           spec-components)])
          (for/fold ([h h])
                    ([k (hash-keys p)])
            (hash-set h k (append (hash-ref p k)
                                  (hash-ref h k '()))))))
      (hash-set bighash subhash-key subhash)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Forms for defining and adding definitions to spec components.

(define-syntax-parser define-spec-component
  [(_ component-name:identifier)
   (define spec-inner-id (datum->syntax #'here (gensym "%%spec-component-inner-id_")))
   ;; We do this in a round-about way to satisfy Racket's separate compilation stuff.
   ;; If we just define it with define-syntax then when we reference it it won't see
   ;; all the side-effects from add-to-grammar and friends.
   ;; The contents of begin-for-syntax (including define-for-syntax) blocks are
   ;; evaluated for every instantiation of a module.  So by doing the mutation
   ;; on a define-for-syntax variable it is visible every time.
   #`(begin
       (define-for-syntax #,spec-inner-id
         (spec-component-struct (hash) (hash) (hash) (hash)))
       (define-syntax component-name
         (spec-component-struct-ref (quote-syntax #,spec-inner-id))))])


(define-syntax-parser add-to-grammar
  [(_ component:spec-component
      clause:grammar-clause-with-inline-props ...)
   #`(begin
       #,(stuff-spec-component #'component
                               #'spec-component-struct-grammar-info
                               #'set-spec-component-struct-grammar-info
                               #'((clause.node-name component-name) ...)
                               #'(clause.grammar-clause ...))

       ;; This should be simpler, but if I try to just use clause.prop.name
       ;; I get errors about the sub-property not being defined.  So I
       ;; wrote this procedural mess instead.
       #,@(flatten
           (for/list ([c (syntax->list #'(clause ...))])
             (syntax-parse c
               [clause:grammar-clause-with-inline-props
                (define node-name #'clause.node-name)
                (for/list ([p (syntax->list #'(clause.prop ...))])
                  (syntax-parse p
                    [(prop:grammar-inline-prop-clause)
                     #`(add-prop component
                                 prop.name
                                 [#,node-name prop.val])]))])))
       )])

(define-for-syntax (add-prop-generic component-getter component-setter arg-stx)
  (syntax-parse arg-stx
    [(component:spec-component
      prop/ag/cm-name:id
      [(~and node-name (~or node-name-id:id #f)) prop:expr] ...+)
     (stuff-spec-component #'component
                           component-getter
                           component-setter
                           #'((prop/ag/cm-name node-name) ...)
                           #'([prop/ag/cm-name node-name prop] ...))]))
(define-syntax-parser add-ag-rule
  [(_ arg ...) (add-prop-generic
                #'spec-component-struct-ag-rule-info
                #'set-spec-component-struct-ag-rule-info
                #'(arg ...))])
(define-syntax-parser add-choice-rule
  [(_ arg ...) (add-prop-generic
                #'spec-component-struct-choice-rule-info
                #'set-spec-component-struct-choice-rule-info
                #'(arg ...))])
(define-syntax-parser add-prop
  [(_ arg ...) (add-prop-generic
                #'spec-component-struct-property-info
                #'set-spec-component-struct-property-info
                #'(arg ...))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions implementing the RACR attributes that xsmith defines by default.

;; Implements the <spec-name>-generate-ast function.
(define ((ast-generator-generator fresh-node-func) node-name)
  (define n (fresh-node-func node-name))
  (let ([fill-in
         (λ (n)
           (cond
             [(ast-list-node? n) #f]
             [(att-value 'is-hole? n)
              (begin
                (rewrite-subtree n (att-value 'xsmith_hole->replacement n))
                (execute-inter-choice-transform-queue)
                #t)]
             [else #f]))])
    (perform-rewrites n 'top-down fill-in))
  n)

(define find-a-descendant-function
  ;;; Find the first node that satisfies the predicate (the given node included)
  (λ (n predicate)
    (if (predicate n)
        n
        (for/or ([c (filter ast-node? (ast-children/flat n))])
          (att-value 'find-a-descendant c predicate)))))
(define find-descendants-function
  ;;; Find all nodes that satisfy the predicate (the given node not included)
  (λ (n predicate)
    (define children (filter ast-node? (ast-children/flat n)))
    (define matches
      (apply append (map (λ (x) (att-value 'find-descendants x predicate))
                         children)))
    (if (predicate n)
        (cons n matches)
        matches)))

(define xsmith_hole->replacement-function
  (λ (n)
    (if (att-value 'is-hole? n)
        (let* ([choices (att-value 'xsmith_hole->choice-list n)]
               [choice? (λ (x) (is-a? x ast-choice%))]
               [should-force-deepen?
                ;; If all choices, before filtering, will over-deepen, don't use
                ;; that filter.  This is for cases where a deepening choice requires
                ;; deepening children -- eg. a form that uses a block child for
                ;; re-use purposes that is chosen at maximum-depth - 1.
                (andmap (λ (x) (not (send x xsmith_wont-over-deepen)))
                        choices)]
               [choices-or-reasons
                (parameterize ([current-force-deepen should-force-deepen?])
                  (map (λ (c) (send c xsmith_apply-choice-filters))
                       choices))]
               [filtered (filter choice? choices-or-reasons)]
               #;[choices-or-reasons
                (map (λ (c) (send c xsmith_apply-choice-filters))
                     choices)]
               #;[choices-or-reasons/no-deepen
                (map (λ (x) (if (choice? x)
                                (let ([result (send x xsmith_wont-over-deepen)])
                                  (if (not result)
                                      (format "Choice ~a: filtered out by ~a method."
                                              x
                                              'xsmith_wont-over-deepen)
                                      result))
                                x))
                     choices-or-reasons)]
               #;[filtered/no-deepen (filter choice? choices-or-reasons/no-deepen)]
               #;[filtered (if (null? filtered/no-deepen)
                             (filter choice? choices-or-reasons)
                             filtered/no-deepen)])
          (if (null? filtered)
              (error 'replace-hole
                     (string-append
                      "All choices for filling in a "
                      (symbol->string (ast-node-type n))
                      " hole were filtered out.\n"
                      (string-join choices-or-reasons
                                   "\n")
                      "\n\n"
                      "Hole tree ancestor node types:\n"
                      (string-join (map symbol->string
                                        (map ast-node-type
                                             (filter (λ (x) (not (ast-list-node? x)))
                                                     (ast-ancestors n))))
                                   "\n")
                      "\n\n"
                      (if (ast-has-parent? n)
                          (format "Type of this node expected by its parent: ~a\n"
                                  (att-value 'xsmith_type-constraint-from-parent n))
                          "")))
              (send (choose-ast filtered) xsmith_fresh)))
        (error 'xsmith_hole->replacement
               "called on non-hole node"))))
(define resolve-reference-name-function
  (λ (node name)
    (resolve-reference
     (reference name (att-value 'xsmith_scope-graph-scope node)))))
(define visible-bindings-function
  (λ (n)
    (visible-bindings (att-value 'xsmith_scope-graph-scope n))))
(define node-field-name-in-parent-function
  (λ (n)
    ;; Parent might be the desired node OR a list node.
    (define parent (ast-parent n))
    (define parent* (parent-node n))
    (dict-ref (att-value 'xsmith_child-node-name-dict parent*)
              (if (eq? parent parent*) n parent))))
(define xsmith_effect-constraints-function
  (λ (n)
    (if (ast-has-parent? n)
        (att-value 'xsmith_effect-constraints-for-child (parent-node n) n)
        '())))
(define xsmith_in-lift-branch-function
  (λ (n)
    (or (ast-child 'xsmithliftdepth n)
        (and (ast-has-parent? n)
             (att-value 'xsmith_in-lift-branch (parent-node n))))))



#|
`assemble-spec-components/core` is the main macro that generates everything.
It takes:
* an identifier for a spec name
* (optional) a list of properties to expand (IE use their transformer function).  All properties referenced in spec components are automatically expanded and don't need to be listed.
* a list of spec components

It defines:
* the spec name as a RACR spec
* spec-generate-ast (with `spec` replaced for the id given as spec), which is a function that accepts the symbol name of a node and generates an AST starting at that node.  IE you give it the top level node name and it gives you a program.

Additionally, it defines the following ag-rules within the RACR spec:
* xsmith_hole->choice-list
* is-hole?
* xsmith_hole->replacement
* find-descendants
* find-a-descendant
* resolve-reference-name
* visible-bindings

It also defines within the RACR spec all ag-rules and choice-rules added by property transformers run (either because they were listed or because they were referenced in a spec component).
|#
(define-syntax-parser assemble-spec-components/core
  [(_ spec:id
      (~and extra-props (prop-name:id ...))
      ;(~optional (~seq #:properties (~and extra-props (prop-name:id ...))))
      component:spec-component ...)
   (with-syntax ([extra-props (or (attribute extra-props) #'())]
                 [(spec-ref ...) (map (λ (x) (spec-component-struct-ref-ref
                                              (syntax-local-value x)))
                                      (syntax->list #'(component ...)))])
     ;; The spec-ref names we got out of the component names are
     ;; phase 1 names that we have at phase 1 in template form, so
     ;; we need to output a template that defines a macro that references them
     ;; in the macro body at phase 1.
     ;; Crazy macro stuff.
     #'(begin
         (define-syntax-parser assemble_stage2
           [(_ spec-name extra-props-name)
            (define parts (list spec-ref ...))
            (define combined (spec-component-merge parts))
            (define (parts->stx key)
              (let ([phash (hash-ref combined key)])
                (datum->syntax #f (map (λ (k) (hash-ref phash k))
                                       (hash-keys phash)))))
            (define g-parts (parts->stx 'grammar-info))
            (define ag-parts (parts->stx 'ag-info))
            (define cm-parts (parts->stx 'cm-info))
            (define props-parts (parts->stx 'props-info))
            #`(assemble_stage3
               spec-name
               extra-props-name
               #,g-parts
               #,ag-parts
               #,cm-parts
               #,props-parts)])
         (assemble_stage2 spec extra-props)))])

(define-syntax-parser assemble_stage3
  ;; These first patterns are error checking patterns that match when there
  ;; are duplicate definitions in the grammar or rules.
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
  ;; If the syntax has not been parsed by one of the above, it is duplicate-free.
  ;; The match expression for this pattern will run property transformers.
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
   ;; Switch from a list of syntax objects to a single syntax object for
   ;; properties that do not `allow-duplicates?`, and a syntax-object list
   ;; for those that do.
   ;; For easier parsing on the receiving side.
   (define prop-hash
     (for/hash ([pk (dict-keys prop-hash-with-lists)])
       (define subhash (dict-ref prop-hash-with-lists pk))
       (values
        pk
        (for/hash ([nk (dict-keys subhash)])
          (values
           nk
           (if (grammar-property-allow-duplicates? pk)
               (datum->syntax #f (dict-ref subhash nk))
               (syntax-parse (dict-ref subhash nk)
                 [(a) #'a]
                 [(a b ...)
                  (raise-syntax-error
                   #f
                   (format
                    "duplicate definition of ~a property for node ~a."
                    (grammar-property-name pk)
                    nk)
                   #'a)])))))))

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

   ;; Run the transformers!
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
     #'(assemble_stage4
        spec
        (n-g-part ...)
        (n-ag-clause ...)
        (n-cm-clause ...)))])

(define-syntax-parser assemble_stage4
  ;; Sort the grammar clauses.
  ;; We sort them based on inheritance, so grammar node types that inherit from
  ;; other grammar nodes are output *after* the nodes they inherit from.
  ;; Thus we avoid “this isn't defined yet” errors.
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
     #'(assemble_stage5
        spec
        (g-part-sorted ...)
        (ag-clause ...)
        (cm-clause ...)))])

(define-syntax-parser assemble_stage5
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
               (length (grammar-node-name->field-info-list
                        node-name
                        grammar-hash)))))
   (define choice-rule-name->node-name->rule-body
     (ag/cm-list->hash (syntax->list #'(cm-clause ...))))

   (with-syntax* ([base-node-name (format-id #'spec "XsmithBaseNode~a" #'spec)]
                  [base-node-choice (node->choice #'base-node-name)]
                  ;; Replace the ag-clauses with versions where
                  ;; #f node names are replaced with base-node-name
                  [(ag-clause ...)
                   (map (prop-clause-false-to-default #'base-node-name)
                        (syntax->list #'(ag-clause ...)))]
                  [fresh-node-func (format-id #'here "~a-fresh-node" #'spec)]
                  [generate-ast-func
                   (format-id #'spec "~a-generate-ast" #'spec)]
                  [([subtype-name ...] ...)
                   (map {get-ast-subtypes #'(g-part ...)}
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
                  [base-node-spec
                   ;; The base node has xsmithliftdepth and xsmithlifterwrapped fields injected
                   (format-id
                    #'spec
                    "~a->xsmithliftdepth-xsmithlifterwrapped"
                    #'base-node-name)]
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
                                              '(xsmith_choice-weight))
                                      #'define/override
                                      #'define/public))
                        (syntax->list #'(choice-method-name ...)))]
                  [(cdef-body-for-base/default ...)
                   (map (λ (name)
                          #`(λ args (error
                                     '#,name
                                     "no default implementation (called on ~a)"
                                     this)))
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

             ;; Here finally is the Master Template

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
                          (append
                           ;; This first list is for xsmithliftdepth and xsmithlifterwrapped
                           (list #f #f)
                           (map (λ (x) (create-ast-bud))
                                (make-list (dict-ref node-attr-length-hash
                                                     node-type)
                                           #f)))))])
                   (splicing-syntax-parameterize
                       ([current-racr-spec (syntax-rules () [(_) spec])]
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
                                   ;; This dict-ref is done solely for the
                                   ;; side-effect of erroring when a key is not found
                                   (λ ()
                                     (error
                                      'fresh-node-func
                                      "Not in the defined grammar: ~a, expected one of: ~a"
                                      node-type
                                      (dict-keys hole-name-hash))))
                         (send (new (dict-ref choice-object-hash node-type)
                                    [hole (make-hole node-type)])
                               xsmith_fresh
                               field-dict))
                       ;; Since with-specification creates a new scope,
                       ;; fresh-node-func can't be defined here and visible
                       ;; outside.  So we `set!` it in place.
                       (set! fresh-node-func fresh-node-func-impl)

                       ;; define some core ag-rules
                       (ag-rule xsmith_hole->choice-list
                                [base-node-name
                                 (λ (n) (error 'xsmith_hole->choice-list
                                               "only implemented for grammar hole nodes"))]
                                [ast-hole-name
                                 (λ (n) (list (new subtype-choice-name [hole n]) ...))]
                                ...)
                       (ag-rule is-hole?
                                [base-node-name (λ (n) #f)]
                                [ast-hole-name (λ (n) #t)]
                                ...)
                       (ag-rule xsmith_hole->replacement
                                [base-node-name xsmith_hole->replacement-function])
                       (ag-rule xsmith_make-lift-do-proc
                                [base-node-name
                                 ;;; This is written in-line because make-hole needs
                                 ;;; to be in a context where it is parameterized.
                                 ;;; TODO - fix this so I can put this code outside
                                 ;;; of this giant macro template.
                                 (λ (destination-node
                                     destination-field
                                     type
                                     lift-depth
                                     lifted-ast-type
                                     lifting-hole-node)
                                   (λ ()
                                     (define name
                                       (if (nominal-record-definition-type? type)
                                           (nominal-record-type-name
                                            (nominal-record-definition-type-type
                                             type))
                                           (fresh-var-name "lift_")))
                                     (define new-hole (make-hole lifted-ast-type))
                                     (define lifting-hole-parent
                                       (ast-parent lifting-hole-node))
                                     (define hole-index-in-parent
                                       (ast-child-index lifting-hole-node))
                                     ;; TODO - these field names should probably be looked up...
                                     (rewrite-terminal 'name new-hole name)
                                     (rewrite-terminal 'xsmithliftdepth
                                                       new-hole lift-depth)
                                     (rewrite-terminal 'type new-hole type)

                                     (define choices
                                       (att-value 'xsmith_hole->choice-list new-hole))
                                     (when (not (equal? (length choices) 1))
                                       (error
                                        'xsmith
                                        (format
                                         "lift attempted for node type with more than 1 replacement choice: ~a"
                                         lifted-ast-type)))
                                     (define new-declaration
                                       (send (car choices) xsmith_fresh
                                             ;; TODO -
                                             ;; I don't actually want to send the type
                                             ;; here, because function definitions
                                             ;; need to know the type before they set
                                             ;; the formal params.  So the fresh
                                             ;; property on function definitions needs
                                             ;; to read the value on the hole node
                                             ;; instead of getting it here...
                                             (hash 'xsmithliftdepth lift-depth
                                                   ;'type type
                                                   )))
                                     (enqueue-inter-choice-transform
                                      (λ ()
                                        (rewrite-insert
                                         (ast-child destination-field
                                                    destination-node)
                                         ;; 1-based index?
                                         1
                                         new-declaration)
                                        (rewrite-terminal
                                         'xsmithlifterwrapped
                                         new-declaration
                                         ;; This is boxed so that it won't be
                                         ;; an ast node, so it won't interfere
                                         ;; with things that just look at all
                                         ;; child nodes of its parent.
                                         (box-immutable
                                          (ast-child hole-index-in-parent
                                                     lifting-hole-parent)))))
                                     name))])
                       (ag-rule find-descendants
                                [base-node-name find-descendants-function])
                       (ag-rule find-a-descendant
                                [base-node-name find-a-descendant-function])
                       (ag-rule resolve-reference-name
                                [base-node-name resolve-reference-name-function])
                       (ag-rule visible-bindings
                                [base-node-name visible-bindings-function])
                       (ag-rule xsmith_node-field-name-in-parent
                                [base-node-name node-field-name-in-parent-function])
                       (ag-rule xsmith_effect-constraints
                                [base-node-name xsmith_effect-constraints-function])
                       (ag-rule xsmith_in-lift-branch
                                [base-node-name xsmith_in-lift-branch-function])
                       (compile-ag-specifications))))

                 ;; Define an ast-generator with a hygiene-bending name
                 (define generate-ast-func (ast-generator-generator fresh-node-func))
                 ))])]))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.