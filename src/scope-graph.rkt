#lang racket/base

(provide
 (struct-out scope)
 (struct-out reference)
 (struct-out binding)
 (struct-out module)
 resolve-reference
 visible-bindings
 current-well-formedness-regexp
 current-path-greater-than
 )

(require
 racket/list
 racket/match
 )

(struct scope (parent bindings imports)
  ;; parent is a scope, bindings are binding structs,
  ;; imports are just names (that should resolve to module structs)
  #:transparent)

(struct reference (name parent-scope)
  #:transparent)

;; Bindings may be to a module
(struct binding (name bound)
  #:transparent)

(struct module (scope)
  #:transparent)



;; This one is just for convenience in implementing the functions in this module.
(struct resolution (binding path)
  #:transparent)

;; reference -> binding
(define (resolve-reference reference)
  (define (resolve* name scope path-so-far)
    (define decl (findf (λ (b) (equal? (binding-name b) name))))
    (define parent-decls
      (resolve* name (scope-parent scope) (cons 'parent path-so-far)))
    (define import-decls
      (flatten
       (for/list ([import (scope-imports scope)])
         (resolve* name
                   (module-scope (resolve-reference import))
                   (cons 'import path-so-far)))))
    (let ([bindings (append parent-decls import-decls)])
      (if decl
          (cons (resolution decl (cons 'declaration path-so-far))
                bindings)
          bindings)))

  (let* ([bindings-with-path (resolve* (reference-name reference)
                                       (reference-parent-scope reference)
                                       (list 'reference))]
         [well-formed-bwp (filter (λ (b) (well-formed? (cdr b)))
                                  bindings-with-path)]
         [best-bwp (apply generic-max #:gt-comparator greater-visibility
                          well-formed-bwp)])
    (car best-bwp)))

;; scope -> (listof binding)
(define (visible-bindings scope)
  (define (resolution-hash-fold lhash rhash)
    (for/fold ([fhash lhash])
              ([r (hash-values rhash)])
              (let* ([name (binding-name (resolution-binding r))]
                     [old-r (hash-ref fhash name #f)])
                (if (and old-r
                         (greater-visibility old-r r))
                    fhash
                    (hash-set fhash name r)))))

  (define (visible-names-hash scope path-so-far)
    (if (not scope)
        (hash)
        (let* ([parent-rs (visible-names-hash
                           (scope-parent scope) (cons 'parent path-so-far))]
               [import-rss (for/list ([import (scope-imports scope)])
                             (visible-names-hash (module-scope
                                                  (resolve-reference import))
                                                 (cons 'import path-so-far)))]
               [resolutions-here
                (map (λ (b) (resolution b (reverse
                                           (cons 'declaration path-so-far))))
                     (scope-bindings scope))]
               [valid-resolutions-here (filter well-formed? resolutions-here)]
               [valid-here-hash (for/hash ([r valid-resolutions-here])
                                  (values (binding-name (resolution-binding r))
                                          r))])
          (for/fold ([fhash (hash)])
                    ([rhash (list* valid-here-hash parent-rs import-rss)])
            (resolution-hash-fold fhash rhash)))))

  (map resolution-binding
       (hash-values (visible-names-hash scope (list 'reference)))))


(define current-well-formedness-regexp (make-parameter #px"rp*i?d"))

(define (well-formed? resolution)
  (define (step->char step)
    (match step
      ['reference #\r]
      ['declaration #\d]
      ['parent #\p]
      ['import #\i]
      [else (error 'scope-graph "Internal error - bad step, this shouldn't happen.")]))
  (regexp-match (current-well-formedness-regexp)
                (apply string (map step->char (resolution-path resolution)))))


(define (default-path-greater-than l r)
  ;; This assumes paths are well formed (IE they start with reference
  ;; and end in declaration), and that they are not the same.
  (define (path-part->n pp)
    (match pp
      ['reference 0]
      ['parent 1]
      ['import 2]
      ['declaration 3]
      [else (error 'scope-graph "Internal error - bad step, this shouldn't happen.")]))
  (cond
    [(equal? (car l) (car r)) (default-path-greater-than (cdr l) (cdr r))]
    [else (> (path-part->n (car l)) (path-part->n (car r)))]))

(define current-path-greater-than (make-parameter default-path-greater-than))

(define (greater-visibility l r)
  ({current-path-greater-than} (resolution-path l)
                               (resolution-path r)))

(define (generic-max #:gt-comparator gt arg1 . args)
  (cond [(empty? args) arg1]
        [(gt arg1 (car args))
         (max #:gt-comparator gt arg1 (cdr args))]
        [else (max #:gt-comparator gt (car args) (cdr args))]))

