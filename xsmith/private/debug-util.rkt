#lang racket/base

;; Some handy utilities for debugging.

(require
 (for-syntax
  racket/base
  syntax/parse
  ))

(provide
 (rename-out [debug-hash-ref hash-ref]))

(define-syntax (debug-hash-ref stx)
  (syntax-parse stx
    [(_ the-hash the-key (~optional the-fallback))
     (with-syntax ([source-name (syntax-source stx)]
                   [source-line (syntax-line stx)]
                   [source-column (syntax-column stx)])
       #'(let ([key-v the-key])
           (hash-ref the-hash key-v
                     (~? the-fallback
                         (λ ()
                           (error
                            'hash-ref
                            "no value found for key ~a at source: ~a:~a:~a"
                            key-v
                            'source-name
                            'source-line
                            'source-column))))))]))

(module+ dict
  (provide
   (rename-out [debug-dict-ref dict-ref])
   (except-out (all-from-out racket/dict) dict-ref)
   )
  (require racket/dict)
  (define-syntax (debug-dict-ref stx)
    (syntax-parse stx
      [(_ the-hash the-key (~optional the-fallback))
       (with-syntax ([source-name (syntax-source stx)]
                     [source-line (syntax-line stx)]
                     [source-column (syntax-column stx)])
         #'(let ([key-v the-key])
             (dict-ref the-hash key-v
                       (~? the-fallback
                           (λ ()
                             (error
                              'dict-ref
                              "no value found for key ~a at source: ~a:~a:~a"
                              key-v
                              'source-name
                              'source-line
                              'source-column))))))]))
  )

(module+ racr
  (provide
   (rename-out [debug-ast-child ast-child])
   (except-out (all-from-out racr) ast-child)
   )
  (require racr)
  (define-syntax (debug-ast-child stx)
    (syntax-parse stx
      [(_ key node)
       (with-syntax ([source-name (syntax-source stx)]
                     [source-line (syntax-line stx)]
                     [source-column (syntax-column stx)])
         #'(let ([key-v key])
             (with-handlers ([exn:fail?
                              (λ (e)
                                (eprintf "ast-child failure for key ~v at source: ~a:~a:~a\n"
                                         key-v 'source-name 'source-line 'source-column)
                                (raise e))])
               (ast-child key-v node))))]))
  )
