#lang racket/base

(provide
 define-xsmith-app
 (rename-out [xsmith-app #%app])
 )

(require
 racket/class
 racr
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-syntax (define-xsmith-app stx)
  (syntax-parse stx
    [(_ xsmith-app-name:id inner-app:id)
     #'(define-syntax (xsmith-app-name stx)
         (syntax-parse stx
           [(_ (quote method:id) target:expr arg:expr (... ...))
            (with-syntax ([(arg-eval (... ...)) (generate-temporaries #'(arg (... ...)))]
                          [source-name (datum->syntax stx (syntax-source #'method))]
                          [line-number (datum->syntax stx (syntax-line #'method))])
              #'(let ([n target]
                      [arg-eval arg] (... ...))
                  (cond [(ast-node? n) (att-value 'method n arg-eval (... ...))]
                        [(object? n) (send n method arg-eval (... ...))]
                        [else
                         (error
                          'xsmith-app
                          "Not given an AST node or choice object for method ~v, in ~a:~a, given: ~v"
                          'method
                          'source-name
                          'line-number
                          n)])))]
           [(_ form (... ...))
            #'(inner-app form (... ...))]))]))

(define-xsmith-app xsmith-app #%app)
