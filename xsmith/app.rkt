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
  racket/string
  ))

(begin-for-syntax
  (define (extract-prefixed-id stx prefix)
    (and (syntax? stx)
         (let ([datum (syntax-e stx)])
           (and (symbol? datum)
                (let ([str (symbol->string datum)])
                  (and (string-prefix? str prefix)
                       (datum->syntax stx
                                      (string->symbol (substring str 1))
                                      stx)))))))
  (define-syntax-class (prefix-id prefix)
    (pattern x:id
             #:when (extract-prefixed-id #'x prefix)
             #:attr extract (extract-prefixed-id #'x prefix)))
  )

(define-syntax (define-xsmith-app stx)
  (syntax-parse stx
    [(_ xsmith-app-name:id inner-app:id prefix:id)
     (with-syntax ([prefix-str (symbol->string (syntax->datum #'prefix))])
       #'(define-syntax (xsmith-app-name stx)
           (syntax-parse stx
             [(_ (~var method (prefix-id prefix-str)) target:expr arg:expr (... ...))
              (with-syntax ([(arg-eval (... ...)) (generate-temporaries #'(arg (... ...)))]
                            [source-name (datum->syntax stx (syntax-source #'method))]
                            [line-number (datum->syntax stx (syntax-line #'method))])
                #'(let ([n target]
                        [arg-eval arg] (... ...))
                    (cond [(ast-node? n) (att-value 'method.extract n arg-eval (... ...))]
                          [(object? n) (send n method.extract arg-eval (... ...))]
                          [else
                           (error
                            'xsmith-app
                            "Not given an AST node or choice object for method ~v, in ~a:~a, given: ~v"
                            'method.extract
                            'source-name
                            'line-number
                            n)])))]
             [(_ form (... ...))
              #'(inner-app form (... ...))])))]))

(define-xsmith-app xsmith-app #%app $)
