#lang racket/base
(require xsmith racr racket/pretty racket/string)

(define-spec-component arith)

(add-to-grammar
 arith
 [Definition #f (name type Expression)
   #:prop binder-info (name type definition)]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LetStar Expression ([definitions : Definition *]
                      [sideEs : Expression * = (random 2)]
                      Expression)
          #:prop strict-child-order? #t]
 [VariableReference Expression (name)
                    #:prop reference-info (read name)]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write name)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50])


(define int (base-type 'int))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (fresh-type-variable)
                                   'sideEs (fresh-type-variable)
                                   'Expression t))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [Addition [int (λ (n t) (hash 'es t))]])

(add-ag-rule
 arith to-s-exp
 [LetStar
  (λ (n)
    `(let* (,@(map (λ (d)
                     `[,(string->symbol (ast-child 'name d))
                       ,(att-value 'to-s-exp
                                   (ast-child 'Expression d))])
                   (ast-children (ast-child 'definitions n))))
       ,@(map (λ (c) (att-value 'to-s-exp c))
              (ast-children (ast-child 'sideEs n)))
       ,(att-value 'to-s-exp (ast-child 'Expression n))))]
 [LiteralInt (λ (n) (ast-child 'v n))]
 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
 [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                  ,(att-value 'to-s-exp
                                              (ast-child 'Expression n)))
                            ,(string->symbol (ast-child 'name n))))]
 [Addition (λ (n) `(+ ,@(map (λ (c) (att-value 'to-s-exp c))
                             (ast-children (ast-child 'es n)))))])


(assemble-spec-components arithmetic arith)

(define (arithmetic-generate-and-print)
  (parameterize ([current-xsmith-type-constructor-thunks (list (λ () int))])
    (pretty-print (att-value 'to-s-exp (arithmetic-generate-ast 'LetStar))
                  (current-output-port)
                  1)))

(xsmith-command-line arithmetic-generate-and-print
                     #:comment-wrap (λ (lines)
                                      (string-join
                                       (map (λ (x) (format ";; ~a" x)) lines)
                                       "\n")))
