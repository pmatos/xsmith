#lang racket/base
(require xsmith xsmith/racr-convenience racr racket/pretty racket/string racket/port)

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
 [Application Expression
              ([procedure : Expression]
               [argument : Expression])
              #:prop choice-weight 50]
 [Lambda Expression (FormalParam
                     [body : Expression])
         #:prop wont-over-deepen #t]
 [FormalParam #f (type [name = (fresh-var-name "arg-")])
              #:prop binder-info (name type parameter)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50])


(define int (base-type 'int))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [Lambda [(function-type (fresh-type-variable) (fresh-type-variable))
                   (λ (n t)
                     (define arg-type (fresh-type-variable))
                     (define return-type (fresh-type-variable))
                     (unify! (function-type arg-type return-type)
                             t)
                     (hash 'FormalParam arg-type 'body return-type))]]
          [Application [(fresh-type-variable)
                        (λ (n t)
                          (define arg-type (fresh-type-variable))
                          (hash 'procedure (function-type arg-type t)
                                'argument arg-type))]]
          [FormalParam [(fresh-type-variable) (hash)]]
          [LiteralInt [int (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [int (λ (n t) (hash 'Expression t))]]
          [Addition [int (λ (n t) (hash 'es t))]])

(add-prop arith render-node-info
          [LetStar
           (λ (n)
             `(let* (,@(map (λ (d)
                              `[,(string->symbol (ast-child 'name d))
                                ,(render-node
                                  (ast-child 'Expression d))])
                            (ast-children (ast-child 'definitions n))))
                ,@(map (λ (c) (render-node c))
                       (ast-children (ast-child 'sideEs n)))
                ,(render-node (ast-child 'Expression n))))]
          [Lambda (λ (n) `(lambda (,(string->symbol
                                     (ast-child 'name (ast-child 'FormalParam n))))
                            ,(render-node (ast-child 'body n))))]
          [Application (λ (n) `(,(render-node (ast-child 'procedure n))
                                ,(render-node (ast-child 'argument n))))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,(render-node
                                             (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) (render-node c))
                                      (ast-children (ast-child 'es n)))))])

(add-prop arith fresh
          [Lambda (let* ([type (att-value 'xsmith_type current-hole)]
                         [atype (fresh-type-variable)]
                         [rtype (fresh-type-variable)]
                         [ftype (function-type atype rtype)]
                         [unification-dumb-return-value (unify! ftype type)]
                         [FormalParam (make-fresh-node 'FormalParam
                                                       (hash 'type atype))])
                    (hash
                     'type type
                     'FormalParam FormalParam))]
          [Definition (hash 'name (if (equal? (top-ancestor-node current-hole)
                                              (parent-node current-hole))
                                      (fresh-var-name "global-")
                                      (fresh-var-name "local-"))
                            'type int)])


(assemble-spec-components arithmetic arith)

(define (arithmetic-generate)
  (parameterize ([current-xsmith-type-constructor-thunks (list (λ () int))])
    (arithmetic-generate-ast 'LetStar)))

(xsmith-command-line
 arithmetic-generate
 #:comment-wrap (λ (lines)
                  (string-join
                   (map (λ (x) (format ";; ~a" x)) lines)
                   "\n"))
 #:format-render (λ (ast)
                  (with-output-to-string
                    (λ ()
                      (pretty-print ast (current-output-port) 1)))))
