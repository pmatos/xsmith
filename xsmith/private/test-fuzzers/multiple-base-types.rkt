#lang racket/base
(require xsmith racr racket/pretty racket/string racket/port racket/random)

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
 [LiteralString Expression ([v = (random-ref '("foo" "bar" "baz"))])]
 [LiteralBool Expression ([v = (random-ref '(#t #f))])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50]
 [And Expression ([es : Expression * = (+ 1 (random 5))])]
 [StringAppend Expression ([es : Expression * = (+ 1 (random 5))])]
 [If Expression ([test : Expression] [then : Expression] [else : Expression])]
 [StringLength Expression ([Expression])]

 )


(define int (base-type 'int))
(define string (base-type 'string))
(define bool (base-type 'bool))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [LiteralBool [bool (λ (n t) (hash))]]
          [LiteralString [string (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [Addition [int (λ (n t) (hash 'es t))]]
          [And [bool (λ (n t) (hash 'es t))]]
          [StringAppend [string (λ (n t) (hash 'es t))]]
          [If [(fresh-type-variable) (λ (n t) (hash 'test bool
                                                    'then t
                                                    'else t))]]
          [StringLength [int (λ (n t) (hash 'Expression string))]]
          )

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
          [LiteralInt (λ (n) (ast-child 'v n))]
          [LiteralBool (λ (n) (ast-child 'v n))]
          [LiteralString (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,(render-node
                                                       (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) (render-node c))
                                      (ast-children (ast-child 'es n)))))]
          [And (λ (n) `(and ,@(map (λ (c) (render-node c))
                                   (ast-children (ast-child 'es n)))))]
          [StringAppend (λ (n) `(string-append
                                 ,@(map (λ (c) (render-node c))
                                        (ast-children (ast-child 'es n)))))]
          [StringLength (λ (n) `(string-length ,(render-node
                                                 (ast-child 'Expression n))))]
          [If (λ (n) `(if ,(render-node (ast-child 'test n))
                          ,(render-node (ast-child 'then n))
                          ,(render-node (ast-child 'else n))))]
          )


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
