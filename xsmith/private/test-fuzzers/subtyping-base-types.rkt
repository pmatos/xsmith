#lang racket/base
(require xsmith racr racket/pretty racket/string racket/port racket/random)

(type-variable-subtype-default #t)

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
 #;[VariableReference Expression (name)
                    #:prop reference-info (read name)]
 #;[SetBangRet Expression (name Expression)
             #:prop reference-info (write name)]
 [LiteralInt Expression ([v = (random 100)])]
 [LiteralFloat Expression ([v = (* (random) (random 100))])]
 [LiteralString Expression ([v = (random-ref '("foo" "bar" "baz"))])]
 [LiteralBool Expression ([v = (random-ref '(#t #f))])
              #:prop choice-weight 1]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 10]
 [And Expression ([es : Expression * = (+ 1 (random 5))])
      #:prop choice-weight 1]
 [StringAppend Expression ([es : Expression * = (+ 1 (random 5))])]
 [If Expression ([test : Expression] [then : Expression] [else : Expression])]
 [StringLength Expression ([Expression])]

 )


(define number (base-type 'number))
(define int (base-type 'int number))
(define float (base-type 'float number))
(define string (base-type 'string))
(define bool (base-type 'bool))

(add-prop arith type-info
          [Definition [(fresh-type-variable)
                       (λ (n t) (hash 'Expression (fresh-subtype-of t)))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression (fresh-subtype-of t)))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [LiteralFloat [float (λ (n t) (hash))]]
          [LiteralBool [bool (λ (n t) (hash))]]
          [LiteralString [string (λ (n t) (hash))]]
          #;[VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          #;[SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [Addition [(fresh-subtype-of number)
                     (λ (n t) (hash 'es (λ (c) (fresh-subtype-of t))))]]
          [And [bool (λ (n t) (hash 'es (λ (c) (fresh-subtype-of t))))]]
          [StringAppend [string (λ (n t) (hash 'es (λ (c) (fresh-subtype-of t))))]]
          [If [(fresh-type-variable) (λ (n t)
                                       (let ([arm-type (fresh-subtype-of t)])
                                         (hash 'test (fresh-subtype-of bool)
                                               'then arm-type
                                               'else arm-type)))]]
          [StringLength [int (λ (n t) (hash 'Expression (fresh-subtype-of string)))]]
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
          [LiteralFloat (λ (n) (ast-child 'v n))]
          [LiteralBool (λ (n) (ast-child 'v n))]
          [LiteralString (λ (n) (ast-child 'v n))]
          #;[VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          #;[SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
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
  (parameterize ([current-xsmith-type-constructor-thunks
                  (list (λ () int) (λ () float) (λ () string) (λ () bool))])
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
