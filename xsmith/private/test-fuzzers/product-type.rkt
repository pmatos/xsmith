#lang racket/base
(require xsmith
         xsmith/racr-convenience
         racr
         racket/pretty
         racket/string
         racket/port
         racket/list)

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
 [Tuple Expression ([expressions : Expression * =
                                 (let* ([t (att-value 'xsmith_type (current-hole))]
                                        [pt (product-type #f)]
                                        [_ (xd-printf "about to unify in tuple fresh\n")]
                                        [_ (unify! t pt)]
                                        [_ (xd-printf "post unify in tuple fresh\n")]
                                        [inners (product-type-inner-type-list pt)])
                                   (if (list? inners)
                                       (length inners)
                                       (+ 1 (random 5))))])
        #:prop wont-over-deepen #t]
 [TupleRef Expression ([tuple : Expression]
                       index
                       tuplelength)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50])

(add-prop arith fresh
          [TupleRef (let ([len (add1 (random 5))])
                      (hash 'tuplelength len
                            'index (random len)))])

(add-att-rule arith tuple-depth
              [#f (λ (n) (if (ast-has-parent? n)
                             (att-value 'tuple-depth (parent-node n))
                             0))]
              [Tuple (λ (n) (add1 (att-value 'tuple-depth (parent-node n))))])
(add-choice-rule arith tuple-not-too-deep?
                 [#f (λ () (not #f))]
                 [Tuple (λ () (not (> (att-value 'tuple-depth (current-hole))
                                      4)))])

(add-prop arith choice-filters-to-apply
          [#f [tuple-not-too-deep?]])

(define int (base-type 'int))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [Tuple [(product-type #f)
                  (λ (n t)
                    (define cs (ast-children (ast-child 'expressions n)))
                    (define ts (map (λ (x) (fresh-type-variable)) cs))
                    (define pt (product-type ts))
                    (xd-printf "about to unify in tuple type-info rule\n")
                    (unify! pt t)
                    (xd-printf "post unify in tuple type-info rule\n")
                    (for/hash ([c cs]
                               [t ts])
                      (values c t)))]]
          [TupleRef [(fresh-type-variable)
                     (λ (n t)
                       (define index (ast-child 'index n))
                       (define tuple-length (ast-child 'tuplelength n))
                       (define inner-types (map (λ (x) (fresh-type-variable))
                                                (make-list tuple-length tuple-length)))
                       (define pt (product-type inner-types))
                       (hash 'tuple pt))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
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
          [Tuple (λ (n) `(tuple ,@(map (λ (n) (render-node n))
                                       (ast-children (ast-child 'expressions n)))))]
          [TupleRef (λ (n) `(tuple-ref ,(render-node (ast-child 'tuple n))
                                       ,(ast-child 'index n)))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,(render-node
                                                       (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) (render-node c))
                                      (ast-children (ast-child 'es n)))))])


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
