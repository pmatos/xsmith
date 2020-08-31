#lang clotho
(require
 xsmith
 xsmith/app
 racr
 racket/pretty
 racket/string
 racket/port
 racket/dict
 racket/list)

(define-spec-component arith)

(add-to-grammar
 arith
 [Definition #f (name type Expression)
   #:prop binder-info ()]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LetStar Expression ([definitions : Definition *]
                      [sideEs : Expression * = (random 2)]
                      Expression)
          #:prop strict-child-order? #t]
 [VariableReference Expression (name)
                    #:prop reference-info (read)]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50]
 [DictRef Expression ([fieldname = (random-field-name)]
                      Expression)]
 [DictSet Expression ([fieldname = (random-field-name)]
                      [dict : Expression]
                      [newval : Expression])]
 [LiteralDict Expression ([fieldnames] [vals : Expression *])
              #:prop wont-over-deepen #t]
 )

(define fieldname-options
  '(a b c d e f g))
(define (random-field-name)
  (random-ref fieldname-options))

(add-prop arith fresh
          [LiteralDict (let* ([t (begin (force-type-exploration-for-node!
                                         (current-hole))
                                        (att-value 'xsmith_type (current-hole)))]
                              [srt (fresh-structural-record-type)]
                              [_side-effect (unify! t srt)]
                              [fd (structural-record-type-known-field-dict srt)]
                              [necessary-fields (dict-keys fd)]
                              ;; Let's inject extra fields.
                              [new-fields (map (λ(_) (random-field-name))
                                               (make-list (add1 (random 2)) #f))]
                              [all-fields (remove-duplicates
                                           (append necessary-fields new-fields))])
                         (hash 'fieldnames all-fields
                               'vals (length all-fields)))])

(define int (base-type 'int))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [Addition [int (λ (n t) (hash 'es t))]]
          [LiteralDict [(λ (n)
                          (if (att-value 'xsmith_is-hole? n)
                              (fresh-subtype-of (fresh-structural-record-type (hash)))
                              (fresh-structural-record-type
                               #:finalized? #t
                               (for/hash ([k (ast-child 'fieldnames n)])
                                 (values k (fresh-type-variable))))))
                        (λ (n t)
                          (define fsrt (fresh-structural-record-type))
                          (unify! fsrt t)
                          (define td (structural-record-type-known-field-dict fsrt))
                          (for/hash ([c (ast-children (ast-child 'vals n))]
                                     [f (ast-child 'fieldnames n)])
                            (values c
                                    (dict-ref td f))))]]
          [DictRef [(fresh-type-variable)
                    (λ (n t) (hash 'Expression
                                   (fresh-structural-record-type
                                    (hash (ast-child 'fieldname n) t))))]]
          [DictSet [(fresh-type-variable)
                    (λ (n t) (hash 'dict (fresh-structural-record-type
                                          (hash (ast-child 'fieldname n) t))
                                   'newval t))]]
          )

(add-prop arith render-node-info
          [LetStar
           (λ (n)
             `(let* (,@(map (λ (d)
                              `[,(string->symbol (ast-child 'name d))
                                ,($xsmith_render-node
                                  (ast-child 'Expression d))])
                            (ast-children (ast-child 'definitions n))))
                ,@(map (λ (c) ($xsmith_render-node c))
                       (ast-children (ast-child 'sideEs n)))
                ,($xsmith_render-node (ast-child 'Expression n))))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,($xsmith_render-node
                                             (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) ($xsmith_render-node c))
                                      (ast-children (ast-child 'es n)))))]
          [DictRef (λ (n) `(dict-ref ,($xsmith_render-node (ast-child 'Expression n))
                                     ,(ast-child 'fieldname n)))]
          [DictSet (λ (n) `(dict-set ,($xsmith_render-node (ast-child 'dict n))
                                     ,(ast-child 'fieldname n)
                                     ,($xsmith_render-node (ast-child 'newval n))))]
          [LiteralDict (λ (n) `(make-dict
                                (,@(ast-child 'fieldnames n))
                                (,@(map (λ (cn) ($xsmith_render-node cn))
                                        (ast-children (ast-child 'vals n))))))]
          )



(define-xsmith-interface-functions
  [arith]
  #:program-node LetStar
  #:type-thunks (list (λ () int))
  #:comment-wrap (λ (lines)
                   (string-join
                    (map (λ (x) (format ";; ~a" x)) lines)
                    "\n"))
  #:format-render (λ (ast)
                    (with-output-to-string
                      (λ ()
                        (pretty-print ast (current-output-port) 1)))))


(module+ main
  (arith-command-line))
