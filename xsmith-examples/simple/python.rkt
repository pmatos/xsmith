#lang clotho/racket/base

(require
 xsmith
 xsmith/app
 clotho
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 pprint
 racket/string
 )

(define-spec-component python-comp)

(add-basic-expressions python-comp
                       #:LambdaWithExpression #t
                       #:Numbers #t
                       #:Booleans #t
                       #:Strings #t
                       #:MutableArray #t
                       #:MutableStructuralRecord #t
                       )
(add-basic-statements python-comp
                      #:ProgramWithBlock #t
                      #:ExpressionStatement #t
                      #:AssignmentStatement #t
                      #:MutableArraySafeAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      )

(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen ($xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   ($xsmith_render-node (ast-child 'r n)) rparen)))
(add-prop
 python-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))

(add-prop
 python-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text "FAKEBLOCK = True")
     (text "safe_divide = lambda a, b: a if (b == 0) else (a / b)")
     (vb-concat
      (list*
       (text "")
       (text "")
       (map (λ (cn) ($xsmith_render-node cn))
            (append definitions
                    (list (ast-child 'Block n))))))
     (text "")
     (apply v-append
            (map (λ (v) (text (format "print(~a)\n"
                                      (ast-child 'name v))))
                 (filter (λ (x) (base-type? (ast-child 'type x)))
                         definitions)))
     ;; Hack to get a newline...
     (text "")))]

 [Definition (λ (n)
               (h-append (text (ast-child 'name n))
                         space
                         equals
                         space
                         ($xsmith_render-node (ast-child 'Expression n))))]

 [Block
  ;; Python doesn't have a stand-alone block construct, so we'll fake it.
  (λ (n)
    (h-append
     (text "if FAKEBLOCK:")
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line))]

 [ExpressionStatement (λ (n) ($xsmith_render-node (ast-child 'Expression n)))]

 [ReturnStatement (λ (n) (h-append (text "return ")
                                   ($xsmith_render-node (ast-child 'Expression n))))]

 [AssignmentStatement
  (λ (n)
    (hs-append (text (format "~a" (ast-child 'name n)))
               equals
               ($xsmith_render-node (ast-child 'Expression n))))]

 [IfElseStatement
  (λ (n)
    (h-append
     (h-append (text "if") space lparen ($xsmith_render-node (ast-child 'test n)) rparen
               space
               (text ":"))
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'then n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "else:")
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'else n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append ($xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithExpression
  (λ (n) (h-append lparen
                   (text "lambda ")
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   colon
                   space
                   ($xsmith_render-node (ast-child 'body n))
                   space
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "True" "False")))]
 [Not (λ (n) (h-append (text "not") lparen
                       ($xsmith_render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "and"))]
 [Or (binary-op-renderer (text "or"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-op-renderer (text "+"))]
 [Minus (binary-op-renderer (text "-"))]
 [Times (binary-op-renderer (text "*"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              ($xsmith_render-node (ast-child 'l n))
                              (text ",") space
                              ($xsmith_render-node (ast-child 'r n))
                              rparen))]

 [StringLiteral (λ (n) (text (format "\"~a\"" (ast-child 'v n))))]
 [StringAppend (binary-op-renderer (text "+"))]
 [StringLength (λ (n) (h-append (text "len")
                                lparen
                                ($xsmith_render-node (ast-child 'Expression n))
                                rparen))]

 [MutableArrayLiteral
  (λ (n) (h-append lbracket
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'expressions n))))
                   rbracket))]
 [MutableArraySafeReference
  (λ (n)
    (define array-rendered ($xsmith_render-node (ast-child 'array n)))
    (h-append array-rendered
              lbracket
              ($xsmith_render-node (ast-child 'index n))
              (text " % ")
              (text "len") lparen array-rendered rparen
              rbracket))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (define array-rendered ($xsmith_render-node (ast-child 'array n)))
    (h-append array-rendered
              lbracket
              ($xsmith_render-node (ast-child 'index n))
              (text " % ")
              (text "len") lparen array-rendered rparen
              rbracket
              space equals space ($xsmith_render-node (ast-child 'newvalue n))))]

 [MutableStructuralRecordLiteral
  (λ (n)
    (h-append lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append dquote (text (format "~a" fieldname)) dquote
                                           colon
                                           ($xsmith_render-node expression-node)))
                               (ast-child 'fieldnames n)
                               (ast-children (ast-child 'expressions n))))
              rbrace))]
 [MutableStructuralRecordReference
  (λ (n) (h-append ($xsmith_render-node (ast-child 'record n))
                   lbracket dquote
                   (text (format "~a" (ast-child 'fieldname n)))
                   dquote rbracket))]
 [MutableStructuralRecordAssignmentStatement
  (λ (n) (h-append ($xsmith_render-node (ast-child 'record n))
                   lbracket dquote
                   (text (format "~a" (ast-child 'fieldname n)))
                   dquote rbracket
                   space equals space
                   ($xsmith_render-node (ast-child 'newvalue n))))]
 )



(assemble-spec-components
 python
 python-comp)

(define (type-thunks-for-concretization)
  (list #;(λ()float-type) #;(λ()number-type) (λ()int-type) (λ()bool-type) (λ()string-type)))

(define (python-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (python-generate-ast 'ProgramWithBlock)))

(define (python-format-render doc)
  (pretty-format doc 120))

(module+ main
  (xsmith-command-line
   python-generate
   #:format-render python-format-render
   #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "# ~a" l)) lines)
                                          "\n"))
   ))
