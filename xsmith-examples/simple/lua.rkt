#lang clotho

(require
 xsmith
 xsmith/app
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 pprint
 racket/string
 )

(define-spec-component lua-comp)

(add-basic-expressions lua-comp
                       #:LambdaWithBlock #t
                       #:Numbers #t
                       #:Booleans #t
                       #:Strings #t
                       #:MutableArray #t
                       #:MutableStructuralRecord #t
                       )
(add-basic-statements lua-comp
                      #:ProgramWithBlock #t
                      #:AssignmentStatement #t
                      #:NullStatement #t
                      #:MutableArraySafeAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      )

(add-prop
 lua-comp
 choice-weight
 [ProcedureApplication 50])

(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen ($xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   ($xsmith_render-node (ast-child 'r n)) rparen)))
(add-prop
 lua-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))

(define (lua-render-let varname binding-expr body-expr)
  (h-append lparen lparen (text (format "function(~a) return " varname))
            body-expr
            (text " end") rparen
            lparen binding-expr rparen rparen))

(add-prop
 lua-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     ;; TODO - this definition maybe needs to change based on lua version.  Is there a way to do conditional evaluation based on which lua implementation I'm in?
     ;; This modulo definition is from the Lua reference manual:
     ;; http://www.lua.org/manual/5.2/manual.html#3.4.1
     (text "modulo = function(a, b) return a - math.floor(a/b)*b end")
     (text "safe_divide = function(a, b) return (b == 0) and a or (a / b) end")
     (text "expression_statement_dummy_var = 0;")
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
  (λ (n)
    (h-append
     (text "do")
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line
     (text "end")))]

 ;; Lua doesn't actually allow expression statements.  Let's hack around that with an assignment to a dummy variable that is never read.
 #;[ExpressionStatement (λ (n) (h-append (text "expression_statement_dummy_var = ")
                                       ($xsmith_render-node (ast-child 'Expression n))
                                       semi))]

 [ReturnStatement (λ (n) (h-append (text "return ")
                                   ($xsmith_render-node (ast-child 'Expression n))))]

 ;; TODO - I'm not sure how to do a null statement in lua offhand.
 [NullStatement (λ (n) (text "expression_statement_dummy_var = 0;"))]
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
               (text "then"))
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
     (text "else")
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'else n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "end")))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append ($xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithBlock
  (λ (n) (h-append lparen
                   (text "function") lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   rparen
                   space
                   ($xsmith_render-node (ast-child 'body n))
                   space
                   (text "end")
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
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
 [StringAppend (binary-op-renderer (text ".."))]
 [StringLength (λ (n) (h-append (text "string.len")
                                lparen
                                ($xsmith_render-node (ast-child 'Expression n))
                                rparen))]

 [MutableArrayLiteral
  (λ (n) (h-append lbrace
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'expressions n))))
                   rbrace))]
 [MutableArraySafeReference
  ;; Lua's array index should start at 1.  And we should define a modulus function, one of the many... frustrating parts of lua is that there wasn't a built-in modulus operator until recently.  So to fuzz older versions we need to define a function for it.
  (λ (n)
    (define array-rendered ($xsmith_render-node (ast-child 'array n)))
    (lua-render-let 'array array-rendered
                    (h-append (text "array")
                              lbracket (text "modulo") lparen
                              ($xsmith_render-node (ast-child 'index n))
                              comma space (text "#") (text "array")
                              rparen (text " + 1") rbracket)))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (define array-rendered ($xsmith_render-node (ast-child 'array n)))
    (lua-render-let 'array array-rendered
                    (h-append (text "array")
                              lbracket (text "modulo") lparen
                              ($xsmith_render-node (ast-child 'index n))
                              comma space (text "#") (text "array")
                              rparen (text " + 1") rbracket
                              space equals space
                              ($xsmith_render-node (ast-child 'newvalue n))))
    )]

 [MutableStructuralRecordLiteral
  (λ (n)
    (h-append lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append (text (format "~a" fieldname))
                                           equals
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
 lua
 lua-comp)

(define (type-thunks-for-concretization)
  (list #;(λ()float-type) #;(λ()number-type) (λ()int-type) (λ()bool-type) (λ()string-type)))

(define (lua-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (lua-generate-ast 'ProgramWithBlock)))

(define (lua-format-render doc)
  (pretty-format doc 120))

(module+ main
  (xsmith-command-line
   lua-generate
   #:format-render lua-format-render
   #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "-- ~a" l)) lines)
                                          "\n"))
   ))
