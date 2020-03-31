#lang racket/base

(require
 xsmith
 racr
 xsmith/racr-convenience
 "core.rkt"
 pprint
 racket/string
 )

(define-spec-component javascript-comp)

(add-basic-expressions javascript-comp
                       #:LambdaWithExpression #t
                       #:Booleans #t
                       #:Strings #t
                       #:MutableArray #t
                       #:MutableStructuralRecord #t)
(add-basic-statements javascript-comp
                      #:AssignmentStatement #t
                      #:MutableArrayAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t)


(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen (render-node (ast-child 'l n))
                   space op-rendered space
                   (render-node (ast-child 'r n)) rparen)))
(add-prop
 javascript-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))

(add-prop
 javascript-comp
 render-node-info

 [Program (λ (n)
            (define definitions (ast-children (ast-child 'definitions n)))
            (v-append
             (text "safe_divide = function(a,b){return b == 0 ? a : a / b}")
             (vb-concat
              (list*
               (text "")
               (text "")
               (map (λ (cn) (render-node cn))
                    (append definitions
                            (list (ast-child 'Block n))))))
             (text "")
             (apply v-append
                    (map (λ (v) (text (format "console.log(~a)\n"
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
                         (render-node (ast-child 'Expression n))
                         semi))]

 [Block
  (λ (n)
    (h-append
     lbrace
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) (render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) (render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line
     rbrace))]

 [ExpressionStatement (λ (n) (h-append (render-node (ast-child 'Expression n))
                                       semi))]

 [AssignmentStatement
  (λ (n)
    (h-append (text (format "~a" (ast-child 'name n)))
              space
              equals
              space
              (render-node (ast-child 'Expression n))
              semi))]

 [IfElseStatement
  (λ (n)
    (h-append
     (h-append (text "if") space lparen (render-node (ast-child 'test n)) rparen)
     (render-node (ast-child 'then n))
     (text "else")
     (render-node (ast-child 'else n))))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append (render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map render-node
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithExpression
  ;; Wrap the function in parentheses, so it always counts as an expression.
  ;; If you try to put a function expression in statement position, it complains that it needs a name.
  (λ (n) (h-append lparen (text "function") lparen
                   (comma-list (map render-node
                                    (ast-children (ast-child 'parameters n))))
                   rparen
                   lbrace
                   (text "return")
                   space
                   (render-node (ast-child 'body n))
                   rbrace
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append (text "!") lparen
                       (render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "&&"))]
 [Or (binary-op-renderer (text "||"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-op-renderer (text "+"))]
 [Minus (binary-op-renderer (text "-"))]
 [Times (binary-op-renderer (text "*"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              (render-node (ast-child 'l n))
                              (text ",") space
                              (render-node (ast-child 'r n))
                              rparen))]

 [StringLiteral (λ (n) (text (format "\"~a\"" (ast-child 'v n))))]
 [StringAppend (binary-op-renderer (text "+"))]
 [StringLength (λ (n) (h-append lparen
                                (render-node (ast-child 'Expression n))
                                rparen
                                (text ".length")))]

 [MutableArrayLiteral
  (λ (n) (h-append lbracket
                   (comma-list (map render-node
                                    (ast-children (ast-child 'expressions n))))
                   rbracket))]
 [MutableArrayReference
  (λ (n)
    (define array-rendered (render-node (ast-child 'array n)))
    (h-append array-rendered
              lbracket
              (render-node (ast-child 'index n))
              space (text "%") space
              array-rendered (text ".length")
              rbracket))]
 [MutableArrayAssignmentStatement
  (λ (n)
    (define array-rendered (render-node (ast-child 'array n)))
    (h-append array-rendered
              lbracket
              (render-node (ast-child 'index n))
              space (text "%") space
              array-rendered (text ".length")
              rbracket
              space equals space (render-node (ast-child 'newvalue n))
              semi))]

 [MutableStructuralRecordLiteral
  (λ (n)
    ;; We need to wrap it in parentheses in statement contexts.
    (h-append lparen lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append (text (format "~a" fieldname))
                                           colon
                                           space
                                           (render-node expression-node)))
                               (ast-child 'fieldnames n)
                               (ast-children (ast-child 'expressions n))))
              rbrace rparen))]
 [MutableStructuralRecordReference
  (λ (n) (h-append (render-node (ast-child 'record n))
                   (text ".")
                   (text (format "~a" (ast-child 'fieldname n)))))]
 [MutableStructuralRecordAssignmentStatement
  (λ (n) (h-append (render-node (ast-child 'record n))
                   (text ".")
                   (text (format "~a" (ast-child 'fieldname n)))
                   space equals space
                   (render-node (ast-child 'newvalue n))))]
 )



(assemble-spec-components
 javascript
 javascript-comp)


(define (javascript-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (javascript-generate-ast 'Program)))

(define (javascript-format-render doc)
  (pretty-format doc 120))

(module+ main
  (xsmith-command-line
   javascript-generate
   #:format-render javascript-format-render
   #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "// ~a" l)) lines)
                                          "\n"))
   ))
