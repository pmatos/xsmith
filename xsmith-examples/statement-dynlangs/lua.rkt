#lang racket/base

(require
 xsmith
 racr
 xsmith/racr-convenience
 "core.rkt"
 pprint
 )

(define-spec-component lua-comp)


(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen (render-node (ast-child 'l n))
                   space op-rendered space
                   (render-node (ast-child 'r n)) rparen)))
(add-prop
 lua-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(add-prop
 lua-comp
 render-node-info

 [Program (λ (n)
            (define definitions (ast-children (ast-child 'definitions n)))
            (v-append
             (vb-concat
              (list*
               (text "")
               (text "")
               (map (λ (cn) (render-node cn))
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
                         (render-node (ast-child 'Expression n))))]

 [Block
  (λ (n)
    (h-append
     (text "do")
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
     (text "end")))]

 [ExpressionStatement (λ (n) (h-append (render-node (ast-child 'Expression n))))]

 #;[AssignmentStatement
  (λ (n)
    (hs-append (text (format "~a" (ast-child 'name n)))
               equals
               (render-node (ast-child 'Expression n))))]

 [IfElseStatement
  (λ (n)
    (h-append
     (h-append (text "if") space lparen (render-node (ast-child 'test n)) rparen
               space
               (text "then"))
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'then n)])
                        (append
                         (map (λ (cn) (render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) (render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "else")
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'else n)])
                        (append
                         (map (λ (cn) (render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) (render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "end")))]



 #;[VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LiteralBool (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append (text "not") lparen
                       (render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "and"))]
 [Or (binary-op-renderer (text "or"))]

 [LiteralInt (λ (n) (text (format "~a" (ast-child 'v n))))]
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

 [LiteralString (λ (n) (text (format "\"~a\"" (ast-child 'v n))))]
 [StringAppend (binary-op-renderer (text ".."))]
 [StringLength (λ (n) (h-append (text "string.len")
                                lparen
                                (render-node (ast-child 'Expression n))
                                rparen))]
 )



(assemble-spec-components
 lua
 statement-dynlangs-core
 lua-comp)


(define (lua-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (lua-generate-ast 'Program)))

(define (lua-format-render doc)
  (pretty-format doc 120))

(module+ main
  (xsmith-command-line
   lua-generate
   #:format-render lua-format-render
   ))
