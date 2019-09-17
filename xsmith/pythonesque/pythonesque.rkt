#!/usr/bin/env racket
#lang racket/base

(require
 xsmith
 xsmith/racr-convenience
 racr
 pprint
 racket/class
 racket/dict
 racket/random
 racket/string
 )

(define-spec-component pythonesque-grammar)

;;;;
;; Grammar specification.

(define (pick-int)
  (random 100))

(define (pick-bool)
  (random-ref (list #t #f)))

(add-to-grammar
 pythonesque-grammar
 ; Special nodes.
 [Node #f ()]
 [Program Node ([decls : VarDecl * = (add1 (random 5))]
                [stmts : Stmt * = (add1 (random 3))]
                [vars : VarRefExpr * = 3])]
 ; Declarations.
 [Decl Node (name type)]
 [VarDecl Decl (Val)]
 ; Statements.
 [Stmt Node ()]
 [Block Stmt ([decls : VarDecl * = (random 2)]
              [stmts : Stmt * = (add1 (random 4))])]
 [AssignStmt Stmt (name Expr)]
 [PassStmt Stmt ()]
 [ReturnStmt Stmt ()]
 [ValReturnStmt ReturnStmt (Expr)]
 [ExprStmt Stmt (Expr)]
 [IfStmt Stmt ([test : Expr]
               [then : Stmt])]
 [IfElseStmt IfStmt ([else : Stmt])]
 ; Expressions.
 [Expr Node ()]
 [BinExpr Expr ([lhs : Expr]
                [rhs : Expr])]
 [AddExpr BinExpr ()]  ;; TODO - I would like to be able to combine all these.
 [SubExpr BinExpr ()]
 [MulExpr BinExpr ()]
 [DivExpr BinExpr ()]
 [VarRefExpr Expr (name)]
 ; Values.
 [Val Node ()]
 [IntVal Val ([val = (pick-int)])]
 [BoolVal Val ([val = (pick-bool)])]
 )

;;;;
;; Properties.

(add-prop
 pythonesque-grammar
 may-be-generated
 [Node #f]
 [Decl #f]
 [Stmt #f]
 [ReturnStmt #f]
 [Expr #f]
 [BinExpr #f]
 [Val #f]
 )

(add-prop
 pythonesque-grammar
 strict-child-order?
 [Program #t]
 [Block #t]
 [IfStmt #t]
 )

(define (no-increase)
  (λ (n) 0))

(add-prop
 pythonesque-grammar
 depth-increase
 [Decl (no-increase)]
 [Block (λ (n) (if (member (node-type (parent-node n))
                           '(IfStmt
                             IfElseStmt))
                   (att-value 'xsmith_ast-depth (parent-node n))
                   (add1 (att-value 'xsmith_ast-depth (parent-node n)))))]
 [AssignStmt (no-increase)]
 [ExprStmt (no-increase)]
 )

(add-prop
 pythonesque-grammar
 wont-over-deepen
 [VarDecl #t]
 [AssignStmt #t]
 [ExprStmt #t]
 [ValReturnStmt #t]
 )

; Binding and reference information.

(add-prop
 pythonesque-grammar
 binder-info
 [Decl (name type definition)]
 [VarDecl (name type definition)]
 )

(add-prop
 pythonesque-grammar
 reference-info
 ;; [AssignStmt (write name)]
 [AssignStmt (write name #:unifies Expr)]
 [VarRefExpr (read name)]
 ;; TODO ---
 ;;  By default, reference-info declares that the node stated must have a type
 ;;  which unifies with the type of whatever it references. This does not work
 ;;  in all cases, such as when the language has unit-type assignments.
 ;;
 ;;  This is because unit-type assignments are statements with the unit type,
 ;;  but which reference a node of a potentially distinct type. Instead, we
 ;;  should be able to specify that unification only needs to happen between the
 ;;  referenced node and *either* the current node (this) or a specified child.
 ;;
 ;[AssignStmt (write name #:unifies 'Expr)]
 ;[VarRefExpr (read name #:unifies this)]
 )

; Fresh generation.

(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))

(add-prop
 pythonesque-grammar
 fresh
 ; Declarations.
 [VarDecl
  (hash 'name (if (equal? (top-ancestor-node (current-hole))
                          (parent-node (current-hole)))
                  (fresh-var-name "GLOBAL_")
                  (fresh-var-name "local_"))
        'type (fresh-concrete-var-type))]
 ; Statements.
 [IfElseStmt
  (hash 'then (make-hole 'Block)
        'else (make-hole 'Block))]
 ; Expressions.
 [VarRefExpr
  (hash 'name (λ ()
                (let* ([choice (send this xsmith_get-reference!)]
                       [parent (parent-node (current-hole))])
                  (binding-name choice))))]
 )

; Lifting.

(add-prop
 pythonesque-grammar
 lift-predicate
 [Program (λ (n t) #t)]
 [Block (λ (n t) (and (not (function-type? t))
                      (not (nominal-record-definition-type? t))))]
 )

(add-prop
 pythonesque-grammar
 lift-type->ast-binder-type
 [#f (λ (t) 'VarDecl)]
 )

; Types.

(define unit (base-type 'unit))
(define int (base-type 'int))
(define bool (base-type 'bool))

(define-generic-type return-type (type))
(define-generic-type no-return-type (type))

(define (no-child-types)
  (λ (n t) (hash)))

(define (fresh-no-return)
  (no-return-type (fresh-type-variable)))

(define (fresh-maybe-return)
  (fresh-type-variable (return-type (fresh-type-variable))
                       (no-return-type (fresh-type-variable))))

(define (bin-expr-types)
  (λ (n t) (hash 'lhs t
                 'rhs t)))

(add-prop
 pythonesque-grammar
 type-info
 ; Special nodes.
 [Program [(fresh-type-variable)
           (λ (n t)
             (hash 'decls (λ (c) (fresh-type-variable))
                   'stmts (λ (c) (fresh-type-variable))
                   'vars (λ (c) (fresh-type-variable))))]]

 ; Statements.
 [Block [unit
          (λ (n t)
            (define stmts (ast-children (ast-child 'stmts n)))
            (define last-stmt (car (reverse stmts)))
            (define stmt-dict
              (for/hash ([s stmts])
                (values s
                        (if (eq? s last-stmt)
                            (fresh-type-variable)
                            (fresh-no-return)))))
            (for/fold ([dict stmt-dict])
                      ([d (ast-children (ast-child 'decls n))])
              (dict-set dict d (fresh-type-variable))))]]
 [AssignStmt [unit
              (λ (n t)
                (hash 'Expr (fresh-type-variable)))]]
 [PassStmt [(fresh-type-variable) (no-child-types)]]
 [ReturnStmt [(error 'typing-non-value-return-stmt) (no-child-types)]]
 [ValReturnStmt [(return-type (fresh-type-variable))
                 (λ (n t)
                   (define rt (return-type (fresh-type-variable)))
                   (unify! t rt)
                   (hash 'Expr (return-type-type rt)))]]
 [ExprStmt [unit
             (λ (n t)
               (hash 'Expr (fresh-type-variable)))]]
 [IfStmt [(fresh-no-return)
          (λ (n t) (hash 'test bool
                         'then (fresh-type-variable)))]]  ;; TODO - in cish, this is t, but here that fails. Why?
 [IfElseStmt [(fresh-maybe-return)
              (λ (n t) (hash 'test bool
                             'then (fresh-type-variable)      ;; TODO - these should be fixed
                             'else (fresh-type-variable)))]]  ;; TODO - because they are wrong
 ; Declarations.
 [VarDecl [(fresh-type-variable)
           (λ (n t)
             (hash 'Val t))]]
 ; Expressions.
 [AddExpr [(fresh-type-variable int) (bin-expr-types)]]
 [SubExpr [(fresh-type-variable int) (bin-expr-types)]]
 [MulExpr [(fresh-type-variable int) (bin-expr-types)]]
 [DivExpr [(fresh-type-variable int) (bin-expr-types)]]
 [VarRefExpr [(fresh-type-variable) (no-child-types)]]
 ; Values.
 [IntVal [int (no-child-types)]]
 [BoolVal [bool (no-child-types)]]
 )

;;;;
;; Pretty printing.

(define (tab d)
  (indent 4 d))

(define (bin-expr op pn)
  (hs-append
   (att-value 'pretty-print (ast-child 'lhs pn))
   (text op)
   (att-value 'pretty-print (ast-child 'rhs pn))))

(add-att-rule
 pythonesque-grammar
 pretty-print
 ; Special nodes.
 [Program (λ (n)
            (define decls (ast-children (ast-child 'decls n)))
            (define stmts (ast-children (ast-child 'stmts n)))
            (define vars (ast-children (ast-child 'vars n)))
            (v-append
             (v-concat
              (map (λ (c) (att-value 'pretty-print c)) decls))
             ; if __name__ == '__main__':
             line
             (text "if __name__ == '__main__':")
             (tab
              (v-concat
               (map (λ (c) (att-value 'pretty-print c))
                    stmts)))))]
 ; Declarations.
 [VarDecl (λ (n)
            (hs-append
             (text (ast-child 'name n))
             (text "=")
             (att-value 'pretty-print (ast-child 'Val n))))]
 ; Statements.
 [Block (λ (n)
          (h-append
            (v-concat
             (append
              (map (λ (cn) (att-value 'pretty-print cn))
                   (ast-children (ast-child 'decls n)))
              (map (λ (cn) (att-value 'pretty-print cn))
                   (ast-children (ast-child 'stmts n)))))))]
 [AssignStmt (λ (n)
               (hs-append
                (text (ast-child 'name n))
                (text "=")
                (att-value 'pretty-print (ast-child 'Expr n))))]
 [PassStmt (λ (n)
             (text "pass"))]
 [ValReturnStmt (λ (n)
                  (hs-append
                   (text "return")
                   (att-value 'pretty-print (ast-child 'Expr n))))]
 [ExprStmt (λ (n)
             (att-value 'pretty-print (ast-child 'Expr n)))]
 [IfStmt (λ (n)
           (v-append
            (h-append
             (text "if ")
             (att-value 'pretty-print (ast-child 'test n))
             (text ":"))
            (tab (att-value 'pretty-print (ast-child 'then n)))))]
 [IfElseStmt (λ (n)
               (v-append
                (h-append
                 (text "if ")
                 (att-value 'pretty-print (ast-child 'test n))
                 (text ":"))
                (tab (att-value 'pretty-print (ast-child 'then n)))
                (text "else:")
                (tab (att-value 'pretty-print (ast-child 'else n)))))]
 ; Expressions.
 [AddExpr (λ (n) (bin-expr "+" n))]
 [SubExpr (λ (n) (bin-expr "-" n))]
 [MulExpr (λ (n) (bin-expr "*" n))]
 [DivExpr (λ (n) (bin-expr "//" n))]
 [VarRefExpr (λ (n)
               (text (ast-child 'name n)))]
 ; Values.
 [IntVal (λ (n)
           (text (number->string (ast-child 'val n))))]
 [BoolVal (λ (n)
            (if (ast-child 'val n)
                (text "True")
                (text "False")))]
 )

;;;;
;; Assemble.

(assemble-spec-components pythonesque pythonesque-grammar)

(define concretized-types
  (map (λ (t) (λ () t))
       (list int bool)))

(define (pythonesque-generate-and-print)
  (parameterize ([current-xsmith-type-constructor-thunks concretized-types])
    (let ([ast (pythonesque-generate-ast 'Program)])
      (pretty-print (att-value 'pretty-print ast)
                    (current-output-port)
                    120))))

(xsmith-command-line pythonesque-generate-and-print)
