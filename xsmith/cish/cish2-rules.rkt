#lang racket/base

(provide cish2-rules)

(require
 "../grammar-macros.rkt"
 "cish2-utils.rkt"
 "../core-properties.rkt"

 racr
 (except-in pprint
            semi rparen rbrace lparen lbrace comma
            colon
            )
 racket/dict
 racket/set
 racket/match
 racket/class
 (prefix-in rt: rosette)
 (except-in racket/list empty)
 "../scope-graph.rkt"
 "../xsmith-options.rkt"
 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  ))


(define-spec-component cish2-rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ag rules

(define-syntax-parser ag
  [(_ arg ...)
   #'(add-ag-rule cish2-rules arg ...)])


(define fresh-int-counter 0)
(define (fresh-int!)
  (begin0
      fresh-int-counter
    (set! fresh-int-counter (add1 fresh-int-counter))))

(ag ast-serial-number
    ;; This is basically just a hack to signal stale state for the Rosette assertion stack.
    ;; I don't think this is really necessary -- checking eq? on the top-ancestor-node should be equivalent to checking this serial number.
    [Program (λ (n) (fresh-int!))]
    [Node (λ (n) (att-value 'ast-serial-number (parent-node n)))])


(ag
 pretty-print
 [Program (λ (n)
            (define children (append (ast-children (ast-child 'declarations n))
                                     (list (ast-child 'main n))))
            (define global-vars (filter (λ (d) (node-subtype? d 'VariableDeclaration))
                                        children))
            (define functions (filter (λ (d) (node-subtype? d 'FunctionDefinition))
                                      children))
            (v-append
             (v-comment
              n
              (vb-concat
               (cons
                (text "#include \"xsmith_safe_math.h\"\n")
                (map (λ (cn) (att-value 'pretty-print cn))
                     (append global-vars functions)))))
             ;; Hack to get a newline...
             (text "")))]
 [FunctionDefinition
  (λ (n)
    (v-comment
     n
     (h-append
      (text (basic-type-name (ast-child 'typename n)))
      space
      (text (ast-child 'name n))
      lparen
      (h-concat
       (add-between
        (map (λ (fp)
               (h-append (text (basic-type-name (ast-child 'typename fp)))
                         space
                         (text (ast-child 'name fp))))
             (ast-children (ast-child 'params n)))
        (h-append comma space)))
      rparen
      line
      (att-value 'pretty-print (ast-child 'Block n)))))]
 [IfStatement
  (λ (n)
    (v-comment
     n
     (h-append
      (h-append if: space lparen
                (att-value 'pretty-print (ast-child 'test n))
                rparen)
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'then n)))))))]
 [IfElseStatement
  (λ (n)
    (v-comment
     n
     (h-append
      (h-append if: space lparen
                (att-value 'pretty-print (ast-child 'test n))
                rparen)
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'then n))))
      line
      else:
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'else n)))))))]
 [WhileStatement
  (λ (n)
    (v-comment
     n
     (h-append
      (h-append while: space lparen
                (att-value 'pretty-print (ast-child 'test n))
                rparen)
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'body n)))))))]
 [DoWhileStatement
  (λ (n)
    (v-comment
     n
     (h-append
      do:
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'body n))))
      line
      (h-append while: space lparen
                (att-value 'pretty-print (ast-child 'test n))
                rparen semi))))]
 [ForStatement
  (λ (n)
    (v-comment
     n
     (h-append
      for: space lparen
      (att-value 'pretty-print (ast-child 'init n))
      ;; since init is a declaration it already prints a semicolon
      space
      (att-value 'pretty-print (ast-child 'test n))
      semi space
      (att-value 'pretty-print (ast-child 'update n))
      rparen
      (nest nest-step
            (h-append line
                      (att-value 'pretty-print (ast-child 'body n)))))))]
 [Block
  (λ (n)
    (v-comment
     n
     (h-append
      lbrace
      (nest nest-step
            (h-append
             line
             (v-concat
              (append
               (map (λ (cn) (att-value 'pretty-print cn))
                    (ast-children (ast-child 'declarations n)))
               (map (λ (cn) (att-value 'pretty-print cn))
                    (ast-children (ast-child 'statements n)))))))
      line
      rbrace)))]
 [ExpressionStatement
  (λ (n)
    (v-comment
     n
     (h-append (att-value 'pretty-print (ast-child 'Expression n))
               semi)))]
 [ValueReturnStatement
  (λ (n)
    (v-comment
     n
     (h-append return: space
               (att-value 'pretty-print (ast-child 'Expression n))
               semi)))]
 [NullStatement
  (λ (n)
    (v-comment
     n
     (h-append semi)))]
 [VariableDeclaration
  (λ (n)
    (v-comment
     n
     (h-append (hs-append
                (text (basic-type-name (ast-child 'typename n)))
                (text (ast-child 'name n))
                eqsign
                (att-value 'pretty-print (ast-child 'Expression n)))
               semi)))]
 [AssignmentExpression
  (λ (n)
    (v-comment
     n
     (hs-append (text (ast-child 'name n))
                eqsign
                (att-value 'pretty-print (ast-child 'Expression n)))))]
 [LiteralInt
  (λ (n)
    (h-comment
     n
     (text (number->string (ast-child 'val n)))))]
 [LiteralFloat
  (λ (n)
    (h-comment
     n
     (text (number->string (ast-child 'val n)))))]
 [VariableReference
  (λ (n)
    (h-comment
     n
     (text (ast-child 'name n))))]
 [FunctionApplicationExpression
  (λ (n)
    (h-comment
     n
     (h-append
      (text (ast-child 'name n))
      lparen
      (h-concat
       (add-between (map (λ (a) (att-value 'pretty-print a))
                         (ast-children (ast-child 'args n)))
                    (h-append comma space)))
      rparen)))]
 [IfExpression
  (λ (n)
    (h-comment
     n
     (h-append lparen
               (hs-append (att-value 'pretty-print (ast-child 'test n))
                          qmark
                          (att-value 'pretty-print (ast-child 'then n))
                          colon
                          (att-value 'pretty-print (ast-child 'else n)))
               rparen)))]

 ;; TODO -- gen the name of the safe op function from the type of the operator
 [AdditionExpression {binary-expression-print/function type->print-add}]
 [SubtractionExpression {binary-expression-print/function type->print-sub}]
 [MultiplicationExpression {binary-expression-print/function type->print-mul}]
 [DivisionExpression {binary-expression-print/function type->print-div}]
 [ModulusExpression {binary-expression-print/function type->print-mod}]

 [UnsafeAdditionExpression {binary-expression-print/infix plus}]
 [UnsafeSubtractionExpression {binary-expression-print/infix minus}]
 [UnsafeMultiplicationExpression {binary-expression-print/infix star}]
 [UnsafeDivisionExpression {binary-expression-print/infix slash}]
 [UnsafeModulusExpression {binary-expression-print/infix percent}]

 [EqualityExpression {binary-expression-print/infix (h-append eqsign eqsign)}]
 [GreaterThanExpression {binary-expression-print/infix greater}]
 [LessThanExpression {binary-expression-print/infix less}]
 [GreaterOrEqualExpression {binary-expression-print/infix (h-append greater eqsign)}]
 [LessOrEqualExpression {binary-expression-print/infix (h-append less eqsign)}]
 )


(ag
 scope-graph-binding
 [FunctionDefinition
  (λ (n) (binding (ast-child 'name n)
                  ;; TODO - decide what should really go here
                  (hash 'declaration-node n
                        'type (append (list '->)
                                      (map (λ (fp) (ast-child 'typename fp))
                                           (ast-children (ast-child 'params n)))
                                      (list (ast-child 'typename n))))))]
 [VariableDeclaration
  (λ (n) (binding (ast-child 'name n)
                  ;; TODO - decide what should really go here
                  (hash 'declaration-node n
                        'type (ast-child 'typename n))))]
 [FormalParam
  (λ (n) (binding (ast-child 'name n)
                  (hash 'declaration-node n
                        'type (ast-child 'typename n))))]
 [Node (λ (n) #f)])

(ag
 illegal-variable-names
 [Node (λ (n) '())]
 [Program (λ (n) (map (λ (cn) (ast-child 'name cn))
                      (ast-children (ast-child 'declarations n))))]
 [Block (λ (n) (map (λ (cn) (ast-child 'name cn))
                    (ast-children (ast-child 'declarations n))))]
 [Declaration (λ (n) (att-value 'illegal-variable-names (parent-node n)))]
 [AssignmentExpression
  (λ (n) (cons (ast-child 'name n)
               (att-value 'illegal-variable-names (parent-node n))))]
 [Expression (λ (n) (att-value 'illegal-variable-names (parent-node n)))]
 )

(ag
 current-function-return-type
 [Statement (λ (n) (att-value 'current-function-return-type (parent-node n)))]
 [FunctionDefinition (λ (n) (ast-child 'typename n))])

(ag
 children-type-dict
 ;; For eg. functions to associate a child node with the type it must be
 [ExpressionStatement (λ (n) (hasheq (ast-child 'Expression n) #f))]
 [ValueReturnStatement (λ (n) (hasheq (ast-child 'Expression n)
                                      (att-value 'current-function-return-type n)))]
 [IfStatement (λ (n) (hasheq (ast-child 'test n) #f))]
 [LoopStatement (λ (n) (hasheq (ast-child 'test n) #f))]
 [ForStatement (λ (n) (hasheq (ast-child 'test n) #f
                              (ast-child 'init n) #f
                              (ast-child 'update n) #f))]
 [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n)
                                     (ast-child 'typename n)))]
 [AssignmentExpression
  (λ (n) (hasheq (ast-child 'Expression n)
                 (hash-ref (binding-bound
                            (resolve-variable-reference-node n))
                           'type)))]
 [FunctionApplicationExpression
  (λ (n)
    (let ([f-bind (resolve-variable-reference-node n)])
      (for/fold ([h (hasheq)])
                ([cn (ast-children (ast-child 'args n))]
                 [t (reverse (cdr (reverse (cdr (hash-ref (binding-bound f-bind)
                                                          'type)))))])
        (hash-set h cn t))))]
 [BinaryExpression (λ (n) (let ([t (or (att-value 'type-context n) (fresh-var-type))])
                            (hasheq (ast-child 'l n) t
                                    (ast-child 'r n) t)))]
 [IntOnlyBinaryExpression (λ (n) (let* ([t (att-value 'type-context n)]
                                        [t (cond [(not t) int-type]
                                                 [(basic-type? t)
                                                  (specify-type t "int")])])
                                   (hasheq (ast-child 'l n) t
                                           (ast-child 'r n) t)))]
 [ComparisonExpression (λ (n) (let ([t (fresh-var-type)])
                                (hasheq (ast-child 'l n) t
                                        (ast-child 'r n) t)))]
 [IfExpression (λ (n) (let ([t (or (att-value 'type-context n) (fresh-var-type))])
                        (hasheq (ast-child 'test n) #f
                                (ast-child 'then n) t
                                (ast-child 'else n) t)))]
 ;; TODO - function call, anything with child expressions...
 )
(ag
 type-context
 [BinaryExpression (λ (n) (let ([t (or (dict-ref (att-value 'children-type-dict
                                                            (parent-node n))
                                                 n))])
                            (or t (fresh-var-type))))]
 [Expression (λ (n) (dict-ref (att-value 'children-type-dict (parent-node n))
                              n))]
 )

(ag
 block-last-statement
 [Block (λ (n) (let ([ns (ast-children (ast-child 'statements n))])
                 (and (not (null? ns)) (car (reverse ns)))))])
(ag
 children-return-position-dict
 ;; Dictionary that will contain true for children that are in return position.
 ;; Else nothing or #f -- IE check with #f as default.
 [Block (λ (n) (if (att-value 'in-return-position? n)
                   (hasheq (att-value 'block-last-statement n) #t)
                   (hasheq)))]
 [IfElseStatement (λ (n) (if (att-value 'in-return-position? n)
                             (hasheq (ast-child 'then n) #t
                                     (ast-child 'else n) #t)
                             (hasheq)))]
 [Statement (λ (n) (hasheq))]
 [FunctionDefinition (λ (n) (hasheq (ast-child 'Block n) #t))])
(ag
 in-return-position?
 [Statement (λ (n) (let ([rp-dict (att-value 'children-return-position-dict
                                             (parent-node n))])
                     (dict-ref rp-dict n #f)))]
 [Node (λ (n) #f)])

(ag
 children-hint-dict
 ;; dictionary will contain a list of hints (or nothing) for each child
 [IfExpression (λ (n) (hasheq (ast-child 'test n) (list bool-hint)))]
 [IfStatement (λ (n) (hasheq (ast-child 'test n) (list bool-hint)
                             (ast-child 'then n) (list block-hint)))]
 [LoopStatement (λ (n) (hasheq (ast-child 'test n) (list bool-hint)
                               (ast-child 'body n) (list block-hint)))]
 [ExpressionStatement
  (λ (n) (hasheq (ast-child 'Expression n)
                 (list assignment-hint application-hint)))]
 [Node (λ (n) (hasheq))])

(ag hints
    [Node (λ (n) (dict-ref (att-value 'children-hint-dict (ast-parent n))
                           n
                           '()))])

(ag
 children-misc-constraint-dict
 ;; dictionary will contain a set of constraints (or nothing) for each child
 [VariableDeclaration (λ (n) (hasheq (ast-child 'Expression n) (list 'constant)))]
 [Node (λ (n) (hasheq))])

(ag misc-constraints
    [Node
     (λ (n)
       (define p (parent-node n))
       (if p
           (let ([cs (set-union (dict-ref (att-value 'children-misc-constraint-dict p)
                                          n
                                          '())
                                (att-value 'misc-constraints p))])
             (if (node-subtype? p 'ExpressionStatement)
                 ;; ExpressionStatements allow assignments and non-constants,
                 ;; but assignments are disallowed everywhere else.
                 (set-subtract cs '(no-assignment constant))
                 (set-union cs '(no-assignment))))
           '()))])


(define ({abstract-binary-op/range op #:unsafe [unsafe #f]} node store flow-returns)
  ;; op is a function of (l-l l-h r-l r-h -> (list low high))
  (match-let* ([(list val-l sto-l ret-l) (abstract-interp-wrap/range
                                          (ast-child 'l node)
                                          store
                                          flow-returns)]
               [(list val-r sto-r ret-r) (abstract-interp-wrap/range
                                          (ast-child 'r node)
                                          sto-l
                                          ret-l)])
    (match val-l
      [(abstract-value/range l-low l-high)
       (match val-r
         [(abstract-value/range r-low r-high)
          (match (op l-low l-high r-low r-high)
            [(list low high)
             (let ([result (abstract-value/range (nan->-inf low) (nan->+inf high))])
               (if unsafe
                   (list result
                         sto-r
                         ret-r)
                   (list (abstract-value-merge/range result val-l)
                         sto-r
                         ret-r)))]
            [else abstract-value/range/top])]
         [else abstract-value/range/top])]
      [else abstract-value/range/top])))

(define {abstract-comparison-op/range op opposite-op}
  {abstract-binary-op/range
   (λ (l-l l-h r-l r-h)
     (cond [(and (op l-l r-l)
                 (op l-l r-h)
                 (op l-h r-l)
                 (op l-h r-h))
            (list 1 1)]
           [(and (opposite-op l-l r-l)
                 (opposite-op l-l r-h)
                 (opposite-op l-h r-l)
                 (opposite-op l-h r-h))
            (list 0 0)]
           [else (list 0 1)]))
   #:unsafe #t})

(define ({abstract-interp-do/range/if one-sided?} n store flow-returns)
  (match-let ([(list (abstract-value/range low high)
                     new-store
                     new-rets)
               (abstract-interp-wrap/range
                (ast-child 'test n) store flow-returns)])
    (cond
      ;; Never false
      [(or (and (< 0 low) (< 0 high))
           (and (> 0 low) (> 0 high)))
       (abstract-interp-wrap/range (ast-child 'then n) new-store new-rets)]
      ;; Never true
      [(and (equal? 0 low) (equal? 0 high))
       (if one-sided?
           (list abstract-value/range/top new-store new-rets)
           (abstract-interp-wrap/range (ast-child 'else n) new-store new-rets))]
      ;; Maybe sometimes true and sometimes false...
      [else
       ;; TODO -- interp BOTH sides and merge the result values and stores
       ;;; (abstract-flow-control-return-merge flow-returns r)
       (match-let ([(list then-v then-s then-r)
                    (abstract-interp-wrap/range (ast-child 'then n)
                                                new-store
                                                new-rets)]
                   [(list else-v else-s else-r)
                    (if one-sided?
                        (list abstract-value/range/top new-store new-rets)
                        (abstract-interp-wrap/range (ast-child 'else n)
                                                    new-store
                                                    new-rets))])
         (list (abstract-value-merge/range then-v else-v)
               (abstract-store-merge*/range then-s else-s)
               (abstract-flow-control-return-merge then-r else-r)))])))

(define (abstract-interp-loop/body n store flow-returns)
  (define altered-refs (att-value 'find-transitive-assignments n))
  (define new-store
    (for/fold ([s store])
              ([a altered-refs])
      (dict-set s a abstract-value/range/top)))
  (define has-return?
    (att-value 'find-a-descendant
               n
               (λ (node)
                 (and (ast-node? node)
                      (node-subtype? node 'ReturnStatement)))))
  ;; interp the test once generically for analysis info to be properly generic
  (abstract-interp-wrap/range (ast-child 'test n)
                              range-store-top
                              empty-abstract-flow-control-return)
  (if has-return?
      (match-let ([(list v s r)
                   (abstract-interp-wrap/range (ast-child 'body n)
                                               new-store
                                               flow-returns)])
        (list abstract-value/range/top
              s
              (abstract-flow-control-return-only-maybe-ify
               (abstract-flow-control-return-merge flow-returns r))))
      (list abstract-value/range/top new-store flow-returns)))


(define addition-op/range
  (λ (l-l l-h r-l r-h)
    (list (+ l-l r-l) (+ l-h r-h))))
(define subtraction-op/range
  (λ (l-l l-h r-l r-h)
    (list (- l-l r-h) (- l-h r-l))))
(define multiplication-op/range
  (λ (l-l l-h r-l r-h)
    (let ([signl (if (equal? (negative? l-l) (negative? l-h))
                     (if (negative? l-l) '- '+)
                     'both)]
          [signr (if (equal? (negative? r-l) (negative? r-h))
                     (if (negative? r-l) '- '+)
                     'both)])
      (match (list signl signr)
        ['(+ +) (list (* l-l r-l) (* l-h r-h))]
        ['(+ both) (list (* l-h r-l) (* l-h r-h))]
        ['(+ -) (list (* l-h r-l) (* l-l r-h))]
        ['(both +) (list (* l-l r-h) (* l-h r-h))]
        ['(both both) (list (min (* l-l r-h) (* l-h r-l))
                            (max (* l-l r-l) (* l-h r-h)))]
        ['(both -) (list (* l-h r-l) (* l-l r-l))]
        ['(- +) (list (* l-l r-h) (* l-h r-l))]
        ['(- both) (list (* l-l r-h) (* l-l r-l))]
        ['(- -) (list (* l-h r-h) (* l-l r-l))]))))
;; TODO - make a real transfer functions.
;; Division and modulus are both undefined when the divisor is 0,
;; division is undefined if the numerator is INT_MIN and the denominator is -1,
;; and I'm not sure about modulus in that case.
(define division-op/range
  (λ (l-l l-h r-l r-h) (list -inf.0 +inf.0)))
(define modulus-op/range
  (λ (l-l l-h r-l r-h) (list -inf.0 +inf.0)))

(define rosette-last-ast-serial-number -1)
(define (maybe-reset-rosette-assertions! n)
  (define current-ast-serial (att-value 'ast-serial-number n))
  (when (not (equal? rosette-last-ast-serial-number
                     current-ast-serial))
    (set! rosette-last-ast-serial-number current-ast-serial)
    (rt:clear-asserts!)))

(add-ag-rule
 cish2-rules
 symbolic-interp
 ;; Get the single global result
 [Node (λ (n)
         (maybe-reset-rosette-assertions! n)
         ;; Interp the containing function to fill the result hash.
         (define result
           (symbolic-interp-wrap
            (att-value 'get-containing-function-definition n)
            symbolic-store-top
            '()
            #f
            '()))
         ;; If the code is unreachable then it will have no result here.
         (match (hash-ref
                 (att-value 'symbolic-interp-result-hash n)
                 n
                 'dead)
           ['dead (list 'dead 'dead)]
           [(list (list vals stores always-rets assert-sets) ...)
            (define fv (fresh-symbolic-var (att-value 'type-context n)))
            (define val-assert (apply rt:|| (map (λ (v) (rt:= fv v)) vals)))
            (list fv (apply set-union (list val-assert) assert-sets))]))])
(ag
 abstract-interp/range
 ;; Get the single global result of abstract interpretation of this node.
 [Node (λ (n)
         ;; Interp the containing function to fill the result hash.
         (define result
           (abstract-interp-wrap/range
            (att-value 'get-containing-function-definition n)
            range-store-top
            empty-abstract-flow-control-return))
         ;; If the code is unreachable then it will have no result here.
         (match (hash-ref
                 (att-value 'abstract-interp-result-hash/range n)
                 n
                 'dead)
           ['dead 'dead]
           [(list (list vs ss rs) ...)
            (list (apply abstract-value-merge*/range vs)
                  (apply abstract-store-merge*/range ss)
                  (apply abstract-flow-control-return-merge* rs))]))])
(ag
 get-containing-function-definition
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main"))
                                n
                                (ast-parent n)))]
 [Program (λ (n) n)]
 [Node (λ (n) (att-value 'get-containing-function-definition (ast-parent n)))])

(ag
 ;; This is essentially the same as abstract-interp-result-hash/range
 symbolic-interp-result-hash
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main"))
                                (make-hasheq)
                                (att-value 'symbolic-interp-result-hash
                                           (ast-parent n))))]
 [Program (λ (n) (make-hasheq))]
 [Node (λ (n) (att-value 'symbolic-interp-result-hash (ast-parent n)))]
 )

(ag
 ;; To get a general interp result for any given node, we need to cache
 ;; the results of evaluation when we interpret the whole function it is in.
 ;; We collect results in this hash, then merge if there are multiple results.
 ;; The hash is MUTATED during interpretation to achieve this.
 ;; The hash holds a list of results for each node (or nothing -- use '() as
 ;; a default).  Although an empty result should mean the node is dead code.
 abstract-interp-result-hash/range
 [FunctionDefinition (λ (n) (if (not (equal? (ast-child 'name n) "main"))
                                (make-hasheq)
                                (att-value 'abstract-interp-result-hash/range
                                           (ast-parent n))))]
 [Program (λ (n) (make-hasheq))]
 [Node (λ (n) (att-value 'abstract-interp-result-hash/range (ast-parent n)))]
 )

(ag
 ;; For implementing abstract-interp/range.
 ;; For now, store is table from binding to abstract value.
 ;; Returns (list abstract-value new-store)
 ;; Note - For functions and operators, argument evaluation order is unspecified.
 ;;        So the store coming out of each side should be the same.
 ;;        This should be enforced by disallowing assignment in these places.
 abstract-interp-do/range

 ;;; Program
 [Program
  (λ (n store flow-returns)
    (define init-store
      (for/fold ([store range-store-top])
                ([global (filter (λ (cn) (node-subtype? cn 'VariableDeclaration))
                                 (ast-children (ast-child 'declarations n)))])
        (match-let* ([(list v n-store n-rets)
                      (abstract-interp-wrap/range
                       global
                       store
                       empty-abstract-flow-control-return)])
          n-store)))
    (abstract-interp-wrap/range
     (ast-child 'main n)
     init-store
     empty-abstract-flow-control-return))]
 [VariableDeclaration
  (λ (n store flow-returns)
    (match-let* ([ref (resolve-variable-reference-node n)]
                 [(list v n-store n-rets)
                  (abstract-interp-wrap/range
                   (ast-child 'Expression n)
                   store
                   empty-abstract-flow-control-return)])
      (list v (dict-set n-store ref v) n-rets)))]

 ;;; Statements
 ;;; Statements return a store but aside from return statements the
 ;;; result value is meaningless
 [NullStatement (λ (n store flow-returns)
                  (list abstract-value/range/top store flow-returns))]
 #|
 TODO - there are no void functions yet, so once there are this (and all
 non-return statements) should return void.
 |#
 #;[VoidReturnStatement
    (λ (n store flow-returns)
      (list abstract-value/range/top store
            (must-return flow-returns abstract-value/range/top store)))]
 [ExpressionStatement
  (λ (n store flow-returns)
    (abstract-interp-wrap/range
     (ast-child 'Expression n) store flow-returns))]
 [ValueReturnStatement
  (λ (n store flow-returns)
    (match (abstract-interp-wrap/range
            (ast-child 'Expression n) store flow-returns)
      [(list v s r)
       (list v s (must-return r v s))]))]
 [Block
  (λ (n store flow-returns)
    (define-values (store-with-decls ret-with-decls)
      (for/fold ([s store]
                 [r flow-returns])
                ([decl (ast-children (ast-child 'declarations n))])
        ;; There are declaration holes and such, so check that it is a variable decl.
        (if (equal? (ast-node-type decl) 'VariableDeclaration)
            (match-let* ([(list v n-store n-rets)
                          (abstract-interp-wrap/range decl s r)])
              (values n-store n-rets))
            (values s r))))
    (define-values (store-after-statements rets-after-statements)
      (for/fold ([s store-with-decls]
                 [r ret-with-decls])
                ([statement (ast-children (ast-child 'statements n))])
        #:break (abstract-flow-control-return-must r)
        (match-let ([(list v n-store n-rets) (abstract-interp-wrap/range
                                              statement
                                              s
                                              r)])
          (values n-store n-rets))))
    (list abstract-value/range/top store-after-statements rets-after-statements))]
 [IfStatement
  {abstract-interp-do/range/if #t}]
 [IfElseStatement
  {abstract-interp-do/range/if #f}]

 [ForStatement
  (λ (n store flow-returns)
    (match-let*
        ([(list v store flow-returns)
          (abstract-interp-wrap/range (ast-child 'init n) store flow-returns)]
         [(list (abstract-value/range low high) store flow-returns)
          (abstract-interp-wrap/range (ast-child 'test n) store flow-returns)])
      (if (and (equal? low 0) (equal? high 0))
          (list abstract-value/range/top store flow-returns)
          (begin

            ;; interp the update once generically for analysis info to be properly generic
            (abstract-interp-wrap/range (ast-child 'update n)
                                        range-store-top
                                        empty-abstract-flow-control-return)
            (abstract-interp-loop/body n store flow-returns)))))]
 [WhileStatement
  (λ (n store flow-returns)
    (match-let* ([(list (abstract-value/range low high) store flow-returns)
                  (abstract-interp-wrap/range (ast-child 'test n) store flow-returns)])
      (if (and (equal? low 0) (equal? high 0))
          (list abstract-value/range/top store flow-returns)
          (abstract-interp-loop/body n store flow-returns))))]
 [DoWhileStatement
  (λ (n store flow-returns)
    (abstract-interp-loop/body n store flow-returns))]

 ;;; Expressions
 [LiteralInt
  (λ (n store flow-returns)
    (list (abstract-value/range (ast-child 'val n) (ast-child 'val n))
          store flow-returns))]
 [LiteralFloat
  (λ (n store flow-returns)
    (list (abstract-value/range (ast-child 'val n) (ast-child 'val n))
          store flow-returns))]

 [IfExpression
  {abstract-interp-do/range/if #f}]

 ;; Function Definitions will be evaluated with store and arguments as top for
 ;; analyzing for potential code transformations.
 [FunctionDefinition
  (λ (func-def-node store flow-returns)
    (match (abstract-interp-wrap/range
            (ast-child 'Block func-def-node)
            range-store-top
            empty-abstract-flow-control-return)
      [(list v s returns)
       (append (abstract-flow-control-return->val-store-list/range returns)
               (list flow-returns))]))]
 ;; Function applications will be evaluated with the arguments given.
 [FunctionApplicationExpression
  (λ (n store flow-returns)
    (match-let* ([binding (resolve-variable-reference-node n)]
                 [bound (binding-bound binding)]
                 [func-def-node (dict-ref bound 'declaration-node)]
                 [func-block (ast-child 'Block func-def-node)]
                 [func-params (ast-children (ast-child 'params func-def-node))]
                 [(list reversed-args store rets)
                  (values->list
                   (for/fold ([args-so-far '()]
                              [store store]
                              [rets flow-returns])
                             ([expr (ast-children (ast-child 'args n))])
                     (match-define (list v s r)
                       (abstract-interp-wrap/range expr store rets))
                     (values (cons v args-so-far) s r)))]
                 [store-for-func (for/fold ([store store])
                                           ([fp func-params]
                                            [arg (reverse reversed-args)])
                                   (dict-set store fp arg))])
      (match (abstract-interp-wrap/range
              func-block
              store-for-func
              empty-abstract-flow-control-return)
        [(list v s returns)
         (append (abstract-flow-control-return->val-store-list/range returns)
                 (list flow-returns))])))]

 [AssignmentExpression
  (λ (n store flow-returns)
    (match-let ([(list val new-store new-rets)
                 (abstract-interp-wrap/range
                  (ast-child 'Expression n)
                  store
                  flow-returns)]
                [ref-node (resolve-variable-reference-node n)])
      (list val (dict-set new-store ref-node val) new-rets)))]
 [VariableReference
  (λ (n store flow-returns)
    (let ([ref-node (resolve-variable-reference-node n)])
      ;; TODO -- I'm using an empty hash to represent an unknown state of the store,
      ;; or at least an unknown state for a variable.  But once there are more than
      ;; ints and floats I'll need to look at the type of the reference to know what
      ;; kind of value to use for top.
      (list (dict-ref store ref-node abstract-value/range/top)
            store
            flow-returns)))]

 [AdditionExpression
  {abstract-binary-op/range addition-op/range}]
 [UnsafeAdditionExpression
  {abstract-binary-op/range addition-op/range #:unsafe #t}]
 [SubtractionExpression
  {abstract-binary-op/range subtraction-op/range}]
 [UnsafeSubtractionExpression
  {abstract-binary-op/range subtraction-op/range #:unsafe #t}]
 [MultiplicationExpression
  {abstract-binary-op/range multiplication-op/range}]
 [UnsafeMultiplicationExpression
  {abstract-binary-op/range multiplication-op/range #:unsafe #t}]
 [DivisionExpression
  {abstract-binary-op/range division-op/range}]
 [UnsafeDivisionExpression
  {abstract-binary-op/range division-op/range #:unsafe #t}]
 [ModulusExpression
  {abstract-binary-op/range modulus-op/range}]
 [UnsafeModulusExpression
  {abstract-binary-op/range modulus-op/range #:unsafe #t}]

 [EqualityExpression
  {abstract-binary-op/range
   (λ (l-l l-h r-l r-h)
     (let ([equal-val (foldl (λ (l r) (and (equal? l r) r))
                             l-l (list l-h r-l r-h))]
           [no-overlap? (or (and (< l-l r-l r-h)
                                 (< l-h r-l r-h))
                            (and (< r-l l-l l-h)
                                 (< r-h l-l l-h)))])
       (cond [equal-val (list 1 1)]
             [no-overlap? (list 0 0)]
             [else (list 0 1)])))}]
 [LessThanExpression {abstract-comparison-op/range < >=}]
 [GreaterThanExpression {abstract-comparison-op/range > <=}]
 [LessOrEqualExpression {abstract-comparison-op/range <= >}]
 [GreaterOrEqualExpression {abstract-comparison-op/range >= <}]
 )

;;; safety-pred is a function of (l-l l-h r-l r-h -> bool)
(define ({safe-binary-op-swap/range unsafe-version safety-pred} n)
  (define l (ast-child 'l n))
  (define r (ast-child 'r n))
  (define l-result (att-value 'abstract-interp/range (ast-child 'l n)))
  (define r-result (att-value 'abstract-interp/range (ast-child 'r n)))
  (if (member 'dead (list l-result r-result))
      #f
      (match-let ([(list (abstract-value/range l-l l-h) store-l returns-l) l-result]
                  [(list (abstract-value/range r-l r-h) store-r returns-r) r-result])
        (if (safety-pred l-l l-h r-l r-h)
            unsafe-version
            #f))))

(define ({result-in-bounds/range range-op min max} l-l l-h r-l r-h)
  (define ({bounded-range min max} low high)
    (and (<= min low) (<= min high) (>= max low) (>= max high)))
  (apply (bounded-range min max) (range-op l-l l-h r-l r-h)))
(define (division-safety-check/range l-l l-h r-l r-h)
  (and
   ;; No division by zero.
   (or (and (< 0 r-l) (< 0 r-h))
       (and (> 0 r-l) (> 0 r-h)))
   ;; No INT_MIN / -1
   (or (< INT_MIN l-h)
       (or (and (< -1 r-l) (< -1 r-h))
           (and (> -1 r-l) (> -1 r-h))))))

(define (verify assert-to-verify other-asserts)
  (rt:solver-push (rt:current-solver))
  (rt:solver-assert (rt:current-solver) other-asserts)
  (rt:solver-assert (rt:current-solver) (list (rt:! assert-to-verify)))
  (define result
    (with-handlers ([(λ _ #t) (λ _ #f)])
      ;; TODO - sometimes I get what looks like a Rosette error:
      ;; “solution: unrecognized solver output: (error line 19 column 10: model is not available)”
      (rt:solver-check (rt:current-solver))))
  (rt:solver-pop (rt:current-solver))
  (rt:unsat? result))

(define ({result-in-bounds/symbolic range-op min max} l r asserts)
  (define ({bounded-symbolic min max} v)
    (verify (rt:&& (rt:>= v min) (rt:<= v max))
            asserts))
  ({bounded-symbolic min max} (range-op l r)))
(define (division-safety-check/symbolic l r asserts)
  (verify (rt:&& (rt:! (rt:= r 0))
                 (rt:|| (rt:! (rt:= l INT_MIN))
                        (rt:! (rt:= r -1))))
          asserts))
(define ({safe-binary-op-swap/symbolic unsafe-version safety-pred} n)
  ;; safety-pred is a predicate on two symbolic values
  (define l (ast-child 'l n))
  (define r (ast-child 'r n))
  (match-define (list l-result l-asserts)
    (att-value 'symbolic-interp (ast-child 'l n)))
  (match-define (list r-result r-asserts)
    (att-value 'symbolic-interp (ast-child 'r n)))
  (if (member 'dead (list l-result r-result))
      #f
      (if (safety-pred l-result r-result (set-union l-asserts r-asserts))
          unsafe-version
          #f)))

(define {symbolic-binary-op #:safety-clause [make-violation-condition #f]
                            #:result-clause make-normal-result
                            #:bool-result? [bool-result? #f]}
  ;; safety-clause and result-clause are both functions that take a
  ;; left and right rosette clause and returns a new clause.
  ;; The safety-clause should be true if undefined behavior would be tripped,
  ;; the result-clause should be the result of the unsafe operation.
  (λ (n store path-condition return-variable assertions)
    (define type (att-value 'type-context n))
    (match-let* ([(list v-l s-l ar-l asserts-l)
                  (symbolic-interp-wrap
                   (ast-child 'l n)
                   store path-condition return-variable assertions)]
                 [(list v-r s-r ar-r asserts-r)
                  (symbolic-interp-wrap
                   (ast-child 'r n)
                   s-l path-condition return-variable asserts-l)]
                 ;; rt:/ errors if the right hand arg is known to be 0...
                 ;; So even though I'm going to check it later, I can't allow
                 ;; it here either.
                 [div-guard? (member make-normal-result (list rt:/ rt:modulo))]
                 [fresh-r (if div-guard? (fresh-symbolic-var type) #f)]
                 [normal-result (if div-guard?
                                    (make-normal-result v-l fresh-r)
                                    (make-normal-result v-l v-r))]
                 [div-guard-assert (if div-guard?
                                       (rt:=> (rt:! (rt:= 0 v-r))
                                              (rt:= v-r fresh-r))
                                       #t)]
                 [fv (fresh-symbolic-var (att-value 'type-context n))])
      (cond [make-violation-condition
             (let* ([violation-condition (make-violation-condition v-l v-r)]
                    [safety-assertion (rt:&& (rt:=> violation-condition
                                                    (rt:= fv v-l))
                                             (rt:=> (rt:! violation-condition)
                                                    (rt:= fv normal-result)))])
               (list fv s-r #f (set-union asserts-r
                                          (list safety-assertion div-guard-assert))))]
            [bool-result?
             (list fv s-r #f (set-union asserts-r
                                        (list (rt:=> (rt:! normal-result)
                                                     (rt:= 0 fv))
                                              (rt:=> normal-result
                                                     (rt:= 1 fv))
                                              div-guard-assert)))]
            [else (list fv s-r #f (set-union asserts-r
                                             (list (rt:= fv normal-result)
                                                   div-guard-assert)))]))))

(define {make-normal-symbolic-safety-func op}
  (λ (l r) (rt:|| (rt:> (op l r) INT_MAX)
                  (rt:< (op l r) INT_MIN))))
(define symbolic-addition-safety {make-normal-symbolic-safety-func rt:+})
(define symbolic-subtraction-safety {make-normal-symbolic-safety-func rt:-})
(define symbolic-multiplication-safety {make-normal-symbolic-safety-func rt:*})
(define symbolic-division-safety
  (λ (l r)
    (rt:|| (rt:= r 0)
           (rt:&& (rt:= l INT_MIN)
                  (rt:= r -1)))))



(ag
 ;;; Return #f if the unsafe op can't be safely used,
 ;;; otherwise return the name of the unsafe type to be used
 ;;; as a refinement.
 unsafe-op-if-possible/range
 [AdditionExpression
  {safe-binary-op-swap/range
   'UnsafeAdditionExpression
   {result-in-bounds/range addition-op/range INT_MIN INT_MAX}}]
 [SubtractionExpression
  {safe-binary-op-swap/range
   'UnsafeSubtractionExpression
   {result-in-bounds/range subtraction-op/range INT_MIN INT_MAX}}]
 [MultiplicationExpression
  {safe-binary-op-swap/range
   'UnsafeMultiplicationExpression
   {result-in-bounds/range multiplication-op/range INT_MIN INT_MAX}}]
 [DivisionExpression
  {safe-binary-op-swap/range
   'UnsafeDivisionExpression
   division-safety-check/range}]
 [ModulusExpression
  {safe-binary-op-swap/range
   'UnsafeModulusExpression
   division-safety-check/range}])

(ag
 unsafe-op-if-possible/symbolic
 [AdditionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeAdditionExpression
   {result-in-bounds/symbolic rt:+ INT_MIN INT_MAX}}]
 [SubtractionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeSubtractionExpression
   {result-in-bounds/symbolic rt:- INT_MIN INT_MAX}}]
 [MultiplicationExpression
  {safe-binary-op-swap/symbolic
   'UnsafeMultiplicationExpression
   {result-in-bounds/symbolic rt:* INT_MIN INT_MAX}}]
 [DivisionExpression
  {safe-binary-op-swap/symbolic
   'UnsafeDivisionExpression
   division-safety-check/symbolic}]
 [ModulusExpression
  {safe-binary-op-swap/symbolic
   'UnsafeModulusExpression
   division-safety-check/symbolic}])

(define (symbolic-store-merge s1 pc1 s2 pc2)
  (define new-asserts '())
  (define new-store
    (for/hash ([k (set-union (hash-keys s1) (hash-keys s2))])
      (define v1 (hash-ref s1 k 'not-found))
      (define v2 (hash-ref s2 k 'not-found))
      (cond [(equal? v1 'not-found) (values k v2)]
            [(equal? v2 'not-found) (values k v1)]
            [(equal? v1 v2) (values k v1)]
            [else
             (define fv (fresh-symbolic-var (dict-ref (binding-bound k) 'type)))
             (set! new-asserts (set-add new-asserts (rt:=> (asserts->condition pc1)
                                                           (rt:= fv v1))))
             (set! new-asserts (set-add new-asserts (rt:=> (asserts->condition pc2)
                                                           (rt:= fv v2))))
             (values k fv)])))
  (values new-store new-asserts))

(define (asserts->condition asserts)
  (apply rt:&& (set->list asserts)))

(define {symbolic-if-interp one-sided? type}
  (λ (n store path-condition return-variable assertions)
    (match-define (list test-val test-store test-always-ret assertions-with-test)
      (symbolic-interp-wrap (ast-child 'test n)
                            store path-condition return-variable assertions))
    (define assert-test
      (if (rt:boolean? test-val)
          test-val
          (rt:! (rt:= 0 test-val))))
    (define pc-true (set-add path-condition assert-test))
    (define pc-false (set-add path-condition (rt:! assert-test)))
    (define then-rets
      (symbolic-interp-wrap (ast-child 'then n)
                            test-store
                            pc-true
                            return-variable
                            (set-add assertions-with-test assert-test)))
    (match-define (list then-v then-store then-always-rets then-asserts-pre)
      then-rets)
    (define then-asserts-unique (set-subtract then-asserts-pre assertions-with-test))
    (define asserts-with-then
      (set-add assertions-with-test
               (rt:=> (asserts->condition pc-true)
                      (asserts->condition then-asserts-unique))))

    (if one-sided?
        (let-values ([(merged-store merge-asserts)
                      (symbolic-store-merge test-store pc-false then-store pc-true)])
          (list #t
                merged-store
                #f
                (set-union asserts-with-then merge-asserts)))
        (let ()
          (match-define
            (list else-v else-store else-always-rets else-asserts-pre)
            (symbolic-interp-wrap (ast-child 'else n)
                                  test-store
                                  pc-false
                                  return-variable
                                  (set-add assertions-with-test (rt:! assert-test))))
          (define else-asserts-unique
            (set-subtract else-asserts-pre assertions-with-test))
          (define asserts-with-else
            (set-add asserts-with-then
                     (rt:=> (asserts->condition pc-false)
                            (asserts->condition else-asserts-unique))))
          (define always-ret? (and then-always-rets else-always-rets))
          (define-values (merged-store merge-asserts)
            (symbolic-store-merge else-store pc-false then-store pc-true))
          (define asserts-post-merge
            (set-union asserts-with-else merge-asserts))
          (if (not type)
              (list #t
                    merged-store
                    always-ret?
                    asserts-post-merge)
              (let ([v (fresh-symbolic-var type)])
                (list v
                      merged-store
                      always-ret?
                      (set-add asserts-post-merge
                               (rt:&& (rt:=> (asserts->condition pc-true)
                                             (rt:= then-v v))
                                      (rt:=> (asserts->condition pc-false)
                                             (rt:= else-v v)))))))))))

(define (symbolic-interp-loop/body n store path-condition return-variable assertions)
  (define altered-refs (att-value 'find-transitive-assignments n))
  (define new-store
    (for/fold ([s store])
              ([a altered-refs])
      (dict-set s a (fresh-symbolic-var (dict-ref (binding-bound a) 'type)))))
  (define has-return?
    (att-value 'find-a-descendant
               n
               (λ (node)
                 (and (ast-node? node)
                      (node-subtype? node 'ReturnStatement)))))
  (match-define (list test-val t-store t-always-rets t-asserts)
    (symbolic-interp-wrap (ast-child 'test n)
                          new-store
                          path-condition
                          return-variable
                          assertions))

  (define assert-test
    (if (rt:boolean? test-val)
        test-val
        (rt:! (rt:= 0 test-val))))
  (define pc-true (set-add path-condition assert-test))
  (define pc-false (set-add path-condition (rt:! assert-test)))
  ;; TODO - I'm not sure right now what I want to do with the loop body.
  ;;        I don't want to try some sort of unrolling, so for now I'll
  ;;        just punt any assigned variables to fresh variables.
  ;;        I really should capture potential returns.  I'll do that later.
  (match-define (list b-v b-store b-always-rets n-asserts)
    (symbolic-interp-wrap (ast-child 'body n)
                          t-store
                          (set-add path-condition assert-test)
                          return-variable
                          assertions))
  (list #f b-store #f t-asserts))

(ag
 symbolic-interp-do
 ;; This rule adds rosette assertions, so it should only be called when
 ;; the rosette environment has been prepared (eg. pushed/popped an
 ;; interpreter, so two interpretations don't step on each other).

 ;; Returns (list val=[symbolic expression] store=[binding->symbolic-expression hash] always-returns=[bool])

 ;;; Declarations
 [Program
  (λ (n store path-condition return-variable assertions)
    (define-values (init-store init-asserts)
      (for/fold ([store store]
                 [assertions assertions])
                ([global (filter (λ (cn) (node-subtype? cn 'VariableDeclaration))
                                 (ast-children (ast-child 'declarations n)))])
        (match-let* ([(list v n-store n-rets n-asserts)
                      (symbolic-interp-wrap
                       global
                       store
                       path-condition
                       return-variable
                       assertions)])
          (values n-store n-asserts))))
    (define main (ast-child 'main n))
    (define main-ret (fresh-symbolic-var int-type))
    (symbolic-interp-wrap
     main
     init-store
     path-condition
     main-ret
     init-asserts))]
 ;; Function Definitions will be evaluated with store and arguments as top for
 ;; analyzing for potential code transformations.
 [FunctionDefinition
  (λ (n store path-condition return-variable assertions)
    (define ret-var (fresh-symbolic-var (ast-child 'typename n)))
    (symbolic-interp-wrap
     (ast-child 'Block n)
     symbolic-store-top
     '()
     ret-var
     assertions))]
 [VariableDeclaration
  (λ (n store path-condition return-variable assertions)
    (let* ([name (ast-child 'name n)]
           [type (ast-child 'typename n)]
           [sym-var (fresh-symbolic-var type)]
           [ref (resolve-variable-reference-node n)])
      (match-define (list v n-store always-rets n-asserts)
        (symbolic-interp-wrap (ast-child 'Expression n)
                              store path-condition return-variable assertions))
      (list v
            (dict-set n-store ref sym-var)
            #f
            (set-add n-asserts (rt:= sym-var v)))))]

 ;;; Statements
 [WhileStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-loop/body n store path-condition return-variable assertions))]
 [DoWhileStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-loop/body n store path-condition return-variable assertions))]
 [ForStatement
  (λ (n store path-condition return-variable assertions)
    (match-define (list i-v i-store always-rets i-asserts)
      (symbolic-interp-wrap (ast-child 'init n)
                            store path-condition return-variable assertions))
    (define u-store
      (for/fold ([s i-store])
                ([a (att-value 'find-transitive-assignments (ast-child 'update n))])
        (dict-set s a (fresh-symbolic-var (dict-ref (binding-bound a) 'type)))))
    (symbolic-interp-loop/body n u-store path-condition return-variable i-asserts))]
 [Block
  (λ (n store path-condition return-variable assertions)
    (define (rec store asserts children-left)
      (if (null? children-left)
          (list #f store #f asserts)
          (match-let ([(list v n-store always-rets n-asserts)
                       (symbolic-interp-wrap (car children-left)
                                             store
                                             path-condition
                                             return-variable
                                             asserts)])
            (if always-rets
                (list #f n-store always-rets n-asserts)
                (rec n-store n-asserts (cdr children-left))))))
    (rec store assertions (ast-children (ast-child 'statements n))))]
 [ValueReturnStatement
  (λ (n store path-condition return-variable assertions)
    (match-define (list v n-store always-rets n-asserts)
      (symbolic-interp-wrap (ast-child 'Expression n)
                            store path-condition return-variable assertions))
    (list v
          n-store
          #t
          (set-add n-asserts (rt:=> (apply rt:&& path-condition)
                                    (rt:= v return-variable)))))]
 [IfStatement {symbolic-if-interp #t #f}]
 [IfElseStatement {symbolic-if-interp #f #f}]
 [ExpressionStatement
  (λ (n store path-condition return-variable assertions)
    (symbolic-interp-wrap (ast-child 'Expression n)
                          store path-condition return-variable assertions))]
 [NullStatement
  (λ (n store path-condition return-variable assertions)
    (list #f store #f assertions))]

 ;;; Expressions
 [FunctionApplicationExpression
  (λ (n store path-condition return-variable assertions)
    ;; TODO - this is almost the same as the one for abstract-interp/range
    ;; I should abstract over them somehow.
    (define ref-node (resolve-variable-reference-node n))
    (define def-node (dict-ref (binding-bound ref-node)
                               'declaration-node))
    (define func-block (ast-child 'Block def-node))
    (define func-params (ast-children (ast-child 'params def-node)))
    (match-define (list reversed-args store-post-arguments asserts-post-arguments)
      (values->list
       (for/fold ([args-so-far '()]
                  [store store]
                  [asserts assertions])
                 ([expr (ast-children (ast-child 'args n))])
         (match-define (list v s ar n-asserts)
           (symbolic-interp-wrap expr store path-condition return-variable asserts))
         (values (cons v args-so-far) s n-asserts))))
    (define store-for-func (for/fold ([store store-post-arguments])
                                     ([fp func-params]
                                      [arg (reverse reversed-args)])
                             (dict-set store
                                       (resolve-variable-reference-node fp)
                                       arg)))

    (define ret-type (ast-child 'typename def-node))
    (define return-var-for-func (fresh-symbolic-var ret-type))

    (match (symbolic-interp-wrap func-block
                                 store-for-func
                                 path-condition
                                 return-var-for-func
                                 asserts-post-arguments)
      [(list v s always-rets as)
       (list return-var-for-func s #f as)]))]
 [IfExpression
  (λ (n store path-condition return-variable assertions)
    ({symbolic-if-interp #f
                         (or (att-value 'type-context n)
                             (att-value 'type-context
                                        (ast-child 'then n)))}
     n store path-condition return-variable assertions))]
 [VariableReference
  (λ (n store path-condition return-variable assertions)
    (define ref (resolve-variable-reference-node n))
    (define val (hash-ref store ref
                          (λ () (fresh-symbolic-var
                                 (hash-ref (binding-bound ref) 'type)))))
    (list val
          store
          #f
          assertions))]
 [AssignmentExpression
  (λ (n store path-condition return-variable assertions)
    (match-let ([(list val new-store always-rets n-asserts)
                 (symbolic-interp-wrap
                  (ast-child 'Expression n)
                  store
                  path-condition
                  return-variable
                  assertions)]
                [ref-node (resolve-variable-reference-node n)])
      (list val (dict-set new-store ref-node val) #f n-asserts)))]
 [LiteralInt
  (λ (n store path-condition return-variable assertions)
    (list (ast-child 'val n) store #f assertions))]
 [LiteralFloat
  (λ (n store path-condition return-variable assertions)
    (list (ast-child 'val n) store #f assertions))]
 [AdditionExpression
  {symbolic-binary-op #:safety-clause symbolic-addition-safety
                      #:result-clause rt:+}]
 [SubtractionExpression
  {symbolic-binary-op #:safety-clause symbolic-subtraction-safety
                      #:result-clause rt:-}]
 [MultiplicationExpression
  {symbolic-binary-op #:safety-clause symbolic-multiplication-safety
                      #:result-clause rt:*}]
 [DivisionExpression
  {symbolic-binary-op #:safety-clause symbolic-division-safety
                      #:result-clause rt:/}]
 [ModulusExpression
  {symbolic-binary-op #:safety-clause symbolic-division-safety
                      #:result-clause rt:modulo}]

 [UnsafeAdditionExpression
  {symbolic-binary-op #:result-clause rt:+}]
 [UnsafeSubtractionExpression
  {symbolic-binary-op #:result-clause rt:-}]
 [UnsafeMultiplicationExpression
  {symbolic-binary-op #:result-clause rt:*}]
 [UnsafeDivisionExpression
  {symbolic-binary-op #:result-clause rt:/}]
 [UnsafeModulusExpression
  {symbolic-binary-op #:result-clause rt:modulo}]

 [EqualityExpression
  {symbolic-binary-op #:result-clause rt:=
                      #:bool-result? #t}]
 [LessThanExpression
  {symbolic-binary-op #:result-clause rt:<
                      #:bool-result? #t}]
 [GreaterThanExpression
  {symbolic-binary-op #:result-clause rt:>
                      #:bool-result? #t}]

 [LessOrEqualExpression
  {symbolic-binary-op #:result-clause rt:<=
                      #:bool-result? #t}]
 [GreaterOrEqualExpression
  {symbolic-binary-op #:result-clause rt:>=
                      #:bool-result? #t}]
 )


(ag
 find-direct-resolved
 [Node (λ (n node-type)
         (remove-duplicates
          (map resolve-variable-reference-node
               (att-value 'find-descendants
                          n (λ (cn) (node-subtype? cn node-type))))))])
(ag
 find-direct-assignments
 [Node (λ (n) (att-value 'find-direct-resolved n 'AssignmentExpression))])
(ag
 find-direct-function-call-refs
 [Node (λ (n) (att-value 'find-direct-resolved n 'FunctionApplicationExpression))])
(ag
 find-direct-variable-references
 [Node (λ (n) (att-value 'find-direct-resolved n 'VariableReference))])

(ag
 find-transitive-function-call-refs
 [Node (λ (n)
         (define (rec to-search searched)
           (if (empty? to-search)
               searched
               (let* ([calls (att-value 'find-direct-function-call-refs
                                        (hash-ref (binding-bound (car to-search))
                                                  'declaration-node))]
                      [searched (cons (car to-search) searched)]
                      [new-calls (set-subtract calls searched)]
                      [to-search (append new-calls (cdr to-search))])
                 (rec to-search searched))))
         (rec (att-value 'find-direct-function-call-refs n) '()))])

(ag
 find-transitive-resolved
 [Node (λ (n node-type)
         (remove-duplicates
          (append (att-value 'find-direct-resolved n node-type)
                  (flatten
                   (map (λ (r) (att-value 'find-direct-resolved
                                          (hash-ref (binding-bound r)
                                                    'declaration-node)
                                          node-type))
                        (att-value 'find-transitive-function-call-refs n))))))])
(ag
 find-transitive-assignments
 [Node (λ (n) (att-value 'find-transitive-resolved n 'AssignmentExpression))])
(ag
 find-transitive-variable-references
 [Node (λ (n) (att-value 'find-transitive-resolved n 'VariableReference))])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; choice rules


(define-syntax-parser cm
  [(_ method [node-name lambda-body] ...+)
   #'(add-choice-rule cish2-rules method [node-name (λ () lambda-body)] ...)])

#|
Apparently class definitions don't let public methods be defined with
let-over-lambda (maybe the class macro rewrites the lambdas...).
So let's have a weak hash table store the mutable state we need in a
few of these methods.
|#

(define ref-choices-filtered-hash (make-weak-hasheq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (hinted-choice-weight stx)
  (syntax-parse stx
    [(_ base-weight hint-name)
     #'(begin
         (define bw base-weight)
         ;(define w (or bw (super choice-weight)))
         ;; TODO - use super choice-weight, not 5.  This is a temporary fix to move debugging along.
         (define w (or bw 5))
         (if (member hint-name (att-value 'hints current-hole))
             (* (hint-weight-multiplier hint-name)
                w)
             w))]
    [(rec hint-name)
     #'(rec #f hint-name)]))

(define-syntax (top-heavy-choice stx)
  ;; TODO - this should take into account the max depth, because higher max-depths
  ;;        now produce really wonky stuff.
  (syntax-parse stx
    [(_ base-weight)
     #'(max 0
            (- base-weight
               (* 2 (att-value 'ast-depth current-hole))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cm misc-constraints
    [Node #t]
    [AssignmentExpression
     (set-empty? (set-intersect '(constant no-assignment)
                                (att-value 'misc-constraints
                                           current-hole)))]
    [FunctionDefinition
     (equal? (node-type (parent-node current-hole)) 'Program)])

(cm choice-weight
    [Node 10]
    [NullStatement 1]
    [ExpressionStatement 70]
    [IfStatement (top-heavy-choice 5)]
    [IfElseStatement (top-heavy-choice 5)]
    [WhileStatement (top-heavy-choice 2)]
    [DoWhileStatement (top-heavy-choice 2)]
    [ForStatement (top-heavy-choice 7)]
    [Block (hinted-choice-weight 1 block-hint)]
    [ReturnStatement 1]
    [ValueReturnStatement 1]
    [AssignmentExpression (hinted-choice-weight assignment-hint)]
    [FunctionApplicationExpression
     (hinted-choice-weight application-hint)]
    [VariableDeclaration 20]
    [VariableReference 15]
    )


(add-prop
 cish2-rules
 wont-over-deepen
 [ExpressionStatement #t]
 [ValueReturnStatement #t]
 [AssignmentExpression #t]
 [VariableDeclaration #t]
 )

(cm respect-return-position
    [Node #t]
    )
(cm respect-return-position
    [Statement (not (att-value 'in-return-position? current-hole))]
    [IfElseStatement #t]
    [Block #t]
    [ReturnStatement #t]
    [ValueReturnStatement #t]
    )

(cm features-enabled
    [Node (let ((disabled (xsmith-option 'features-disabled)))
            (not (ormap (λ (f) (dict-ref disabled f #f))
                        (send this features))))])
(cm features
    [NullStatement '(null)]
    [IfStatement '(if-statement)]
    [LoopStatement '(loop)]
    [IfExpression '(if-expression)]
    )

(add-choice-rule
 cish2-rules
 constrain-type
 [Node (λ () #t)]
 [AssignmentExpression
  (λ ()
    (let ([ref-choices-filtered (hash-ref ref-choices-filtered-hash this #f)])
      (if ref-choices-filtered
          ref-choices-filtered
          (let ()
            (define visibles (att-value 'visible-bindings current-hole))
            (define legal-refs
              (filter (λ (b) (not (member (binding-name b)
                                          (att-value 'illegal-variable-names
                                                     current-hole))))
                      visibles))
            (define type-needed (att-value 'type-context current-hole))
            (define not-functions (filter (λ (b) (not (function-type?
                                                       (dict-ref (binding-bound b)
                                                                 'type))))
                                          legal-refs))
            (define legal-with-type
              (if type-needed
                  (filter (λ (b) (type-satisfies? (dict-ref (binding-bound b) 'type)
                                                  type-needed))
                          not-functions)
                  not-functions))
            (hash-set! ref-choices-filtered-hash this legal-with-type)
            (and (not (null? legal-with-type)) legal-with-type)))))]
 [VariableReference
  (λ ()
    (let ([ref-choices-filtered (hash-ref ref-choices-filtered-hash this #f)])
      (if ref-choices-filtered
          ref-choices-filtered
          (let ()
            (define visibles (att-value 'visible-bindings current-hole))
            (define legal-refs
              (filter (λ (b) (not (member (binding-name b)
                                          (att-value 'illegal-variable-names
                                                     current-hole))))
                      visibles))
            (define type-needed (att-value 'type-context current-hole))
            (define legal-with-type
              (if type-needed
                  (filter (λ (b) (type-satisfies? (dict-ref (binding-bound b) 'type)
                                                  type-needed))
                          legal-refs)
                  (filter (λ (b) (not (function-type?
                                       (dict-ref (binding-bound b) 'type))))
                          legal-refs)))
            (hash-set! ref-choices-filtered-hash this legal-with-type)
            (and (not (null? legal-with-type)) legal-with-type)))))]
 [FunctionApplicationExpression
  (λ ()
    (let ([ref-choices-filtered (hash-ref ref-choices-filtered-hash this #f)])
      (if ref-choices-filtered
          ref-choices-filtered
          (let ()
            (define visibles (filter (λ (b) (function-type?
                                             (dict-ref (binding-bound b) 'type)))
                                     (att-value 'visible-bindings current-hole)))
            (define legal-refs
              (filter (λ (b) (not (member (binding-name b)
                                          (att-value 'illegal-variable-names
                                                     current-hole))))
                      visibles))
            (define type-needed (att-value 'type-context current-hole))
            (define legal-with-type
              (if type-needed
                  (filter (λ (b) (type-satisfies?
                                  (car (reverse (dict-ref (binding-bound b) 'type)))
                                  type-needed))
                          legal-refs)
                  legal-refs))
            (define final-choices (filter (λ (b) (not (equal? "main" (binding-name b))))
                                          legal-with-type))
            (hash-set! ref-choices-filtered-hash this final-choices)
            (and (not (null? final-choices)) final-choices)))))]
 )



(define-syntax (define-basic-literal-choice stx)
  (syntax-parse stx
    ;; btype is the basic type that it satisfies
    ;; generator-e is the expression to generate a value
    ;; if-zero-generator-e is used if nonzero is required and the first generator gives 0
    [(_ nodename btype feature generator-e if-zero-generator-e)
     #'(begin
         (add-prop cish2-rules fresh
                   [nodename (let* ([t (att-value 'type-context current-hole)]
                                    [v1 generator-e]
                                    [constraints (if (basic-type? t)
                                                     (basic-type-constraints t)
                                                     '())]
                                    [v (if (and (member 'nonzero constraints)
                                                (equal? 0 v1))
                                           if-zero-generator-e
                                           v1)])
                               (hash 'val v))])
         (cm features [nodename '(feature)])
         (cm choice-weight [nodename 3])
         (cm constrain-type
             [nodename (let ([t (att-value 'type-context current-hole)])
                         ;; This isn't necessarily nonzero, but it will be if needed.
                         (and (type-satisfies? btype t) this))])
         )]))

(define-basic-literal-choice LiteralInt nonzero-int-type int
  (* (random 100)
     (if (equal? 0 (random 1))
         1
         -1))
  (+ 1 (random 10)))
(define-basic-literal-choice LiteralFloat nonzero-float-type float
  (* (random) (random 10))
  (+ .1 (random)))



(define-syntax (define-binary-op-choice stx)
  (syntax-parse stx
    [(_ nodename ;; Name of grammar node, also generates choice name
        feature
        input-typelist ;; types the operator accepts
        output-type ;; type the operator returns, or #f if it returns its input type
        bool-like ;; #t if the operator returns something bool-y, otherwise #f
        )
     #'(begin
         (cm features [nodename '(feature)])
         ;; TODO - this broke at some point when I switched to using all the macros.
         ;;        Commenting it out is quicker than fixing it.
         #;(cm choice-weight
               [nodename (if (and bool-like (member bool-hint
                                                    (att-value 'hints current-hole)))
                             (* (hint-weight-multiplier bool-hint)
                                (super choice-weight))
                             (super choice-weight))])
         (cm constrain-type
             [nodename (let ([t (att-value 'type-context current-hole)])
                         (cond [(if output-type
                                    (type-satisfies? output-type t)
                                    (ormap (λ (avail-type)
                                             (type-satisfies? avail-type t))
                                           input-typelist))
                                #t]
                               [else #f]))])
         )]))

(define-binary-op-choice AdditionExpression addition
  (list int-type float-type) #f #f)
(define-binary-op-choice MultiplicationExpression multiplication
  (list int-type float-type) #f #f)
(define-binary-op-choice SubtractionExpression subtraction
  (list int-type float-type) #f #f)
(define-binary-op-choice DivisionExpression division
  (list int-type float-type) #f #f)

(define-binary-op-choice ModulusExpression modulus
  (list int-type) #f #f)

(define-binary-op-choice EqualityExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice LessThanExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice GreaterThanExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice LessOrEqualExpression comparisons
  (list int-type float-type) int-type #t)
(define-binary-op-choice GreaterOrEqualExpression comparisons
  (list int-type float-type) int-type #t)

