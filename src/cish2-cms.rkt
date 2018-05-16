#lang racket/base

(require
 "grammar-macros.rkt"
 "cish2-utils.rkt"

 racr
 racr/testing ;; racr/testing is needed for print-ast
 racket/random
 racket/string
 racket/dict
 racket/set
 racket/match
 racket/math
 racket/class
 (prefix-in rt: rosette)
 (except-in racket/list empty)
 "random.rkt"
 "choice.rkt"
 "scope-graph.rkt"
 "xsmith-options.rkt"
 "xsmith-version.rkt"
 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(declare-spec cish2)

(define-syntax-parser cm
  [(_ method [node-name lambda-body] ...+)
   #'(add-cm cish2 method [node-name (λ () lambda-body)] ...)])

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

(cm misc-constraints [Node this])
(cm choice-weight [Node 10])
(cm features-enabled
    [Node (let ((disabled (xsmith-option 'features-disabled)))
            (if (ormap (λ (f) (dict-ref disabled f #f))
                       (send this features))
                #f
                this))])
(cm wont-over-deepen
    [Node (if (<= (att-value 'ast-depth current-hole) (xsmith-option 'max-depth))
              this
              #f)])
(cm constrain-type [Node this])
(cm top-level-declaration-at-top-level [Node this])
(cm respect-return-position [Node this])

(cm respect-return-position
    [Statement
     (and (not (att-value 'in-return-position? current-hole))
          this)])
(cm fresh [NullStatement (fresh-node 'NullStatement)])
(cm features [NullStatement '(null)])
(cm wont-over-deepen [NullStatement this])
(cm choice-weight [NullStatement 1])
(cm choice-weight [ExpressionStatement 70])
(cm fresh [ExpressionStatement (fresh-node 'ExpressionStatement
                                           (fresh-node 'ExpressionHole))])
(cm wont-over-deepen [ExpressionStatement this])
(cm choice-weight [IfStatement (top-heavy-choice 5)])
(cm features [IfStatement '(if-statement)])
(cm fresh [IfStatement (fresh-node 'IfStatement
                                   (fresh-node 'ExpressionHole)
                                   (fresh-node 'StatementHole))])

(cm choice-weight [IfElseStatement (top-heavy-choice 5)])
(cm fresh [IfElseStatement (fresh-node 'IfElseStatement
                                       (fresh-node 'ExpressionHole)
                                       (fresh-node 'BlockHole
                                                   (create-ast-list '())
                                                   (create-ast-list '()))
                                       (fresh-node 'BlockHole
                                                   (create-ast-list '())
                                                   (create-ast-list '())))])
(cm respect-return-position [IfElseStatement this])
(cm features [LoopStatement '(loop)])
(cm choice-weight [WhileStatement (top-heavy-choice 2)])
(cm choice-weight [DoWhileStatement (top-heavy-choice 2)])
(cm choice-weight [ForStatement (top-heavy-choice 7)])
(cm fresh [WhileStatement (fresh-node 'WhileStatement
                                      (fresh-node 'ExpressionHole)
                                      (fresh-node 'StatementHole))])
(cm fresh [DoWhileStatement (fresh-node 'DoWhileStatement
                                        (fresh-node 'ExpressionHole)
                                        (fresh-node 'StatementHole))])
(cm fresh [ForStatement (fresh-node 'ForStatement
                                    (fresh-node 'ExpressionHole)
                                    (fresh-node 'StatementHole)
                                    ;; init, update
                                    (fresh-node 'DeclarationHole "standin-name")
                                    (fresh-node 'ExpressionHole))])

(cm choice-weight [Block (hinted-choice-weight 1 block-hint)])

(cm fresh
    [Block (fresh-node 'Block
                       ;; declarations
                       (create-ast-list (map (λ (x) (fresh-node 'DeclarationHole
                                                                "standin-name"))
                                             (make-list (random 2) #f)))
                       ;; statements
                       (create-ast-list
                        (map (λ (x) (fresh-node 'StatementHole))
                             (let ([l (make-list (random 5) #f)])
                               (if (att-value 'in-return-position? current-hole)
                                   ;; don't allow an empty block in return position
                                   (cons #f l)
                                   l)))))])
(cm respect-return-position [Block this])
(cm choice-weight [ReturnStatement 1])
(cm wont-over-deepen [ReturnStatement this])
(cm respect-return-position [ReturnStatement this])
(cm choice-weight [ValueReturnStatement 1])
(cm fresh [ValueReturnStatement (fresh-node 'ValueReturnStatement
                                            (fresh-node 'ExpressionHole))])
(cm respect-return-position [ValueReturnStatement this])


(define-syntax (define-basic-literal-choice stx)
  (syntax-parse stx
    ;; btype is the basic type that it satisfies
    ;; generator-e is the expression to generate a value
    ;; if-zero-generator-e is used if nonzero is required and the first generator gives 0
    [(_ nodename btype feature generator-e if-zero-generator-e)
     #'(begin
         (cm fresh [nodename (let* ([t (att-value 'type-context current-hole)]
                                    [v1 generator-e]
                                    [constraints (if (basic-type? t)
                                                     (basic-type-constraints t)
                                                     '())]
                                    [v (if (and (member 'nonzero constraints)
                                                (equal? 0 v1))
                                           if-zero-generator-e
                                           v1)])
                               (fresh-node 'nodename v))])
         (cm features [nodename '(feature)])
         (cm wont-over-deepen [nodename this])
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

(cm choice-weight [AssignmentExpression (hinted-choice-weight assignment-hint)])
(cm wont-over-deepen [AssignmentExpression this])
(cm misc-constraints
    [AssignmentExpression (and (set-empty? (set-intersect '(constant no-assignment)
                                                          (att-value 'misc-constraints
                                                                     current-hole)))
                               this)])

#|
Apparently class definitions don't let public methods be defined with
let-over-lambda (maybe the class macro rewrites the lambdas...).
So let's have a weak hash table store the mutable state we need in a
few of these methods.
|#

(define ref-choices-filtered-hash (make-weak-hasheq))

(add-cm
 cish2 constrain-type
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
            (and (not (null? legal-with-type)) this)))))])
(cm fresh
    [AssignmentExpression
     (fresh-node 'AssignmentExpression
                 (binding-name (random-ref (send this constrain-type)))
                 (fresh-node 'ExpressionHole))])

(add-cm
 cish2 constrain-type
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
            (and (not (null? legal-with-type)) this)))))])
(cm fresh
    [VariableReference (fresh-node 'VariableReference
                                   (binding-name (random-ref
                                                  (send this constrain-type))))])
(cm choice-weght [VariableReference 15])
(cm wont-over-deepen [VariableReference this])

(add-cm
 cish2
 constrain-type
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
            (and (not (null? final-choices)) this)))))])
(cm fresh [FunctionApplicationExpression
           (let ([chosen-func (random-ref (send this constrain-type))])
             (fresh-node 'FunctionApplicationExpression
                         (binding-name chosen-func)
                         (create-ast-list
                          (map (λ (x) (fresh-node 'ExpressionHole))
                               (make-list (- (length (dict-ref (binding-bound
                                                                chosen-func)
                                                               'type))
                                             2)
                                          #f)))))])
(cm choice-weight [FunctionApplicationExpression
                   (hinted-choice-weight application-hint)])

(cm fresh [IfExpression (fresh-node 'IfExpression
                                    (fresh-node 'ExpressionHole)
                                    (fresh-node 'ExpressionHole)
                                    (fresh-node 'ExpressionHole))])
(cm features [IfExpression '(if-expression)])


(define-syntax (define-binary-op-choice stx)
  (syntax-parse stx
    [(_ nodename ;; Name of grammar node, also generates choice name
        feature
        input-typelist ;; types the operator accepts
        output-type ;; type the operator returns, or #f if it returns its input type
        bool-like ;; #t if the operator returns something bool-y, otherwise #f
        )
     #'(begin
         (cm fresh [nodename (fresh-node 'nodename
                                         (fresh-node 'ExpressionHole)
                                         (fresh-node 'ExpressionHole))])
         (cm features [nodename '(feature)])
         (cm choice-weight
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
                                this]
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

(cm wont-over-deepen [VariableDeclaration this])
(cm choice-weight [VariableDeclaration 20])
(cm fresh [VariableDeclaration
           (let ([name (if (equal? (att-value 'top-level-node current-hole)
                                   (parent-node current-hole))
                           (fresh-var-name "global_")
                           (fresh-var-name "local_"))])
             (fresh-node 'VariableDeclaration
                         name
                         (fresh-var-type)
                         (fresh-node 'ExpressionHole)))])
(cm top-level-declaration-at-top-level
    [FunctionDefinition (if (att-value 'at-top-level? current-hole)
                            this
                            #f)])
(cm fresh
    [FunctionDefinition
     (let* ([p (parent-node current-hole)]
            [main? (and (eq? (node-type p) 'Program)
                        (eq? (ast-child 'main p) current-hole))])
       (fresh-node 'FunctionDefinition
                   (if main? "main" (fresh-var-name "func_"))
                   (if main? int-type (fresh-var-type))
                   ;; parameters
                   (if main?
                       (create-ast-list '())
                       (create-ast-list (map (λ (x) (fresh-node 'FormalParam
                                                                (fresh-var-type)
                                                                (fresh-var-name "arg_")))
                                             (make-list (random 5) #f))))
                   (fresh-node 'BlockHole
                               (create-ast-list '())
                               (create-ast-list '()))))])




