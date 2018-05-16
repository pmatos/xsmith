#lang racket

(require
 "grammar-macros.rkt"

 racr
 racr/testing ;; racr/testing is needed for print-ast
 pprint
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
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))


(declare-spec cish2)

(add-to-grammar
 cish2
 [Node #f (precomment postcomment serialnumber)]
 [Program Node ([Declaration *] [main : FunctionDefinition])]

 [Declaration Node (name)]
 [DeclarationHole Declaration ()]
 [VariableDeclaration Declaration (typename Expression)]
 [FunctionDefinition Declaration (typename [FormalParam *] Block)]
 [FunctionDefinitionHole FunctionDefinition ()]
 [FormalParam Node (typename name)]


 [Statement Node ()]
 [NullStatement Statement ()]
 [Block Statement ([Declaration *] [Statement *])]
 [ExpressionStatement Statement (Expression)]
 [IfStatement Statement ([test : Expression] [then : Statement])]
 [IfElseStatement IfStatement ([else : Statement])]
 [ReturnStatement Statement ()]
 [VoidReturnStatement ReturnStatement ()]
 [ValueReturnStatement ReturnStatement (Expression)]
 [StatementHole Statement ()]
 [BlockHole Block ()]

 [LoopStatement Statement ([test : Expression] [body : Statement])]
 [WhileStatement LoopStatement ()]
 [DoWhileStatement LoopStatement ()]
 [ForStatement LoopStatement ([init : Declaration] [update : Expression])]

 [Expression Node ()]
 [ExpressionHole Expression ()]

 [AssignmentExpression Expression (name Expression)]
 [FunctionApplicationExpression Expression (name [Expression *])]
 [BinaryExpression Expression ([l : Expression] [r : Expression])]
 [AdditionExpression BinaryExpression ()]
 [UnsafeAdditionExpression AdditionExpression ()]
 [SubtractionExpression BinaryExpression ()]
 [UnsafeSubtractionExpression SubtractionExpression ()]
 [MultiplicationExpression BinaryExpression ()]
 [UnsafeMultiplicationExpression MultiplicationExpression ()]
 [DivisionExpression BinaryExpression ()]
 [UnsafeDivisionExpression DivisionExpression ()]

 [IntOnlyBinaryExpression BinaryExpression ()]
 [ModulusExpression IntOnlyBinaryExpression ()]
 [UnsafeModulusExpression ModulusExpression ()]

 [ComparisonExpression BinaryExpression ()]
 [EqualityExpression ComparisonExpression ()]
 [GreaterThanExpression ComparisonExpression ()]
 [LessThanExpression ComparisonExpression ()]
 [LessOrEqualExpression ComparisonExpression ()]
 [GreaterOrEqualExpression ComparisonExpression ()]

 [IfExpression Expression ([test : Expression]
                           [then : Expression]
                           [else : Expression])]
 [LiteralInt Expression (val)]
 [LiteralFloat Expression (val)]
 [VariableReference Expression (name)]

 )
