#lang scribble/manual

@(require
"util.rkt"
racket/runtime-path
racket/file
)

@(define-runtime-path minimal-example-path "minimal-example.rkt")
@(define-runtime-path minimal-example-with-variables-path
  "minimal-example-with-variables.rkt")


@title{Xsmith Guide}

Xsmith is a library for creating fuzzers.

It also comes bundled with some program generators created with the library.  If you just want to run them, see @secref["running-fuzzers"].

Xsmith includes a DSL which defines a function which generates random ASTs for the language.
The Xsmith DSL is used to specify a language's grammar, typing rules, and other information which guides generation choices.
Xsmith also includes utilities for creating a command-line interface for generating a single program or starting a web server that generates one program per request.

Xsmith uses @(racr), an attribute grammar library, in its implementation, and some knowledge of @(racr) is necessary when using Xsmith.

To create a fuzzer with Xsmith, users create a specification by combining @italic{spec components}, defined with @racket[define-spec-component].
Each spec component can have portions of grammar as well as @italic{properties} added to them (using @racket[add-to-grammar] and @racket[add-prop]).
The grammar and properties are used to generate a @(racr) grammar, attributes for the grammar, and @italic{choice objects}, which guide AST generation.

Program generation starts by generating an AST hole for a given grammar production.
Generation continues by filling holes with concrete AST nodes (which may introduce new holes as child nodes).
Each time a hole is to be filled, the grammar specification is used to determine potential replacements.
For example, in a grammar with addition and subtraction expressions, an expression hole may be replaced by an addition or subtraction node.
A choice object is created for each legal replacement.
Choice objects have methods (choice-rules) which aid in choosing a concrete replacement.
Some of these methods act as predicates to filter out choices that are not legal in a particular context, such as choices that introduce more holes when the maximum tree depth has been reached.
The @racket[choice-weight] property defines a method which determines the relative probability of each choice being chosen.
The @racket[fresh] property defines a method which determines how the choice is instantiated as a @(racr) node.
Additional methods may be defined as helpers.
Choice objects have access to the @racket[current-hole], so they may query @(racr) attributes in method bodies.
Choice object classes follow the same hierarchy as the grammar, so method inheritance for choice objects is similar to attribute inheritance for @(racr) nodes.

@(racr) attributes and choice object methods may be added directly with @racket[add-att-rule] and @racket[add-choice-rule], respectively, but many are defined indirectly by various Xsmith properties.
Properties allow users to specify various attributes and choice rules in a more declarative fashion.


@section[#:tag "racr"]{RACR overview}

RACR is a library for Reference Attribute Grammars.  Xsmith's DSL defines a RACR grammar specification as well as various attributes.  The attributes are queried to determine how to generate the AST.

RACR caches the results of attribute queries and keeps track of the nodes accessed for any attribute.  When nodes used in an attribute computation are changed, future queries to that attribute are re-computed.

Users can specify new RACR attributes for Xsmith generators, but they should use @racket[add-att-rule] or @racket[add-prop] from Xsmith rather than using RACR functions directly.  In expressions evaluated in the context of RACR attributes (att-rules) or choice rules, RACR attributes may be queried.

The main RACR APIs of interest are:

Functions for querying the AST:

@itemlist[
@item{att-value}
@item{ast-child}
@item{ast-children}
@item{ast-parent}
]

Xsmith provides a function which generates a complete AST, but users can also perform AST rewrites after initial program generation.  Relevant RACR functions for performing AST rewrites include:

@itemlist[
@item{perform-rewrites}
]


Full RACR documentation is @hyperlink["https://github.com/christoff-buerger/racr/blob/master/racr/documentation/contents.md"]{here}.


@section{Holes and Choice Objects}
Hole nodes are @(racr) AST nodes.
For every node type in the grammar, a hole node is created as a subclass of it, inheriting all of its @(racr) attributes.
A hole can be recognized by the @racket['xsmith_is-hole?] attribute.

Consider the following (partial) grammar:
@racketblock[
(add-to-grammar
 my-spec-component
 [Expression #f ()]
 [LiteralInt Expression (v = (random 1000))]
 [AdditionExpression Expression ([left : Expression] [right : Expression])])
]

When a fresh AdditionExpression is created, it will include two Expression hole nodes.
When the generator gets to those holes, a choice object is created for each subclass of Expression (including Expression itself unless it is disabled with the @racket[may-be-generated] property).
The choice objects have types corresponding to LiteralInt and AdditionExpression, and therefore may have different implementations for various choice methods.
The choice objects all have access to the Expression hole (through @racket[current-hole]), but while choice objects have access to their specialized choice method implementations, the hole is of type Expression, and so all @(racr) attributes (att-rules) that may be queried are specialized only as far as Expression, not to LiteralInt or AdditionExpression.

Note that hole node types are created for every type in the grammar (including LiteralInt and AdditionExpression), but more specialized holes are only used if the grammar specifies that a node's child must be specifically that kind of expression, or if a custom @racket[fresh] implementation uses @racket[make-hole] with the specific kind of expression.


@section{Scope Graphs}
Xsmith uses the language-independent resolution algorithm of scope graphs.

The theory of scope graphs is described in the paper “A Theory of Name Resolution with Extended Coverage and Proofs”, (available at @url{https://researchr.org/publication/NeronTVW15}).


@section{Minimal Example}

@(verbatim (file->string minimal-example-path))

@section{Minimal Example With Variables}

@(verbatim (file->string minimal-example-with-variables-path))