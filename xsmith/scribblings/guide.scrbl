#lang scribble/manual
@; -*- mode: Scribble -*-
@;
@; Copyright (c) 2019 The University of Utah
@; All rights reserved.
@;
@; This file is part of Xsmith, a generator of highly effective fuzz testers.
@;
@; Redistribution and use in source and binary forms, with or without
@; modification, are permitted provided that the following conditions are met:
@;
@;   * Redistributions of source code must retain the above copyright notice,
@;     this list of conditions and the following disclaimer.
@;
@;   * Redistributions in binary form must reproduce the above copyright
@;     notice, this list of conditions and the following disclaimer in the
@;     documentation and/or other materials provided with the distribution.
@;
@; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
@; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
@; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
@; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
@; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
@; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
@; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
@; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
@; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
@; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
@; POSSIBILITY OF SUCH DAMAGE.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@(require
  "util.rkt"
  racket/runtime-path
  racket/file
  scribble/core
  (for-label
   xsmith
   (except-in clotho module)))

@(define-runtime-path minimal-example-path "minimal-example.rkt")
@(define-runtime-path minimal-example-with-variables-path
  "minimal-example-with-variables.rkt")
@(define-runtime-path minimal-example-with-canned-components-path
  "minimal-example-with-canned-components.rkt")


@title{Xsmith Guide}

Xsmith uses @(racr), an attribute grammar library, in its implementation, and some knowledge of @(racr) is necessary when using Xsmith.

To create a fuzzer with Xsmith, users create a specification by combining @italic{specification components} (more commonly referred to as @italic{spec components}), defined with @racket[define-spec-component].
A spec component can define productions of a grammar with @racket[add-to-grammar], or it can provide @italic{properties} of grammar productions with @racket[add-prop].
The grammar and properties are used to generate a @(racr) grammar, @italic{attributes} for the grammar, and @italic{choice objects}, which guide AST generation.

Program generation starts by generating an AST hole for a given grammar production.
Generation continues by filling holes with concrete AST nodes, which may introduce new holes as child nodes.
The grammar specification is used to determine how to fill holes in the AST.
For example, in a grammar with addition and subtraction expressions, a generic @verbatim|{Expression}| hole may be replaced by an @verbatim|{Addition}| or @verbatim|{Subtraction}| node.
A choice object is created for each valid possible replacement.
Choice objects have methods (called @italic{choice-rules}) which aid in choosing a concrete replacement.
Some of these methods act as predicates to filter out choices that are not legal in a particular context, such as choices that introduce more holes when the maximum tree depth has been reached.
The @racket[choice-weight] property defines a method which determines the relative probability of each choice being chosen.
The @racket[fresh] property defines a method which determines how the choice is instantiated as a @(racr) node.
Additional methods may be defined as helpers.
Choice objects have access to the @racket[current-hole], so they may query @(racr) attributes in method bodies.
Choice object classes follow the same hierarchy as the grammar, so method inheritance for choice objects is similar to attribute inheritance for @(racr) nodes.

@(racr) attributes and choice object methods may be added directly with @racket[add-att-rule] and @racket[add-choice-rule], respectively, but many are defined indirectly by various Xsmith properties.
Properties allow users to specify various attributes and choice rules in a more declarative fashion.

Xsmith was primarily designed for the implementation of language specifications for differential compiler/interpreter testing.
Therefore Xsmith takes pains to avoid producing programs that rely on commonly unspecified behaviors, such as the order of evaluation of function or operator arguments.
Because the language-agnostic analysis within Xsmith is quite conservative, there are many valid programs that will not be generated.
However, Xsmith makes it easy to create a fuzzer that obeys such rules without much language-specific work.

@section[#:tag "racr"]{RACR Overview}

RACR is a library for Reference Attribute Grammars.
Xsmith's DSL defines a RACR grammar specification as well as various attributes.
The attributes are queried to determine how to generate the AST.

RACR caches the results of attribute queries and keeps track of the nodes accessed for any attribute.
When nodes used in an attribute computation are changed, future queries to that attribute are re-computed.

Users can specify new RACR attributes for Xsmith generators, but they should use @racket[add-att-rule] or @racket[add-prop] from Xsmith rather than using RACR functions directly.
In expressions evaluated in the context of RACR attributes (att-rules) or choice rules, RACR attributes may be queried.

The main RACR APIs of interest are:

Functions for querying the AST:

@itemlist[
@item{att-value}
@item{ast-child}
@item{ast-children}
@item{ast-parent}
]

Xsmith provides a function which generates a complete AST, but users can also perform AST rewrites after initial program generation.
Relevant RACR functions for performing AST rewrites include:

@itemlist[
@item{perform-rewrites}
]


Full RACR documentation is @hyperlink["https://github.com/christoff-buerger/racr/blob/master/racr/documentation/contents.md"]{here}.


@section{Holes and Choice Objects}
Hole nodes are @(racr) AST nodes.
For every node type in the grammar, a hole node is created as a subclass of that node, inheriting all of its @(racr) attributes.
A hole can be recognized by the @rule[xsmith_is-hole?] attribute.

Consider the following (partial) grammar defined in the @verbatim|{my-spec-component}| grammar specification:

@racketblock[
(add-to-grammar
 my-spec-component
 [Expression #f ()]
 [LiteralInt Expression (v = (random 1000))]
 [AdditionExpression Expression
                     ([left : Expression]
                      [right : Expression])])
]

When a fresh AdditionExpression is created, it will include two Expression hole nodes.
When the generator gets to those holes, a choice object is created for each subclass of Expression (including Expression itself unless it is disabled with the @racket[may-be-generated] property).
The choice objects have types corresponding to LiteralInt and AdditionExpression, and therefore may have different implementations for various choice methods.
The choice objects all have access to the Expression hole (through @racket[current-hole]), but while choice objects have access to their specialized choice method implementations, the hole is of type Expression, and so all @(racr) attributes (att-rules) that may be queried are specialized only as far as Expression, not to LiteralInt or AdditionExpression.

Although Xsmith @italic{can} create holes for any type of production defined in the grammar, by default it will only generate more general holes.
In the case of this example, the default behavior would be for Xsmith to generate Expression holes, but not LiteralInt or AdditionExpression holes.
More specific holes are used either when a grammar production specifies that a child must be of a specific kind, or when a custom @racket[fresh] implementation uses @racket[make-hole] with a specific kind of production.
For example, we could extend the above example:

@racketblock[
(define-spec-component my-spec-component)

(add-to-grammar
 my-spec-component
 [Expression #f ()]
 [LiteralInt Expression (v = (random 1000))]
 [AdditionExpression Expression
                     ([left : Expression]
                      [right : Expression])]
 [LiteralSubtractionExpression Expression
                               ([left : LiteralInt]
                                [right : LiteralInt])])
]

Now, an Expression hole could be filled with LiteralInt, AdditionExpression, or LiteralSubtractionExpression.
However, where the AdditionExpression's two Expression child holes could be filled with any kind of Expression, the LiteralSubtractionExpression's children will only be LiteralInts.



@section[#:tag "scope-graph"]{Scope Graphs}
Xsmith uses the language-independent resolution algorithm of scope graphs.

The theory of scope graphs is described in the paper “A Theory of Name Resolution with Extended Coverage and Proofs”, (available at @url{https://researchr.org/publication/NeronTVW15}).


@section{Attributes, Choices, and Properties, Oh My!}

When creating fuzzers with Xsmith, you will deal with attributes (eg. via @racket[add-att-rule]), choice rules (via @racket[add-choice-rule]), and properties (via @racket[add-prop]).
Exactly what these are and how they relate can be confusing.

@itemlist[
@item{
@bold{Attributes}, written with @racket[add-att-rule], are RACR attributes.
They are evaluated dynamically by traversing the generated tree.
RACR caches the results of attribute evaluation, flushing the cache when there are changes to parts of the tree that were previously used in computing an attribute.
For a full understanding of attributes, you must also read @hyperlink["https://github.com/christoff-buerger/racr/blob/master/racr/documentation/contents.md"]{the RACR documentation.}

You can get the values of attributes with code such as: @racket[(att-value 'xsmith_type node-in-question)].
Some attributes are defined automatically by properties.
You may or may not need to write custom attributes while creating a fuzzer.
Attributes generally can not directly access choice rules or properties.
}
@item{
@bold{Choice Rules}, written with @racket[add-choice-rule], are methods on choice objects.
When Xsmith fills in a hole node, it creates a choice object for each grammar production that could fit in the given hole.
Choice rules are used to filter and choose which of those productions to generate and guide generation.

You can call choice rules with code such as: @racket[(send xsmith_choice-weight some-choice-object)].
Choice rules are just Racket class methods.
You may or may not need to write custom choice rules while creating a fuzzer, but custom choice rules are probably less likely to be needed than custom attributes.
During evaluation of a choice rule, the hole in question is available as @racket[current-hole], so attributes may still be queried in the context of choice rules, but choice rules can not directly access properties.
}
@item{
@bold{Properties} are basically macros that generate attributes and choice rules.
The values of properties are written with @racket[add-prop].
Properties have associated transformers that desugar the values they get from @racket[add-prop], essentially turning them into @racket[add-att-rule] and @racket[add-choice-rule] invocations, but transforming their bodies on the way.
Properties are evaluated statically, but define attributes and choice rules that are evaluated dynamically.
The values that you write as the right-hand-side of an @racket[add-prop] form often include code that will be included verbatim inside one or more attributes or choice rules, but they are also often values read statically to generate completely different code.
Each property can behave differently, so you must read the documentation for each property to know what kind of right-hand-side to write, whether it can access a node to evaluate attributes on, or whether it can access a @racket[current-hole]!

Generally, core attributes and choice rules used in Xsmith are defined by properties, such as the @racket[fresh] property and the @racket[type-info] property.
While you certainly need to write property values with @racket[add-prop] for various built-in properties to create an Xsmith fuzzer, you probably don't need to define a custom property with @racket[define-property] unless you want to use it in multiple fuzzers, since writing the attributes and choice rules directly is simpler than defining a new property with an associated transformer.
However, if you are writing multiple fuzzers, properties may help you create useful abstractions for use in multiple fuzzers.
Property transformers can read the static values of other properties, and generate any number of attributes and choice rules.
Property transformers can not statically access attribute or choice rule values, but they may generate code that uses them dynamically.
}
]

Remember: Attributes and choice rules are functions used (and usable) within specific contexts within Xsmith fuzzers.
Properties are compile-time macros for generating attributes and choice rules.

@section{Lifting}

The term “lift” is used throughout this document.
In the context of Xsmith, lifting refers to creating a definition after it's needed by a reference.
In other words, when a reference node is created and there is not an available definition that satisfies the type system and effect system, a new definition is “lifted” to a binding form that is visible from that reference location.
Additionally, with some probability, new definitions may be lifted even when there is a suitable definition available.
This probability can be controlled with the @racket[reference-choice-info] property.


@section[#:tag"getting-started"]{Getting Started}

This gives a small walkthrough of creating a simple fuzzer @italic{without} the canned components library.
Then we will build a simple fuzzer @italic{with} the canned components library.
Using the canned components is preferred.


First, we must require some Xsmith and related modules.
In particular, we need @tt{xsmith} and @tt{racr}.
Other modules may also be convenient.
@racketblock[
(require xsmith racr racket/string)
]

To define a fuzzer, first define a @italic{spec component} with @racket[define-spec-component].
@racketblock[
(define-spec-component arith)
]

The spec component is essentially a place we can store definitions of the grammar and related info.
Let's add to the grammar defined by this spec component.
We need a main node where generation must start, we'll call it @tt{Program}, and it will have a single @tt{Expression}.
Note that the names of node types should be capitalized camel case (punctuation characters like @tt{-} and @tt{_} are disallowed).
When adding nodes, we list the node name, its supertype, and a list of children.

@racketblock[
(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()]
 )
]

Program and Expression are not subtypes of some other node type, so their parent node type is the top node type, @racket[#f].
Since the Program node only has one child of Expression type, we may write just @tt{Expression} for both the name and type of the node.

We want the @tt{Expression} node to be abstract and not generated itself.
@racketblock[
(add-prop arith
          may-be-generated
          [Expression #f])
]

We can put properties inline with the grammar definition when we want, replacing the above definition:

@racketblock[
(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()
             #:prop may-be-generated #f]
 )
]

If we add node types that are subtypes of Expression, they can be generated in Expression holes.
Let's add a node for literal integers.

@racketblock[
(add-to-grammar
 arith
 [LiteralInt Expression ([v = (random 100)])])
]

Note that the literal node contains a child @tt{v} that is a normal Racket value, not a grammar node type.
It is initialized with the expression on the right-hand side of the @tt{=} sign.
Well, our literal integers will only be values from 0 to 99.
Note that we can add the initialization expression inline as above or with the @racket[fresh] property.
If we don't add an initialization expression, then non-node fields will be initialized with @racket[#f], while node fields will be initialized with hole nodes of the appropriate type..

Let's add addition.
Because we have multiple Expressions, we need to give them names.
Note that we aren't supplying initialization code.
Because they are grammar node typed, they will be initialized with Expression Hole nodes (same with the Program node's Expression child).

@racketblock[
(add-to-grammar
 arith
 [Addition Expression ([l : Expression] [r : Expression])])
]

Our language only has one type.
This means we don't necessarily have to add type rules.
However, let's add rules anyway since any real language will have multiple types.

@racketblock[
(define int (base-type 'int))
(add-prop arith type-info
          [Program [int (λ (n t) (hash 'Expression int))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [Addition [int (λ (n t) (hash 'l int 'r int))]])
]

The left hand side of each rule is an expression that returns the type the node can inhabit.
If a node can inhabit multiple types, @racket[fresh-type-variable] can be used to specify an unconstrained or partially constrained type.

The right hand side is a function that returns a dictionary mapping children (by name or by node object) to a type.
Note that type variables are unified during the type analysis, so you should take care with object/pointer equality (IE @racket[eq?]) of type variables.


Now we need to specify how to print our programs, or “render” them.

@racketblock[
(add-prop arith render-node-info
          [Program (λ (n) (att-value 'xsmith_render-node (ast-child 'Expression n)))]
          [LiteralInt (λ (n) (number->string (ast-child 'v n)))]
          [Addition (λ (n) (format "(~a + ~a)"
                                   (att-value 'xsmith_render-node (ast-child 'l n))
                                   (att-value 'xsmith_render-node (ast-child 'r n))))])
]

In this case are rendering directly to a string, but that's not usually the best approach.
Rather, programs may be rendered to any intermediate data structure, such as s-expressions (convenient for lisp generators) or nodes from the @tt{pprint} library.

We put everything together with the @racket[assemble-spec-components] macro.
@racketblock[
(assemble-spec-components arithmetic arith)
]
Note that in our case we only have one component, but in principle we could define multiple, perhaps in different files, and combine them.
The @racket[assemble-spec-components] macro defines the @tt{arithmetic-generate-ast} function (named based on the name given as the first argument).

To turn it into a complete program we can run, we hook it up to the command-line machinery.
OK, honestly, this following part is not a great design.
Just cargo cult it and live with it for now.
The @racket[xsmith-command-line] takes a thunk to generate the program, which we create by simply wrapping the @tt{arithmetic-generate-ast} function and giving it the name of the node to generate.
The @tt{arithmetic-generate-ast} may be called with the name of a node, in our case we want to generate Program nodes.
We also give it (optionally, but recommended) a function that takes a list of strings and formats them as comments for our language.

@racketblock[
(xsmith-command-line
 (λ () (arithmetic-generate-ast 'Program))
 #:comment-wrap (λ (lines)
                  (string-join
                   (map (λ (x) (format "// ~a" x)) lines)
                   "\n")))
]


@section{Minimal Example}

Here follows a generator more-or-less the same as what we defined in @secref{getting-started}.
Note that we use @tt{#lang clotho} instead of @tt{#lang racket}, which allows us to capture, replay, and modify random choices during generation.

@(nested-flow
(style 'code-inset '())
(list
 (filebox
  "minimal-example.rkt"
  (typeset-code
   #:keep-lang-line? #t
   #:indent 0
   #:context #'here
   (file->string minimal-example-path)))))



@section{Another Small Example With Variables}

Here is a bigger example that contains variables.

Note that instead of a @tt{Program} node, we use the @tt{LetStar} node as the node to generate.
The real reason to have a specific @tt{Program} node is to be sure that the top-level node can have definitions lifted to it.
The @tt{LetStar} node in the following example satisfies that.

This example also renders to s-expressions rather than directly to strings.
We give @racket[xsmith-command-line] another optional argument specifying how we convert our rendered format into strings.

@(nested-flow
(style 'code-inset '())
(list
 (filebox
  "minimal-example-with-variables.rkt"
  (typeset-code
   #:keep-lang-line? #t
   #:indent 0
   #:context #'here
   (file->string minimal-example-with-variables-path)))))



@section{Upgrading to use Canned Components}

The @tt{xsmith/canned-components} library provides a lot of boilerplate stuff for us, and has a bunch of type rules and such already written correctly.
Let's just use it.

@(nested-flow
(style 'code-inset '())
(list
 (filebox
  "minimal-example-with-canned-components.rkt"
  (typeset-code
   #:keep-lang-line? #t
   #:indent 0
   #:context #'here
   (file->string minimal-example-with-canned-components-path)))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
