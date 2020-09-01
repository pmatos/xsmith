#lang scribble/manual
@; -*- mode: Scribble -*-
@;
@; Copyright (c) 2019-2020 The University of Utah
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
A spec component can define productions of a grammar with @racket[add-to-grammar], or it can provide @italic{properties} of grammar productions with @racket[add-property].
The grammar and properties are used to generate a @(racr) grammar, @italic{attributes} for the grammar, and @italic{choice objects}, which guide AST generation.

Program generation starts by generating an AST hole for a given grammar production.
Generation continues by filling holes with concrete AST nodes, which may introduce new holes as child nodes.
The grammar specification is used to determine how to fill holes in the AST.
For example, in a grammar with addition and subtraction expressions, a generic Expression hole may be replaced by an Addition or Subtraction node.
A choice object is created for each valid possible replacement.
Choice objects have methods (called @italic{choice-methods}) which aid in choosing a concrete replacement.
Some of these methods act as predicates to filter out choices that are not legal in a particular context, such as choices that introduce more holes when the maximum tree depth has been reached.
The @racket[choice-weight] property defines a method which determines the relative probability of each choice being chosen.
The @racket[fresh] property defines a method which determines how the choice is instantiated as a @(racr) node.
Additional methods may be defined as helpers.
Choice objects have access to the @racket[current-hole], so they may query @(racr) attributes in method bodies.
Choice object classes follow the same hierarchy as the grammar, so method inheritance for choice objects is similar to attribute inheritance for @(racr) nodes.

@(racr) attributes and choice object methods may be added directly with @racket[add-attribute] and @racket[add-choice-method], respectively, but many are defined indirectly by various Xsmith properties.
Properties allow users to specify various attributes and choice methods in a more declarative fashion.

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

Users can specify new RACR attributes for Xsmith generators, but they should use @racket[add-attribute] or @racket[add-property] from Xsmith rather than using RACR functions directly.
In expressions evaluated in the context of RACR attributes (attributes) or choice methods, RACR attributes may be queried.

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


@section[#:tag "holes-and-choice-objects"]{Holes and Choice Objects}
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
The choice objects all have access to the Expression hole (through @racket[current-hole]), but while choice objects have access to their specialized choice method implementations, the hole is of type Expression, and so all @(racr) attributes that may be queried are specialized only as far as Expression, not to LiteralInt or AdditionExpression.

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

Aside from the grammar productions themselves, Xsmith language specifications deal with @italic{attributes} (eg. via @racket[add-attribute]), @italic{choice methods} (via @racket[add-choice-method]), and @italic{properties} (via @racket[add-property]).
The exact nature of these terms and how they relate to one another can be confusing, so let's talk about them.

@itemlist[
@item{
@bold{Attributes}, written with @racket[add-attribute], are RACR attributes.
They are evaluated dynamically by traversing the generated tree.
RACR caches the results of attribute evaluation, flushing the cache when there are changes to parts of the tree that were previously used in computing an attribute.
For a full understanding of attributes, you must also read @hyperlink["https://github.com/christoff-buerger/racr/blob/master/racr/documentation/contents.md"]{the RACR documentation.}

Attributes are evaluated using @racket[att-value].
For example, if you have a node @verbatim|{my-node}| and wish to evaluate the attribute @verbatim|{xsmith_type}| on that node, you would do: @racket[(att-value 'xsmith_type my-node)].

Some attributes are defined automatically by properties.
You may or may not need to write custom attributes while creating a fuzzer, though we have tried to provide as many useful attributes as possible to avoid this.

Attributes generally can not directly access choice methods or properties.
}

@item{
@bold{Choice Methods}, written with @racket[add-choice-method], are methods on @seclink["holes-and-choice-objects"]{choice objects}.
When Xsmith begins filling in a hole node, it creates a choice object for each grammar production that could fit in that hole.
choice methods are used to filter and choose which of those productions to generate and guide generation.

choice methods are run using the @racket[send] method-calling syntax on the choice object.
For example, if you wish to run the @verbatim|{xsmith_choice-weight}| rule on @verbatim|{some-choice-object}|, you would do: @racket[(send xsmith_choice-weight some-choice-object)].

choice methods are just Racket class methods.
You may or may not need to write custom choice methods while creating a fuzzer, but custom choice methods are less likely to be needed than custom attributes.
During evaluation of a choice method, the hole in question is available as @racket[current-hole], so attributes may still be queried in the context of choice methods, but choice methods can not directly access properties.
}

@item{
@bold{Properties}, written with @racket[add-property], are macros that automatically generate attributes and choice methods using a syntax that is often more convenient than manually implementing the attributes and choice methods separately yourself.
Properties are evaluated statically, but the attributes and choice methods they define are evaluated dynamically.
Each property may require its arguments to be given in a different way, so it is important to read the documentation for each property careful to know how to implement it correctly.

Most of the core attributes and choice methods provided by Xsmith, such as @racket[fresh] and @racket[type-info], are actually defined in terms of properties.
These properties are provided by Xsmith to make it easy to define a language specification, and many of them are required to use Xsmith successfully.
For the majority of programming languages in the mainstream, these supplied properties are sufficient for a full specification.

It is unlikely that you will need to implement custom properties.
Generally, you will want a custom property when you have some common set of attributes or choice methods that you would like to use in multiple separate fuzzers, in which case it may be worth defining a custom property for those fuzzers to share.
Such custom properties can be implemented with @racket[define-property].
A property can read the static specifications of other properties, and generate any number of attributes and choice methods, but it cannot statically access attribute or choice methods values because those are only available during AST generation.
However, a property can use the values of attributes and choice methods dynamically.
}
]

Remember: Attributes and choice methods are functions that can be used within specific contexts of Xsmith fuzzers.
On ther other hand, properties are compile-time macros for generating attributes and choice methods and so are not themselves used during generation.

@section{Lifting}

The term “lift” is used throughout this document and the Xsmith library.
In the context of Xsmith, "lifting" refers to creating a binding definition after it's needed by a reference.
In other words, when a reference node (i.e., a node which implements the @racket[reference-info] property) is created and there is not an available definition that satisfies the type system and effect system, a new definition is “lifted” to a binding form that is visible from that reference location.
Additionally, with some probability, new definitions may be lifted even when there is a suitable definition available.
This probability can be controlled with the @racket[reference-choice-info] property.


@section[#:tag"getting-started"]{Getting Started}

This section is a small walkthrough of creating a simple fuzzer using only basic Xsmith forms to show the fundamentals.
However, we recommend using @seclink["canned-components"]{Canned Components} for any serious fuzzer implementation.

To get started, we must require @tt{xsmith} and @tt{racr}.
Other modules may also be convenient, such as @tt{racket/string}.
@racketblock[
(require xsmith
         racr
         racket/string)
]

To define a fuzzer, first define a @italic{spec component} with @racket[define-spec-component].
We're going to create a simple arithmetic language, so we'll name our spec component @tt{arith} for short.
@racketblock[
(define-spec-component arith)
]

The spec component is where we store definitions of the grammar productions, properties, attributes, and choice methods.
Let's add some productions to the grammar defined by this spec component!
When adding nodes, we must always specify three components: the node's name, its supertype (also called a "parent type"), and a list of children nodes.
We'll define a top-level node that we will later use to specify where to start program generation, which we'll call @tt{Program}.
The @tt{Program} production will have a single child: an @tt{Expression}.
Therefore, we will also need to provide a base @tt{Expression} production.
Neither of these nodes has a supertype, so we will supply @racket[#f] in the supertype field after the node name.
Note that the names of node types should be capitalized camel case (punctuation characters like @tt{-} and @tt{_} are disallowed).

@racketblock[
(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()]
 )
]

We want the @tt{Expression} node to be abstract and not generated itself, so we'll use the @racket[may-be-generated] property to restrict it.

@racketblock[
(add-property
 arith
 may-be-generated
 [Expression #f])
]

We can also put properties inline with the grammar definition if we prefer.
We can replace the above definition using this style:

@racketblock[
(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()
             #:prop may-be-generated #f]
 )
]

(Note that we cannot use both forms, as this will produce an error due to conflicting definitions of a property value for a given grammar production.)

If we add node types that are subtypes of Expression, they can be generated in Expression holes.
Let's add a node for literal integers.

@racketblock[
(add-to-grammar
 arith
 [LiteralInt Expression ([v = (random 100)])])
]

Note that the literal node contains a child @tt{v} that is a normal Racket value, not a grammar node type.
It is initialized with the expression on the right-hand side of the @tt{=} sign.

The literal integers generated in our language will only be values from 0 to 99.
Note that we can add the initialization expression inline as above or with the @racket[fresh] property.
If we don't add an initialization expression, then non-node fields will be initialized with @racket[#f], while node fields will be initialized with hole nodes of the appropriate type..

Let's add addition expressions.
Because we have multiple Expressions, we need to give them names.
Note that we aren't supplying initialization code.
Because they are grammar node typed, they will be initialized with Expression Hole nodes (same with the Program node's Expression child).

@racketblock[
(add-to-grammar
 arith
 [Addition Expression ([l : Expression]
                       [r : Expression])])
]

Our language only has one type: integers.
This means we don't @italic{need} to add type rules, but we will anyway for the sake of a complete tutorial.
We will call the type @tt{int} and add an implementation of the @racket[type-info] property for our language so far.

@racketblock[
(define int (base-type 'int))
(add-property
 arith
 type-info
 [Program [int (λ (n t) (hash 'Expression int))]]
 [LiteralInt [int (λ (n t) (hash))]]
 [Addition [int (λ (n t) (hash 'l int 'r int))]])
]

Each rule supplied to the property is surrounded in square brackets, @tt{[]}.

The left-hand side of each of these rules is an expression that returns the type the node can inhabit.
Most often, this will just be the name of the node type, such as @tt{Program} and @tt{LiteralInt} in this example.
If a node can inhabit multiple types, @racket[fresh-type-variable] can be used to specify an unconstrained or partially constrained type.

On the right-hand side is a function with two parameters: the node currently being evaluated, and the type that is currently being used to confine generation of that node.
The body of the function returns a dictionary mapping children (by name or by node object) to a type.
Note that type variables are unified during the type analysis, so you should take care with object/pointer equality (i.e., @racket[eq?]) of type variables.

Now we need to specify how to print our programs, or “render” them.
For this, we use the @racket[render-node-info] property.

@racketblock[
(add-property
 arith
 render-node-info
 [Program
  (λ (n) (att-value 'xsmith_render-node (ast-child 'Expression n)))]
 [LiteralInt
  (λ (n) (number->string (ast-child 'v n)))]
 [Addition
  (λ (n) (format "(~a + ~a)"
                 (att-value 'xsmith_render-node (ast-child 'l n))
                 (att-value 'xsmith_render-node (ast-child 'r n))))])
]

In this case, we are rendering each node directly to a string, but that's not usually the best approach.
More often, we render programs with some intermediate data structure and convert that to a string only as a last step.
Most Xsmith fuzzers either use s-expressions for rendering Lisp-like languages, or else the @tt{pprint} Racket library's document objects.
You can, of course, use your own custom implementation if you prefer.

We put everything together with the @racket[define-xsmith-interface-functions] macro, which compiles our language specification and defines the @tt{arith-command-line} function.

@racketblock[
(define-xsmith-interface-functions
  [arith]
  #:comment-wrap (λ (lines)
                   (string-join
                    (map (λ (x) (format "// ~a" x)) lines)
                    "\n")))
]

Note that we give it a function for how it wraps comments.
Specifically it is a function that takes a list of single-line strings and returns a single string that's been appropriately formatted to be commented in your language.
There are many optional arguments to the @racket[define-xsmith-interface-functions] that changes the way it behaves.

To actually run our fuzzer, we need to run the @tt{arith-command-line} function.
Let's put it in our main submodule.
Then when we run our program from the command line it will generate a program.
It automatically has @tt{--help} support, and has various options, such as setting a seed, a max depth, etc.

@racketblock[
(module+ main
  (arith-command-line))
]


@section[#:tag "minimal-example"]{Minimal Example}

Below is a complete generator of arithmetic expressions, following the implementation we just outlined in @secref{getting-started}.
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



@section{Another Small Example with Variables}

Here is a bigger example that contains variables and references to those variables.

Note that instead of a @tt{Program} node, we use the @tt{LetStar} node as the base node from which to begin generation.
The purpose in having a specific top-level @tt{Program} node is to be sure that the top-level node can have definitions lifted to it.
The @tt{LetStar} node in the following example satisfies this purpose.

This example also renders to s-expressions rather than directly to strings like the @seclink["minimal-example"]{previous example}.
Because of this change, we have to give @racket[xsmith-command-line] another optional argument, called @tt{format-render}, specifying how we convert our rendered format into strings.

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



@section{An Upgrade: Using Canned Components}

Many programming languages share various features in common, such as binding scope, arithmetic expressions, functions, etc.
The @seclink["canned-components"]{@tt{xsmith/canned-components}} library in Xsmith provides tools for quickly building this common infrastructure for your language specification with minimal effort.
It supplies the ability to enable various productions as needed, as well as default implementations of the @racket[type-info] and @racket[fresh] properties for those nodes.
Here is an example of a fuzzer built using Canned Components.

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
