#lang scribble/manual
@; -*- mode: Scribble -*-
@;
@; Copyright (c) 2018-2019 The University of Utah
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

@(require (for-label
(except-in racket/base
           module)
xsmith

racket/contract/base
racket/dict

@; if racr had scribble documentation this would provide hyperlinks
@;racr
))
@(require
racket/runtime-path
racket/file
)

@(define-runtime-path minimal-example-path "minimal-example.rkt")


@title{Xsmith}
@defmodule[xsmith]


@section{Overview}

Xsmith is a library for creating fuzzers.  It has a DSL for specifying a grammar auxiliary information about the grammar to produce a function which generates random ASTs for the language, as well as utilities for creating a command-line interface for generating a single program or starting a web server to generate one program per request.

To create a fuzzer, users create a specification by combining @italic{spec components}, defined with @racket[define-spec-component].
Each spec component can have portions of grammar as well as @italic{properties} added to them (using @racket[add-to-grammar] and @racket[add-prop]).
The grammar and properties are used to generate a RACR grammar, attributes for the grammar, and @italic{choice objects}, which guide AST generation.

Program generation starts by generating an AST hole for a given grammar production.
Generation continues by filling holes with concrete AST nodes (which may introduce new holes as child nodes).
Each time a hole is to be filled, the grammar specification is used to determine potential replacements, such as an addition expression or a subtraction expression for an expression hole.
A choice object is created for each legal replacement.
Choice objects have methods (choice-rules) which aid in choosing a concrete replacement.
Some of these methods act as predicates to filter out choices that are not legal in a particular context, such as choices that introduce more holes when the maximum tree depth has been reached.
The @racket[choice-weight] property defines a method which determines the relative probability of each choice being chosen.
The @racket[fresh] property defines a method which determines how the choice is instantiated as a RACR node.
Additional methods may be defined as helpers.
Choice objects have access to the @racket[current-hole], so they may query RACR attributes in method bodies.
Choice object classes follow the same hierarchy as the grammar, so method inheritance for choice objects is similar to attribute inheritance for RACR nodes.

RACR attributes may be added directly with @racket[add-ag-rule] and @racket[add-choice-rule], but many are defined indirectly by various Xsmith properties.
Properties allow users to specify various attributes and choice rules in a more declarative fashion.


@subsection{RACR overview}

RACR is a library for Reference Attribute Grammars.  Xsmith's DSL defines a RACR grammar specification as well as various attributes.  The attributes are queried to determine how to generate the AST.

RACR caches the results of attribute queries and keeps track of the nodes accessed for any attribute.  When nodes used in an attribute computation are changed, future queries to that attribute are re-computed.

Users can specify new RACR attributes for Xsmith generators, but they should use @racket[add-ag-rule] or @racket[add-prop] from Xsmith rather than using RACR functions directly.  In expressions evaluated in the context of RACR attributes (ag-rules) or choice rules, RACR attributes may be queried.

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


@subsection{Holes and Choice Objects}
Hole nodes are RACR AST nodes.
For every node type in the grammar, a hole node is created as a subclass of it, inheriting all of its RACR attributes.
A hole can be recognized by the @racket['is-hole?] attribute.

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
The choice objects all have access to the Expression hole (through @racket[current-hole]), but while choice objects have access to their specialized choice method implementations, the hole is of type Expression, and so all RACR attributes (ag-rules) that may be queried are specialized only as far as Expression, not to LiteralInt or AdditionExpression.

Note that hole node types are created for every type in the grammar (including LiteralInt and AdditionExpression), but more specialized holes are only used if the grammar specifies that a node's child must be specifically that kind of expression, or if a custom @racket[fresh] implementation uses @racket[make-hole] with the specific kind of expression.


@subsection{Scope Graphs}
Xsmith uses the language-independent resolution algorithm of scope graphs.

The theory of scope graphs is described in the paper “A Theory of Name Resolution with Extended Coverage and Proofs”, (available at @url{https://researchr.org/publication/NeronTVW15}).


@subsection{Minimal Example}

TODO - this example should be... more minimal.

@(verbatim (file->string minimal-example-path))

@section{API Reference}

@subsection[#:tag "generated-rules"]{Auto-generated ag-rules and choice-rules}
Many attributes and choice-rules are auto-generated by Xsmith.

The following are always defined by @racket[assemble-spec-components]:
@itemlist[
@;@item{@racket['xsmith_hole->choice-list]}
@item{@racket['is-hole?]

Accepts no arguments other than the node to call it on.
Returns @racket[#t] when called on a hole node, otherwise returns @racket[#f].

Example:
@racketblock[
(code:comment "Within the context of a choice method, this will return #t")
(att-value 'is-hole? (current-hole))
]
}
@;@item{@racket['xsmith_hole->replacement]}
@item{@racket['find-descendants]

Accepts the node to call it on, then a predicate.
Returns a list of all descendant nodes that satisfy the predicate.

Example:
@racketblock[
(code:comment "This will return a list of all addition and subtraction nodes.")
(code:comment "It will not return any nodes that are subtypes, though!")
(att-value 'find-descendants ast-node (λ (n) (member (ast-node-type n)
                                                     '(AdditionExpression
                                                       SubtractionExpression))))
]
}
@item{@racket['find-a-descendant]

Accepts the node to call it on, then a predicate.
Like @racket['find-descendants], but it only returns one.
If no descendant matching the predicate is found, it returns @racket[#f].
It is mostly useful if you just want to know if there is any descendant matching a type, such as to determine whether to do an analysis based on the presence or absence of some feature.
}
@item{@racket['resolve-reference-name]

Accepts the node to call it on and a name to resolve.
Returns the binding object for the name at the given node.

The lookup is done using the scope graph model.
The binding object is whatever was bound based on the @racket['scope-graph-binding] attribute.
It should be a scope graph @racket[binding] object.

TODO (code) - the scope-graph-binding attribute should be generated by a property.  There should be some stuff that makes binding declaration easier, including things about types.  So far bindings are always the binding object with a hash table inside with information about types and the node that it was bound at.
}
@item{@racket['xsmith_visible-bindings]

TODO - change this because I'm going to modify this.

Accepts the node to call it on and nothing else.
Returns a list of @racket[binding] objects.
The list will contain all bindings visible in the scope of the given node.
}
]


The following attributes are defined by properties:
@itemlist[
@item{@racket['ast-depth]

Accepts the node it is called on, and no other arguments.
Returns the tree depth at that node.
Determined by @racket[depth-increase].
}
@item{@racket['xsmith_get-reference!]
This is a choice method that can be used when creating a reference node.
If no binding of the appropriate type for the current node is in scope, this method will cause a fresh appropriate definition to be lifted into a node that can hold definitions.
}
]


Node type names, attribute names, and choice rule names are just symbols, so they are not hygienic.  The names of implementation-private symbols start with @tt{xsmith}.  So don't name your attributes and methods similarly.


@subsection{grammar-macros.rkt}

@defform[(define-spec-component component-name)]{
Defines a spec component.  Spec components include information about a language grammar and attributes, and can be combined to generate an xsmith fuzzer.  You add grammar productions with @racket[add-to-grammar], you add properties with @racket[add-prop], and you can add ag-rules and choice-rules with @racket[add-ag-rule] and @racket[add-choice-rule], respectively.  Spec components are combined with @racket[assemble-spec-components].

Example:
@racketblock[
(define-spec-component my-spec-component)
(code:comment "Now use add-to-grammar, add-prop, etc.")
(code:comment "Then use my-spec-component in assemble-part-specs.")
]
}

@defform[(assemble-spec-components spec-name
                                   maybe-properties
                                   spec-component ...)
#:grammar [(maybe-properties (code:line)
                             (code:line #:properties list-of-properties))]
          @;[list-of-properties (property ...)]
          ]{

Combines spec components and generates a RACR specification.

Defines @racket[spec-name] as a RACR specification.

Defines @tt{<spec-name>-generate-ast} as a function.  The function accepts the name of a grammar production as a symbol and produces a random tree starting from a fresh node of that nonterminal.  Essentially, given the name of the top-level program node, this function generates a random program.

Various ag-rules are automatically defined within the spec, see @secref{generated-rules}.

Properties (defined with @racket[define-property]) are used to derive more RACR ag-rules as well as Xsmith choice-rules.
Each property may have a transformer function that alters other properties, ag-rules, or choice-rules.
All properties referenced within a spec-component are used to generate ag-rules and choice-rules, as well as any properties specified in the @racket[maybe-properties] list.
Unless values for that property have also been specified within a spec component, properties in the @racket[maybe-properties] list will only be able to generate rules based on the default value for the property.

Example:
@racketblock[
(assemble-spec-components
 my-spec
 #:properties (depth-increase fresh wont-over-deepen introduces-scope)
 my-spec-component
 my-other-spec-component
 )
(code:comment "Now `my-spec` is defined as a RACR spec,")
(code:comment "and `my-spec-generate-ast` is defined as a function which")
(code:comment "accepts a symbol argument representing the name of an AST node.")
]
}

@defform[(add-to-grammar spec-component grammar-clause ...)
#:grammar [(grammar-clause (node-name parent-name (field ...) maybe-prop ..))
           (parent-name identifier #f)
           (field name/type-id
                  (name/type-id maybe-type-id maybe-kleene-star maybe-init-expr))
           (maybe-type-id (code:line)
                          (code:line : type-name))
           (maybe-kleene-star (code:line) *)
           (maybe-init-expr (code:line) (code:line = init-expr))
           (maybe-prop (code:line #:prop prop-id prop-val))]]{

Adds grammar productions to @racket[spec-component].

@racket[node-name] will be the name of the grammar production in RACR.
@racket[parent-name] is either the name of the parent grammar production or @racket[#f].

Names for the node and fields are limited to alphabetic characters.  You may want to use camelCase style names since kebab-style or snake_style names due to this limitation.

Fields are then specified.
Each nonternimal inherits all fields of its parent nodes.
A field has a name, a type, an optional kleene star, and an optional initialization expression.
The type of each field is the name of the nonterminal that it must be or @racket[#f] for fields that may contain arbitrary Racket values.
A field name may be the same as the type, in which case the type does not need to be specified.
If a type is not specified and the name does not match the name of a nonterminal, then the type #f is used.
If the optional kleene star is supplied, the field will be a list field.
If a kleene star is provided for a non-false type, the name and type must be specified separately.

The @racket[init-expr] for each field specifies a default value for the field.
When a node is generated, each @racket[init-expr] is evaluated unless a non-default value is supplied to the generating function.
If no @racket[init-expr] is supplied, the following defaults are used:

@itemlist[
@item{For false types, @racket[#f].}
@item{For nonterminal types, a hole node of the appropriate type.}
@item{For fields with a kleene star, an empty list.}
]

For nodes with a kleene star, @racket[init-expr] may return a list or a number.
If a number is provided, the default value is a list of that many of the non-kleene-star default value.

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [Expression #f ()]
 [LiteralInt Expression (v = (random 1000))]
 [AdditionExpression Expression ([left : Expression] [right : Expression])]
 [SumExpression Expression ([addends : Expression * = (random 5)])])
]

The example defines a piece of a grammath that includes some kinds of expressions.
When a @tt{LiteralInt} expression is generated, it's @tt{v} field will be populated with a random number.
Since the @racket[random] expression is evaluated for each fresh @tt{LiteralInt}, they will (probably) receive different values for @tt{v}.
When an @tt{AditionExpression} node is generated, it will be populated with an @tt{Expression} hole node for each of its @tt{left} and @tt{right} fields.
When a fresh @tt{SumExpression} is generated, its @tt{addends} field will be populated with a list of zero to four @tt{Expression} hole nodes.
}

@defform[(add-ag-rule spec-component rule-name rule-clause ...)
#:grammar [(rule-clause (nonterminal-name rule-function))]]{
Adds a RACR ag-rule to the spec-component.

Example:
@racketblock[
(add-ag-rule
 my-spec-component
 interp
 [LiteralInt (λ (n) (ast-child 'v n))]
 [AdditionExpression (λ (n) (+ (att-value 'interp (ast-child 'left n))
                               (att-value 'interp (ast-child 'right n))))]
 [SumExpression (λ (n) (apply + (map (λ (c) (att-value 'interp c))
                                     (ast-children (ast-child 'addends n)))))])
]
}

@defform[(add-choice-rule spec-component rule-name rule-clause ...)
#:grammar [(rule-clause (nonterminal-name rule-function))]]{
Adds an Xsmith choice-rule to the spec-component.

Xsmith creates a choice object class for each node type in the specification grammar, following the same class hierarchy that AST nodes themselves do.  Choice objects are created every time Xsmith fills in a hole node.  One choice object is created for every node type that is legal to use in filling the hole.  Choice objects are then filtered according to the @racket[choice-filters-to-apply] property, and then the @racket[choice-weight] property of the remaining choice objects is used to determine the probability of choosing each one.  When a choice object is selected, its @racket[fresh] property is used to generate a new AST node of its type.  If all choices are eliminated, an exception is raised with a message stating which filter step invalidated each potential choice.

Choice rules are methods on the choice objects.  Some choice rules are used by @racket[choice-filters-to-apply] to filter choices.  Other choice rules may be used by those filters or in the body of the @racket[fresh] property as helper methods.  While most information about the AST and the current choice are probably computed using ag-rules, information about choosing a specific node type to fill in an abstract hole (such as an expression hole which may be filled with many different types of expressions) are computed using choice rules.

Choice rules are methods in Racket's class system and therefore have the @racket[this] macro available for use in their bodies to access other methods (eg. with the @racket[send] macro).
Choice rules also have the @racket[current-hole] macro available within their body so that they can query attributes of the RACR AST being elaborated (eg. with @tt{att-value} to access ag-rules and @tt{ast-parent} to inspect other nodes in the AST).

Since choice rules are methods in Racket's @racket[class] system, they must be defined with a literal @racket[lambda] (with no parameter for the implicit @racket[this] argument).  If a method needs to modify state (such as to cache the computation of available references of the appropriate type), I would normally recommend the “let-over-lambda” pattern, but that is not allowed in this case.  To make up for this, I recommend using @racket[make-weak-hasheq] to hold the state, using the @racket[this] object as a key.


This is a poor example, but it demonstrates how ag-rules and choice-rules can be used together to help make choices:
@racketblock[
(add-choice-rule
 my-spec-component
 my-weight-helper
 [#f 7]
 [AdditionExpression
  (λ () (if (att-value 'my-helper-ag-rule (current-hole))
            20
            5))])
(add-prop
 my-spec-component
 choice-weight
 [#f (send this my-weight-helper)])
]

}

@defform[(add-prop spec-component prop-name prop-clause ...)
#:grammar [(prop-clause (nonterminal-name prop-value))]]{
Adds property values to the spec-component.

Since property transformers are macros that may accept arbitrary domain-specific syntax, the grammar of prop-value varies for each property.
}

@defform[(current-racr-spec)]{
In code within the context of a spec component (eg. in the body of @racket[add-ag-rule], @racket[add-prop], @racket[add-to-grammar], etc), @racket[(current-racr-spec)] returns the RACR spec object for the grammar ultimately combined by @racket[assemble-spec-components].

Elsewhere it raises a syntax error.

TODO - this should probably be private (It's really only needed for the @racket[fresh] property)
}

@defform[(current-hole)]{
Within the body of a choice-rule, @racket[(current-hole)] returns the hole node being considered for replacement.
This allows choice-rules to query ag-rule attributes of the grammar.

Elsewhere it raises a syntax error.
}

@defform[(make-hole hole-type-expression)]{
Within the context of a spec component (eg. in the body of @racket[add-ag-rule], @racket[add-prop], @racket[add-to-grammar], etc), @racket[make-hole] is a function to generate a hole of a given type.

For example, to make a hole node that will eventually be replaced with some type of @tt{Expression} node:
@racketblock[(make-hole 'Expression)]

This function is essentially used by @racket[add-to-grammar] as the default value for grammar fields with nonterminal types that lack an init-expr.

Outside of a spec component context, it raises a syntax error.
}

@defform[(make-fresh-node node-type-expression optional-field-value-dict)]{
Within the context of a spec component (eg. in the body of @racket[add-ag-rule], @racket[add-prop], @racket[add-to-grammar], etc), @racket[make-fresh-node] is a function to generate a fresh node of the given type.  Construction of the new node is guided by the @racket[fresh] property.

For example, to generate a fresh @tt{AdditionExpression} node, specifying values for some of its fields:
@racketblock[(make-fresh-node 'AdditionExpression
                               (hash 'left (make-fresh-node 'LiteralInt
                                                            (hash 'v 5))))]
}

@defform[(define-property name
                          maybe-dups
                          maybe-reads
                          maybe-rewrites
                          maybe-appends
                          maybe-transformer)
#:grammar [(maybe-dups (code:line)
                       (code:line #:allow-duplicates? literal-bool))
           (maybe-reads (code:line)
                        (code:line #:reads transformer-arg-spec ...))
           (maybe-rewrites (code:line)
                           (code:line #:rewrites transformer-arg-spec ...))
           (maybe-appends (code:line)
                          (code:line #:appends transformer-arg-spec ...))
           (maybe-transformer (code:line)
                              (code:line #:transformer transformer-function))
           (transformer-arg-spec (property prop-name)
                                 (ag-rule rule-name)
                                 (choice-rule rule-name)
                                 (grammar))
           ]]{
Defines a property for use with @racket[add-prop].

Properties are used to create domain-specific languages or terse declarative syntax for specifying ag-rules and choice-rules.

The transformer function accepts dictionaries of syntax specifying values for grammar nodes for the property itself or other properties that the property reads.  The function must return a list of dictionaries for the values of ag-rules, choice-rules, or properties that it writes.

Property transformers are run during @racket[assemble-spec-components] in an order determined by the properties read and written by each property transformer.

A property that is read by a property transformer (an argument of the @racket[#:reads] keyword) will be received as an argument by the transformer.  Other properties and the grammar produced by combining spec components may be read, but ag-rules and choice-rules may not be read.

A property that is appended to by a property transformer (an argument of the @racket[#:appends] keyword) must have a dictionary specifying values returned by the transformer.  The values returned will be mixed with values appended by other properties or specified by the user.  If an ag-rule or choice-rule for a node is specified more than once by any of these parties, an error is raised.  Other properties, ag-rules, and choice-rules may be appended to.

A property that is rewritten by a property transformer (an argument of the @racket[#:rewrites] keyword) is received as a dictionary argument to be read, and a similar dictionary must be returned in the return list.  The returned dictionary replaces the received dictionary.  Only other properties may be rewritten.

Example showing the arguments and return type needed by transformers:
@racketblock[
(define-property my-property
  #:reads (property a) (property b)
  #:rewrites (property c)
  #:appends (ag-rule d) (choice-rule e)
  #:transformer
  (λ (this-prop-dict prop-a-dict prop-b-dict prop-c-dict)
    (code:comment "compute output dictionaries...")
    (list dict-for-c dict-for-d dict-for-e)
    ))
]

Each dictionary maps grammar node type names (as symbols) to syntax objects.

The syntax object value for a property can be anything, since property transformers define the grammar and semantics of properties.

The syntax object value for ag-rules and choice-rules should be a syntax object specifying a function (IE a @racket[lambda]).  Ag-rules may be any syntax that evaluates to a function (so you may return an identifier that references a function or an expression that computes a function such as let-over-lambda), but choice-rule syntax is provided to Racket's @racket[class] macro, which requires literal @racket[lambda] forms.

The syntax object value for grammar productions when @tt{(grammar)} is read is a syntax object of class @racket[grammar-clause].

Dictionaries may or may not contain an entry for each nonterminal in the grammar (except the grammar dictionary which always contains all nonterminals).  A dictionary may even be empty.

In addition to nonterminals, each dictionary may include a mapping for the value @racket[#f], which will define a default value used for the (super secret) parent node that @racket[assemble-spec-components] defines.  If nothing is specified for #f, ag-rules and choice-rules will have a default which errors, providing a helpful error message.

If the @racket[#:allow-duplicates?] argument is supplied and is @racket[#t], then @racket[add-prop] may be used more than once for the property for the same node, and the syntax object in the dictionary for the property will be a syntax list of the syntax objects specified in the various @racket[add-prop] calls.  But by default only one use of @racket[add-prop] is allowed per property per node type, and the syntax object in the dict is the single syntax object from the one call.

TODO - is that feature really necessary?  I could cut out #:allow-duplicates? without affecting any of my properties, but somehow I felt like I should allow it...

TODO - a real example.  Maybe something from core-properties.rkt, or something simplified.
}

@defform[(define-non-inheriting-rule-property property-name
                                              rule-type
                                              maybe-rule-name
                                              default-value
                                              maybe-transformer)
#:grammar [(maybe-rule-name (code:line)
                            (code:line #:rule-name rule-name))
           (default-value (code:line #:default default-expr))
           (maybe-transformer (code:line)
                              (code:line #:transformer transformer-func))]]{
Defines a property that generates an ag-rule or a choice-rule that does NOT inherit its implementation from its superclass.

@racket[rule-name] must be either @tt{ag-rule} or @tt{choice-rule}.

@racket[rule-name] defaults to @racket[property-name], but you can make it give the rule a different name than the property.

@racket[default-expr] is the default value of the property.  Any nonterminal that does not have a different value specified gets this one.

@racket[transformer-func] is an optional transformer to transform the value.  It is not called with a dictionary like the transformers of @racket[define-property], but rather it receives each value individually.  This allows a small amount of sugar.  Note that the value supplied as the @racket[default-expr] is also transformed by the @racket[transformer-func] when it is supplied.  When no @racket[transformer-func] is supplied, values are passed through literally.

Example:
@racketblock[
(define-non-inheriting-rule-property
  some-bool-flag
  ag-rule
  #:default #f
  #:transformer (syntax-parser [#f #'(λ () #f)]
                               [#t #'(λ () #t)]))
(add-to-grammar
 a-spec-component
 [A #f ()]
 [B A ()]
 [C B ()])
(add-prop
 a-spec-component
 some-bool-flag
 [A #t]
 [C #t])
]

Normally @tt{B} would inherit a method from @tt{A} when none was specified for it.  But in this case it inherits the default (@racket[#f]).  When a user tries @tt{(att-value 'some-bool-flag <node-of-type-B>)} it will return @racket[#f], not @racket[#t].
}


@subsubsection{grammar-macros.rkt for-syntax}

@defform[#:kind "syntax class" #:id grammar-clause grammar-clause]{
This is a syntax class used for parsing grammar clauses.  If you parse one with @racket[syntax-parse], you will have access to the following fields:

@itemlist[
@item{@tt{node-name} - the node name as an identifier}
@item{@tt{parent} - the parent node name as an identifier or @racket[#f] (as syntax, not as a bare @racket[#f]) if the node has no parent.}
@item{@tt{component ...} - a list of @racket[grammar-components]}
]

TODO - (properties and the grammar) should this be in the public interface?  Yes if I allow properties to change the grammar, but otherwise I'm not sure.  For reading, it would probably be easier to get a dict full of a struct with: node name, chain of parent nodes, field list (of grammar-node-field-structs).  It will also be faster if I construct that once, because in various properties I am reconstructing some of that info multiple times.

Example:
@racketblock[
(syntax-parse grammar-stx
  [x:grammar-clause
   (code:comment "This basically reconstructs it.")
   #'(x.node-name x.parent (x.component ...))])
]
}
@defform[#:kind "syntax class" #:id grammar-component grammar-component]{
This is a syntax class used for parsing the fields of @racket[grammar-clause]s.
If you parse one with @racket[syntax-parse], you will have access to the following fields:
@itemlist[
@item{@tt{name} - the field name as an identifier}
@item{@tt{type} - optional: the field type as an identifier if there is one, otherwise @racket[#f] (IE use @racket[attribute] to check if it is present).}
@item{@tt{kleene-star} - optional: literal @tt{*} character as an identifier if the field is a list type, otherwise @racket[#f] (IE use @racket[attribute] to check if it is present).}
@item{@tt{init-expr} - optional: syntax object for the initial expression if present, otherwise @racket[#f] (IE use @racket[attribute] to check if it is present).}
]

TODO - the name should be changed to grammar-field or grammar-clause-field or something.

TODO - (properties and the grammar) should this be in the public interface?  As with @racket[grammar-clause], I think it should only if properties can add to the grammar.  As with the TODO comment in @racket[grammar-clause].
}

@defthing[grammar-clause->parent-chain any/c]{
TODO - (properties and the grammar) this should probably not be in the public interface.

In general I should provide better ways to inspect the grammar in property transformers, but I haven't needed them yet...
}

@defproc[(grammar-node-name->field-info-list [name symbol?] [grammar-clause-hash any/c])
         (listof/c grammar-node-field-struct?)]{
This function should be called with a node type name and the grammar object given as an argument to a property transformer that reads the grammar.

It returns a list of structs containing information about the node's fields (including fields inherited from super node types).

TODO - (properties and the grammar) this should probably not be in the public interface
}

@defstruct[grammar-node-field-struct
           ([name symbol?]
            [type (or/c symbol? #f)]
            [kleene-star? boolean?]
            [init-expr syntax?])
           #:omit-constructor]{
The struct type in the list returned by @racket[grammar-node-name->field-info-list].

TODO - (properties and the grammar) Properties asking to read the grammar should get a dict full of grammar clause structs that include a list of these.
}



@subsection{scope-graph.rkt}

@defstruct[binding ([name string?]
                    [ast-node ast-node?]
                    [type type?]
                    [def-or-param (or/c 'definition 'parameter)])
                   #:omit-constructor]{
Struct for binding information of nodes that create bindings.

TODO - example, better explanation...
}

@defparam[current-well-formedness-regexp r regexp?
          #:value #px"rp*i?d"]{

TODO - explanation of scope graphs so this makes sense.

When the attributes @racket['resolve-reference-name] and @racket['xsmith_visible-bindings] are used, this regexp determines valid scope-graph resolution paths.
The path elements (reference, parent, import, definition) are turned into characters (r, p, i, and d respectively).
If the path from reference to definition matches this regexp, it is valid.
If two definitions have the same name and paths from a reference to both definitions are valid, the definition that is in scope for the reference is determined by @racket[current-path-greater-than].
}

@defparam[current-path-greater-than comparator
         (-> (listof/c (or/c 'reference 'parent 'import 'declaration))
             (listof/c (or/c 'reference 'parent 'import 'declaration))
             any/c)]{
TODO - explanation of scope graphs so this makes sense.

If there are two valid resolution paths (determined by @racket[current-well-formedness-regexp])for a name, this comparator determines which path is chosen.
The comparator must return a non-false value if the left operand is greater than the right, otherwise @racket[#f]
The greatest path is chosen.

By default the comparator walks down the paths, comparing each element.
A path is greater than another if its first differing element is greater.
From least to greatest, path elements are @racket['reference], @racket['parent], @racket['import], @racket['declaration].

}

@subsubsection{scope-graph.rkt -- other things provided but that maybe aren't part of the public interface}
TODO - these are also provided by scope-graph.rkt, but maybe should be private to xsmith, or at any rate aren't used yet.
@itemlist[
@item{resolve-reference -- used by the resolve-reference attribute}
@item{visible-bindings -- used by the xsmith_visible-bindings attribute}
@item{scope -- struct used by basically all of the functions, but should be invisible to end-user}
@item{reference -- struct used internally like scope}
@item{module -- struct that I made for paths that include imports, but I haven't actually used that yet, so it is just languishing...}
]



@subsection{core-properties.rkt}

@defform[#:kind "spec-property" #:id may-be-generated may-be-generated]{
#;This property defines the @tt{xsmith_may-be-generated-method} non-inheriting choice-rule.
Acceptable values for this property are @racket[#t] or @racket[#f], and the default is @racket[#t].

If may-be-generated is false, the node is not added to the list of possibile choices to replace an appropriate AST hole.
It is useful to set it to false for abstract node types or for specialized node types that are meant to be swapped in only after a full tree is generated, such as by a later analysis to determine validity of an unsafe operation.
This property is NOT inherited by subclasses.

Example:
@racketblock[
(add-prop
 my-spec-component
 may-be-generated
 [MyAbstractNode #f]
 [UnsafeAddition #f])
]
}

@defform[#:kind "spec-property" #:id depth-increase depth-increase]{
This property defines the @tt{ast-depth} non-inheriting ag-rule.

The property accepts an expression which much evaluate to a function of one argument (the RACR AST node) which returns a truthy value for nodes which increase the depth of the AST and #f otherwise.  The default is @racket[(λ (n) #t)].
This property is NOT inherited by subclasses.

This is useful to allow node re-use.  For example, the body of an @tt{if} or @tt{for} statement might be a block and have the same semantics, but you might want a block inside an @tt{if} to only be considered a depth increase of 1, not 2.

Example:
@racketblock[
(define no-depth-if-body-is-block
  (λ (n) (if (node-subtype? (ast-child 'body n) 'Block) 0 1)))
(add-prop
 my-spec-component
 depth-increase
 [IfStatement no-depth-if-body-is-block]
 [ForStatement no-depth-if-body-is-block])
]
}

@defform[#:kind "spec-property" #:id fresh fresh]{
@;This property defines the @tt{xsmith_fresh} choice-rule.
This property determines how fresh nodes are constructed (by the @racket[make-fresh-node] function).

Acceptable values for this property are expressions which produce a @racket[dict?] object, or expressions which produce a function of type (-> dict? dict?).  Keys of the dictionary must be field names of the node being generated.  The values in the dictionary are used to fill node fields of the appropriate name.  Any field whose name is not in the dictionary will be filled by evaluating the default init-expr defined in the grammar (via @racket[add-to-grammar]).

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [Expression #f ()]
 [LiteralInt Expression (v = (random 1000))]
 [AdditionExpression Expression ([left : Expression] [right : Expression])])
(add-prop
 my-spec-component
 fresh
 (code:comment "Make AdditionExpressions always be generated with a literal 7 argument.")
 [AdditionExpression (hash 'left (make-fresh-node LiteralInt (hash 'v 7)))])
]

This is useful for fields that must be determined together.  For examlpe, a function call needs the function name and the number of arguments to be chosen together rather than independently.

As with all choice-rules, @racket[this] and @racket[current-hole] are available for use in expressions, which you may want to do for eg. accessing available bindings or mutable information connected to the choice object.

If the result is a procedure instead of a dictionary, that procedure must accept and return a dictionary.  It is called with a dictionary that is empty unless the node being created is the result of lifting a definition.  In that case it will have the appropriate name and type fields with the name and type chosen by the lifting mechanism.  In the case of lifting a definition, the name and type fields in the return dictionary are ignored.  This procedure option is allowed because your fresh expression may need access to the name or type to determine the values of other fields.  If a definition node only has a name and type field then a fresh property is unnecessary when lifting, and if lifting is the only way you generate definitions then fresh properties or initializers for definition nodes are unnecessary.

If the value for a field (IE values inside the result dictionary) is a procedure, it will be called with 0 arguments.  This allows the fresh property to provide a default value that is not evaluated when @racket[make-fresh-node] is called with an appropriate value.

}

@defform[#:kind "spec-property" #:id wont-over-deepen wont-over-deepen]{
The default for this property is probably what you want, so probably just be sure to add this to the extra #:properties flag of @racket[assemble-part-specs].

TODO (code) - this property should probably be in a base set that is just always run be default.

But if you want to set it:

The property accepts expressions which will evaluate to booleans (IE anything but only #f is false...), which are evaluated if the choice is made at the point where the AST is at it maximum depth.  A true value means that the choice is acceptable, false otherwise.  The default is computed by checking whether a node includes AST-node-typed fields.  If it does not it is considered atomic and therefore acceptable to choose when the AST is already at its maximum depth.
}

@defform[#:kind "spec-property" #:id introduces-scope introduces-scope]{
This property needs to be in the #:properties list of @racket[assemble-spec-components].  But it should not actually be used.

TODO (code) - this property should be in a base set that is always run by default, and it should not even be exported to users.

}

@defform[#:kind "spec-property" #:id binder-info binder-info]{
This property is used to mark nodes that define bindings.
The property consists of a length-3 list.
The first two are field names, one for the name of the field that stores the binding name, one for the name of the field that stores the binding type.
The last field is either @tt{definition} or @tt{parameter}, reflecting whether the binding is a function parameter.  This is used by some Xsmith analyses about higher order values.

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [Definition #f (name type Expression)]
 [Reference #f (name)])
(add-prop
 my-spec-component
 binder-info
 [Definition (name type definition)])
]
}
@defform[#:kind "spec-property" #:id reference-info reference-info]{
This property marks nodes that are reference nodes.  The argument for the property is a list containing:

@itemlist[
@item{The identifier @tt{read} or the identifier @tt{write}, indicating whether the reference reads or writes the variable}
@item{The name of the field that stores the reference name (as an identifier).}
]

Example:
@racketblock[
(add-prop
 my-spec-component
 reference-info
 [Reference (read name)])
]
}

@defform[#:kind "spec-property" #:id binding-structure binding-structure]{
This property is used on nodes that can have binders as children.
It determines the visibility of those binders to their siblings.
Options are @racket['serial] (like @tt{let*} in scheme), @racket['parallel] (like @tt{let} in scheme), and @racket['recursive] (like @tt{letrec} in scheme).

If the property is not specified, @racket['serial] is assumed and used as a default.

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [Let #f ([definitions : Definition *] Expression)]
 [Letstar #f ([definitions : Definition *] Expression)]
 [Letrec #f ([definitions : Definition *] Expression)]
 )
(add-prop
 my-spec-component
 binding-structure
 [Let 'parallel]
 (code:comment "Letstar we can leave blank if we want because serial is the default.")
 [Letrec 'recursive])
]
}


@defform[#:kind "spec-property" #:id strict-child-order strict-child-order]{
Specifies that a node's children are guaranteed by the language to have a strict evaluation order.  The default is false.  This property is used to determine whether nodes have a dependency in their read/write/io effect conditions.

Example:
@racketblock[
(add-prop
 my-spec-component
 strict-child-order
 (code:comment "Most languages have some sort of sequential construct, like a block")
 [Block #t])
]
}


@defform[#:kind "spec-property" #:id io io]{
Used to specify that a node has some kind of IO effect, such as printing or reading a volatile variable.

Example:
@racketblock[
(add-prop
 my-spec-component
 io
 [Print #t])
]
}


@defform[#:kind "spec-property" #:id lift-predicate lift-predicate]{
This property specifies a predicate for whether a definition of a given type can be lifted to a node.

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [Let #f ([definitions : Definition *] Expression)]
 [Letstar #f ([definitions : Definition *] Expression)]
 [Letrec #f ([definitions : Definition *] Expression)]
 )
(add-prop
 my-spec-component
 lift-predicate
 (code:comment
  "Allow any definition type to be lifted into the top level of a program.")
 [Program (λ (n type) #t)]
 (code:comment
  "Lifting a definition to Lambda's formal parameter list would require changing all calls.")
 [Lambda (λ (n type) #f)]
 (code:comment
  "Allow local variables to be lifted, except make all functions top-level.")
 [Let (λ (n type) (not (function-type? type)))])
]
}

@defform[#:kind "spec-property" #:id lift-type->ast-binder-type
lift-type->ast-binder-type]{
If you have more than one binding node in your language (IE via @racket[binder-info]) you must specify this property.
This property should be defined once for the base node (#f).
It is a mapping from the type of a desired definition (eg. int, float, int -> int, ...) to the AST node type (eg. VariableDefinition, FunctionDefinition).
This is important when different kinds of definitions use different AST nodes.
Otherwise it is just boilerplate...
@; introduces these private rules:

Example:
@racketblock[
(add-to-grammar
 my-spec-component
 [VariableDefinition #f (name type Expression)]
 [FunctionDefinition #f (name type Body)]
 )
(add-prop
 my-spec-component
 lift-type->ast-binder-type
 [#f (λ (type) (if (function-type? type)
                   'FunctionDefinition
                   'VariableDefinition))])
]
}

@defform[#:kind "spec-property" #:id type-info type-info]{
@; introduces these private rules:
@;xsmith_type -- returns the type of a node
@;xsmith_my-type-constraint -- returns the type that a node must fulfill (the first half of the type info property)
@;xsmith_children-type-dict -- returns a dict mapping nodes (or node field names) to types
@;xsmith_satisfies-type-constraint? -- choice predicate -- tests if a hole's type and a choice object are compatible
@;xsmith_reference-options! -- returns a list of options for a variable to reference that are type compatible.  BUT - it unifies the type of the reference with a fully concrete version.  One of the list members is a thunk that can be applied to get a lifted binding.
@;xsmith_get-reference! -- like xsmith_reference-options! but it just returns one (pre-called in the case of lifts).

This property is used to specify the type system used by the generator.
You should specify a type system even for dynamically typed languages so that programs don't just crash with dynamic type errors.

Example:
@racketblock[
(define int (base-type 'int))
(define float (base-type 'float))
(define bool (base-type 'bool))
(add-prop
 my-spec-component
 type-info
 [AdditionExpression [(fresh-type-variable int float)
                      (λ (n t) (hash 'l t 'r t))]]
 [EqualityExpression [bool
                      (λ (n t)
                        (define arg-type (fresh-type-variable))
                        (hash 'l arg-type 'r arg-type))]]
 [Lambda [(function-type (fresh-type-variable) (fresh-type-variable))
          (λ (n t) (hash 'arg (function-type-arg-type t)
                         'Expression (function-type-return-type t)))]])
]

The property is two armed.

The first part is the type (or partially-constrained type variable) that the given node can inhabit.  The expression given is evaluated fresh every time a node is type checked or considered for generation.

The second part is a function that takes a node, its type, and must return a dictionary mapping its children nodes to types.  The dictionary keys may be the node objects of the node's children OR the symbol of the field name the child inhabits.  For kleene-star children, use their node unless they all should receive the same type.
}


@defform[#:kind "spec-property" #:id choice-weight choice-weight]{
This property determines the probability that different kinds of nodes will be chosen.  When choices have been filtered (based on @racket[choice-filters-to-apply]), one of the remaining choices is chosen at random with probability (choice-weight / sum-of-choice-weights).

The expression provided as the choice weight will be evaluated in the context of a method call, so @racket[this] and @racket[current-hole] are available.

Choice weights should be positive integer values.  The default weight is 10 unless set explicitly.

Example:
@racketblock[
(add-prop
 my-spec-component
 choice-weight
 (code:line "The default choice weight.")
 [#f (λ () 10)]
 (code:line "Generate more AdditionExpressions")
 [AdditionExpression 20]
 [MultiplicationExpression 15]
 (code:line "Generate fewer SumExpressions")
 [SumExpression 5])
]
}

@defform[#:kind "spec-property" #:id choice-filters-to-apply choice-filters-to-apply]{
@;This property defines the @tt{xsmith_apply-choice-filters} choice-rule.

This property accepts a syntax list of choice-rule names to use as a filter for the node type.  Generally this should be set on the greatest super node type (or @racket[#f] if there is no explicit super node type in your grammar).  Each choice-rule in the list is called on the choice object with no arguments.  Each rule that returns @racket[#f] rules the node out as a choice for filling in a hole.

Example:
@racketblock[
(add-prop
 my-spec-component
 choice-filters-to-apply
 [#f (my-custom-filter-choice-rule my-other-filter-choice-rule)])
]

Some core methods are always applied in addition to this list, such as the method defined by the @racket[may-be-generated] property.
If you don't make custom filtering rules you don't need to specify this property.


}

@subsection{types}

While there are various predicates for different types, at any point in type checking you might actually have a type variable instead of a concrete type.  So if you want to check if you have a particular type (and maybe deconstruct it), you should maybe create an instance of the type you are interested in, check if it @racket[can-unify?], then @racket[unify!]-ing it if you want to deconstruct it.

TODO - write a bit about nominal-record-types, because they are a little more complicated to use than other types.

@defproc[(type? [t any/c]) bool?]{
Predicate for types.
}

@defproc[(fresh-type-variable [args type?] ...) type?]{
Creates a fresh type variable.  If given no arguments it is unconstrained and can unify with any type.  Arguments can be provided, in which case the type variable is constrained to be one of the types given.
In the optional arguments, only one function type is allowed.

Example:
@racketblock[
(code:comment "Unconstrained")
(define v1 (fresh-type-variable))

(define int (base-type 'int))
(define float (base-type 'float))
(define bool (base-type 'bool))

(define v2 (fresh-type-variable int bool))

(unify! v1 v2)

(can-unify? v1 float) (code:comment "#f")
(can-unify? v1 int) (code:comment "#t")

(unify! v2 bool)
(can-unify? v1 int) (code:comment "#f")
]
}

@defproc[(type-variable? [t any/c]) bool?]{
Predicate for type variables.
}

@defproc[(can-unify? [t1 type?] [t2 type?]) bool?]{
Returns whether two types can be unified without actually unifying them.
}
@defproc[(unify! [t1 type?] [t2 type?]) void?]{
Unifies two types.  This mutates type variables so that they match other variables or types going forward.

If unification fails an exception is raised.  Right now a failure to unify might mean that type variables are left in a bad state, so code generation should just give up at that point.
}

@defproc[(base-type [name symbol?]) type?]{
Creates a base type.  Base types with the same name are the same.
}

@defproc[(product-type [types (or/c (listof types?) #f)]) type?]{
Creates a product type (tuple).  If @racket[types] is @racket[#f], the length of the tuple is unspecified, and it can be @racket[unify!]-ed with a product type of any length.

Example:
@racketblock[
(define any-length (product-type #f))
(define l2 (product-type (list int int)))
(define l3 (product-type (list int int int)))

(can-unify? any-length l2) (code:comment "#t")
(can-unify? any-length l3) (code:comment "#t")
(unify! any-length l2)
(can-unify? any-length l2) (code:comment "#t")
(can-unify? any-length l3) (code:comment "#f")
]
}

@defproc[(product-type? [t any/c]) bool?]{
Predicate for product types.
}

@defproc[(product-type-inner-type-list [t product-type?]) any/c]{
TODO - this is the raw struct accessor, but what it can return is somewhat complicated because product types are actually a kind of type variable since they can have a #f “list” and then be unified.  So this needs a wrapper if it is actually to be used by xsmith users.
}

@defproc[(generic-type [name symbol?] [args (listof type?)]) type?]{
Used to create generic types.

Example:
@racketblock[
(define (vector-type t)
  (generic-type 'vector (list t)))
]

Generic types can be unified with other generic types with the same name and argument list length.

TODO - This should probably have some kind of wrapper that guarantees that all instances of a given generic have the same name and argument length.  Maybe a function that generates a constructor function?
}

@defproc[(generic-type? [t any/c]) bool?]{
Predicate for generic types.
}

@defproc[(generic-type-name [t generic-type?]) symbol?]{
Returns the name of a generic type.
}
@defproc[(generic-type-type-arguments [t generic-type?]) (listof type?)]{
Returns the inner types of a generic type.
}

@defproc[(function-type [arg-type type?] [return-type type?]) type?]{
Creates a function type.  For multi-argument functions, use a @racket[product-type] for the argument type.
}
@defproc[(function-type? [t any/c]) bool?]{
Predicate for function types.
}
@defproc[(function-type-arg-type [t function-type?]) type?]{
}
@defproc[(function-type-return-type [t function-type?]) type?]{
}

@defproc[(nominal-record-type? [t any/c]) bool?]{
Predicate for nominal record types.
}
@defproc[(nominal-record-type [name any/c] [inners dict?]) type?]{
TODO - @racket[name] should maybe be string? or symbol?

@racket[inners] should be a dictionary from names to types.

A partially defined nominal-record-type can be created that will @racket[unify!] with a fully defined one by giving #f as the name and a dictionary containing a mapping from #f to a needed type.
}
@defproc[(nominal-record-type-name [t nominal-record-type?]) any/c]{
}
@defproc[(nominal-record-type-inners [t nominal-record-type?]) dict?]{
}

@defproc[(nominal-record-definition-type? [t any/c]) bool?]{
Predicate for nominal record definition types.

Nominal records need to be defined to their name.  This is the type to give to that definition.
}
@defproc[(nominal-record-definition-type [t nominal-record-type?]) type?]{
Constructor.
}
@defproc[(nominal-record-definition-type-type [t nominal-record-definition-type?]) nominal-record-type?]{
Getter.
}

@defproc[(concretize-type [t type?]) type?]{
Returns a fully concrete (no type variables) type that @racket[can-unify?] with @racket[t].

TODO - this function is used for lifting, but I'm not sure whether it's a good idea for this to be available to users.  At any point user code is inspecting a type, it may have been lazily unified and @racket[concretize-type] may return a type that can't actually unify with @racket[t] if the lazy unification is forced.  So unless I provide a way to force lazy unification this is not a good function for users to use.
}

@defparam[current-xsmith-type-constructor-thunks thunk-list (listof (-> type?))]{
TODO

This needs to be parameterized for @racket[concretize-type], which is called for lifting.
It should consist of a list of thunks that each produce a fully concrete type when called.
Cish parameterizes this in multiple places, and it should be something that can be set somewhere once.
}

TODO

Record types -- they are only partially implemented.

Sum types



@subsection{xsmith-command-line}

@defproc[(xsmith-command-line
[generate-and-print-func (-> any/c)]
[#:comment-wrap comment-wrap (-> (listof string?) string?)])
any/c]{
This function parses the current command-line arguments for xsmith fuzzers.  It is basically to be used in the main function of a fuzzer.
Based on options supplied, it may print a help message and terminate the program, generate a single program, or start a web server to generate many programs.

@racket[generate-and-print-func] must be a function that generates and prints a single program.  It is called within @racket[xsmith-command-line] with the random seed parameterized according to command-line options (and for the web server reset and incremented for each call), and with all xsmith-options parameterized according to the command line.

@racket[comment-wrap] takes a list of strings which contain info about the generated program, such as the command line used to generate it and the random seed number.  It should return a string representing those lines commented out.  Such as the following, assuming the "#" character is the line-comment character in your language:

@racketblock[
(λ (lines)
  (string-join
   (map (λ (x) (format "# ~a" x)) lines)
   "\n"))]
}


@subsection{xsmith-utils}

@subsubsection{generator state}
@defstruct[generator-state ([fresh-name-counter exact-integer?]) #:omit-constructor]{
Contains mutable state for program generation.

TODO - cish has a separate counter that I use somewhere that needs to be rolled into this.
}
@defproc[(make-generator-state) generator-state?]{}

@defparam[xsmith-state state generator-state?]{
Parameter holding the current xsmith generator state.
}

@subsubsection{RACR convenience functions}
These are a group of convenience functions around RACR.  They are not necessary, and some may not be the best choices.  I should probably revisit them.  TODO.

@defproc[(ast-children/flat [n ast-node?]) (listof/c any/c)]{
Returns a list containing all of @racket[n]'s children (IE fields).
Instead of returning @racket[ast-list-node?]s for kleene star fields, it flattens them into the list.

TODO - this is just used in grammar-macros.rkt, it should be private.
}

@defform[(expr->ast-list length-expression field-expression)]{
Creates an @racket[ast-list-node?] containing a list of length @racket[length-expression].
For each element of the list, @racket[field-expression] is evaluated again.
}

@defproc[(node-type [n any/c]) any/c]{
Returns the symbol of the type of n, or #f if n is not a proper non-bud, non-list @racket[ast-node?].

Wrapper for @racket[ast-node-type] that returns false rather than erroring when it gets bud nodes or list nodes...
}

@defproc[(parent-node [n any/c]) any/c]{
Wrapper for ast-parent that returns #f rather than erroring when the given node doesn't have a parent.

TODO - this could probably go away.  I know there is somewhere I check if it returns false, but I should use RACR's @racket[ast-has-parent?] function instead.
}

@defproc[(top-ancestor-node [n any/c]) any/c]{
Calls @racket[parent-node] until it reaches the last parent, and returns it.
}

@defproc[(node-subtype? [n any/c]) any/c]{
Wrapper for @racket[ast-subtype?] that returns #f rather than erroring when the given node is a bud, list, or non-node.
}


@subsection{xsmith-options}

@defparam[xsmith-options options dict?]{
Parameter with the current xsmith options.

TODO - this could probably be private...
}

@defthing[xsmith-options-defaults dict?]{
Defaults for xsmith options.
}

@defproc[(xsmith-option [key any/c]) any/c]{
Gets the current option for the key, from @racket[xsmith-options].

TODO - this should probably follow the dict-ref interface and accept a default value/thunk.
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Cish}

Cish is a C program generator made with the Xsmith library.  It has co-evolved with Xsmith, and is essentially the reference Xsmith program generator.

When xsmith is installed as a Racket package, an @tt{xsmith-cish} executable is placed in your Racket package @tt{bin} directory (usually at @tt{$HOME/.racket/racket-<version>/bin}).  Additinally, Cish can be run with the command @tt{racket -l xsmith/cish --} (the final @tt{--} causes further flags to be parsed by cish and not by Racket).

To see command-line options, run Cish with the @tt{--help} flag.  The options are the same as with any program that uses @racket[xsmith-command-line].

Cish supports the following features for the @tt{--with} and @tt{--without} flags:

@itemlist[
  @item{
    These features are enabled by default:
    @itemlist[
    @item{@tt{null-statement}}
    @item{@tt{if-statement}}
    @item{@tt{if-expression}}
    @item{@tt{loop-statement}}
    @item{@tt{float}}
    @; Technically you can disable int, but then it can't make choices because main is hard-coded to be an int.
    @; This really ought to be changed -- we should generate a sub-main function to be called by main, which can have any return type.  Then we should have "main" always be the same (accepting arguments, calculating a checksum, printing something...) but calling the sub-main function with appropriate arguments.
    @;@item{@tt{int}}
    ]
  }
  @item{
    These features are disabled by default:
    @itemlist[
    @item{@tt{unsafe-math/range} -- Use a range analysis to convert safe math operations to bare unsafe math operations (when shown to be safe).}
    @item{@tt{unsafe-math/symbolic} -- Use a symbolic analysis to convert safe math operations to bare unsafe math operations (when shown to be safe).}
    ]
  }
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
