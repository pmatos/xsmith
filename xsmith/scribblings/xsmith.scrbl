#lang scribble/manual

@(require (for-label
(except-in racket/base
           module)
xsmith/grammar-macros
xsmith/core-properties
xsmith/scope-graph
xsmith/xsmith-command-line
racket/class

@; if racr had scribble documentation this would provide hyperlinks
@;racr
))

@title{Xsmith}

TODO - there should be documentation about what so-far constitutes general infrastructure/library/language for fuzzer definition (IE what is xsmith itself) as well as documentation about Cish, a fuzzer implementation that serves as an example and a source of ideas to pull into xsmith the library.

@section{Overview}

TODO

@section{API Reference}

@subsection{grammar-macros.rkt}
@defmodule[xsmith/grammar-macros]

@defform[(define-spec-component component-name)]{
Defines a spec component.  Spec components include information about a language grammar and attributes, and can be combined to generate an xsmith fuzzer.  You add grammar productions with @racket[add-to-grammar], you add properties with @racket[add-prop], and you can add ag-rules and choice-rules with @racket[add-ag-rule] and @racket[add-choice-rule], respectively.  Spec components are combined with @racket[assemble-spec-components].

TODO - example.
}

@defform[(assemble-spec-components spec-name maybe-properties spec-component ...)
#:grammar [(maybe-properties (code:line)
                             (code:line #:properties list-of-properties))]
          @;[list-of-properties (property ...)]
          ]{

Combines spec components and generates a RACR specification.

Defines @racket[spec-name] as a RACR specification.

Defines @tt{<spec-name>-generate-ast} as a function.  The function accepts the name of a grammar production as a symbol and produces a random tree starting from a fresh node of that nonterminal.  Essentially, given the name of the top-level program node, this function generates a random program.

Within the RACR spec, the following ag-rules are automatically defined:
@itemlist[
@item{@racket['hole->choice-list], which does TODO}
@item{@racket['is-hole?], which does TODO}
@item{@racket['hole->replacement], which does TODO}
@item{@racket['find-descendants], which does TODO}
@item{@racket['find-a-descendant], which does TODO}
@item{@racket['resolve-reference-name], which does TODO}
@item{@racket['visible-bindings], which does TODO}
]

Properties (defined with @racket[define-property]) are used to derive more RACR ag-rules as well as Xsmith choice-rules.
Each property may have a transformer function that alters other properties, ag-rules, or choice-rules.
All properties referenced within a spec-component are used to generate ag-rules and choice-rules, as well as any properties specified in the @racket[maybe-properties] list.
Unless values for that property have also been specified within a spec component, properties in the @racket[maybe-properties] list will only be able to generate rules based on the default value for the property.

TODO - example.
}

@defform[(add-to-grammar spec-component grammar-clause ...)
#:grammar [(grammar-clause (node-name parent-name (field ...)))
           (parent-name identifier #f)
           (field name/type-id
                  (name/type-id maybe-type-id maybe-kleene-star maybe-init-expr))
           (maybe-type-id (code:line)
                          (code:line : type-name))
           (maybe-kleene-star (code:line) *)
           (maybe-init-expr (code:line) (code:line = init-expr))]]{
TODO - note that node-name is limited (due to RACR)
aoeu

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

TODO - explain ag-rules vs choice-rules, etc.

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

TODO - explain ag-rules vs choice-rules, etc.

TODO - example, although it looks identical to @racket[add-ag-rule].
}

@defform[(add-prop spec-component prop-name prop-clause ...)
#:grammar [(prop-clause (nonterminal-name prop-value))]]{
Adds property values to the spec-component.

Since property transformers are macros that may accept arbitrary domain-specific syntax, the grammar of prop-value varies for each property.
}

@defform[(current-xsmith-grammar)]{
In code within the context of a spec component (eg. in the body of @racket[add-ag-rule], @racket[add-prop], @racket[add-to-grammar], etc), @racket[(current-xsmith-grammar)] returns the RACR spec object for the grammar ultimately combined by @racket[assemble-spec-components].

Elsewhere it raises a syntax error.

TODO - it needs a better name.  Probably @tt{current-racr-spec}.
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
Within the context of a spec component (eg. in the body of @racket[add-ag-rule], @racket[add-prop], @racket[add-to-grammar], etc), @racket[make-fresh-node] is a function to generate a fresh node of the given type.

For example, to generate a fresh @tt{AdditionExpression} node, specifying values for some of its fields:
@racketblock[(make-fresh-node 'AdditionExpression
                               (hash 'left (make-fresh-node 'LiteralInt
                                                            (hash 'v 5))))]
}

@defform[(define-property name maybe-dups maybe-reads maybe-rewrites maybe-appends maybe-transformer)
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

Each dictionary maps nonterminal names (as symbols) to syntax objects.

The syntax object value for a property can be anything, since property transformers define the grammar and semantics of properties.

The syntax object value for ag-rules and choice-rules should be a syntax object specifying a function (IE a @racket[lambda]).  Ag-rules may be any syntax that evaluates to a function (so you may return an identifier that references a function or an expression that computes a function such as let-over-lambda), but choice-rule syntax is provided to Racket's @racket[class] macro, which requires literal @racket[lambda] forms.

The syntax object value for grammar productions when @tt{(grammar)} is read is... TODO.

Dictionaries may or may not contain an entry for each nonterminal in the grammar (except the grammar dictionary which always contains all nonterminals).  A dictionary may even be empty.

In addition to nonterminals, each dictionary may include a mapping for the value @racket[#f], which will define a default value used for the (super secret) parent node that @racket[assemble-spec-components] defines.  If nothing is specified for #f, ag-rules and choice-rules will have a default which errors, providing a helpful error message.

TODO - a real example.  Maybe something from core-properties.rkt, or something simplified.
}

@defform[(define-non-inheriting-rule-property property-name rule-type maybe-rule-name default-value maybe-transformer)
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
@defthing[grammar-component any/c]{TODO}
@defthing[grammar-clause any/c]{TODO}
@defthing[grammar-clause->parent-chain any/c]{TODO}
@defthing[grammar-node-name->field-info any/c]{TODO}
@defthing[grammar-node-field-struct any/c]{TODO}
@defthing[grammar-node-field-struct-name any/c]{TODO}
@defthing[grammar-node-field-struct-type any/c]{TODO}
@defthing[grammar-node-field-struct-kleene-star? any/c]{TODO}
@defthing[grammar-node-field-struct-init-expr any/c]{TODO}

@subsubsection{grammar-macros.rkt explanation of private functions?}


@subsection{scope-graph.rkt}
@defmodule[xsmith/scope-graph]

@defthing[binding any/c]{TODO}
@defthing[current-well-formedness-regexp any/c]{TODO}

@defthing[current-path-greater-than any/c]{TODO}

@subsubsection{scope-graph.rkt -- other things provided but that maybe aren't part of the public interface}



@subsection{core-properties.rkt}
@defmodule[xsmith/core-properties]

@defthing[may-be-generated any/c]{TODO}
@defthing[depth-increase-predicate any/c]{TODO}
@defthing[fresh any/c]{TODO}
@defthing[wont-over-deepen any/c]{TODO}
@defthing[introduces-scope any/c]{TODO}
@defthing[choice-filters-to-apply any/c]{TODO}

@subsection{xsmith-command-line.rkt}
@defmodule[xsmith/xsmith-command-line]

@defthing[xsmith-command-line any/c]{TODO}


@subsection{xsmith-utils.rkt}
TODO
@subsection{xsmith-options.rkt}
TODO



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{What else should go here?}
Something about the implementation of cish.  Other stuff?
