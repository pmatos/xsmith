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

TODO - High level overview of Xsmith -- it has a DSL for specifying a grammar and auxiliary information to create a fuzzer for a language.

@subsection{RACR overview}
TODO - explain at a high-level and link to the RACR docs.

@subsection{Holes and Choice Objects}
TODO - explain what they are and how they work

@subsection{Minimal Example}
TODO - I think a very minimal arithmetic language definition could fit right here.

@section{API Reference}

@subsection{Auto-generated ag-rules and choice-rules}
TODO - give full list of them

@subsection{grammar-macros.rkt}
@defmodule[xsmith/grammar-macros]

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

Example:
@racketblock[
(assemble-spec-components
 my-spec
 #:properties (depth-increase-predicate fresh wont-over-deepen introduces-scope)
 my-spec-component
 my-other-spec-component
 )
(code:comment "Now `my-spec` is defined as a RACR spec,")
(code:comment "and `my-spec-generate-ast` is defined as a function which")
(code:comment "accepts a symbol argument representing the name of an AST node.")
]
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

Xsmith creates a choice object class for each node type in the specification grammar, following the same class hierarchy that AST nodes themselves do.  Choice objects are created every time Xsmith fills in a hole node.  One choice object is created for every node type that is legal to use in filling the hole.  Choice objects are then filtered according to the @racket[choice-filters-to-apply] property, and then the @tt{choice-weight} method of the remain choice objects is called to determine the probability of choosing each one.  When a choice object is selected, its @racket[fresh] method is called to generate a new AST node of its type.  If all choices are eliminated, an exception is raised with a message stating which filter step invalidated each potential choice.

Choice rules are methods on the choice objects.  Some choice rules are used by @racket[choice-filters-to-apply] to filter choices.  Other choice rules may be used by those filters or in the body of the @racket[fresh] rule as helper methods.  While most information about the AST and the current choice are probably computed using ag-rules, information about choosing a specific node type to fill in an abstract hole (such as an expression hole which may be filled with many different types of expressions) are computed using choice rules.

Choice rules are methods in Racket's class system and therefore have the @racket[this] macro available for use in their bodies to access other methods (eg. with the @racket[send] macro).
Choice rules also have the @racket[current-hole] macro available within their body so that they can query attributes of the RACR AST being elaborated (eg. with @tt{att-value} to access ag-rules and @tt{ast-parent} to inspect other nodes in the AST).

Since choice rules are methods in Racket's @racket[class] system, they must be defined with a literal @racket[lambda] (with no parameter for the implicit @racket[this] argument).  If a method needs to modify state (such as to cache the computation of available references of the appropriate type), I would normally recommend the “let-over-lambda” pattern, but that is not allowed in this case.  To make up for this, I recommend using @racket[make-weak-hasheq] to hold the state, using the @racket[this] object as a key.

Example:
@racketblock[
(add-choice-rule
 my-spec-component
 choice-weight
 [#f (λ () 10)]
 (code:line "Generate more AdditionExpressions")
 [AdditionExpression (λ () 20)]
 (code:line "Generate fewer SumExpressions")
 [SumExpression (λ () 5)])
]

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

Each dictionary maps nonterminal names (as symbols) to syntax objects.

The syntax object value for a property can be anything, since property transformers define the grammar and semantics of properties.

The syntax object value for ag-rules and choice-rules should be a syntax object specifying a function (IE a @racket[lambda]).  Ag-rules may be any syntax that evaluates to a function (so you may return an identifier that references a function or an expression that computes a function such as let-over-lambda), but choice-rule syntax is provided to Racket's @racket[class] macro, which requires literal @racket[lambda] forms.

The syntax object value for grammar productions when @tt{(grammar)} is read is... TODO.

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
TODO -- essentially the syntax class used for parsing @racket[add-to-grammar].  Basically it allows accessing the fields.

TODO - should this be in the public interface?  I think no, unless I allow properties to change the grammar.
}
@defform[#:kind "syntax class" #:id grammar-component grammar-component]{
TODO -- like @racket[grammar-clause] is used by @racket[add-to-grammar], @racket[grammar-component] is used by @racket[grammar-clause].

TODO - the name should be changed to grammar-field or grammar-clause-field or something.

TODO - should this be in the public interface?  I think no, unless I allow properties to change the grammar.
}

@defthing[grammar-clause->parent-chain any/c]{
TODO - I think I added this to the docs and the public interface accidentally.  It shouldn't be here...  Perhaps a similar function should be, but probably not this one.

In general I should provide better ways to inspect the grammar in property transformers, but I haven't needed them yet...
}

@defproc[(grammar-node-name->field-info [name symbol?] [grammar-clause-hash any/c])
         (listof/c grammar-node-field-struct?)]{
This function should be called with a node type name and the grammar object given as an argument to a property transformer that reads the grammar.

It returns a list of structs containing information about the node's fields (including fields inherited from super node types).
}

@defstruct[grammar-node-field-struct
           ([name symbol?]
            [type (or/c symbol? #f)]
            [kleene-star? boolean?]
            [init-expr syntax?])
           #:omit-constructor]{
The struct type in the list returned by @racket[grammar-node-name->field-info].
}


@subsubsection{grammar-macros.rkt explanation of private functions?}


@subsection{scope-graph.rkt}
@defmodule[xsmith/scope-graph]

@defstruct[binding ([name string?] [bound any/c]) #:omit-constructor]{
Struct for binding information of nodes that create bindings.

TODO - example, better explanation...
}

@defparam[current-well-formedness-regexp r regexp?
          #:value #px"rp*i?d"]{
TODO
}

@defparam[current-path-greater-than comparator
         (-> (listof/c (or/c 'reference 'parent 'import 'declaration))
             (listof/c (or/c 'reference 'parent 'import 'declaration))
             any/c)]{
TODO
}

@subsubsection{scope-graph.rkt -- other things provided but that maybe aren't part of the public interface}



@subsection{core-properties.rkt}
@defmodule[xsmith/core-properties]

@defform[#:kind "spec-property" #:id may-be-generated may-be-generated]{
This property defines the @tt{may-be-generated-method} non-inheriting choice-rule.
Acceptable values for this property are @racket[#t] or @racket[#f], and the default is @racket[#t].

TODO - the choice-rule name really ought to be invisible to the user.

If may-be-generated is false, the node is not added to the list of possibile choices to replace an appropriate AST hole.
It is useful to set it to false for abstract node types or for specialized node types that are meant to be swapped in only after a full tree is generated, such as by a later analysis to determine validity of an unsafe operation.

Example:
@racketblock[
(add-prop
 my-spec-component
 may-be-generated
 [MyAbstractNode #f]
 [UnsafeAddition #f])
]
}

@defform[#:kind "spec-property" #:id depth-increase-predicate depth-increase-predicate]{
This property defines the @tt{ast-depth} non-inheriting ag-rule.

The property accepts an expression which much evaluate to a function of one argument (the RACR AST node) which returns a truthy value for nodes which increase the depth of the AST and #f otherwise.  The default is @racket[(λ (n) #t)].

This is useful to allow node re-use.  For example, the body of an @tt{if} or @tt{for} statement might be a block and have the same semantics, but you might want a block inside an @tt{if} to only be considered a depth increase of 1, not 2.

Example:
@racketblock[
(define no-depth-if-body-is-block
  (λ (n) (node-subtype? (ast-child 'body n) 'Block)))
(add-prop
 my-spec-component
 depth-increase-predicate
 [IfStatement no-depth-if-body-is-block]
 [ForStatement no-depth-if-body-is-block])
]

TODO -- this should probably return a number to add rather than #t or #f -- then some nodes could increase the counted depth faster than normal or even decrease the counted depth (which probably wouldn't be useful?).
}

@defform[#:kind "spec-property" #:id fresh fresh]{
This property defines the @tt{fresh} choice-rule.

Acceptable values for this property are expressions which produce a @racket[dict?] object.  Keys of the dictionary must be field names of the node being generated.  The values in the dictionary are used to fill node fields of the appropriate name.  Any field whose name is not in the dictionary will be filled by evaluating the default init-expr defined in the grammar (via @racket[add-to-grammar]).

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
}

@defform[#:kind "spec-property" #:id wont-over-deepen wont-over-deepen]{
This rule defines the @tt{wont-over-deepen} choice-rule.

The default for this property is probably what you want, so probably just be sure to add this to the extra #:properties flag of @racket[assemble-part-specs].

TODO - this property should probably be in a base set that is just always run be default.  If it were possible to make hygienic method names it would be good to make the choice name invisible as well...

But if you want to set it:

The property accepts expressions which will evaluate to booleans (IE anything but only #f is false...), which are evaluated if the choice is made at the point where the AST is at it maximum depth.  A true value means that the choice is acceptable, false otherwise.  The default is computed by checking whether a node includes AST-node-typed fields.  If it does not it is considered atomic and therefore acceptable to choose when the AST is already at its maximum depth.
}

@defform[#:kind "spec-property" #:id introduces-scope introduces-scope]{
This property is used to determine whether a node introduces a scope where new variable bindings may occur.

This property defines the @tt{scope-graph-introduces-scope?} ag-rule, the @tt{scope-graph-scope} ag-rule, and the @tt{scope-graph-descendant-bindings} ag-rule.  TODO - really all of these methods ought to be invisible to the user.

This property accepts literal boolean values @racket[#t] and @racket[#f].  The default is @racket[#f].

Examlpe:
@racketblock[
(add-prop
 my-spec-component
 introduces-scope
 [Program #t]
 [Block #t])
]

}

@defform[#:kind "spec-property" #:id choice-filters-to-apply choice-filters-to-apply]{
This property defines the @tt{apply-choice-filters} choice-rule.  TODO - this generated method should also be invisible to the user.

This property accepts a syntax list of choice-rule names to use as a filter for the node type.  Generally this should be set on the greatest super node type (or @racket[#f] if there is no explicit super node type in your grammar).  Each choice-rule in the list is called on the choice object with no arguments.  Each rule that returns @racket[#f] rules the node out as a choice for filling in a hole.

Example:
@racketblock[
(add-prop
 my-spec-component
 choice-filters-to-apply
 [#f (may-be-generated-method
      features-enabled
      wont-over-deepen
      respect-return-position
      misc-constraints
      constrain-type
      )])
]

TODO - some of the core methods (which ought to be invisible to the user...) should always be included in the list, and the given list should just extend the list of filters.
}


@subsection{xsmith-command-line.rkt}
@defmodule[xsmith/xsmith-command-line]

@defproc[(xsmith-command-line [generate-and-print-func (-> any/c)]) any/c]{
This function parses the current command-line arguments for xsmith fuzzers.  It is basically to be used in the main function of a fuzzer.
Based on options supplied, it may print a help message and terminate the program, generate a single program, or start a web server to generate many programs.

@racket[generate-and-print-func] must be a function that generates and prints a single program.  It is called within @racket[xsmith-command-line] with the random seed parameterized according to command-line options (and for the web server reset and incremented for each call), and with all xsmith-options parameterized according to the command line.
}


@subsection{xsmith-utils.rkt}
TODO
@subsection{xsmith-options.rkt}
TODO



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{What else should go here?}
Something about the implementation of cish.  Other stuff?
