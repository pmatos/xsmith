#lang scribble/manual

@title{Xsmith}

TODO - there should be documentation about what so-far constitutes general infrastructure/library/language for fuzzer definition (IE what is xsmith itself) as well as documentation about Cish, a fuzzer implementation that serves as an example and a source of ideas to pull into xsmith the library.

@section{Overview}

TODO

@section{API Reference}

@subsection{grammar-macros.rkt}
@defthing[define-spec-component any/c]{TODO}
@defthing[assemble-spec-components any/c]{TODO}
@defthing[add-to-grammar any/c]{TODO}
@defthing[add-ag-rule any/c]{TODO}
@defthing[add-choice-rule any/c]{TODO}
@defthing[add-prop any/c]{TODO}
@defthing[current-xsmith-grammar any/c]{TODO}
@defthing[current-hole any/c]{TODO}
@defthing[make-hole any/c]{TODO}
@defthing[make-fresh-node any/c]{TODO}
@defthing[define-property any/c]{TODO}
@defthing[define-non-inheriting-rule-property any/c]{TODO}


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

@defthing[binding any/c]{TODO}
@defthing[current-well-formedness-regexp any/c]{TODO}

@defthing[current-path-greater-than any/c]{TODO}

@subsubsection{scope-graph.rkt -- other things provided but that maybe aren't part of the public interface}



@subsection{core-properties.rkt}

@defthing[may-be-generated any/c]{TODO}
@defthing[depth-increase-predicate any/c]{TODO}
@defthing[fresh any/c]{TODO}
@defthing[wont-over-deepen any/c]{TODO}
@defthing[introduces-scope any/c]{TODO}
@defthing[choice-filters-to-apply any/c]{TODO}

@subsection{xsmith-command-line.rkt}

@defthing[xsmith-command-line any/c]{TODO}


@subsection{xsmith-utils.rkt}
TODO
@subsection{xsmith-options.rkt}
TODO



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{What else should go here?}
Something about the implementation of cish.  Other stuff?
