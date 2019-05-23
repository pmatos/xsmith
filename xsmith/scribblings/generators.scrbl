#lang scribble/manual

@(require
"util.rkt"
)

@title[#:tag "running-fuzzers"]{Xsmith's bundled generators (and how to run them)}

When xsmith is installed as a Racket package, executables for the bundled generators are placed in your Racket package @verb{bin} directory.
Usually this directory is @verb{$HOME/.racket/racket-<version>/bin} on Linux, maybe @verb{$HOME/Library/Racket/<version>/bin} on normal MacOS installs, and maybe @verb{/usr/local/bin} for MacOS Homebrew installs.

These fuzzers can be run on the command line to generate a single program or as an http server that generates one program per request.

Command-line options for bundled Xsmith generators are all the same, and provided by @racket[xsmith-command-line].


@section[#:tag "cish"]{Cish}

Cish is a C program generator made with the Xsmith library.  It has co-evolved with Xsmith, and is essentially the reference Xsmith program generator.

The executable for Cish is called @verb{xsmith-cish}.  Additionally, Cish can be run with the command @verb{racket -l xsmith/cish --} (the final @verb{--} causes further flags to be parsed by cish and not by Racket).

The command-line options available in Cish are:

@(command-line-options-table)

Cish supports the following features for the @verb{--with} and @verb{--without} flags:

@itemlist[
  @item{
    These features are enabled by default:
    @itemlist[
    @item{@verb{null-statement}}
    @item{@verb{if-statement}}
    @item{@verb{if-expression}}
    @item{@verb{loop-statement}}
    @item{@verb{float}}
    @; Technically you can disable int, but then it can't make choices because main is hard-coded to be an int.
    @; This really ought to be changed -- we should generate a sub-main function to be called by main, which can have any return type.  Then we should have "main" always be the same (accepting arguments, calculating a checksum, printing something...) but calling the sub-main function with appropriate arguments.
    @;@item{@verb{int}}
    ]
  }
  @item{
    These features are disabled by default:
    @itemlist[
    @item{@verb{unsafe-math/range} -- Use a range analysis to convert safe math operations to bare unsafe math operations (when shown to be safe).}
    @item{@verb{unsafe-math/symbolic} -- Use a symbolic analysis to convert safe math operations to bare unsafe math operations (when shown to be safe).}
    ]
  }
]

To compile cish output, you need to include Csmith's runtime directory in your header path to get safe_math.h.

eg.

@verb{xsmith-cish > cish-output.c && gcc -I $CSMITH_DIR/runtime -o cish-output cish-output.c}


@section[#:tag "schemely"]{Schemely}

The executable for Schemely is called @verb{xsmith-schemely}.  Additionally, Schemely can be run with the command @verb{racket -l xsmith/schemely --} (the final @verb{--} causes further flags to be parsed by cish and not by Racket).

The command-line options available in Schemely are:

@(command-line-options-table)

Schemely currently has no features for the @verb{--with} or @verb{--without} flags.

TODO - at the time of writing, Schemely really just supports Racket.  At some future point it should generate portable Scheme code.
