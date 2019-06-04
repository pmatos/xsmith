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
)

@title[#:tag "running-fuzzers"]{Xsmith's Bundled Generators (And How to Run Them)}

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

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
