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
xsmith/scribblings/util
;; for cish features list
"../cish/cish-main.rkt"
xsmith/private/xsmith-version
racket/list
)

@title[#:tag "running-fuzzers"]{Xsmith's Bundled Generators (And How to Run Them)}

@author[(@author+email "William Hatch" "william@hatch.uno")
        (@author+email "Pierce Darragh" "pierce.darragh@gmail.com")
        (@author+email "Eric Eide" "eeide@cs.utah.edu")]

Version @xsmith-version-string

The @tt{xsmith-examples} package has some pre-built fuzzers that @emph{vary in their level of quality and/or utility as a learning aid}.

Included examples:

@itemlist[
@item{The @tt{simple} directory, with several fuzzers using the @tt{canned-components} library.  These are the best fuzzers to look at if you want to build a new fuzzer using canned-components (recommended).
  @itemlist[
    @item{simple/python}
    @item{simple/javascript}
    @item{simple/lua}
    @item{simple/racket}
  ]}
@item{The @tt{racket-kernel} fuzzer is another fuzzer built using the @tt{canned-components} library, but is a bigger fuzzer aimed at lower-level code.}
@item{The @tt{schemely} fuzzer, which is really a Racket fuzzer because I haven't yet bothered to turn it into a generic scheme fuzzer instead.  It is the best example of a fuzzer that does @emph{not} use the @tt{canned-components} library.  It is recommended that you @emph{do} use that library, but if you really don't want to, the best code to read is in @tt{schemely} and in the implementation of @tt{canned-components}.  However, everything that @tt{schemely} does, the @tt{simple/racket} fuzzer does as well, and simpler.}
@item{The @tt{cish} fuzzer, which is a C fuzzer.  @tt{cish} was the first fuzzer, and it is a little rougher than the others and less useful as a guide for what you ought to do with Xsmith.  That said, it does use more of the analysis and refinery machinery than other fuzzers.}

@item{The @tt{verilog} fuzzer is not very featureful, it hasn't yet had enough work to generate anything interesting.}
@item{The @tt{pythonesque} fuzzer was started before @tt{canned-components}, and has been superceded by @tt{simple/python}.}
@item{The @tt{future} directory has some experiments revolving around the design of the @tt{refiners} feature, and are not really for pedagogical consumption.}
]


When xsmith-examples is installed as a Racket package, executables for some of the bundled generators are placed in your Racket package @verb{bin} directory.
Usually this directory is @verb{$HOME/.racket/racket-<version>/bin} on Linux, maybe @verb{$HOME/Library/Racket/<version>/bin} on normal MacOS installs, and maybe @verb{/usr/local/bin} for MacOS Homebrew installs.

These fuzzers can be run on the command line to generate a single program or as an http server that generates one program per request.

Command-line options for bundled Xsmith generators are all the same, and provided by @racket[xsmith-command-line].


@section[#:tag "cish"]{Cish}

Cish is a C program generator made with the Xsmith library.  It has co-evolved with Xsmith, and is essentially the reference Xsmith program generator.

The executable for Cish is called @verb{xsmith-cish}.  Additionally, Cish can be run with the command @verb{racket -l xsmith/cish --} (the final @verb{--} causes further flags to be parsed by cish and not by Racket).

The command-line options available in Cish are:

@(command-line-options-table)

Cish supports the following features for the @verb{--with-<feature>} flags:

@(apply itemlist
(map
(λ (name default)
@item{@tt{@(symbol->string name)} -- defaults to @(format "~a" default)})
(map first cish-features-list)
(map second cish-features-list)))

To compile cish output, you need to include cish's directory and Csmith's runtime directory in your header path to get safe_math.h.

eg.

@verb{xsmith-cish > cish-output.c && gcc -I $XSMITH_EXAMPLES_DIR/cish -I $CSMITH_DIR/runtime -o cish-output cish-output.c}


@section[#:tag "schemely"]{Schemely}

The executable for Schemely is called @verb{xsmith-schemely}.  Additionally, Schemely can be run with the command @verb{racket -l xsmith/schemely --} (the final @verb{--} causes further flags to be parsed by cish and not by Racket).

The command-line options available in Schemely are:

@(command-line-options-table)

Schemely currently has no features for the @verb{--with-<feature>} flags.

At the time of writing, Schemely really just supports Racket.  At some future point it should generate portable Scheme code.

@section[#:tag "verilog"]{Verilog}

The program generator for Verilog is called @verb{xsmith-verilog}.  It can be
also run with the command @verb{racket -l xsmith/verilog --} (the final
@verb{--} causes further flags to be parsed by the Verilog generator and not by
Racket).

The command-line options for the Verilog generator are:

@(command-line-options-table)

The Verilog generator currently has no features for the @verb{--with-<feature>}
flags.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
