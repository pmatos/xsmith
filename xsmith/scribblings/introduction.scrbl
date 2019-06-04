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

@title{Introduction}

Xsmith is a library for creating fuzz testers, also known as @italic{fuzzers},
for programming language compilers and interpreters.  In other words, Xsmith is
a library for creating @italic{random program generators}.

It comes bundled with some program generators created with the library.  If you just want to run them, see @secref["running-fuzzers"].

Xsmith implements a domain-specific language (DSL) for defining random program generators.
The Xsmith DSL is used to specify a language's grammar, typing rules, and other information which guides generation choices.
Xsmith also includes utilities for creating a command-line interface for generating a single program or starting a web server that generates one program per request.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
