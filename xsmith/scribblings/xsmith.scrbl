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

@(require
"util.rkt"
)


@title{Xsmith}
@defmodule[xsmith]

@section{How to Install Xsmith}

First, install Racket.  If your operating system's package manager doesn't have a package or you want a fresher version, @hyperlink["https://download.racket-lang.org/"]{download it}.

Then run @verb{raco pkg install xsmith}.

TODO - this won't work until xsmith is publicly available and listed on Racket's package repository.


@include-section["guide.scrbl"]

@include-section["reference.scrbl"]

@include-section["generators.scrbl"]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
