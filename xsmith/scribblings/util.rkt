#lang at-exp racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require
 (only-in scribble/core make-element)
 scribble/base
 (for-syntax
  racket/base
  syntax/parse
  ))

(define (verb . content)
  (make-element 'tt content))


(define (racr)
  @seclink["racr"]{RACR})

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ name:id)
     #'@seclink["generated-rules" (symbol->string 'name)]]))


(define (command-line-options-table)
  @itemlist[
            @item{@verb{--help} -- see all command-line options.  The @verb{--help} list will automatically stay up to date, unlike this documentation.}
            @item{@verb{--server <true-or-false>} -- whether to run the web server.  Defaults to false.}
            @item{@verb{--server-port <port-number>} -- Port to use when running server.  Defaults to 8080.}
            @item{@verb{--server-ip <ip-address>} -- Listen for connections to <ip-address>.  Use @verb{false} to accept connections from all the machine's addresses.  Defaults to @verb{127.0.0.1}.}
            @item{@verb{--seed} -- Random seed for program generation.  Defaults to whatever Racket does to initialize randomness normally.}
            @item{@verb{--output-file <filename>} -- Outputs to <filename> instead of standard output when not used with @verb{--server}.}
            @item{@verb{--max-depth <n>} -- Maximum depth of the generated program tree.}
            @item{@verb{--version} -- prints the version info of xsmith and exits.}
            @; TODO - the version option should be able to print the xsmith version AND the version of a fuzzer made using xsmith.  There should be an argument for giving a program version.
            @item{@verb{--with-<feature> <bool>} -- enables or disables generator-specific features, where @verb{<feature>} is replaced with concrete feature names.}
            ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
