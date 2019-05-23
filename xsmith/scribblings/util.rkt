#lang at-exp racket/base

(provide (all-defined-out))

(require
 (only-in scribble/core make-element)
 scribble/base
 )

(define (verb . content)
  (make-element 'tt content))


(define (racr)
  @seclink["racr"]{RACR})


(define (command-line-options-table)
  @itemlist[
            @item{@verb{--help} -- see all command-line options.  The @verb{--help} list will automatically stay up to date, unlike this documentation.}
            @item{@verb{--server <true-or-false>} -- whether to run the web server.  Defaults to false.}
            @item{@verb{--server-port <port-number>} -- Port to use when running server.  Defaults to 8080.}
            @item{@verb{--server-ip <ip-address>} -- Listen for connections to <ip-address>.  Use @verb{false} to accept connections from all the machine's addresses.  Defaults to @verb{127.0.0.1}.}
            @item{@verb{--seed} -- Random seed for program generation.  Defaults to whatever Racket does to initialize randomness normally.}
            @item{@verb{--output-file <filename>} -- Outputs to <filename> instead of standard output when not used with @verb{--server}.}
            @item{@verb{--max-depth <n>} -- Maximum depth of the generated program tree.}
            @item{@verb{--with <language-feature>} -- enables a fuzzer-dependent feature.  See the documentation specific to the fuzzer for a list of features.}
            @item{@verb{--without <language-feature>} -- disables a fuzzer-dependent feature.}
            @item{@verb{--version} -- prints the version info of xsmith and exits.}
            @; TODO - the version option should be able to print the xsmith version AND the version of a fuzzer made using xsmith.  There should be an argument for giving a program version.
            ])
