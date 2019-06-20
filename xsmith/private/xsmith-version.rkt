#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2019 The University of Utah
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

(provide xsmith-version-string)

(require
 racket/port
 racket/string
 racket/system
 pkg/lib
 setup/getinfo)

(define xsmith-info (get-info (list "xsmith")))

;;
;; Try to determine the short git commit hash of this version of Xsmith by
;; looking for the hash in the package info, i.e., the `info.rkt` file.
;;
;; If the sources for this Xsmith installation were made by `git archive`, then
;; `git archive` may have put the short hash there.  I expect it will be rare
;; to find source trees made by `git archive`, but in case we do, we should
;; believe what `git archive` tells us.
;;
(define (find-package-info-git-commit-string)
  (if xsmith-info
      (let [(info-rev (xsmith-info 'git-commit))]
        ;; Was the git commit hash inserted into `info.rkt` by `git archive`?
        ;; If so, the first character of `rev` will not be #\$.
        (if (not (char=? (string-ref info-rev 0) #\$))
            info-rev
            #f))
      #f))

;;
;; Try to determine the short git commit hash of this version of Xsmith by
;; looking for the hash associated with the installation of this package.
;;
;; According to "2.2 Package Sources" of "Package Management in Racket", for a
;; remote URL naming a git repository (or GitHub repository), "The package's
;; checksum is the hash identifying <rev> if <rev> is a branch or tag,
;; otherwise <rev> itself serves as the checksum."
;; https://docs.racket-lang.org/pkg/Package_Concepts.html#%28part._concept~3asource%29
;;
;; And for packages that come from a package catalog, "A package catalog is
;; consulted to determine the source and checksum for the package."
;;
;; The upshot of this is that, if users install Xsmith from a Racket package
;; catalog, and that catalog obtains Xsmith from a git repository, then the
;; package checksum is the (full version of the) git commit hash that we are
;; looking for.
;;
;; We distribute Xsmith via the standard Racket package catalog as described
;; above.  This is how I expect that most "regular users" will obtain Xsmith.
;;
(define (find-package-git-commit-string)
  (let ((xsmith-pkg-info
         (or (hash-ref (installed-pkg-table #:scope 'user)
                       "xsmith"
                       #f)
             (hash-ref (installed-pkg-table #:scope 'installation)
                       "xsmith"
                       #f))))
    (if (pkg-info? xsmith-pkg-info)
        (let ((checksum (pkg-info-checksum xsmith-pkg-info)))
          (if (string? checksum)
              (substring checksum 0 7)
              #f))
        #f)))

;;
;; Try to determine the short git commit hash of this version of Xsmith by
;; looking in the source directory of the package.
;;
;; If the source directory is part of a git repository's working tree, running
;; `git rev-parse` there will give us the git commit hash that we are looking
;; for.  If the source directory is not part of a git working tree, then
;; running `git rev-parse` there won't do anything useful, but neither will it
;; do any harm.
;;
;; This is how Xsmith developers would normally work with Xsmith, i.e.,
;; installing the package from a local directory that is part of a git
;; repository's working tree.
;;
(define (find-working-tree-git-commit-string)
  (let [(package-directory (pkg-directory "xsmith"))]
    (if package-directory
        (let* [(rc #f)
               (head-rev
                (string-trim
                 (parameterize [(current-directory package-directory)
                                (current-error-port (open-output-nowhere))]
                   (with-output-to-string
                     (lambda ()
                       (set! rc (system "git rev-parse --short HEAD")))))))]
          (if rc
              head-rev
              #f))
        #f)))

;;
;; The short hash of the git commit corresponding to this version of Xsmith.
;; If the hash cannot be determined, this is the string "unknown".
;;
(define xsmith-git-commit-string
  (or (find-package-info-git-commit-string)
      (find-package-git-commit-string)
      (find-working-tree-git-commit-string)
      "unknown"))

;;
;; The descriptive version string for Xsmith.
;;
(define xsmith-version-string
  (if xsmith-info
      (format "~a ~a (~a)"
              (xsmith-info 'name)
              (xsmith-info 'version)
              xsmith-git-commit-string)
      "unable to determine program version"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
