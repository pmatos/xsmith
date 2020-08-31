#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019-2020 The University of Utah
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

(require
 xsmith
 xsmith/app
 racr
 racket/pretty
 racket/string
 racket/port)

(define-spec-component arith)

(add-to-grammar
 arith
 [Definition #f (name type Expression)
   #:prop binder-info ()]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LetStar Expression ([definitions : Definition *]
                      [sideEs : Expression * = (random 2)]
                      Expression)
          #:prop strict-child-order? #t]
 [VariableReference Expression (name)
                    #:prop reference-info (read)]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50])


(add-property arith render-node-info
          [LetStar
           (λ (n)
             `(let* (,@(map (λ (d)
                              `[,(string->symbol (ast-child 'name d))
                                ,($xsmith_render-node
                                  (ast-child 'Expression d))])
                            (ast-children (ast-child 'definitions n))))
                ,@(map (λ (c) ($xsmith_render-node c))
                       (ast-children (ast-child 'sideEs n)))
                ,($xsmith_render-node (ast-child 'Expression n))))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,($xsmith_render-node
                                             (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) ($xsmith_render-node c))
                                      (ast-children (ast-child 'es n)))))])

(define extra-print-1-param (make-parameter #f))
(define extra-print-2-param (make-parameter #f))


(define-xsmith-interface-functions
  [arith]
  #:program-node LetStar
  #:comment-wrap (λ (lines)
                   (string-join
                    (map (λ (x) (format ";; ~a" x)) lines)
                    "\n"))
  #:format-render (λ (ast)
                    (and (extra-print-1-param)
                         (eprintf "extra-print-1: ~a\n" (extra-print-1-param)))
                    (and (extra-print-2-param)
                         (eprintf "extra-print-2: ~a\n" (extra-print-2-param)))
                    (with-output-to-string
                      (λ ()
                        (pretty-print ast (current-output-port) 1))))
  #:features ([test-feature #f])
  #:extra-parameters ([extra-print-1
                       "print an extra thing with no normalizer"
                       extra-print-1-param
                       #f]
                      [extra-print-2
                       "print an extra thing backwards"
                       extra-print-2-param
                       (λ (s) (apply string (reverse (string->list s))))])
  )

(module+ main
  (arith-command-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
