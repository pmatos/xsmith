#lang clotho
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

(require
 xsmith
 racr
 pprint)

(define-spec-component btree-grammar)

(add-to-grammar
 btree-grammar
 [Node #f ()
       #:prop may-be-generated #f]
 [Branch Node ([l : Node]
               [r : Node]
               Val)]
 [Leaf Node (Val)
       #:prop wont-over-deepen #t]
 [Val #f ([v = (random 100)])]
 )

(define int (base-type 'int))

(add-property
 btree-grammar
 type-info
 [Branch [int
          (λ (n t)
            (hash 'l t
                  'r t
                  'Val int))]]
 [Leaf [int
        (λ (n t)
          (hash 'Val t))]]
 [Val [int (λ (n t) (hash))]]
 )

(add-property
 btree-grammar
 render-node-info
 [Branch (λ (n)
           (v-append
            (text "(branch")
            (indent 2
                    (v-append
                     (render-node (ast-child 'Val n))
                     (render-node (ast-child 'l n))
                     (h-append
                      (render-node (ast-child 'r n))
                      (text ")"))))))]
 [Leaf (λ (n)
         (h-append
          (text "(leaf ")
          (render-node (ast-child 'Val n))
          (text ")")))]
 [Val (λ (n)
        (text (number->string (ast-child 'v n))))]
 )

(add-property
 btree-grammar
 render-hole-info
 [#f (λ (h) (h-append
             (text "<")
             (text (symbol->string (ast-node-type h)))
             (text ">")))])

(assemble-spec-components btree btree-grammar)

(define (btree-generate)
  (btree-generate-ast 'Branch))

(xsmith-command-line
 btree-generate
 #:fuzzer-name "btree"
 #:default-max-depth 20
 #:format-render (λ (d) (pretty-format d 120)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
