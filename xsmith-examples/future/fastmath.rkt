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
 pprint
 )

(define-spec-component mathy-grammar)

(add-to-grammar
 mathy-grammar
 [Program #f ([maths : MathOp * = (add1 (random 15))])]
 [MathOp #f () #:prop may-be-generated #f]
 [BinaryMathOp MathOp ([l : Num]
                       [r : Num])
               #:prop may-be-generated #f]
 [Add BinaryMathOp ()]
 [Sub BinaryMathOp ()]
 [Mul BinaryMathOp ()]
 [Div BinaryMathOp ()]
 [FastAdd Add ()]
 [FastSub Sub ()]
 [FastMul Mul ()]
 [FastDiv Div ()]
 [Num #f ([v = (random 100)])]
 )

(define int (base-type 'int))
(define none (base-type 'none))
(add-property
 mathy-grammar
 type-info
 [Program [none (λ (n t) (hash 'maths (λ (c) (fresh-type-variable))))]]
 [BinaryMathOp [(fresh-type-variable) (λ (n t) (hash 'l t
                                                     'r t))]]
 [Num [int (λ (n t) (hash))]]
 )

(define (render-binary op n)
  (hs-append
   (render-node (ast-child 'l n))
   (text op)
   (render-node (ast-child 'r n))))

(add-property
 mathy-grammar
 render-node-info
 [Program (λ (n) (v-concat
                  (map render-node (ast-children (ast-child 'maths n)))))]
 [Add (λ (n) (render-binary "+" n))]
 [Sub (λ (n) (render-binary "-" n))]
 [Mul (λ (n) (render-binary "*" n))]
 [Div (λ (n) (render-binary "/" n))]
 [FastAdd (λ (n) (render-binary "%+"))]
 [FastSub (λ (n) (render-binary "%-"))]
 [FastMul (λ (n) (render-binary "%*"))]
 [FastDiv (λ (n) (render-binary "%/"))]
 [Num (λ (n) (text (number->string (ast-child 'v n))))]
 )

(define (could-be-fast? n)
  (...))

#|
- no separate predicate
- internal match
- in-place mutation
|#
(add-transformer
 mathy-grammar
 fast-math
 (λ (n)
   (when (could-be-fast? n)
     (rewrite-refine
      n
      (match (ast-node-type n)
        [Add FastAdd]
        [Sub FastSub]
        [Mul FastMul]
        [Div FastDiv])))))

#|
- uses predicate
- internal match
- in-place mutation
|#
(add-transformer
 mathy-grammar
 fast-math
 #:predicate could-be-fast?
 #:transformer (λ (n)
                 (rewrite-refine  ; XXX - should users have to use this function?
                  n
                  (match (ast-node-type n)
                    [Add FastAdd]
                    [Sub FastSub]
                    [Mul FastMul]
                    [Div FastDiv]))))

#|
- uses predicate
- internal match
- produces new nodes
|#
(add-transformer
 mathy-grammar
 fast-math
 #:predicate could-be-fast?
 #:transformer (λ (n)
                 (match (ast-node-type n)
                   [Add (mk-FastAdd n)]
                   [Sub (mk-FastSub n)]
                   [Mul (mk-FastMul n)]
                   [Div (mk-FastDiv n)])))

#|
- uses predicate
- top-level match
- produces new nodes
|#
(add-transformer
 mathy-grammar
 fast-math
 #:predicate could-be-fast?
 [Add (λ (n) (mk-FastAdd n))]
 [Sub (λ (n) (mk-FastSub n))]
 [Mul (λ (n) (mk-FastMul n))]
 [Div (λ (n) (mk-FastDiv n))]
 )

#|
- no separate predicate
- top-level match
- produces new nodes
|#
(add-transformer
 mathy-grammar
 fast-math
 [Add (λ (n) (and (could-be-fast? n)
                  (mk-FastAdd n)))]
 [Sub (λ (n) (and (could-be-fast? n)
                  (mk-FastSub n)))]
 [Mul (λ (n) (and (could-be-fast? n)
                  (mk-FastMul n)))]
 [Div (λ (n) (and (could-be-fast? n)
                  (mk-FastDiv n)))]
 )

#|
- no separate predicate
- internal match
- requires manually walking the tree
|#
(add-transformer
 mathy-grammar
 fast-math
 (λ (n)
   (if (could-be-fast? n)
       (match (ast-node-type n)
         [Add (rewrite-refine n FastAdd)]
         [Sub (rewrite-refine n FastSub)]
         [Mul (rewrite-refine n FastMul)]
         [Div (rewrite-refine n FastDiv)])
       (map (get-transformer fast-math)
            (ast-children n)))))

(add-property
 mathy-grammar
 render-hole-info
 [#f (λ (h) (h-append (text "<")
                      (text (symbol->string (ast-node-type h)))
                      (text ">")))])

(assemble-spec-components mathy mathy-grammar)

(xsmith-command-line
 (λ () (mathy-generate-ast 'Program))
 #:fuzzer-name "mathy"
 #:format-render (λ (d) (pretty-format d 120)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
