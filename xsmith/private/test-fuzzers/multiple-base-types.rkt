#lang clotho/racket/base
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
 racket/pretty
 racket/string
 racket/port)

(define-spec-component arith)

(add-to-grammar
 arith
 [Definition #f (name type Expression)
   #:prop binder-info (name type definition)]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LetStar Expression ([definitions : Definition *]
                      [sideEs : Expression * = (random 2)]
                      Expression)
          #:prop strict-child-order? #t]
 [VariableReference Expression (name)
                    #:prop reference-info (read name)]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write name)]
 [LiteralInt Expression ([v = (random 100)])]
 [LiteralString Expression ([v = (random-ref '("foo" "bar" "baz"))])]
 [LiteralBool Expression ([v = (random-ref '(#t #f))])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50]
 [And Expression ([es : Expression * = (+ 1 (random 5))])]
 [StringAppend Expression ([es : Expression * = (+ 1 (random 5))])]
 [If Expression ([test : Expression] [then : Expression] [else : Expression])]
 [StringLength Expression ([Expression])]

 )


(define int (base-type 'int))
(define string (base-type 'string))
(define bool (base-type 'bool))
(add-prop arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [LiteralInt [int (λ (n t) (hash))]]
          [LiteralBool [bool (λ (n t) (hash))]]
          [LiteralString [string (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [Addition [int (λ (n t) (hash 'es t))]]
          [And [bool (λ (n t) (hash 'es t))]]
          [StringAppend [string (λ (n t) (hash 'es t))]]
          [If [(fresh-type-variable) (λ (n t) (hash 'test bool
                                                    'then t
                                                    'else t))]]
          [StringLength [int (λ (n t) (hash 'Expression string))]]
          )

(add-prop arith render-node-info
          [LetStar
           (λ (n)
             `(let* (,@(map (λ (d)
                              `[,(string->symbol (ast-child 'name d))
                                ,(att-value 'xsmith_render-node
                                            (ast-child 'Expression d))])
                            (ast-children (ast-child 'definitions n))))
                ,@(map (λ (c) (att-value 'xsmith_render-node c))
                       (ast-children (ast-child 'sideEs n)))
                ,(att-value 'xsmith_render-node (ast-child 'Expression n))))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [LiteralBool (λ (n) (ast-child 'v n))]
          [LiteralString (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,(att-value 'xsmith_render-node
                                                       (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) (att-value 'xsmith_render-node c))
                                      (ast-children (ast-child 'es n)))))]
          [And (λ (n) `(and ,@(map (λ (c) (att-value 'xsmith_render-node c))
                                   (ast-children (ast-child 'es n)))))]
          [StringAppend (λ (n) `(string-append
                                 ,@(map (λ (c) (att-value 'xsmith_render-node c))
                                        (ast-children (ast-child 'es n)))))]
          [StringLength (λ (n) `(string-length ,(att-value 'xsmith_render-node
                                                           (ast-child 'Expression n))))]
          [If (λ (n) `(if ,(att-value 'xsmith_render-node (ast-child 'test n))
                          ,(att-value 'xsmith_render-node (ast-child 'then n))
                          ,(att-value 'xsmith_render-node (ast-child 'else n))))]
          )


(assemble-spec-components arithmetic arith)

(define (arithmetic-generate)
  (parameterize ([current-xsmith-type-constructor-thunks (list (λ () int))])
    (arithmetic-generate-ast 'LetStar)))

(xsmith-command-line
 arithmetic-generate
 #:comment-wrap (λ (lines)
                  (string-join
                   (map (λ (x) (format ";; ~a" x)) lines)
                   "\n"))
 #:format-render (λ (ast)
                  (with-output-to-string
                    (λ ()
                      (pretty-print ast (current-output-port) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
