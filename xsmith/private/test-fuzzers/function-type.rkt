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
 xsmith/racr-convenience
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
 [Application Expression
              ([procedure : Expression]
               [argument : Expression])
              #:prop choice-weight 50]
 [Lambda Expression (FormalParam
                     [body : Expression])
         #:prop wont-over-deepen #t]
 [FormalParam #f (type [name = (fresh-var-name "arg-")])
              #:prop binder-info (#:binder-style parameter)]
 [LiteralInt Expression ([v = (random 100)])]
 [LiteralFloat Expression ([v = (* (random) (random 100))])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50])


(define number (base-type 'number #:leaf? #f))
(define int (base-type 'int number))
(define float (base-type 'float number))
(add-property arith type-info
          [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [LetStar [(fresh-type-variable)
                    (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                                   'sideEs (λ (cn) (fresh-type-variable))
                                   'Expression t))]]
          [Lambda [(function-type (fresh-type-variable) (fresh-type-variable))
                   (λ (n t)
                     (define arg-type (fresh-type-variable))
                     (define return-type (fresh-type-variable))
                     (unify! (function-type arg-type return-type)
                             t)
                     (hash 'FormalParam arg-type 'body return-type))]]
          [Application [(fresh-type-variable)
                        (λ (n t)
                          (define arg-type (fresh-type-variable))
                          (hash 'procedure (function-type arg-type t)
                                'argument arg-type))]]
          [FormalParam [(fresh-type-variable) (hash)]]
          [LiteralInt [int (λ (n t) (hash))]]
          [LiteralFloat [float (λ (n t) (hash))]]
          [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
          [SetBangRet [number (λ (n t) (hash 'Expression t))]]
          [Addition [number (λ (n t) (hash 'es t))]])

(add-property arith render-node-info
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
          [Lambda (λ (n) `(lambda (,(string->symbol
                                     (ast-child 'name (ast-child 'FormalParam n))))
                            ,(att-value 'xsmith_render-node (ast-child 'body n))))]
          [Application (λ (n) `(,(att-value 'xsmith_render-node (ast-child 'procedure n))
                                ,(att-value 'xsmith_render-node (ast-child 'argument n))))]
          [LiteralInt (λ (n) (ast-child 'v n))]
          [LiteralFloat (λ (n) (ast-child 'v n))]
          [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]
          [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                           ,(att-value 'xsmith_render-node
                                             (ast-child 'Expression n)))
                                     ,(string->symbol (ast-child 'name n))))]
          [Addition (λ (n) `(+ ,@(map (λ (c) (att-value 'xsmith_render-node c))
                                      (ast-children (ast-child 'es n)))))])

(add-property arith fresh
          [Lambda (let* ([type (att-value 'xsmith_type current-hole)]
                         [atype (fresh-type-variable)]
                         [rtype (fresh-type-variable)]
                         [ftype (function-type atype rtype)]
                         [unification-dumb-return-value (unify! ftype type)]
                         ;; this exploration should be unnecessary
                         ;[_ (force-type-exploration-for-node! current-hole)]
                         [FormalParam (make-fresh-node 'FormalParam
                                                       (hash 'type atype))])
                    (hash
                     'type type
                     'FormalParam FormalParam))]
          [Definition (hash 'name (if (equal? (top-ancestor-node current-hole)
                                              (parent-node current-hole))
                                      (fresh-var-name "global-")
                                      (fresh-var-name "local-"))
                            'type int)])


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
