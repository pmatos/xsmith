#!/usr/bin/env racket
#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2020 The University of Utah
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
 xsmith/racr-convenience
 pprint
 ; racket/pretty
 ; racket/random
 ; racket/list
 ; racket/class
 racket/port
 racket/string
 )

(define max-modules
  (make-parameter 5))

(define max-module-items
  (make-parameter 5))

(define max-block-statements
  (make-parameter 5))

(define indent-spaces
  (make-parameter 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-spec-component verilog-core)

;;
;;
;;
(add-to-grammar
 verilog-core
 [Source
  #f
  ([modules : ModuleDecl * = (random 1 (max-modules))])]
 [ModuleDecl
  #f
  ([name = (fresh-module-name)]
   [items : ModuleItem * = (random 1 (max-module-items))])]

 [ModuleItem
  #f
  ()
  #:prop may-be-generated #f]
 [InitialConstruct
  ModuleItem
  ([stmt : Statement])]
 [AlwaysConstruct
  ModuleItem
  ([stmt : Statement])]
 [Declaration
  ModuleItem
  ([dir : MaybeDirection]
   [net : MaybeNet]
   [signed : MaybeSigned]
   [name = (fresh-id)]
   [init : MaybeConstExpression])]

 ;;;;;

 ;; "Maybe pattern" instances

 [MaybeDirection	#f		() #:prop may-be-generated #f]
 [JustDirection		MaybeDirection	([dir : Direction])]
 [NothingDirection	MaybeDirection	()]

 [MaybeNet		#f		() #:prop may-be-generated #f]
 [JustNet		MaybeNet	([net : Net])]
 [NothingNet		MaybeNet	()]

 [MaybeSigned		#f		() #:prop may-be-generated #f]
 [JustSigned		MaybeSigned	([signed : Signed])]
 [NothingSigned		MaybeSigned	()]

 [MaybeConstExpression	#f		() #:prop may-be-generated #f]
 [JustConstExpression	MaybeConstExpression	([cexpr : ConstExpression])]
 [NothingConstExpression	MaybeConstExpression	()]

 ;;;;;

 ;; "One-of-keyword pattern" instances

 [Direction		#f		() #:prop may-be-generated #f]
 [InputDirection	Direction	()]
 [OutputDirection	Direction	()]
 [InOutDirection	Direction	()]

 [Net			#f		() #:prop may-be-generated #f]
 [RegNet		Net		()]
 [WireNet		Net		()]

 [Signed		#f		() #:prop may-be-generated #f]
 [UnsignedSigned	Signed		()]
 [SignedSigned		Signed		()]

 ;;;;;

 [Statement
  #f
  ()
  #:prop may-be-generated #f]
 [BlockStatement
  Statement
  ([stmts : Statement * = (random 1 (max-block-statements))])]
 [ArrowStatement
  Statement
  ([lhs = (fresh-lhs)]
   [rhs : Expression])]
 [EqualStatement
  Statement
  ([lhs = (fresh-lhs)]
   [rhs : Expression])]
 [IfStatement
  Statement
  ([cond : Expression]
   [then : MaybeStatement]
   [else : MaybeStatement])]

 [MaybeStatement	#f		() #:prop may-be-generated #f]
 [JustStatement		MaybeStatement  ([stmt : Statement])]
 [NothingStatement 	MaybeStatement  ()]

 [Expression
  #f
  ()
  #:prop may-be-generated #f]
 [NumberExpression
  Expression
  ([v = (random 0 10)])]

 [ConstExpression
  #f
  ()
  #:prop may-be-generated #f]
 [NumberConstExpression
  ConstExpression
  ([v = (random 0 10)])]
 )

;;
;;
;;
(add-prop
 verilog-core
 wont-over-deepen
 [ArrowStatement #t]
 [EqualStatement #t]
 )


;;
;;
;;
(define kw-always	(text "always"))
(define kw-arrow	(text "<="))
(define kw-begin	(text "begin"))
(define kw-else		(text "else"))
(define kw-end		(text "end"))
(define kw-endmodule	(text "endmodule"))
(define kw-equal	(text "="))
(define kw-if		(text "if"))
(define kw-initial	(text "initial"))
(define kw-inout	(text "inout"))
(define kw-input	(text "input"))
(define kw-module	(text "module"))
(define kw-output	(text "output"))
(define kw-reg		(text "reg"))
(define kw-signed	(text "signed"))
(define kw-unsigned	(text "unsigned"))
(define kw-wire		(text "wire"))

(add-prop
 verilog-core
 render-node-info
 [Source
  (λ (n)
    (v-concat
     (apply-infix line
                  (map render-node
                       (ast-children (ast-child 'modules n))))))]
 [ModuleDecl
  (λ (n)
    (v-append
     (h-append kw-module space (text (ast-child 'name n)) semi)
     (indent (indent-spaces)
             (v-concat
              (map render-node
                   (ast-children (ast-child 'items n)))))
     kw-endmodule))]

 [InitialConstruct
  (λ (n)
    (h-append kw-initial space (render-node (ast-child 'stmt n))))]
 [AlwaysConstruct
  (λ (n)
    (h-append kw-always space (render-node (ast-child 'stmt n))))]
 [Declaration
  (λ (n)
    (let* ([init-doc (render-node (ast-child 'init n))]
           [eq-doc (if (eq? init-doc empty)
                       empty
                       kw-equal)])
      (h-append
       (hs-concat (filter
                   (λ (d) (not (eq? d empty)))
                   (list
                    (render-node (ast-child 'dir n))
                    (render-node (ast-child 'net n))
                    (render-node (ast-child 'signed n))
                    (text (ast-child 'name n))
                    eq-doc
                    init-doc)))
       semi)))]

 [BlockStatement
  (λ (n)
    (v-append
     kw-begin
     (indent (indent-spaces)
             (v-concat
              (map render-node
                   (ast-children (ast-child 'stmts n)))))
     kw-end))]
 [ArrowStatement
  (λ (n)
    (h-append (text (ast-child 'lhs n))
              space kw-arrow space
              (render-node (ast-child 'rhs n))
              semi))]
 [EqualStatement
  (λ (n)
    (h-append (text (ast-child 'lhs n))
              space kw-equal space
              (render-node (ast-child 'rhs n))
              semi))]
 [IfStatement
  (λ (n)
    (v-append
     (h-append kw-if space lparen
               (render-node (ast-child 'cond n))
               rparen)
     (indent (indent-spaces) (render-node (ast-child 'then n)))
     kw-else
     (indent (indent-spaces) (render-node (ast-child 'else n)))))]
 [JustStatement
  (λ (n)
    (render-node (ast-child 'stmt n)))]
 [NothingStatement
  (λ (n)
    semi)]

 [NumberExpression
  (λ (n)
    (text (number->string (ast-child 'v n))))]

 [NumberConstExpression
  (λ (n)
    (text (number->string (ast-child 'v n))))]

 ;; "Maybe pattern" instances

 [JustDirection		(λ (n) (render-node (ast-child 'dir n)))]
 [NothingDirection	(λ (n) empty)]

 [JustNet		(λ (n) (render-node (ast-child 'net n)))]
 [NothingNet		(λ (n) empty)]

 [JustSigned		(λ (n) (render-node (ast-child 'signed n)))]
 [NothingSigned		(λ (n) empty)]

 [JustConstExpression		(λ (n) (render-node (ast-child 'cexpr n)))]
 [NothingConstExpression	(λ (n) empty)]

 ;; "One-of-keyword pattern" instances

 [InputDirection	(λ (n) kw-input)]
 [OutputDirection	(λ (n) kw-output)]
 [InOutDirection	(λ (n) kw-inout)]

 [RegNet		(λ (n) kw-reg)]
 [WireNet		(λ (n) kw-wire)]

 [UnsignedSigned	(λ (n) kw-unsigned)]
 [SignedSigned		(λ (n) kw-signed)]
 )

;;
;;
;;
(assemble-spec-components
 verilog
 #:properties (wont-over-deepen)
 verilog-core)

;;
;;
;;
(define verilog-module-type (base-type 'module))

(define (type-thunks-for-concretization)
  (list (λ () verilog-module-type)))

(define (fresh-module-name)
  (fresh-var-name "module"))

(define (fresh-lhs)
  (fresh-var-name "v"))

(define (fresh-id)
  (fresh-var-name "id"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;
;;
(define (verilog-generate)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (verilog-generate-ast 'Source)))

;;
;;
;;
(define (verilog-format-render form)
  (with-output-to-string
    (λ ()
      (define (pp x)
        (pretty-print x (current-output-port) 1))
      (pp form))))

;;
;;
;;
(define (verilog-comment-wrap strings)
  (string-join strings "\n// " #:before-first "// "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (xsmith-command-line
   verilog-generate
   #:fuzzer-name	"verilog"
   #:fuzzer-version	"0.0"
   #:comment-wrap	verilog-comment-wrap
   #:format-render	verilog-format-render
   #:extra-parameters
   (list
    ;;
    ;; Options that set code-size limits
    ;;
    (list "--max-modules"
          "The maximum number of Verilog modules in the generated program"
          max-modules
          string->number)
    (list "--max-module-items"
          "The maximum number of items in a Verilog module"
          max-module-items
          string->number)
    (list "--max-block-statements"
          "The maximum number of statements within a block statement"
          max-modules
          string->number)
    ;;
    ;; Options that control pretty-printing
    ;;
    (list "--indent-spaces"
          "The number of spaces per level of indentation"
          max-modules
          string->number)
    )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
