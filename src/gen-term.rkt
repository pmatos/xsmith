#!r6rs
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2016 The University of Utah
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

(import (rnrs) (racr core) (racr testing))
;; (racr testing) is needed for print-ast

(define spec (create-specification))

(with-specification spec
  (ast-rule 'Prog->Stmt)
  (ast-rule 'Stmt->)
  (ast-rule 'Let:Stmt->name-val-Stmt<body)
  (ast-rule 'Eval:Stmt->Term)
  (ast-rule 'Block:Stmt->Stmt<first-Stmt<second)
  (ast-rule 'StmtHole:Stmt->)
  (ast-rule 'Term->)
  (ast-rule 'Num:Term->val)
  (ast-rule 'Ref:Term->name)
  (ast-rule 'Sum:Term->Term<left-Term<right)
  (ast-rule 'TermHole:Term->)

  (compile-ast-specifications 'Prog)

  (ag-rule value
           (Num (lambda (n) (ast-child 'val n)))
           (Ref (lambda (n) (let ((def (att-value 'def n (ast-child 'name n))))
                              (if def
                                  (ast-child 'val def)
                                  -1))))
           (Sum (lambda (n) (+ (att-value 'value (ast-child 'left n))
                               (att-value 'value (ast-child 'right n)))))
           )

  (ag-rule def
           (Prog (lambda (n name) #f)) ;; the not-defined error
           (Stmt (lambda (n name) (att-value 'def (ast-parent n) name)))
           (Let (lambda (n name) (if (eq? (ast-child 'name n) name)
                                     n
                                     (att-value 'def (ast-parent n) name))))
           (Term (lambda (n name) (att-value 'def (ast-parent n) name)))
           )
  
  (ag-rule level
           (Prog (lambda (n) 0))
           (Stmt (lambda (n) (+ 1 (att-value 'level (ast-parent n)))))
           (Term (lambda (n) (+ 1 (att-value 'level (ast-parent n))))))
  
  (ag-rule pp
           (Prog (lambda (n) (att-value 'pp (ast-child 1 n))))
           (Let (lambda (n) (list 'let
                                  (ast-child 'name n)
                                  (ast-child 'val n)
                                  (att-value 'pp (ast-child 'body n)))))
           (Eval (lambda (n) (list 'eval (att-value 'pp (ast-child 1 n)))))
           (Block (lambda (n) (list 'block
                                    (att-value 'pp (ast-child 'first n))
                                    (att-value 'pp (ast-child 'second n)))))
           (Num (lambda (n) (ast-child 'val n)))
           (Ref (lambda (n) (ast-child 'name n)))
           (Sum (lambda (n) (list '+
                                  (att-value 'pp (ast-child 'left n))
                                  (att-value 'pp (ast-child 'right n))))))
  
  (compile-ag-specifications))

(define *max-level* 5)

(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (mod (+ (* seed a) c) m)))
      (/ seed m))))

(define (randint . args)
  (cond ((= (length args) 1)
          (floor (* (random) (car args))))
        ((= (length args) 2)
          (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
        (else (error 'randint "usage: (randint [lo] hi)"))))

(define (replace-with-term n)
  (let ((r (randint 3)))
    (cond ((or (> (att-value 'level n) *max-level*)
               (< r 1))
           ;; Replace with a Num.
           (rewrite-subtree n (create-ast spec 'Num (list (randint 10)))))
          ((< r 2)
           ;; Replace with a Sum.
           (rewrite-subtree n (create-ast spec 'Sum (list
                                                     (create-ast spec 'TermHole (list))
                                                     (create-ast spec 'TermHole (list))))))
          (else
           ;; Replace with a Ref.
           (rewrite-subtree n (create-ast spec 'Ref (list 'a))))
          )))

(define (replace-with-stmt n)
  (let ((r (randint 3)))
    (cond ((or (> (att-value 'level n) *max-level*)
               (< r 1))
           ;; Replace with an Eval.
           (rewrite-subtree n (create-ast spec 'Eval (list (create-ast spec 'TermHole (list))))))
          ((< r 2)
           ;; Replace with a Block.
           (rewrite-subtree n (create-ast spec 'Block (list
                                                       (create-ast spec 'StmtHole (list))
                                                       (create-ast spec 'StmtHole (list))))))
          (else
           ;; Replace with a Let.
           (rewrite-subtree n (create-ast spec 'Let (list 'a
                                                          (randint 10)
                                                          (create-ast spec 'StmtHole (list))))))
          )))

(define (generate-random-prog n)
  (let ((fill-in
         (lambda (n)
           (cond ((eq? 'TermHole (ast-node-type n))
                  (replace-with-term n)
                  #t)
                 ((eq? 'StmtHole (ast-node-type n))
                  (replace-with-stmt n)
                  #t)
                 (else #f)))))
    (perform-rewrites n 'top-down fill-in))
  n)

(define (do-it)
  (define (print name) (cons name (lambda (v) v)))
  (define printer
    (list (print 'value) (print 'pp)))
  (let ((ast
         (generate-random-prog
          (create-ast spec 'Prog (list (create-ast spec 'StmtHole (list))))
          )))
    (print-ast ast
               printer
               (current-output-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
