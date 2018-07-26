#lang info

(define name "xsmith")
(define version "0.0.0")
(define git-version "1234abcd")
(define scribblings '(("scribblings/xsmith.scrbl" () (library))))
(define deps '("base"
               "pprint"
               "racr"
               "rosette"
               ))
(define build-deps '("scribble-lib"))
