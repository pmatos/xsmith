#lang clotho

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 racket/string
 racket/list
 racket/pretty
 syntax/parse/define
 (for-syntax
  clotho/racket/base
  syntax/parse
  racket/match
  ))

;; Likely I'll want to fuzz both with and without exceptions.  Let's make it toggleable.
;; TODO - this should be backed by a parameter or something so I can set it as a command-line option.
(define NE? #t)

(define random-max 4294967087)


(define-generic-type box-type ([type covariant]))
(define bool (base-type 'bool #:leaf? #f))

(define number (base-type 'number bool #:leaf? #f))
(define complex (base-type 'complex number #:leaf? #f))
(define real (base-type 'real complex #:leaf? #f))
(define rational (base-type 'rational real #:leaf? #f))
(define int (base-type 'int rational #:leaf? #f))
(define nat (base-type 'nat int #:leaf? #f))

(define char (base-type 'char bool))
(define string (base-type 'string bool #:leaf? #f))
(define mutable-string (base-type 'mutable-string string))
(define immutable-string (base-type 'immutable-string string))
(define symbol (base-type 'symbol bool))
(define keyword (base-type 'keyword bool))
(define date (base-type 'date bool #:leaf? #f))
(define date* (base-type 'date* date))
;; for literal #t and #f
(define exact-bool (base-type 'exact-bool bool))


(define-basic-spec-component racket-comp)

(add-basic-expressions racket-comp
                       #:ProgramWithSequence #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:VoidExpression #t
                       #:AssignmentExpression #t
                       #:IfExpression #t
                       #:ExpressionSequence #t
                       #:LetSequential #t
                       #:LambdaWithExpression #t
                       ;#:Numbers #t
                       ;#:Booleans #t
                       ;#:Strings #t
                       #:ImmutableList #t
                       #:MutableArray #t
                       #:MutableArraySafeAssignmentExpression #t
                       #:ImmutableArray #t
                       #:MutableStructuralRecord #t
                       #:MutableStructuralRecordAssignmentExpression #t
                       #:ImmutableStructuralRecord #t
                       #:int-type int
                       #:bool-type bool
                       )


(add-property
 racket-comp
 render-hole-info
 [#f (λ (h) '_HOLE_)])


(define-for-syntax (racr-ize-symbol sym)
  ;; Turn common racket identifiers into something RACR can deal with.
  (define split-chars (string->list "-<>?!*+/=18"))

  (define-values (parts-rev dividers-rev)
    (let ()
      (define-values (parts dividers current-thing current-divider?)
        (for/fold ([parts '()]
                   [dividers '()]
                   [current ""]
                   [current-divider? #f])
                  ([c (string->list (symbol->string sym))])
          (define divider? (member c split-chars))
          (cond [(and divider? current-divider?)
                 (values parts dividers (string-append current (string c)) #t)]
                [divider?
                 (values (cons current parts) dividers (string c) #t)]
                [current-divider?
                 (values parts (cons current dividers) (string c) #f)]
                [else
                 (values parts dividers (string-append current (string c)) #f)])))
      (cond [current-divider? (values parts (cons current-thing dividers))]
            [else (values (cons current-thing parts) dividers)])))

  (define part-strings
    (map string-titlecase (reverse parts-rev)))
  (define divider-strings
    (map (λ (divider)
           (match divider
             ["->" "To"]
             ["/" "With"]
             ["?" "P"]
             ["!" "Bang"]
             ["1" "One"]
             ["8" "Eight"]
             ["*" "Star"]
             ["*?" "StarP"]
             ["*-" "Star"]
             [else ""]))
         (reverse dividers-rev)))

  (define (merge-parts accum ps ds)
    (match (list ps ds)
      [(list (list) (list)) accum]
      [(list (list p ps ...) (list d ds ...))
       (merge-parts (string-append accum p d) ps ds)]
      [(list (list p ps ...) (list))
       (merge-parts (string-append accum p) ps null)]))

  (define converted-string (merge-parts "" part-strings divider-strings))
  (string->symbol converted-string))
(define-for-syntax (racr-ize-id id)
  (datum->syntax id
                 (racr-ize-symbol (syntax-e id))
                 id))

(define (render-child sym n)
  (att-value 'xsmith_render-node (ast-child sym n)))
(define (render-children sym n)
  (map (λ (cn) (att-value 'xsmith_render-node cn)) (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))

(define ((render-variadic variadic-function-name) n)
  `(,variadic-function-name ,@(render-children 'minargs n)
                           ,@(render-children 'moreargs n)))
(define (moreargs-default)
  (random 5))
(define no-child-types (λ (n t) (hash)))

(define-syntax-parser ag [(_ arg ...) #'(add-to-grammar racket-comp arg ...)])
(define-syntax-parser ap [(_ arg ...) #'(add-property racket-comp arg ...)])


(ag
 [EmptyListLiteral Expression ()
                   #:prop choice-weight 1
                   #:prop type-info [(immutable (list-type (fresh-type-variable)))
                                     no-child-types]
                   #:prop render-node-info (λ (n) 'null)]
 [MutableStringLiteral Expression ([v = (random-string)])
                       #:prop choice-weight 1
                       #:prop type-info [mutable-string no-child-types]
                       #:prop render-node-info (λ (n)
                                                 `(string-copy ,(ast-child 'v n)))]
 [DateLiteral Expression ([v = (random-int)])
              #:prop choice-weight 1
              #:prop type-info [date* no-child-types]
              ;; OK, so I'm just using seconds->date.  Not literally a date literal.
              #:prop render-node-info (λ (n) `(seconds->date ,(ast-child 'v n)))]
 [VariadicExpression Expression ([minargs : Expression *]
                                 [moreargs : Expression * = (random 5)])
                     #:prop may-be-generated #f])


(define-syntax-parser ag/atomic-literal
  [(_ name:id type:expr fresh-expr:expr)
   #'(ag [name Expression ([v = fresh-expr])
               #:prop choice-weight 1
               #:prop type-info [type no-child-types]
               #:prop render-node-info (λ (n) `(quote ,(ast-child 'v n)))])])
(ag/atomic-literal IntLiteral int (random-int))
(ag/atomic-literal CharLiteral char (random-char))
(ag/atomic-literal BoolLiteral exact-bool (random-bool))
(ag/atomic-literal ImmutableStringLiteral immutable-string (random-string))
(ag/atomic-literal SymbolLiteral symbol (string->symbol (random-string)))
(ag/atomic-literal KeywordLiteral keyword (string->keyword (random-string)))

(define-syntax-parser ag/variadic
  [(_ racket-name:id min-args:expr
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'minargs t 'moreargs t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (datum->syntax
                                              #'racket-name
                                              (racr-ize-symbol
                                               (syntax-e #'racket-name))
                                              #'racket-name)]))
           (~optional (~seq #:NE-name NE-name:id)
                      #:defaults ([NE-name #'racket-name])))
      ...)
   #'(ag [racr-name VariadicExpression ()
               #:prop fresh (hash 'minargs min-args)
               #:prop type-info [type (λ (n t) (hash 'minargs t 'moreargs t))]
               #:prop render-node-info (render-variadic
                                        (if NE? 'NE-name 'racket-name))])])
(ag/variadic * 0 #:racr-name Times)
(ag/variadic + 0 #:racr-name Plus)
(ag/variadic - 1 #:racr-name Minus)
(ag/variadic / 1 #:racr-name Divide #:NE-name NE/ #:type real)
(ag/variadic bitwise-and 0 #:type int)
(ag/variadic bitwise-ior 0 #:type int)
(ag/variadic bitwise-xor 0 #:type int)
(ag/variadic lcm 0 #:type int)
(ag/variadic gcd 0 #:type int)
(ag/variadic min 1 #:type real)
(ag/variadic max 1 #:type real)
(ag/variadic append 0 #:type (immutable (list-type (fresh-type-variable))))
(ag/variadic string 0 #:type string
             #:ctype (λ (n t) (hash 'minargs char 'moreargs char)))
(ag/variadic string-append 0 #:type mutable-string
             #:ctype (λ (n t) (hash 'minargs string 'moreargs string)))

 ;; The numerical comparison operators require at least 1 argument.  I'm not sure why they don't accept 0 args -- eg. as a predicate that an empty list is sorted.
(define-syntax-parser ag/number-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs real 'moreargs real))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/number-compare LessThan '<)
(ag/number-compare LessThanEqual '<=)
(ag/number-compare NumericEqual '=)
(ag/number-compare GreaterThan '>)
(ag/number-compare GreaterThanEqual '>=)
(define-syntax-parser ag/char-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs char 'moreargs char))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/char-compare CharLessThan 'char<?)
(ag/char-compare CharLessThanEqual 'char<=?)
(ag/char-compare CharEqual 'char=?)
(ag/char-compare CharGreaterThan 'char>?)
(ag/char-compare CharGreaterThanEqual 'char>=?)
(ag/char-compare CharCILessThan 'char-ci<?)
(ag/char-compare CharCILessThanEqual 'char-ci<=?)
(ag/char-compare CharCIEqual 'char-ci=?)
(ag/char-compare CharCIGreaterThan 'char-ci>?)
(ag/char-compare CharCIGreaterThanEqual 'char-ci>=?)
(ag [SymbolLessThan VariadicExpression ()
                    #:prop fresh (hash 'minargs 1)
                    #:prop type-info
                    [bool (λ (n t) (hash 'minargs symbol 'moreargs symbol))]
                    #:prop render-node-info (render-variadic 'symbol<?)])
(define-syntax-parser ag/string-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs string 'moreargs string))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/string-compare StringLessThan 'string<?)
(ag/string-compare StringLessThanEqual 'string<=?)
(ag/string-compare StringEqual 'string=?)
(ag/string-compare StringGreaterThan 'string>?)
(ag/string-compare StringGreaterThanEqual 'string>=?)
(ag/string-compare StringCILessThan 'string-ci<?)
(ag/string-compare StringCILessThanEqual 'string-ci<=?)
(ag/string-compare StringCIEqual 'string-ci=?)
(ag/string-compare StringCIGreaterThan 'string-ci>?)
(ag/string-compare StringCIGreaterThanEqual 'string-ci>=?)

(define-syntax-parser ag/single-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'Expression t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression (Expression)
                    #:prop type-info [type ctype]
                    #:prop render-node-info
                    (λ (n) `(,(if NE? 'NE-name 'name)
                             ,(render-child 'Expression n)))])])
(define-syntax-parser Ectype
  [(_ etype:expr)
   #'(λ (n t) (hash 'Expression etype))])

(define-syntax-parser ag/two-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'l t 'r t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression ([l : Expression]
                                [r : Expression])
                    #:prop type-info [type ctype]
                    #:prop render-node-info
                    (λ (n) `(,(if NE? 'NE-name 'name)
                             ,(render-child 'l n)
                             ,(render-child 'r n)))])])
(define-syntax-parser E2ctype
  [(_ etypel:expr etyper:expr)
   #'(λ (n t) (hash 'l etypel 'r etyper))])

(ag/single-arg abs #:type real)
(ag/single-arg cos #:type real)
(ag/single-arg acos #:type real)
(ag/single-arg sin #:type real)
(ag/single-arg asin #:type real)
(ag/single-arg tan #:type real)
(ag/single-arg atan #:racr-name AtanOne #:NE-name NE/atan-1 #:type real)
(ag/single-arg add1)
(ag/single-arg sub1)
(ag/single-arg angle #:type real)
(ag/single-arg ceiling)
(ag/single-arg floor)
(ag/single-arg round)
(ag/single-arg truncate)
(ag/single-arg imag-part)
(ag/single-arg real-part)
(ag/single-arg magnitude)
;; TODO - numerator and denominator take rational reals (not imaginaries or irrationals)
(ag/single-arg numerator #:type int)
(ag/single-arg denominator #:type int)
;; TODO - what should I do about exp, expt, arithmetic-shift, and other functions that potentially need a limited domain?  Make NE versions?

(ag/single-arg not #:type bool)
(ag/single-arg bitwise-not #:type int)
(ag/single-arg zero? #:type bool #:ctype (Ectype number))
(ag/single-arg null? #:type bool
               #:ctype (Ectype (immutable (list-type (fresh-type-variable)))))
(ag/single-arg symbol-interned? #:type bool #:ctype (Ectype symbol))
(ag/single-arg symbol-unreadable?
               #:type bool #:ctype (Ectype symbol))
(ag/single-arg integer-length #:type int)
(ag/single-arg even? #:type bool #:ctype (Ectype int))
(ag/single-arg odd? #:type bool #:ctype (Ectype int))
(ag/single-arg exact? #:type bool #:ctype (Ectype number))
(ag/single-arg inexact? #:type bool #:ctype (Ectype number))
(ag/single-arg exact-integer? #:type bool #:ctype (Ectype number))
(ag/single-arg exact-positive-integer? #:type bool #:ctype (Ectype number))
(ag/single-arg exact-nonnegative-integer? #:type bool #:ctype (Ectype number))
(ag/single-arg inexact-real? #:type bool #:ctype (Ectype number))
(ag/single-arg exact->inexact #:type number)
(ag/single-arg inexact->exact #:type number)

(ag/single-arg char-downcase #:type char)
(ag/single-arg char-foldcase #:type char)
(ag/single-arg char-titlecase #:type char)
(ag/single-arg char-upcase #:type char)
(ag/single-arg char-utf-8-length #:type int #:ctype (Ectype char))
(ag/single-arg char-general-category #:type symbol
               #:ctype (Ectype char))

(ag/single-arg date*-nanosecond #:type number #:ctype (Ectype date*))
(ag/single-arg date*-time-zone-name #:type immutable-string #:ctype (Ectype date*))
(ag/single-arg date-day #:type int #:ctype (Ectype date))
(ag/single-arg date-dst? #:type bool #:ctype (Ectype date))
(ag/single-arg date-hour #:type int #:ctype (Ectype date))
(ag/single-arg date-minute #:type int #:ctype (Ectype date))
(ag/single-arg date-month #:type int #:ctype (Ectype date))
(ag/single-arg date-second #:type int #:ctype (Ectype date))
(ag/single-arg date-time-zone-offset #:type int #:ctype (Ectype date))
(ag/single-arg date-week-day #:type int #:ctype (Ectype date))
(ag/single-arg date-year #:type int #:ctype (Ectype date))
(ag/single-arg date-year-day #:type int #:ctype (Ectype date))

(define-syntax-parser ag/char-pred
  [(_ name:id)
   #'(ag/single-arg name #:type bool #:ctype (Ectype char))])
(ag/char-pred char-alphabetic?)
(ag/char-pred char-blank?)
(ag/char-pred char-graphic?)
(ag/char-pred char-iso-control?)
(ag/char-pred char-lower-case?)
(ag/char-pred char-numeric?)
(ag/char-pred char-punctuation?)
(ag/char-pred char-symbolic?)
(ag/char-pred char-title-case?)
(ag/char-pred char-upper-case?)
(ag/char-pred char-whitespace?)

(ag/single-arg string-downcase #:type string)
(ag/single-arg string-foldcase #:type string)
(ag/single-arg string-titlecase #:type string)
(ag/single-arg string-upcase #:type string)

(ag/single-arg string-length #:type int #:ctype (Ectype string))
(ag/single-arg string-utf-8-length
               #:type int #:ctype (Ectype string))
(ag/single-arg string-copy #:type mutable-string #:ctype (Ectype string))
(ag/single-arg string-normalize-nfc
               #:type mutable-string #:ctype (Ectype string))
(ag/single-arg string-normalize-nfd
               #:type mutable-string #:ctype (Ectype string))
(ag/single-arg string-normalize-nfkc
               #:type mutable-string #:ctype (Ectype string))
(ag/single-arg string-normalize-nfkd
               #:type mutable-string #:ctype (Ectype string))

(define-syntax-parser ag/converter
  [(_ name:id from:expr to:expr)
   #'(ag/single-arg name #:type to #:ctype (Ectype from))])
(ag/converter char->integer char int)
(ag/converter string->symbol string symbol)
(ag/converter string->uninterned-symbol string symbol)
(ag/converter string->unreadable-symbol string symbol)
(ag/converter symbol->string symbol string)
(ag/converter string->keyword string keyword)
(ag/converter keyword->string keyword string)
(ag/converter string->list string (immutable (list-type char)))
(ag/converter string->immutable-string string immutable-string)
;; TODO - should be real instead of int
;; TODO - needs a second boolean arg for whether it's local time (the default #t is local) -- or maybe I should always use UTC?
(ag/converter seconds->date int date*)
(ag/single-arg vector->list
               #:racr-name ImmutableVectorToList
               #:type (immutable (list-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! t (immutable (list-type inner-type)))
                         (hash 'Expression (immutable (array-type inner-type)))))
(ag/single-arg vector->list
               #:racr-name MutableVectorToList
               #:type (immutable (list-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! t (immutable (list-type inner-type)))
                         (hash 'Expression (mutable (array-type inner-type)))))
;; This one is kinda dumb, it probably checks and is just identity.
(ag/single-arg vector->immutable-vector
               #:racr-name ImmutableVectorToImmutableVector
               #:type (immutable (array-type (fresh-type-variable))))
(ag/single-arg vector->immutable-vector
               #:racr-name MutableVectorToImmutableVector
               #:type (immutable (array-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! t (immutable (array-type inner-type)))
                         (hash 'Expression (mutable (array-type inner-type)))))

(define-syntax-parser ag/type-predicate
  [(_ name:id)
   #'(ag/single-arg name
                    #:type bool
                    #:ctype (Ectype (fresh-type-variable)))])
(ag/type-predicate boolean?)
(ag/type-predicate box?)
(ag/type-predicate byte?)
(ag/type-predicate char?)
(ag/type-predicate complex?)
(ag/type-predicate date?)
(ag/type-predicate date*?)
(ag/type-predicate evt?)
(ag/type-predicate exn?)
(ag/type-predicate hash?)
(ag/type-predicate immutable?)
(ag/type-predicate integer?)
(ag/type-predicate interned-char?)
(ag/type-predicate keyword?)
(ag/type-predicate list?)
(ag/type-predicate list-pair?)
(ag/type-predicate mpair?)
(ag/type-predicate number?)
(ag/type-predicate pair?)
(ag/type-predicate parameter?)
(ag/type-predicate parameterization?)
(ag/type-predicate path?)
(ag/type-predicate pregexp?)
(ag/type-predicate procedure?)
(ag/type-predicate rational?)
(ag/type-predicate real?)
(ag/type-predicate regexp?)
(ag/type-predicate string?)
(ag/type-predicate struct-type?)
(ag/type-predicate struct-type-property?)
(ag/type-predicate struct?)
(ag/type-predicate symbol?)
(ag/type-predicate syntax?)
(ag/type-predicate true-object?)
(ag/type-predicate vector?)
(ag/type-predicate void?)

(ag/single-arg box
               #:racr-name MutableBoxLiteral
               #:type (mutable (box-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! (mutable (box-type inner-type)) t)
                         (hash 'Expression inner-type)))
(ap wont-over-deepen [MutableBoxLiteral #t])
(ag/single-arg box-immutable #:racr-name ImmutableBoxLiteral
               #:type (immutable (box-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner-type (fresh-type-variable))
                         (unify! (immutable (box-type inner-type)) t)
                         (hash 'Expression inner-type)))
(ap wont-over-deepen [ImmutableBoxLiteral #t])
(ag/single-arg unbox
               #:type (fresh-type-variable)
               #:ctype (λ (n t) (hash 'Expression (fresh-type-variable
                                                   (immutable (box-type t))
                                                   (mutable (box-type t))))))
(ap mutable-container-access [Unbox (read 'box)])
(ag [SetBoxBang Expression ([box : Expression] [newval : Expression])
                #:prop mutable-container-access (write 'box)
                #:prop type-info [void-type
                                  (λ (n t)
                                    (define inner-type (fresh-type-variable))
                                    (hash 'box (mutable (box-type inner-type))
                                          'newval inner-type))]
                #:prop render-node-info (λ (n) `(set-box! ,(render-child 'box n)
                                                          ,(render-child 'newval n)))])


(ag/two-arg equal?
            #:type bool
            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
(ag/two-arg eqv?
            #:type bool
            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
(ag/two-arg eq?
            #:type bool
            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
(ag/two-arg make-polar
            #:type number
            ;; TODO - should be real real, once I fix the numeric tower
            #:ctype (E2ctype int int))
(ag/two-arg make-rectangular
            #:type number
            ;; TODO - should be real real, once I fix the numeric tower
            #:ctype (E2ctype int int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rendering for basic expressions (IE add-basic-expression)
;;;;; Mostly this is from the simple/racket fuzzer.

(add-property
 racket-comp
 render-node-info

 [ProgramWithSequence
  (λ (n)
    `(
      ;; I'm sick of trying to write wrappers in #lang kernel.
      ;; Let's use a submodule in racket/base instead.
      (module wrappers-and-helpers racket/base
        (provide (all-defined-out))
        (require
         racket/match
         racket/format
         racket/string
         )
        (define-values (NE/)
          (λ (arg1 . args)
            (if (null? args)
                (if (eq? arg1 0)
                    0
                    (/ arg1))
                (apply / arg1 (map (λ (x) (if (eq? 0 x) 1 x)) args)))))
        ;; TODO - add safe/NoException wrappers
        (define-values (NE/atan-2)
          (λ (x y)
            (if (equal? x 0)
                0
                (if (equal? y 0)
                    0
                    (atan x y)))))
        (define-values (NE/atan-1)
          (λ (x)
            (if (equal? x 0+1i)
                0
                (if (equal? x 0-1i)
                    0
                    (atan x)))))
        (define-values (safe-car)
          (λ (list fallback)
            (if (null? list)
                fallback
                (car list))))
        (define-values (safe-cdr)
          (λ (list fallback)
            (if (null? list)
                fallback
                (cdr list))))
        (define (NE/vector-ref vec index)
          (vector-ref vec (modulo index (vector-length vec))))
        (define-values (immutable-vector-set)
          (λ (vec index-raw val)
            (define-values (index) (modulo index-raw (vector-length vec)))
            (vector->immutable-vector
             (build-vector (vector-length vec)
                           (λ (i) (if (equal? i index)
                                      val
                                      (vector-ref vec i)))))))

        (define (my-format/hash-inner the-hash)
          (define (hash-sort-lt l r)
            (match (list l r)
              [(list (? number?) (? number?)) (< l r)]
              [(list (? string?) (? string?)) (string<? l r)]
              [(list (? symbol?) (? symbol?)) (string<? (symbol->string l)
                                                        (symbol->string r))]
              [(list (? keyword?) (? keyword?)) (string<? (keyword->string l)
                                                          (keyword->string r))]
              [else (error 'fuzzer-format "TODO - add more ways to sort for hash tables.  Given: ~v and ~v" l r)]))
          (string-join
           (for/list ([k (sort (hash-keys the-hash) hash-sort-lt)])
             (format "[~a . ~a]"
                     (my-format k)
                     (my-format (hash-ref the-hash k))))))
        (define (my-format val)
          (define (mutable? x)
            (not (immutable? x)))
          (match val
            [(list v ...) (format "(~a)" (string-join (map my-format v)))]
            [(and (? immutable?) (vector v ...))
             (format "#{vector-immutable ~a}" (string-join (map my-format v)))]
            [(vector v ...)
             (format "#{vector-mutable ~a}" (string-join (map my-format v)))]
            [(and (? immutable?) (? hash?) (? hash-eq?))
             (format "#{immutable-hasheq ~a}" (my-format/hash-inner val))]
            [(and (? immutable?) (? hash?) (? hash-eqv?))
             (format "#{immutable-hasheqv ~a}" (my-format/hash-inner val))]
            [(and (? immutable?) (? hash?) (? hash-equal?))
             (format "#{immutable-hashequal ~a}" (my-format/hash-inner val))]
            [(and (? mutable?) (? hash?) (? hash-eq?))
             (format "#{mutable-hasheq ~a}" (my-format/hash-inner val))]
            [(and (? mutable?) (? hash?) (? hash-eqv?))
             (format "#{mutable-hasheqv ~a}" (my-format/hash-inner val))]
            [(and (? mutable?) (? hash?) (? hash-equal?))
             (format "#{mutable-hashequal ~a}" (my-format/hash-inner val))]
            [(and (? immutable?) (box v))
             (format "#{immutable-box ~a}" (my-format v))]
            [(and (? mutable?) (box v))
             (format "#{mutable-box ~a}" (my-format v))]
            [(? procedure?)
             ;; Different versions of Racket can have different results for
             ;; `object-name`, so let's just print all procedures equally.
             (format "#<procedure>")]
            [(or (? void?)
                 (? number?)
                 (? string?)
                 (? symbol?)
                 (? keyword?)
                 #t #f
                 (? char?)
                 ;; Dates are structs, but they can only contain atomic data.
                 ;; So we don't need to worry about applying a specialized
                 ;; printer recursively.
                 (? date?)
                 )
             (~a val)]))
        (define (my-print x)
          (println (my-format x)))
        )
      (#%require (submod "." wrappers-and-helpers))
      ,@(render-children 'definitions n)
      (define-values (program-result) ,(render-child 'ExpressionSequence n))
      (begin
        ,(if #;(base-type? (att-value 'xsmith_type
                                      (ast-child 'ExpressionSequence n)))
             #t
             '(printf "Program body result: ~v\n" program-result)
             '(void))
        ,@(for/list ([c (ast-children (ast-child 'definitions n))]
                     #:when #t #;(base-type? (concretize-type
                                              (att-value 'xsmith_type c)))
                     )
            `(printf "Var ~a: ~a\n"
                     ',(string->symbol (ast-child 'name c))
                     (my-format ,(string->symbol (ast-child 'name c))))))))]

 [Definition (λ (n)
               `(define-values (,(string->symbol (ast-child 'name n)))
                  ,(render-child 'Expression n)))]
 [AssignmentExpression
  (λ (n) `(set! ,(string->symbol (ast-child 'name n))
                ,(render-child 'newvalue n)))]
 [ExpressionSequence
  (λ (n)
    `(begin
       ,@(render-children 'effectexpressions n)
       ,(render-child 'finalexpression n)))]
 [LetSequential
  (λ (n)
    (define let-pairs
      (map (λ (dn) `[(,(string->symbol (ast-child 'name dn)))
                     ,(render-child 'Expression dn)])
           (ast-children (ast-child 'definitions n))))
    (foldr (λ (v accum)
             `(let-values (,v) ,accum))
           (render-child 'body n)
           let-pairs))]

 [IfExpression
  (λ (n)
    `(if ,(render-child 'test n)
         ,(render-child 'then n)
         ,(render-child 'else n)))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]

 [ProcedureApplication
  (λ (n)
    `(#%app ,(render-child 'procedure n)
            ,@(render-children 'arguments n)))]
 [FormalParameter (λ (n) (string->symbol (ast-child 'name n)))]
 [LambdaWithExpression
  (λ (n)
    `(lambda (,@(render-children 'parameters n))
       ,(render-child 'body n)))]

 ;[BoolLiteral (λ (n) (not (not (ast-child 'v n))))]
 ;[Not (λ (n) `(not ,(render-child 'Expression n)))]
 ;[And (binary-op-renderer 'and)]
 ;[Or (binary-op-renderer 'or)]

 ;[IntLiteral (λ (n) (ast-child 'v n))]
 ;[Plus (binary-op-renderer '+)]
 ;[Minus (binary-op-renderer '-)]
 ;[Times (binary-op-renderer '*)]
 ;[LessThan (binary-op-renderer '<)]
 ;[GreaterThan (binary-op-renderer '>)]
 ;[SafeDivide (binary-op-renderer 'safe-divide)]

 ;[StringLiteral (λ (n) (ast-child 'v n))]
 ;[StringAppend (binary-op-renderer 'string-append)]
 ;[StringLength (λ (n) `(string-length ,(render-child 'Expression n)))]

 [ImmutableListLiteral
  (λ (n) `(list ,@(render-children 'expressions n)))]
 [ImmutableListSafeCar
  (λ (n) `(safe-car ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListSafeCdr
  (λ (n) `(safe-cdr ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListCons
  (λ (n) `(cons ,(render-child 'newvalue n)
                ,(render-child 'list n)))]
 [MutableArrayLiteral
  (λ (n) `(vector ,@(render-children 'expressions n)))]
 [ImmutableArrayLiteral
  (λ (n) `(vector->immutable-vector (vector ,@(render-children 'expressions n))))]
 [MutableArraySafeReference
  (λ (n) `(NE/vector-ref ,(render-child 'array n) ,(render-child 'index n)))]
 [ImmutableArraySafeReference
  (λ (n) `(NE/vector-ref ,(render-child 'array n) ,(render-child 'index n)))]
 [MutableArraySafeAssignmentExpression
  (λ (n)
    (define-values (array-rendered) (render-child 'array n))
    `(vector-set! ,array-rendered
                  (modulo ,(render-child 'index n)
                          (vector-length ,array-rendered))
                  ,(render-child 'newvalue n)))]
 [ImmutableArraySafeSet
  (λ (n)
    `(immutable-vector-set ,(render-child 'array n)
                           ,(render-child 'index n)
                           ,(render-child 'newvalue n)))]

 [MutableStructuralRecordLiteral
  (λ (n)
    `(make-hash (list ,@(map (λ (name val)
                               `(cons ',name
                                      ,(att-value 'xsmith_render-node val)))
                             (ast-child 'fieldnames n)
                             (ast-children (ast-child 'expressions n))))))]
 [ImmutableStructuralRecordLiteral
  (λ (n)
    `(hash ,@(apply append
                    (map (λ (name val)
                           `(',name
                             ,(att-value 'xsmith_render-node val)))
                         (ast-child 'fieldnames n)
                         (ast-children (ast-child 'expressions n))))))]
 [MutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [ImmutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [MutableStructuralRecordAssignmentExpression
  (λ (n)
    `(hash-set! ,(render-child 'record n)
                ',(ast-child 'fieldname n)
                ,(render-child 'newvalue n)))]
 [ImmutableStructuralRecordSet
  (λ (n)
    `(hash-set ,(render-child 'record n)
               ',(ast-child 'fieldname n)
               ,(render-child 'newvalue n)))]
 [VoidExpression (λ (n) '(void))]
 )



(define (type-thunks-for-concretization)
  (list
   (λ()bool)
   (λ()number)
   (λ()int)
   (λ()char)
   (λ()string)
   (λ()symbol)
   (λ()keyword)
   (λ() (immutable (list-type (fresh-type-variable))))
   (λ() (immutable (box-type (fresh-type-variable))))
   (λ() (mutable (box-type (fresh-type-variable))))
   ))

(define (racket-format-render s-exps)
  (define out (open-output-string))
  (for ([symex s-exps])
    (pretty-print symex out 1))
  (format "\n(module random-fuzzing-module '#%kernel\n~a\n)\n"
          (get-output-string out)))

(define-xsmith-interface-functions
  [racket-comp]
  #:fuzzer-name racket
  #:program-node ProgramWithSequence
  #:type-thunks type-thunks-for-concretization
  #:format-render racket-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format ";; ~a" l)) lines)
                                         "\n"))

  )

(module+ main (racket-command-line))
