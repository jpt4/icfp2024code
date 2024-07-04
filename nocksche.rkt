;;  nocksche.rkt
;;  2024
;;  ICFP
;;  Nock 4K in Racket
;;  Racket v8.12

#lang racket
(require racket/match)

;external syntax of nouns is strictly atoms and cells 

;internal syntax includes nock prefix operators 

;`tar` as tag in pattern matching within monolithic nock evaluator,
;with explicit internal syntax

;(tar) as function applied to nouns, without explicit internal syntax

(define (noun n)              ; A noun
  (or (atom n) (cell n)))     ; is an atom or a cell.

(define (atom a)              ; An atom
  (natural? a))               ; is a natural number.

(define (cell c)              ; A cell
  (match c
    [`(,a ,b)                 ; is an ordered pair
     (and (noun a) (noun b))] ; of nouns.
    [_ #f]))

;This analysis aims to interrogate the claims of the Urbit project
;regarding the simplicity and naturality of the Nock specification, by
;examining an implementation of Nock 4K in Racket Scheme and
;miniKanren, and the process of producing this implementation from the
;specification.

;This paper is a study in the distance between the specification of
;Nock, and a compliant specification. Nock is defined by an official,
;but not formal, pseudocode, suggestive of a series of reduction
;rules, and framed by natural language scaffolding. Most load bearing
;of these is:

;Reduce by the first matching pattern; variables match any noun.

;We name this the "evaluation rule", because akin to the suggestivity
;of the pseudocode, it suggests an evaluation strategy, without fully
;explicating any in particular.

;"Variables match any noun" suggests that the redexes only operate on
;terms that have been rendered into nouns, not pending evals, thus
;strict evaluation.

; Explicit implementation of an outer reduction rule:
; https://github.com/urbit/vere/commit/ddc0f8ac87a7030ba9bdafecf8917611ed3e8b71

; https://github.com/urbit/vere/issues/660
; *[0 [9 [2 2] 0 1]] -> *[*[a c] 2 [0 1] 0 b]
;; *[*[0 0 1] 2 [0 1] 0 [2 2]]
;; *[*[*[0 0 1] [0 1]] *[*[0 0 1] 0 [2 2]]]
;; *[*[*[0 0 1] [0 1]] /[[2 2] *[0 0 1]]] <- could error here, if inner(most) reduction
;; *[*[/[1 0] [0 1]] /[[2 2] /[1 0]]]
;; *[*[0 [0 1]] /[[2 2] 0]]
;; *[/[1 0] /[[2 2] 0]]
;; *[0 /[[2 2] 0]]
;; !

;; https://github.com/urbit/urbit/issues/4464
;; .*([3 0 1] [9 1 [0 1]])
;; *([3 0 1] [9 1 [0 1]])
;;      a       b   c
;; *[*[a c] 2 [0 1] 0 b]
;; *[*[[3 0 1] [0 1]] 2 [0 1] [0 1]]
;;            a           b     c
;; *[*[a b] *[a c]]
;; *[*[*[[3 0 1] [0 1]] [0 1]] *[*[[3 0 1] [0 1]] [0 1]]]
;;              a          b
;; *[/[1 *[[3 0 1] [0 1]]] /[1 *[[3 0 1] [0 1]]]]
;;                a                     a
;; *[*[[3 0 1] [0 1]] *[[3 0 1] [0 1]]]
;;        a       b        a       b
;; *[/[1 [3 0 1]] /[1 [3 0 1]]]
;; *[[3 0 1] [3 [0 1]]]
;;      a         b
;; ?*[[3 0 1] [0 1]]
;;       a       b
;; ?/[1 [3 0 1]]
;; ?[3 [0 1]]
;; 0

;Narrow adherence to the evaluation rule will quickly reveal the Nock
;spec to be insufficient unto itself, because there are right column
;patterns without left column matches, for which reduction should not
;yet terminate. E.g., for *[a [b c] d]=>[*[a b c] *[a d]], no left
;column pattern corresponds to a cell with two tar-prefixed
;nouns. Therefore, implementation of the evaluation rule must
;recognize that reduction recurses within the cell, and address each
;tar expression individually. No longer bound by a narrow adherence to
;the evaluation rule, the whole field of evaluation strategies would
;now appear to be available, and the choice of strategy entirely
;implementation dependent. For the majority of reduction rules, this
;is indeed the case, but certain reductions will provide, implicitly,
;additional information beyond the evaluation rul, and thus impose
;additional constraints on a compliant implementation.

;produce a right associated noun

;Why should we include this normalization step in our implementation?
;If expressions are passed to `nock` Why should we perform this
;normalization step? None of the left column patterns are in fully
;right associated form, so it would seem that any application of `ras`
;would prematurely malform the expression, leading to
;loop/termination. As with the evaluation rule, the right association
;normalization is intended to recur, and so its implementation must
;make the choice of how to do so. nocksche chooses to convert all
;input expressions into fully right-associated normal form, and fully
;parenthesize all reduction rules.

;There are no indefinite length patterns in the reduction rules, and
;so full normalization is not required so long as each term of the
;expression is grammatical. However, this would require per redex
;checking and ras-ing, so we instead define a general procedure to
;call at the entry to reduction rules.

(define (ras a)
  (match a
   [(? noun) a]
   [`(,x ,y . ,z)
    #:when (not (null? z))
    `[,(ras (car a)) ,(ras (cdr a))]
    ]
   [`(,x ,y . ,z)
    #:when (null? z)
    `[,(ras (car a)) ,(ras (cadr a))]
    ]
   [_ 'error-not-a-noun]
   ))

;'[[1 [[2 3 4 5] 6] 7] [8 9] [10 11]]   

;; racket@nocksche> (ras '(0 (0 0 0 0 0)))
;; '(0 (0 (0 (0 (0 0)))))
;; racket@nocksche> racket@nocksche> (ras 0)
;; 0
;; racket@nocksche> (ras '(0 0))
;; '(0 0)
;; racket@nocksche> (ras '(0 (0 0)))
;; '(0 (0 0))
;; racket@nocksche> (ras '(0 (0 0 0)))
;; '(0 (0 (0 0)))
;; racket@nocksche> (ras '(0 0 0 (0 0)))
;; '(0 (0 (0 (0 0))))
;; racket@nocksche> (ras '((0 0 0) (0 0)))
;; '((0 (0 0)) (0 0))
;; racket@nocksche> (ras '((0 0 0) (0 0) 0))
;; '((0 (0 0)) ((0 0) 0))
;; racket@nocksche> (ras '((0 (0 0 0) 0) (0 0) 0))
;; '((0 ((0 (0 0)) 0)) ((0 0) 0)) 

;We will make the additional distinction between a noun and a Nock
;Expression (nexp), the latter of which includes the syntax of the
;internal representations

;the internal representations of the operators are not, strictly
;speaking, nouns, and thus their interpretation need not be
;homoiconic. If it is desired to interpret NIR (Nock IR) expressions,
;then `ras` must have a notion of nexps.

;list of Nock operators
(define nops '(wut lus tis fas hax tar))
(define (nop n) (member n nops))

(define (nexp e)
  (match e
    [`(,n ,a)
     #:when (and (nop n) (nexp a))
     #t]
    [(? noun) #t]
    [_ #f]))

(define (ras-nir a) 
  (match a
    [(? nexp) a]
    [`(,x ,y . ,z)
     #:when (and (nop x) (not (null? z)))
     `[,x ,(ras-nir (cdr a))]
     ]
    [`(,x ,y . ,z)
     #:when (and (nop x) (null? z))
     `[,x ,(ras-nir (cadr a))]
     ]
    [`(,x ,y . ,z)
    #:when (not (null? z))
    `[,(ras-nir (car a)) ,(ras-nir (cdr a))]
    ]
    [`(,x ,y . ,z)
     #:when (null? z)
     `[,(ras-nir (car a)) ,(ras-nir (cadr a))]
     ]
    [_ 'error-not-a-nexp]
    ))

(define (nock a) 
  ;must decide whether to introduce a runtime assert of the type of a
  ;- I think yes, ras'ing a is the realization of the spirit of 
  ;[a b c]=>[a [b c]]
  (tar (ras a)))

(define (tar a)
  (match a
    [_ a]))

;term rewrite nock eval
#|

decided against `neval` because the goal is to write an interpreter
for nouns called nock, and `nock` the keyword is the entry point to
the interpreter, not an element of syntax for either nouns or
nexps. Thus, this layer of indirection/abstraction is unncessary.

(define (neval n)
  (let ([p
        (match (ras n)
          [`(nock ,a) `(tar ,a)] 
          ;the right association rule is diffused across various
          ;ras/noun checks; literal implementation would prevent valid
          ;nexps (e.g. [0 1 0]) from matching any LHS pattern.
          [`(wut [,a ,b])
          ; #:when (and (noun a) (noun b))
           0]
          [`(wut ,a)
;           #:when (atom a) ;necessary? could either keep minimal
                           ;structural LHS, or remove structure from
                           ;first wut LHS, and check for cell -
                           ;regression tests should assess this.
           1]
          
          [`(tar [,a 1 ,b])
           #:when (and (noun a) (noun b)) ;noun checks correspond to
                                          ;"variables match any noun"
                                          ;(can per redex noun checks
                                          ;be replaced by deep ras
                                          ;upon entry via neval?
           b]

          [`(tar ,n) #:when (noun n) n]
          [_ n]
          )])
    (if (eq? n p) 
        p
        (neval p))))
#|

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

; https://github.com/urbit/urbit/commit/50aaa27ed1e3d181ada436456389541be8d08064
; reference tests

(define (run-tests)
  (begin
    ;functional tests
    (test 'valid-noun-atom (noun 5) #t)
    (test 'valid-noun-cell (noun '[0 0]) #t)

    (test 'invalid-noun-atom (noun 'a) #f)
    (test 'invalid-noun-cell (noun '[0]) #f)

    (test 'valid-atom (atom 5) #t)

    (test 'invalid-atom-symbol (atom 'a) #f)
    (test 'invalid-atom-negative (atom -1) #f)
    (test 'invalid-atom-cell (atom '[0 0]) #f)

    (test 'valid-cell (cell '[5 5]) #t)
    (test 'valid-cell-nested-head (cell '[[5 4] 3]) #t)
    (test 'valid-cell-nested-tail (cell '[5 [4 3]]) #t)
    (test 'valid-cell-nested-head-and-tail (cell '[[5 4] [3 2]]) #t)

    (test 'invalid-cell-long (cell '[5 5 5]) #f)
    (test 'invalid-cell-short (cell '[5]) #f)

    ;term rewrite tests
    (test 'trw-wut-cell (neval '(wut [0 0])) 0)
    (test 'trw-wut-atom (neval '(wut 0)) 1)
    (test 'trw-nock1 (neval '(nock [0 1 0])) 0) 

#| Convert to tests of `nexp`
 nocksche.rkt> (nexp '(wut [0 [0 0]]))
#t
nocksche.rkt> (nexp '(wut 0 [0 0]))
#f
nocksche.rkt> (nexp '(wut 0 0 0))
#f
nocksche.rkt> (nexp '(wut))
#f
nocksche.rkt> (nexp '(wut 0))
#t
nocksche.rkt> (nexp '(0))
#f
nocksche.rkt> (nexp 0)
#f
nocksche.rkt> 
|#    

    )) 
