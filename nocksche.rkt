;;  nocksche.rkt
;;  2024
;;  ICFP
;;  Nock 4K in Racket
;;  Racket v8.12

#lang racket
(require racket/match)

;prior work:
;https://github.com/nvasilakis/Noq/commit/5e8c9fbeec13450d7d712c6a0c520f652031a27f
;https://github.com/runtimeverification/knock/blob/master/k-src/nock.k - lifts the right associativity rule to the type/hand-crafted heuristics level
;future work:
;formal specification would assure, e.g., that all redexes preserve
;the [tree structure of expressions|grammati cality of nexps]

;discussion elsewhere
#|In my ongoing Nock interpreter project, I have found the specification of syntactic normalization as an explicit reduction rule to be one of the more interesting aspects of fidelitously translating the informal Nock spec to an executable implementation.

That is, including [a b c] -> [a [b c]] as a symbolic reduction rule, rather than natural language commentary (as used to describe atoms, cells, and nouns), strongly implies that a valid Nock implementation not only can accept and coerce agramatical input, but that doing so is an operation on par with wut, fas, tis, tar, etc.

One then either must build the interpreter such that normalization is a first class operation, or decide that the rule is meant to inform the spirit but not the letter of an executable Nock, and push such normalization outside the redexes, to the type level, or a validation pass on expressions prior to entry to the main redexes via nock(a).

This points to the more general matter, which is the non-trivial amount of discretion required in producing a formal Nock grammar. In the strict sense, Nock is an evaluator of nouns, which are homoiconic, but the Nock pseudocode spec also provides an "internal representation" syntax, in which reductions are defined in terms of operation symbols (+, /, =, *, #, ?) that are neither cells nor atoms.

Ought /[2 /[2 [[3 4] 2]]] be evaluable? If so, that means an interpreter must have a notion Nock internal representations (NIRs) as first class expressions. Ought /[2 /[2 [3 4] 2]]? If so, then the right association mechanism of the rule [a b c] -> [a [b c]] must be able to handle NIRs as well as nouns.

The efforts at formal verification above generally either adopt a relaxed attitude towards pseudocode fidelity, lifting [a b c] -> [a [b c]] out of the main redex body into the type level, or incorporating the rule into validation of input expressions, or they hardcode heuristics to normalize select agrammatical expressions, allowing them to be handled first-class.

A fully formal definition of Nock would need to address the question of its grammar; separately, it should prove that all reduction rules preserve the grammaticality of expressions.

To take the nested fas example further, nothing in the spec forbids using the nock keyword as an operator, interchangeably with *. It would require the subsidization of decision on the part of an implementer to determine whether nock( nock(a)) is valid input.
|#


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
(define nops '(nock wut lus tis fas hax tar)) 
;we interpret the keywork "nock" as transparently synonymous with
;"tar". If other code builds nexps with nested `nock`s, then that is
;at minimum a stylistic error on the caller's side.

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

;function call nock
(define (nock a) 
  ;must decide whether to introduce a runtime assert of the type of
  ;`a` - I think yes, ras'ing `a` is the realization of the spirit of
  ;[a b c]=>[a [b c]] (see note ras-nir-trw).
  (tar (ras a)))

(define (fas a)
  (match a
    [ `[1 ,a]                       a ]
    [ `[2 [,a ,b]]                  a ]
    [ `[3 [,a ,b]]                  b ]
;#[(a + a) b c]      #[a [b /[(a + a + 1) c]] c]
;#[(a + a + 1) b c]  #[a [/[(a + a) c] b] c]
;^variables clearly do not match any noun, but are informed by context to be atoms or cells
    [ `[,a ,b] 
      #:when 
      (and (even? a) (> a 2))       (fas `[2 ,(fas `[,(/ a 2) ,b])]) ] 
    ;absent either (> 2 a) or (cell b) guards, [2 atom] causes a
    ;cycle
    [ `[,a ,b]
      #:when (and (odd? a) (> a 3)) (fas `[3 ,(fas `[,(/ (- a 1) 2) ,b])]) ] 
    ;absent either (> 3 a) or (cell b) guards, [3 atom] causes a
    ;cycle
    [ _                'error-fas]))

(define (tar a) ;no need for noun checks on a, given that standard
                ;usage should follow from the nock entry
                ;point. Defining `tar` at the top level is easier to
                ;test, or use modularly; a stricter variant would nest
                ;the definition within `nock`, or make it private via
                ;module mechanisms.
  (match a ;TODO `-->` or `==>` macro
    [ `[,a [[,b ,c] ,d]] `[,(tar `[,a [,b ,c]]) ,(tar `[,a ,d])] ] 
    [ `[,a [0 ,b]]       (fas `[,b ,a]) ]
    [ `[,a [1 ,b]]       b] ;K combinator
    [ `[,a [2 [,b ,c]]]  (tar `[,(tar `[,a ,b]) ,(tar `[,a ,c])]) ]
    ;^ compare S-combinator, Sxyz = xz(yz)
    #|

**Encoding the SKI calculus as Nock expressions**

I combinator:

    Ix = x
    xI = x
    [x I] = x
**  I := [0 1]
    xI = nock([x [0 1]]) = /[1 x] = x

K combinator:

    Kxy = x
    yxK = x
    [[y x] K] = x
**  K := [0 3]
    yxK = [[y x] K] = nock([[y x] [0 3]]) = /[3 [y x]] = x

S combinator:

    Sxyz = xz(yz)  
    zyxS = ((zy)(zx))
    [z [y x]]S = *[*[z y] *[z x]]
**  S := [[0 2] [[1 2] [0 3]]]
    zyxS = [z [[y x] S]] = nock([z [[y x] [[0 2] [[1 2] [0 3]]]]]) =
    [...TODO, but see Derivation below...]
    *[z [2 [y x]]]
    *[*[z y] *[z x]]

    Derivation:
    S := [[b c] d]

    [[z [y x]] [[b c] d]]
    [*[[z [y x]] [b c]] *[[z [y x]] d]]
    [*[[z [y x]] [0 2]] *[[z [y x]] d]]
    [z *[[z [y x]] d]]
    [z *[[z [y x]] [[e f] g]]]
    [z [*[[z [y x]] [1 2]] *[[z [y x]] g]]]
    [z [2 *[[z [y x]] g]]]
    [z [2 *[[z [y x]] [0 3]]]]
    [z [2 [y x]]]
    [z 2 y x]



    Kxy = x
    *[y 1 x] = x <- suggestively similar, but currently doubtful
    
**Scratch work**
    yxK = x
    *[y K x] = x
    Sxyz = ((xz) (yz))
    *[z 2 x y] = *[*[z x] *[z y]]
    zyxS = ((z y) (z x)) ;reverse polish combinators
    *[z S y x] = *[*[z y] *[z x]]
    Ix = (SK_)x = SK_x = Kx(_x) = x (lazy eval)
    xI = ((x_)K)S = *[x S _ K] = *[*[x _] *[x K]] = *[*[x 0 1] *[x 1 ]]
**Scratch work**

    Ix = x
    xI = x
    [x I] = x
**  I := [0 1]
    xI = [x [0 1]] = /[1 x] = x

    Kxy = x
    yxK = x
    [[y x] K] = x <- TODO: rederive as [y [x K]]?
    K := [2 b c]
         *[*[[y x] b] *[[y x] c]]
         *[*[[y x] b] *[[y x] [e f] d]]
         *[*[[y x] b] [*[[y x] e f] *[[y x] d]]]
         *[*[[y x] b] [*[[y x] 1 1] *[[y x] d]]] <- recurring problem of needing to pull y or x from the subject on its own. fas seems necessary; can nock1 be reduced to nock0?
                             
    [y 1 x] = x

    Kxy = x
    yxK = x
    [[y x] K] = x
**  K := [0 3]
    yxK = [[y x] K] = [[y x] [0 3]] = /[3 [y x]] = x

    [y [x K]] = x <- does not work, nothing to match on in redexes
    K := [] 

    Sxyz = xz(yz)  
    zyxS = ((zy)(zx))
    [z [y x]]S = *[*[z y] *[z x]]
    S := [[b c] d]

    [[z [y x]] [[b c] d]]
    [*[[z [y x]] [b c]] *[[z [y x]] d]]
    [*[[z [y x]] [0 2]] *[[z [y x]] d]]
    [z *[[z [y x]] d]]
    [z *[[z [y x]] [[e f] g]]]
    [z [*[[z [y x]] [1 2]] *[[z [y x]] g]]]
    [z [2 *[[z [y x]] g]]]
    [z [2 *[[z [y x]] [0 3]]]]
    [z [2 [y x]]]
    [z 2 y x]

 **   S := [[0 2] [[1 2] [0 3]]]

 **The following does not work, due to a potential mismatch between definitions of combinator-qua-combinator and combinator as lambda term**
    Ix = SKKx = x
    xI = xKKS = [x [0 3] [0 3] [[0 2] [[1 2] [0 3]]]]

    *[x [[0 3] [[0 3] [[0 2] [[1 2] [0 3]]]]]]
    [*[x [0 3]] *[x [[0 3] [[0 2] [[1 2] [0 3]]]]]] <- problem, incomplete arguments to K, aka [0 3], i.e. x not a cell
    [*[x [0 3]] *[x [[0 3] [[0 2] [[1 2] [0 3]]]]]]

    Ix = SKKx = x
    xI = ((xK)K)S = [[[x [0 3]] [0 3]] [[0 2] [[1 2] [0 3]]]]
    
    *[[[x [0 3]] [0 3]] [[0 2] [[1 2] [0 3]]]]
    [*[[[x [0 3]] [0 3]] [0 2]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [/[2 [[x [0 3]] [0 3]]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [[x [0 3]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [[x [0 3]] [*[[[x [0 3]] [0 3]] [1 2]] *[[[x [0 3]] [0 3]] [0 3]]]]
    [[x [0 3]] [*[[[x [0 3]] [0 3]] [1 2]] *[[[x [0 3]] [0 3]] [0 3]]]]



    Ix = SKSx = ((Kx) (Sx))
    xI = xSKS = x 2 S K = *[*[x S] *[x K]] = *[*[x 2] *[x 1]] 

    
    
    |#
    [_ 'error-tar]))

;term rewrite nock eval
(define (neval n)
  (match n
    [`(nock ,a)
     #:when (noun a) ;variables match any noun, so check for noun status
     (neval `(tar ,a))]
    [`(tar ,a)
     #:when (noun a)
     `(tar ,a) ;control looping by calling, or not, nevalo on RHS
     ]
      ))

(define (neval-loop n)
  (let ([p
         (match n
           [`(nock ,a)
            #:when (noun a)
            `(tar ,a)]
           [`[,a ,b . ,c] 
            (ras-nir n)] ;Note ras-nir-trw: in other impl, pull this
                         ;outside the main match, to run on entry,
                         ;rather than each reduction pass. The
                         ;inclusion of the syntactic normalization
                         ;rule within the redexes introduces a runtime
                         ;component to the determination of the
                         ;grammar of valid nock expressions. If
                         ;agrammatical expressions are excluded from
                         ;evaluation, either statically, or via a
                         ;check outside the main pattern collection
                         ;(e.g., upon first entry of `nock`), then
                         ;this rule need not be included in the
                         ;pattern collection.
           [`(tar ,a)
            #:when (noun a)
            `(tar ,a)]
           [_ 'error-not-evaluable]
           )])
    (if (equal? n p) ;break out loop or not to single check outside
                     ;the main match, rather than per redex
        p
        (neval-loop p))))


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
|#

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

    (test 'fas-1 (fas '[1 0]) 0)
    (test 'fas-2 (fas '[2 [1 2]]) 1)
    (test 'fas-3 (fas '[3 [1 2]]) 2)
    (test 'fas-4 (fas '[4 [[3 4] 2]]) 3)
    (test 'fas-5 (fas '[5 [[3 4] 2]]) 4)
    (test 'fas-4-atom (fas '[4 0]) 'error-fas)
    (test 'fas-5-atom (fas '[5 0]) 'error-fas)

    (test 'tar0_1 (tar '[0 [0 1]]) 0)
    (test 'tar0_2 (tar '[[1 2] [0 2]]) 1)
    (test 'tar0_3 (tar '[[1 2] [0 3]]) 2)
    (test 'tar0_4 (tar '[[[3 4] 2] [0 4]]) 3)
    (test 'tar0_5 (tar '[[[3 4] 2] [0 5]]) 4)
    (test 'tar0_6 (tar '[[[3 4] [5 6]] [0 6]]) 5)
    (test 'tar0_7 (tar '[[[3 4] [5 6]] [0 7]]) 6)
    
    (test 'tar1-atom (tar '[0 [1 2]]) 2)
    (test 'tar1-cell (tar '[0 [1 [2 3]]]) '[2 3])

    ;term rewrite tests
;    (test 'trw-wut-cell (neval '(wut [0 0])) 0)
 ;   (test 'trw-wut-atom (neval '(wut 0)) 1)
  ;  (test 'trw-nock1 (neval '(nock [0 1 0])) 0) 

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
