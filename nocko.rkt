;;  nocko.rkt
;;  2024
;;  Nock 4K in miniKanren
;;  Racket v8.12

#lang racket

(require minikanren)
(require minikanren/numbers)

;"A noun is an atom or a cell.  An atom is a natural number.  A cell
;is an ordered pair of nouns." - these translate directly into mK:

(define (nouno i)
  (conde
   [(atomo i)]
   [(cello i)]))

(define (atomo i)
  (fresh (d)
	 (== `(nat . ,d) i)
	 (nato d)))

(define (nato i)
  (fresh (d)
	 (conde
	  [(== '(0) i)]
	  [(== '(1) i)]
	  [(== `(0 . ,d) i)
	   (=/= '(0) d)
	   (nato d)]
	  [(== `(1 . ,d) i)
	   (=/= '(0) d)
	   (nato d)])))
   
(define (cello i)
  (fresh (a d)
	 (== `(,a ,d) i)
	 (nouno a)
	 (nouno d)))

;Reduce by the first matching pattern; variables match any noun.

;A sequential pattern match implicitly accumulates conditions as each
;previous match clause fails. To replicate the behavior of a
;sequential pattern match in a pure relational language, fully
;disjunct match criteria must be explicitly encoded. This is
;straightforward for the supermajority of reduction expressions
;(redexes), but will be a point of discussion late in the
;implementation. The directive to "reduce by the first matching
;pattern" suggests a direct term rewriting approach; this is adopted,
;though an alternative structure of the program using functions
;instead of expression tags is presented.

;nock(a)             *a

;The first design decision is whether to build a single (nocko)
;procedure, or separate the nops (nock operations) into individual
;relations, and if so, with what grouping.  There is also the choice
;between translating `nock(a)` as an expression to be evaluated by an
;interpreter (the `nevalo` design) or considerating `nock` to be the
;name of the evaluator (the `nocko` design). The latter is chosen.

;We begin with a monolithic `nocko` relation, using pattern matching
;on quoted expressions to perform the redexes.

(define (nocko i o) (taro i o))

(define (wuto i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,b] i) (== 0 o) ]
	  [ (atomo i)       (== 1 o) ]
	  )))

(define (luso i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,b] i) (== `[lus ,i] o) ]
	  [ (atomo i)       (add1o i o) ] ;requires defining add1o for atoms
	  )))

(define (tiso i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,a] i)           (== 0 o) ]
	  [ (== `[,a ,b] i) (=/= a b) (== 1 o) ]
	  )))

(define (faso i o)
  (fresh (a b ind num resa)
	 (conde
	  [ (== `[(nat 1) ,a] i)                (== a o) ]
	  [ (== `[(nat 0 1) [,a ,b]] i)         (== a o) ]
	  [ (== `[(nat 1 1) [,a ,b]] i)         (== b o) ]
	  [ (== `[,ind ,b] i) 
	    (== `(nat . ,num) ind) (=/= '(0 1) num)
	    (pluso a a num) (== `(nat . ,a) resa) (faso `[,resa ,b]) ]
	  ))
  )

(define (taro i o) "")

#|
(define (nevalo i o)
  (fresh (a b neg)
	 (noun a) (noun b)
	 (conde
	  [(== `(tar (,a ((nat 0) ,b))) i)
	   (== `(fas (,b ,a)) o)]
	  #;[(nexp i neg)
	   (=/= 't neg)]
	  ))
|#

#|
(define (nexp i o)
  (fresh (a b)
;	 (noun a) (noun b)
	 (conde
	  [(== `(tar (,a ((nat 0) ,b))) i)
	   (== 't o)])))   
|#

#|

a: noun ->
a: atom -> |_
a: cell -> 
[b c]
  b: noun -> T
c: atom -> |_
c: cell -> 
[b [d e]]
    d: atom >=12 -> |_
  d: cell -> [b [[f g] e]] -> T
d: atom <12 -> T
d: 0 -> 
  e: noun -> T
d: 1 -> 
  e: noun -> T
d: 2 ->
  e: atom -> |_
e: cell -> T
d: 3 -> 
  e: noun -> T
d: 4 -> 
  e: nount -> T
d: 5 -> 
  e: atom -> |_
  e: cell -> [b [7 [f g]]] -> T
e: cell -> T
d: 6 ->
  e: atom -> |_
e: cell -> [b [6 [f g]]]
  f: noun -> T
g: atom -> |_
g: cell -> [b [6 [f [h i]]]]
  h: noun -> T
  i: noun -> T
d: 7 ->
  e: atom -> |_
  e: cell -> [b [7 [f g]]] -> T
d: 8 ->
  e: atom -> |_
  e: cell -> [b [8 [f g]]] -> T
d: 9 ->
  e: atom -> |_
  e: cell -> [b [9 [f g]]] -> T
d: 10 ->
  e: atom -> |_
  e: cell -> [b [10 [f g]]]
    g: noun -> T
  f: atom -> |_
     cell -> [b [10 [[h i] g]]]
    h: noun -> T
    i: noun -> T
d: 11 ->
  e: atom -> |_
  e: cell -> [b [11 [f g]]]
    g: noun -> T
  f: atom -> |_
     cell -> [b [11 [[h i] g]]]
    h: noun -> T
    i: noun -> T
d: 11 ->
  e: atom -> |_
  e: cell -> [b [11 [f g]]] -> T

|#

(define (npluso m n o) 
  (fresh (a b c)
	 (== `(nat . ,a) m) (=/= '() a) ;(nato a/b/c) causes divergence
	 (== `(nat . ,b) n) (=/= '() b)
	 (== `(nat . ,c) o) (=/= '() c)
	 (conde
	  [(== '(0) a) (== '(0) b) (== '(0) c)]
	  [(== '(0) a) (=/= '(0) b) (pluso '() b c)]
	  [(=/= '(0) a) (== '(0) b) (pluso a '() c)]
	  [(=/= '(0) a) (=/= '(0) b) (pluso a b c)]
	  ))
  )

(define (nadd1o i o) (npluso i '(nat 1) o))

#|
(define (add1o i o)
  (fresh (num res)
	 (conde
	  [ (== '(nat 0) i) (== '(nat 1) o) ]
	  [ (== `(nat . ,num) i) (=/= '() num)
	    (== `(nat . ,res) o) 
	    (pluso num '(1) res) ] ;pluso call must be last, else run* diverges
	  )))
|#

;the nock operator `*`, meaning "evaluate", is pronouned "tar" (sTAR),
;which is used as the tag for `*` terms.

;[a b c] [a [b c]] 

;nock expressions are right associative. This redex suggests that, in
;addition to scanning the reduction rules from top to bottom, the nexp
;must be scanned from right to left. Given a non-fully delimited nexp
;[a1 a2 a3 ... ak], matching from left to right (comparable to
;outermost first reduction) to produce [a1 [a2 a3 ... ak]] requires a3
;... ak to already be understood as encompassed by `c` in the pattern
;[a b c]. However, if matched from right to left, [a1 ... ak-1 ak] can
;be parsed as [a1 ... [ak-1 ak]], with `c` matched to ak in terminal
;position. For both strategies, the recursive cases must be handled,
;whether of more than three terms at the same depth of the expression,
;or decomposition of any of terms a, b, or c into terms that must be
;auto-consed. The intent of a fully right-associated expression is
;conveyed by this line of the specification, but the mechanism is
;implementation dependent. If, for example, c is a deeply nested,
;non-fully right associated expression, e.g. [0 0 0 0], then there
;must be a way to focus on c, and perform an internal pattern match.


;autocons cannot match a term with four nouns, because it's pattern
;does not admit a list (of potentially arbitary size >=3), with the
;tail captured in the pattern variable `c`, but strictly an ordered
;tuple of three elements. Thus, an unparsed nock([a 6 b c d e]) -> *[a
;6 b c d e] -> *a -> LOOP. For autocons to group `d` and `e` within
;the cell `f`, the autocons window must intercept the six element
;tuple and check it from right to left, grouping as necessary.

;Kelvin versioning is an example of a finite critical developmental
;period in the lifecycle of a piece of software, with some precedent
;in Knuth's Pi versioning of TeX, which will similarly close its canon
;upon his death.
