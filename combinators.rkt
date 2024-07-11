;Naive strict application

;Obligatory curried application
;S := [[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]]
(define Sc '[[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]])

;K := [8 [[1 1] [0 1]]]
(define Kc '[8 [[1 1] [0 1]]]

;Adaptive partial application
;S := 
(define S '[])

;K := [6 [[3 [0 1]] [[0 3] [[8 [[1 1] [0 1]]]]]]]
(define K '[6 [[3 [0 1]] [[0 3] [[8 [[1 1] [0 1]]]]]]])

#|

Z combinator

Zgv = g(Zg)v

(?) g 8 Z v = *[[*[g Z] g] v]

g 8 f v = *[[*[g f] g] v]

a1 8 K v = *[[*[a1 K] K] v] 
           *[[Ka1 K] v]         

Partial evaluation:
  *[b *[a N]] = *[[b a] N]

*[a1 K] = [a1 K] = K'
*[a2 K'] = a1
*[[a2 a1] K] = a1

*[ a1 [[b c] d] ]
[*[a1 b c] *[a1 d]]

*[a1 K] = K'

K := [8 [[1 K2] [0 1]]]

*[a1 8 [1 K2] [0 1]]
*[[*[a b] a] c]
*[[*[a1 [1 K2]] a1] [0 1]]
*[[K2 a1] [0 1]]
/[1 [K2 a1]]
[K2 a1] = K'


*[a2 K'] = a1

*[a2 [K2 a1]]
K2 := 1
*[a2 [1 a1]]
a1

K := [8 [[1 1] [0 1]]]

*[y *[x K]] = x 

^Here we use `tar` as backtick is used in iota/jot, to force
application of an argument with a combinator.  This expression is a
nexp, not a noun; there is furthermore no LHS pattern that matches the
whole expression. nock([y *[x K]]) would fail to evaluate under a
rigid implementation that only accepted nouns upon entry. Does `tar`
create a lexical scope for interpretation, or must the global
expression be valid prior to any reduction being performed?

Kp[artial] := [8 [[1 1] [0 1]]]

***K := if cell?(arg) [0 3] else Kp

[x K] = *[x 6 cell? [0 3] Kp]

***K := [6 [[3 [0 1]] [[0 3] [[8 [[1 1] [0 1]]]]]]] TODO: Derive Nock6 and Nock8 from primitives (-1 to 5), plus operators.

More speculative: are there universal combinators that do not require
partial application (or this partial application trick)?

Sx = (Sx) = S'
S'y = (Sx)y = S''
S''z = ((Sx)y)z = ((xz)(yz))

Curried application only:

*[x S] = [S2 x] = S'

*** S := [[1 S2] [0 1]]

*[x S]
*[x [[b c] d]] 
[*[x [b c]] *[x d]]
[*[x [1 S2]] *[x d]]
[S2 *[x d]]
[S2 *[x [0 1]]]
[S2a [S2b x]]
S'

*[y S'] = [S3 [y x]] = S''

*[y [S2a [S2b x]]]
*[y [[S2a1 S2a2] [S2b x]]]
*[*[y [S2a1 S2a2]] *[y [S2b x]]]



[S3 [y x]]
S''

#[2 b c]                  
#[1 [b /[3 c]] c]
#[1 [b c.t] c]
[b c.t]

x S1 -> S1 x

[x [1 [1 S3]] [[1 [0 1]] [[1 1] [0 1]]]]
[*[x [b c]] *[x d]]
[*[x [1 [1 S3]]] *[x [[1 [0 1]] [[1 1] [0 1]]]]]
[[1 S3] *[x [[1 [0 1]] [[1 1] [0 1]]]]]
[[1 S3] [*[x [1 [0 1]]] *[x [[1 1] [0 1]]]]]
[[1 S3] [[0 1] *[x [[1 1] [0 1]]]]]
[[1 S3] [[0 1] [*[x [1 1]] *[x [0 1]]]]]
[[1 S3] [[0 1] [1 *[x [0 1]]]]]
[[1 S3] [[0 1] [1 x]]]

[y [S2 x]] -> [S3 [x y]]

[y [1 S3] [[0 1] [1 x]]]
*[y [b c] [[0 1] [1 x]]]
[*[y b c] *[y [0 1] [1 x]]]
[*[y 1 S3] *[y [0 1] [1 x]]]
  *[y [0 1] [1 x]] -> [y x]
  *[y [0 1] [1 x]]
  [*[y 0 1] [y 1 x]]
  [y x]
[*[y 1 S3] [y x]]
[S3 [y x]]

[z [S3 [y x]]] -> *[*[z y] *[z x]]

[z [2 [y x]]]
*[*[z y] *[z x]]

S3 := 2

**Obligatorily partially evaluable S combinator**
****  S := [[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]]

Using the partially evaluable forms of S and K, and a Nock syntax
which allows for passing arbitrary `tar` annotated NIR nexps to the
Nock interpreter, Unlambda [0] can be implemented, substituting `*` for
```.

[0] https://esolangs.org/wiki/Unlambda

**  S := [[0 2] [[1 2] [0 3]]]

SKKx
*[[x K K] S]

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

    Kx = Kx
    (Kx)y = x

    xK = xK
    y(xK) = x
    xK = Kx
    [y [K x]] = x

    [x K] 
    [x [[b c] d]]
    [*[x [b c]] *[x d]]
    [*[x [b c]] *[x [0 1]]]
    [*[x [b c]] x]
    [*[x [2 [e f]]] x]
    [*[*[x [1 2]] *[x f]] x]
    [*[2 *[x [1 [[0 1] [1 ]]]]] x] <- need recursive combinator to duplicate f
    
    [[[2 [[1 2] f]] [0 1]] x]

    K is: 
    1) the combinator which when applied to any value x produces
    the function (Kx), that returns x when applied to any argument
    y, (Kx)y = x, aka partially evaluated.

    2) the combinator which detects the arity of its arguments, and
    either produces Kx or x in response.

    Need either the Z combinator or a "detect atom v cell" component
    to simulate partial application.

    Detect atom v cell:

S combinator:

    Sxyz = xz(yz)  
    zyxS = ((zy)(zx))
    [z [y x]]S = *[*[z y] *[z x]]
**  S := [[0 2] [[1 2] [0 3]]]
    zyxS = [[z [y x]] S] = nock([z [[y x] [[0 2] [[1 2] [0 3]]]]]) =
    nock([z [[y x] [[0 2] [[1 2] [0 3]]]]])
    *[z [[y x] [[0 2] [[1 2] [0 3]]]]]
    [*[z [y x]] *[z [[0 2] [[1 2] [0 3]]]]]


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

  
**Scratch work**

    Kxy = x
    *[y 1 x] = x <- suggestively similar, but currently doubtful

    yxK = x
    *[y K x] = x
    Sxyz = ((xz) (yz))
    *[z 2 x y] = *[*[z x] *[z y]]
    zyxS = ((z y) (z x)) ;reverse polish combinators
    *[z S y x] = *[*[z y] *[z x]]
    Ix = (SK_)x = SK_x = Kx(_x) = x (lazy eval)
    xI = ((x_)K)S = *[x S _ K] = *[*[x _] *[x K]] = *[*[x 0 1] *[x 1 ]]

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
    xI = ((xK)K)S = [[[x [0 3]] [0 3]] [[0 2] [[1 2] [0 3]]]] <- cannot do this, because would have to deeply nest x inside subject of S, as opposed to simple prepending.
    
    *[[[x [0 3]] [0 3]] [[0 2] [[1 2] [0 3]]]]
    [*[[[x [0 3]] [0 3]] [0 2]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [/[2 [[x [0 3]] [0 3]]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [[x [0 3]] *[[[x [0 3]] [0 3]] [[1 2] [0 3]]]]
    [[x [0 3]] [*[[[x [0 3]] [0 3]] [1 2]] *[[[x [0 3]] [0 3]] [0 3]]]]
    [[x [0 3]] [*[[[x [0 3]] [0 3]] [1 2]] *[[[x [0 3]] [0 3]] [0 3]]]]



    Ix = SKSx = ((Kx) (Sx))
    xI = xSKS = x 2 S K = *[*[x S] *[x K]] = *[*[x 2] *[x 1]] 

    xI = x_KS
**Scratch work**    
    
    |#











































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































