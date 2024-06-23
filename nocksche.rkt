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

; Explicit implementation of an outer reduction rule:
; https://github.com/urbit/vere/commit/ddc0f8ac87a7030ba9bdafecf8917611ed3e8b71

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
;expression is grammatical.

(define (ras a)
  (match a
   [(? noun) a]
   [`(,x ,y . ,z)
    #:when (not (null? z))
    `[,(ras (car a)) ,(ras (cdr a))]
    ]
   [`(,x ,y . ,z)
    #:when (null? z)
    `[,(ras (car a)) . ,(cdr a)]
    ]
   ))

'[[1 [[2 3 4 5] 6] 7] [8 9] [10 11]]   

;the internal representations of the operators are not, strictly
;speaking nock expressions, and thus their interpretation need not be
;homoiconic. If it is desired to interpret NIR (Nock IR) expressions,
;then `ras` must have a notion of them.

(define (ras-nir a) "ras-nir" )

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

(define (run-tests)
  (begin
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

    
    )) 
