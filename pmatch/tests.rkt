#lang racket

(require rackunit)
(require "./pmatch.rkt")

; Make sure table is set up properly
(check-equal? (hash-ref match-table '(?is  . single-match)) match-is)
(check-equal? (hash-ref match-table '(?or  . single-match)) match-or)
(check-equal? (hash-ref match-table '(?and . single-match)) match-and)
(check-equal? (hash-ref match-table '(?not . single-match)) match-not)
(check-equal? (hash-ref match-table '(?*  . segment-match)) segment-match)
(check-equal? (hash-ref match-table '(?+  . segment-match)) segment-match+)
(check-equal? (hash-ref match-table '(??  . segment-match)) segment-match?)
(check-equal? (hash-ref match-table '(?if . segment-match)) match-if)

; segment-match-fn should return appropriate function
(check-equal? (segment-match-fn '?*)  segment-match)
(check-equal? (segment-match-fn '?+)  segment-match+)
(check-equal? (segment-match-fn '??)  segment-match?)
(check-equal? (segment-match-fn '?if) match-if)
(check-equal? (single-match-fn '?is)  match-is)
(check-equal? (single-match-fn '?or)  match-or)
(check-equal? (single-match-fn '?and) match-and)
(check-equal? (single-match-fn '?not) match-not)

(check-equal? (pmatch '(a (?* ?x) d) '(a b c d)) '((?x b c)))
(check-equal? (pmatch '(a (?* ?x) (?* ?y) d) '(a b c d))'((?y b c) (?x)))
(check-equal? (pmatch '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d)))
              '((?y d) ((?x) b c)))

(check-equal? (pmatch '(?x ?op ?y is ?z (?if (equal? (?op ?x ?y) ?z)))
                      '(3 + 4 is 7))
              '((?z . 7) (?y . 4) (?op . +) (?x . 3)))
(check-equal? (pmatch '(?x ?op ?y (?if (?op ?x ?y))) '(3 > 4)) #f)
(check-equal? (pmatch '(?x ?op ?y (?if (?op ?x ?y))) '(4 > 3))
              '((?y . 3) (?op . >) (?x . 4)))
