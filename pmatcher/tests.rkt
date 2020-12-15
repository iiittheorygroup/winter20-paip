#lang racket

(require rackunit)
(require "./pmatcher.rkt")

; Make sure table is set up properly
(check-equal? (hash-ref match-table '(?is  . single-match)) 'match-is)
(check-equal? (hash-ref match-table '(?or  . single-match)) 'match-or)
(check-equal? (hash-ref match-table '(?and . single-match)) 'match-and)
(check-equal? (hash-ref match-table '(?not . single-match)) 'match-not)
(check-equal? (hash-ref match-table '(?*  . segment-match)) 'segment-match)
(check-equal? (hash-ref match-table '(?+  . segment-match)) 'segment-match+)
(check-equal? (hash-ref match-table '(??  . segment-match)) 'segment-match?)
(check-equal? (hash-ref match-table '(?if . segment-match)) 'match-if)

; segment-match-fn should return appropriate function
(check-equal? (segment-match-fn '?*) 'segment-match)
(check-equal? (segment-match-fn '?+) 'segment-match+)
(check-equal? (segment-match-fn '??) 'segment-match?)
(check-equal? (segment-match-fn '?if) 'match-if)
(check-equal? (single-match-fn '?is) 'match-is)
(check-equal? (single-match-fn '?or) 'match-or)
(check-equal? (single-match-fn '?and) 'match-and)
(check-equal? (single-match-fn '?not) 'match-not)
