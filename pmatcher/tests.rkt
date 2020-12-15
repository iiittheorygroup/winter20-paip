#lang racket

(require rackunit)
(require "./pmatcher.rkt")

(check-equal? (hash-ref match-table '(?is  single-match)) 'match-is)
(check-equal? (hash-ref match-table '(?or  single-match)) 'match-or)
(check-equal? (hash-ref match-table '(?and single-match)) 'match-and)
(check-equal? (hash-ref match-table '(?not single-match)) 'match-not)
(check-equal? (hash-ref match-table '(?*  segment-match)) 'segment-match)
(check-equal? (hash-ref match-table '(?+  segment-match)) 'segment-match+)
(check-equal? (hash-ref match-table '(??  segment-match)) 'segment-match?)
(check-equal? (hash-ref match-table '(?if segment-match)) 'match-if)
