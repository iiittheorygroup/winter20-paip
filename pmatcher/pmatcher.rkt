#lang racket

(provide match-table)

; Grammar for the pattern matcher
; pat ::= var         (match any one expression)
;       | constant    (match just this atom)
;       | segment-pat (match against a sequence)
;       | single-pat  (match against single expression)
;       | (pat . pat) (match car and cdr)
;
; single-pat ::= (?is var predicate) (test predicate on expression)
;              | (?or pat ...)          (match any pattern on one expression)
;              | (?and pat ...)         (match every pattern on one expression)
;              | (?not pat ...)         (succeed if pattern(s) do not match)
;
; segment-pat ::= ((?*var) ...) (match zero or more expressions)
;               | ((?+var) ...) (match one or more expressions)
;               | ((??var) ...) (match zero or one expression)
;               | ((?if exp) ...) (test if exp is true)
;
; var ::= ?char       (a symbol starting with ?)
; constant ::= symbol (any non variable symbol)

(define match-table
  (make-hash
    '(((?is  single-match)  . match-is)
      ((?or  single-match)  . match-or)
      ((?and single-match)  . match-and)
      ((?not single-match)  . match-not)
      ((?*   segment-match) . segment-match)
      ((?+   segment-match) . segment-match+)
      ((??   segment-match) . segment-match?)
      ((?if  segment-match) . match-if))))
