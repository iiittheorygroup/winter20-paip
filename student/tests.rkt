#lang racket

(require rackunit)
(require "./student.rkt")

; Sanity checks if expr data structure is changed
(check-equal? (expr-rhs (expr '= 2 3)) 3)
(check-equal? (expr-lhs (expr '= 2 3)) 2)
(check-equal? (expr-op (expr '= 2 3)) '=)
(check-equal? (expr? (expr '= 2 3)) #t)
(check-equal? (expr? '(= 2 3)) #f)

; Case 1
(check-equal? (isolate (expr '= 'x 2) 'x) (expr '= 'x 2))
; Case 2
(check-equal? (isolate (expr '= 2 'x) 'x) (expr '= 'x 2))
(check-equal? (isolate (expr '= 2 (expr '+ 'x 1)) 'x) (expr '= 'x (expr '- 2 1)))
(check-equal? (isolate (expr '= 2 (expr '- 'x 1)) 'x) (expr '= 'x (expr '+ 2 1)))
(check-equal? (isolate (expr '= 2 (expr '* 'x 1)) 'x) (expr '= 'x (expr '/ 2 1)))
(check-equal? (isolate (expr '= 2 (expr '/ 'x 1)) 'x) (expr '= 'x (expr '* 2 1)))
; Case 3
(check-equal? (isolate (expr '= (expr '* 'x 4) 2) 'x) (expr '= 'x (expr '/ 2 4)))
; Case 4
(check-equal? (isolate (expr '= (expr '* 4 'x) 2) 'x) (expr '= 'x (expr '/ 2 4)))
; Case 5
(check-equal? (isolate (expr '= (expr '/ 4 'x) 2) 'x) (expr '= 'x (expr '/ 4 2)))

(check-equal? (prefix->infix (expr '+ 1 2)) '(1 + 2))
(check-equal? (prefix->infix (expr '+ (expr '- 4 1) 2)) '((4 - 1) + 2))

(check-equal? (no-unknown (expr '+ 1 2)) #t)
(check-equal? (one-unknown (expr '+ 'x 2)) 'x)
