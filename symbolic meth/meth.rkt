#lang racket

(require "../pmatch/pmatch.rkt")

; provide the respective packages

; infix->prefix for a given symbolic mathematical expression
(define (infix-to-prefix infix-exp)
  (cond 
    [(cons? infix-exp) infix-exp] 
    [(= (length infix-exp) 1) (infix-to-prefix (first infix-to-prefix))]
    []))

; f(f(exp)) = exp {Yes, maths is ❤️}
(define prefix-to-infix infix-to-prefix)

; Mathematical convention for variables
(define (variable-p exp)
  (member exp '(x y z m n o p q r s t u v w)))

(define pat

