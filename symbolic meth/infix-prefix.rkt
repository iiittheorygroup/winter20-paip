#lang racket

; Using this version for a fully parenthesized infix notation
(define (infix->prefix expression)
  (cond
    [(symbol? expression) expression]
    [(equal? (length expression) 1) (infix->prefix (car expression))]
    [()]
    []
    )
  )

(define prefix->infix infix->prefix)
