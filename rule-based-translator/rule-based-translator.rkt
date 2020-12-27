#lang racket

(require "../pmatch/pmatch.rkt")

(define (rule-based-translator input rules [matcher pmatch] [rule-if first] [rule-then rest] [action sublis])
  (ormap (lambda (rule)
           (let ([result (apply matcher (apply rule-if rule) input)])
             (if (not (eq? result fail))
               (apply action `(,result ,(apply rule-then rule)))
               #f)))
         rules))
