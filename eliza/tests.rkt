#lang racket

(require rackunit)
(require "./eliza.rkt")

(check-equal? (sublis '() '(a (b c) d (e (f)))) '(a (b c) d (e (f))))
(check-equal? (sublis '((a . z) (f . u)) '(a (b c) d (e (f))))
              '(z (b c) d (e (u))))
(check-equal? (sublis '((z . u) (x . y)) '(a (b c) d (e (f))))
              '(a (b c) d (e (f))))
(check-equal? (sublis '((a . u) (x . y)) '(a (b c) d (e (f))))
              '(u (b c) d (e (f))))
