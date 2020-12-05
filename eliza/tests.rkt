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

(check-equal? (simple-equal? 'a 'a) #t)
(check-equal? (simple-equal? 'eval 'apply) #f)
(check-equal? (simple-equal? '(eval) '(apply)) #f)

(check-equal? (get-binding 'a '((a . z) (f . u))) '(a . z))
(check-equal? (get-binding 'b '((a . z) (f . u))) #f)

(check-equal? (binding-val '(a . z)) 'z)
(check-equal? (binding-val '(b . y)) 'y)

(check-equal? (extend-bindings 'a 'y '((a . z) (f . u)))
              '((a . y) (a . z) (f . u)))
(check-equal? (extend-bindings 'a 'z '((a . z) (f . u)))
              '((a . z) (a . z) (f . u)))
(check-equal? (extend-bindings 'b 'z '((a . z) (f . u)))
              '((b . z) (a . z) (f . u)))

(check-equal? (pat-match '(i love lisp) '(i hate lisp) fail) '())
(check-equal? (pat-match '(i love lisp) '(i hate lisp)) '())
(check-equal? (pat-match '() '(i hate lisp)) '((t . t)))
(check-equal? (pat-match '(i love lisp) '(i hate lisp)) '())
(check-equal? (pat-match '(?X loves ?Y) '(Bharath loves lisp))
              '((?Y . lisp) (?X . Bharath)))
(check-equal? (pat-match '((?* ?X) loves ?Y) '(Bharath really loves lisp))
              '((?Y .  lisp) (?X .  (Bharath really))))

(check-equal? (variable? '?X) #t)
(check-equal? (variable? 'X) #f)
(check-equal? (variable? '?foo) #t)
(check-equal? (variable? 'foo) #f)

(check-equal? (segment-variable? '(?* ?X)) #t)
(check-equal? (segment-variable? '?X) #f)
(check-equal? (segment-variable? 'foo) #f)

(check-equal? (segment-pattern? '(not a segment pattern)) #f)
(check-equal? (segment-pattern? '((?* ?x) foo bar)) #t)
(check-equal? (segment-pattern? '(?x foo bar)) #f)

(check-equal? (position 'a '(a b c) 0 1) 1)
(check-equal? (position 'a '(a b c) 1 1) 0)
(check-equal? (position 'a '(a b c) 2 1) 0)
(check-equal? (position 'z '(a b c) 1 1) 0)

(check-equal? (segment-match '((?* ?Y) apples are liked by (?* ?X))
                             '(apples are liked by Ryuk)
                             '((t . t)))
              '((?X Ryuk) (?Y)))
(check-equal? (segment-match '((?* ?X) are not liked by Ryuk)
                             '(apples are liked by Ryuk)
                             '((t . t)))
              '())
