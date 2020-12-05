#lang racket

(provide sublis
         simple-equal?
         get-binding
         binding-val
         extend-bindings
         pat-match
         variable?
         segment-variable?
         segment-pattern?
         position
         segment-match)

; select random element from list l
(define (random-elt l)
  (list-ref l (random (length l))))

; perform substitutions in tree according to the association list
(define (sublis al tree)
  (cond
    [(cons? tree)
     (cons (sublis al (car tree)) (sublis al (cdr tree)))]
    [(symbol? tree)
     (let ([p (assoc tree al)])
       (if p (cdr p) tree))]
    [else tree]))

; symbol and list equality
(define (simple-equal? x y)
  (if (or (symbol? x) (symbol? y))
    (eq? x y)
    (and (simple-equal? (car x) (car y))
         (simple-equal? (cdr x) (cdr y)))))

(define fail empty)

(define no-bindings '((t . t)))

; get pair with key var in the association list bindings
(define (get-binding var bindings)
  (assoc var bindings))

; get value for the pair binding
(define (binding-val binding)
  (cdr binding))

; add new key value pair to the association list
(define (extend-bindings var val bindings)
  `((,var . ,val) . ,(if (equal? bindings no-bindings) empty bindings)))

(define (pat-match pattern input [bindings no-bindings])
  (cond
    ; failure only occurs when bindings are empty
    [(eq? bindings fail) fail]
    ; no more elements to match, return current bindings
    [(empty? pattern) bindings]
    ; try to match using simple equality
    [(simple-equal? pattern input) bindings]
    [(variable? pattern) (match-variable pattern input bindings)]
    [(segment-pattern? pattern) (segment-match pattern input bindings)]
    ; recur if nothing works out
    [(and (cons? pattern) (cons? input)) (pat-match (cdr pattern)
                                                    (cdr input)
                                                    (pat-match (car pattern)
                                                               (car input)
                                                               bindings))]
    ; give up and just fail
    [else fail]))

; variables are of the form '?x, '?y etc
(define (variable? x)
  (and (symbol? x) (eq? (string-ref (symbol->string x) 0) #\?)))

; segment-variables are of the form '(?* ?x), '(?* ?y) etc
(define (segment-variable? x)
  (and (list? x)
       (equal? (length x) 2)
       (equal? (symbol->string (car x)) "?*")
       (symbol? (cadr x))
       (variable? (cadr x))))

; segment-patterns are of the form '((?* ?x) hello there),
; '((?* ?x) i feel happy)
(define (segment-pattern? p)
  (and (list? p) (segment-variable? (car p))))

; Incrementally match pattern against input to see if any of the bindings work
; out. For example, segment matching '((?* ?x) a b (?* ?x)) against
; '(c d a b a b c d a b) results in first binding '(x . (c d)), which fails in
; the recursive call and next binding '(x . (c d a b)) which works out.
(define (segment-match pattern input bindings [start 0])
  (let ([var (second (first pattern))]
        [pat (rest pattern)])
    (if (empty? pat)
      (match-variable var input bindings)
      (let ([pos (position (first pat) input start 1)])
        (if (zero? pos)
          fail
          (let ([b2 (pat-match pat
                               (drop input (- pos 1))
                               (match-variable var
                                               (take input (- pos 1))
                                               bindings))])
            (if (equal? b2 fail)
              (segment-match pattern input bindings (+ start 1))
              b2)))))))

; return position of element e in list l after skipping s occurrences
; c = current position in list
(define (position e l s c)
  (cond
    [(empty? l) 0]
    [(let ([h (car l)]
           [t (cdr l)])
       (if (equal? e h)
         (if (zero? s) c (position e t (- s 1) (+ c 1)))
         (position e t s (+ c 1))))]))

; if matching binding already exsist for var, return bindings as it is. If it
; doesn't exist extend binding to include it
(define (match-variable var input bindings)
  (let ([binding (get-binding var bindings)])
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

; rule = '(pattern response-1 respose-2 ... response-3)
(define (rule-pattern rule) (first rule))
(define (rule-responses rule) (rest rule))

(define eliza-rules
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem))
    (((?* ?x) i want (?* ?y))
     (Why do you want ?y ?)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really  think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying no just to be negative?))
    (((?* ?x) i was (?* ?y))
     (Were you really?)
     (Perhaps i already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) i feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) i felt (?* ?y))
     (What other feelings do you have?))))

(define (eliza input)
  (ormap (lambda (rule)
           (let ([result (pat-match (rule-pattern rule) input)])
             (if (not (equal? result fail))
               (flatten (sublis (switch-viewpoint result)
                       (random-elt (rule-responses rule))))
               #f)))
         eliza-rules))

(define (switch-viewpoint words)
  (sublis '((i . you) (you . i) (me . you) (am . are))
          words))
