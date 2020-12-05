#lang racket

(define (simple-equal? x y)
  (if (or (symbol? x) (symbol? y))
    (eq? x y)
    (and (simple-equal? (car x) (car y))
         (simple-equal? (cdr x) (cdr y)))))

(define fail empty)

(define no-bindings '((t . t)))

(define (get-binding var bindings)
  (assoc var bindings))

(define (binding-val binding)
  (cdr binding))

(define (lookup var bindings)
  (binding-val (get-binding var bindings)))

(define (extend-bindings var val bindings)
  `((,var . ,val) . ,(if (equal? bindings no-bindings) empty bindings)))

(define (pat-match pattern input [bindings no-bindings])
  (cond
    [(eq? bindings fail) fail]
    [(empty? pattern) bindings]
    [(simple-equal? pattern input) bindings]
    [(variable? pattern) (match-variable pattern input bindings)]
    [(segment-pattern? pattern) (segment-match pattern input bindings)]
    [(and (cons? pattern) (cons? input)) (pat-match (cdr pattern)
                                                    (cdr input)
                                                    (pat-match (car pattern)
                                                               (car input)
                                                               bindings))]
    [else (error "Bad pattern/input")]))

(define (variable? x)
  (and (symbol? x) (eq? (string-ref (symbol->string x) 0) #\?)))

(define (segment-variable? x)
  (and (list? x)
       (equal? (length x) 2)
       (equal? (symbol->string (car x)) "?*")
       (symbol? (cadr x))
       (variable? (cadr x))))

(define (segment-pattern? p)
  (segment-variable? (car p)))

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
    (((?* ?x) I want (?* ?y))
     (Why do you want ?y)
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
    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(define (sublis al tree)
  (cond
    [(cons? tree)
     (cons (sublis al (car tree)) (sublis al (cdr tree)))]
    [(symbol? tree)
     (let ([p (assoc tree al)])
       (if p (cdr p) tree))]
    [else tree]))
