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
    [(and (cons? pattern) (cons? input)) (pat-match (cdr pattern)
                                                    (cdr input)
                                                    (pat-match (car pattern)
                                                               (car input)
                                                               bindings))]
    [else error "Bad pattern/input"]))

(define (variable? x)
  (and (symbol? x) (eq? (string-ref (symbol->string x) 0) #\?)))

(define (match-variable var input bindings)
  (let ([binding (get-binding var bindings)])
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))
