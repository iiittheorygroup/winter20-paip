#lang racket

(require "../pmatch/pmatch.rkt")

(provide expr
         expr-op
         expr-lhs
         expr-rhs
         expr?
         isolate
         no-unknown
         one-unknown
         prefix->infix)

(define student-rules
  (make-hash
    `((((?* ?x) |.|)  ?x)
      (((?* ?x) |.| (?* ?y))  (?x ?y))
      ((if (?* ?x) |,| then (?* ?y) (?x ?y)))
      ((if (?* ?x) then (?* ?y) (?x ?y)))
      ((if (?* ?x) |,| (?* ?y) (?x ?y)))
      (((?* ?x) |,| and (?* ?y)))
      ((find (?* ?x) and (?* ?y)) ((= to-find-1 ?x) (= to-find-2 ?y)))
      ((find (?* ?x)) (= to-find ?x))
      (((?* ?x) equals (?* ?y)) (= ?x ?y))
      (((?* ?x) same as (?* ?y)) (= ?x ?y))
      ((?* ?x) = (?* ?y))
      (((?* ?x) is equal to (?* ?y)) (= ?x ?y))
      (((?* ?x) is (?* ?y)) (= ?x ?y))
      (((?* ?x) - (?* ?y)) (- ?x ?y))
      (((?* ?x) minus (?* ?y)) (- ?x ?y))
      ((difference between (?* ?x) and (?* ?y)) (- ?y ?x))
      ((difference (?* ?x) and (?* ?y)) (- ?y ?x))
      (((?* ?x) plus (?* ?y)) (+ ?x ?y))
      (((?* ?x) + (?* ?y)) (+ ?x ?y))
      ((sum (?* ?x) and (?* ?y)) (+ ?x ?y))
      ((product (?* ?x) and (?* ?y)) (* ?x ?y))
      (((?* ?x) * (?* ?y)) (* ?x ?y))
      (((?* ?x) times (?* ?y)) (* ?x ?y))
      (((?* ?x) / (?* ?y)) (/ ?x ?y))
      (((?* ?x) per (?* ?y)) (/ ?x ?y))
      (((?* ?x) divided by (?* ?y)) (/ ?x ?y))
      ((half (?* ?x)) (/ ?x 2))
      ((one half (?* ?x)) (/ ?x 2))
      ((twice (?* ?x)) (* ?x 2))
      ((square (?* ?x)) (* ?x ?x))
      (((?* ?x) % less than (?* ?y))  (* ?y (/ (- 100 ?x) 100)))
      (((?* ?x) % more than (?* ?y))  (* ?y (/ (+ 100 ?x) 100)))
      (((?* ?x) % (?* ?y)) (* (/ ?x 100) ?y)))))

(struct expr (op lhs rhs) #:transparent)

(define (isolate e x)
  (cond
    ; Case 1 e : x = A => x = A
    [(eq? (expr-lhs e) x)
     e]
    ; Case 2 e : A = f(x) => f(x) = A
    [(in-expr x (expr-rhs e))
     (isolate (expr '=
                    (expr-rhs e) ; f(x)
                    (expr-lhs e)) ; A
              x)]
    ; Case 3 e : f(x) * A = B => f(x) = B / A
    [(in-expr x (expr-lhs (expr-lhs e)))
     (isolate (expr '=
                    (expr-lhs (expr-lhs e)) ; f(x)
                    (expr (inverse-op (expr-op (expr-lhs e))) ; * -> /
                          (expr-rhs e) ; B
                          (expr-rhs (expr-lhs e)))) ; A
              x)]
    ; Case 4 e : A * f(x) = B => f(x) = B / A
    [(commutative? (expr-op (expr-lhs e)))
     (isolate (expr '=
                    (expr-rhs (expr-lhs e)) ; f(x)
                    (expr (inverse-op (expr-op (expr-lhs e))) ; * -> /
                          (expr-rhs e) ; B
                          (expr-lhs (expr-lhs e)))) ; A
              x)]
    ; Case 5 e : A / f(x) = B => f(x) = A / B
    [else
      (isolate (expr '=
                     (expr-rhs (expr-lhs e)) ; f(x)
                     (expr
                       (expr-op (expr-lhs e)) ; /
                       (expr-lhs (expr-lhs e)) ; A
                       (expr-rhs e))) ; B
               x)]))

(define operators-and-inverses
  '((+ -)
    (- +)
    (* /)
    (/ *)
    (= =)))

(define binary-operators
  '(+ - * / =))

(define (inverse-op op)
  (second (assoc op operators-and-inverses)))

(define (in-expr x e)
  (or (eq? x e)
      (and (expr? e)
           (or (in-expr x (expr-lhs e))
               (in-expr x (expr-rhs e))))))

(define (commutative? op)
  (member op '(+ * =)))

(define (no-unknown e)
  (cond
    [(unknown? e) #f]
    [(atom? e) #t]
    [(no-unknown (expr-lhs e))
     (no-unknown (expr-rhs expr))]
    [else '()]))

(define (one-unknown e)
  (cond
    [(unknown? e) e]
    [(and (not (expr? e)) (atom? e)) #f]
    [(no-unknown (expr-lhs e))
     (one-unknown (expr-rhs e))]
    [(no-unknown (expr-rhs e))
     (one-unknown (expr-lhs e))]
    [else '()]))

(define (binary-expr? e)
  (and (expr? e)
       (member (expr-op e) binary-operators)))

; slick!
(define (prefix->infix e)
  (if (binary-expr? e)
    (map prefix->infix (list (expr-lhs e) (expr-op e) (expr-rhs e)))
    e))

; atom - any object that is not a cons
(define (atom? a)
  (not (cons? a)))

(define (unknown? e)
  (symbol? e))
