#lang racket

(require "../pmatch/pmatch.rkt")

(provide expr
         expr-op
         expr-lhs
         expr-rhs
         expr?
         isolate)

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

(define (expr op lhs rhs)
  (list 'expr op lhs rhs))
(define (expr-op e)
  (second e))
(define (expr-lhs e)
  (third e))
(define (expr-rhs e)
  (fourth e))
(define (expr? e)
  (and (list? e)
       (eq? (car e) 'expr)))

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

(define (inverse-op op)
  (second (assoc op operators-and-inverses)))

(define (in-expr x e)
  (or (eq? x e)
      (and (expr? e)
           (or (in-expr x (expr-lhs e))
               (in-expr x (expr-rhs e))))))

(define (commutative? op)
  (member op '(+ * =)))
