#lang racket

(provide match-table
         segment-match-fn
         single-match-fn
         segment-match
         match-is
         match-or
         match-and
         match-not
         segment-match
         segment-match?
         segment-match+
         match-if
         pmatch)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; Grammar for the pattern matcher
; pat ::= var         (match any one expression)
;       | constant    (match just this atom)
;       | segment-pat (match against a sequence)
;       | single-pat  (match against single expression)
;       | (pat . pat) (match car and cdr)
;
; single-pat ::= (?is var predicate) (test predicate on expression)
;              | (?or pat ...)          (match any pattern on one expression)
;              | (?and pat ...)         (match every pattern on one expression)
;              | (?not pat ...)         (succeed if pattern(s) do not match)
;
; segment-pat ::= ((?*var) ...) (match zero or more expressions)
;               | ((?+var) ...) (match one or more expressions)
;               | ((??var) ...) (match zero or one expression)
;               | ((?if exp) ...) (test if exp is true)
;
; var ::= ?char       (a symbol starting with ?)
; constant ::= symbol (any non variable symbol)

(define (segment-match-fn x)
  (when (symbol? x) (hash-ref match-table (cons x 'segment-match))))

(define (single-match-fn x)
  (when (symbol? x) (hash-ref match-table (cons x 'single-match))))

; segment-patterns are of the form '((?* ?x) hello there),
; '((?* ?x) i feel happy)
(define (segment-pattern? p)
  (and (cons? p)
       (cons? (car p))
       (symbol? (caar p))
       (segment-match-fn (caar p))))

; single-patterns are of the form '(?is x predicate), '(?and . patterns)
; HACK : Leaky abstraction! Relies on the fact that match-table is a hashmap
(define (single-pattern? p)
  (and (cons? p) (hash-has-key? match-table (car p)) (single-match-fn (car p))))

(define (segment-matcher pattern input bindings)
  (apply (segment-match-fn (caar pattern)) (list pattern input bindings)))

(define (segment-match pattern input bindings [start 0])
  (let ([var (cadar pattern)]
        [pat (rest pattern)])
    (if (null? pat)
      (match-variable var input bindings)
      (let ([pos (first-match-pos (first pat) input start)])
        (if (null? pos)
          fail
          (let ([b2 (pmatch pat
                            (drop input pos)
                            (match-variable var (take input pos) bindings))])
            (if (eq? b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(define (first-match-pos pat input start)
  (cond
    [(and (symbol? pat) (not (variable? pat)))
     (position pat input start 0)]
    [(< start (length input)) start]
    [else null]))

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

(define (segment-match+ pattern input bindings)
  (segment-match pattern input bindings 1))

(define (segment-match? pattern input bindings)
  (let ([var (second (first pattern))]
        [pat (rest pattern)])
    (or (pmatch (cons var pat) input bindings)
        (pmatch pat input bindings))))

(define (single-matcher pattern input bindings)
  (apply (single-match-fn (car pattern)) (cdr pattern) input bindings))

; succeed and bind var v if the input satisfies predicate p
(define (match-is v/p input bindings)
  (let* ([var (first v/p)]
         [pred (second v/p)]
         [new-binds (pmatch var input bindings)])
    (if (or (eq? new-binds fail)
            (not (apply pred input)))
      fail
      new-binds)))

; succeed if all patterns match the input
(define (match-and patterns input bindings)
  (cond [(eq? bindings fail) fail]
        [(null? patterns) bindings]
        [else (match-and (cdr patterns)
                          input
                          (pmatch (car patterns) input bindings))]))

; succed if at least one pattern matches the input
(define (match-or patterns input bindings)
  (if (null? bindings)
    fail
    (let ([new-binds (pmatch (car patterns) input bindings)])
      (if (eq? new-binds fail)
        (match-or (rest patterns) input bindings)
        new-binds))))

; Succeed if none of the patterns match the input
(define (match-not patterns input bindings)
  (if (match-or patterns input bindings) fail bindings))

(define (match-if pattern input bindings)
  (and (eval
         `(let ,(map (lambda (x) (list (car x) (cdr x))) bindings)
            ,(second (first pattern))) ns) ; SLICK!
       (pmatch (rest pattern) input bindings)))

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
; (define (simple-equal? x y)
;   (if (or (symbol? x) (symbol? y))
;     (eq? x y)
;     (and (simple-equal? (car x) (car y))
;          (simple-equal? (cdr x) (cdr y)))))
(define (simple-equal? x y)
  (cond
    [(or (symbol? x) (symbol? y))
    (eq? x y)]
    [(and (cons? x) (cons? y)) (and (simple-equal? (car x) (car y))
                                    (simple-equal? (cdr x) (cdr y)))]
    [else #f]))

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

; if matching binding already exsist for var, return bindings as it is. If it
; doesn't exist extend binding to include it
(define (match-variable var input bindings)
  (let ([binding (get-binding var bindings)])
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

(define (pmatch pattern input [bindings no-bindings])
  (cond
    ; failure only occurs when bindings are empty
    [(eq? bindings fail) fail]
    ; no more elements to match, return current bindings
    [(empty? pattern) bindings]
    ; try to match using simple equality
    [(simple-equal? pattern input) bindings]
    [(variable? pattern) (match-variable pattern input bindings)]
    [(segment-pattern? pattern) (segment-matcher pattern input bindings)]
    [(single-pattern? pattern) (single-matcher pattern input bindings)]
    ; recur if nothing works out
    [(and (cons? pattern) (cons? input))
     (pmatch (cdr pattern)
             (cdr input)
             (pmatch (car pattern) (car input) bindings))]
    ; give up and just fail
    [else fail]))

(define match-table
  (make-hash
    `(((?is  . single-match)  . ,match-is)
      ((?or  . single-match)  . ,match-or)
      ((?and . single-match)  . ,match-and)
      ((?not . single-match)  . ,match-not)
      ((?*   . segment-match) . ,segment-match)
      ((?+   . segment-match) . ,segment-match+)
      ((??   . segment-match) . ,segment-match?)
      ((?if  . segment-match) . ,match-if))))
