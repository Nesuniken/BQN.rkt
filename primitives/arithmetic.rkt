#lang racket/base
(require racket/match racket/provide racket/math racket/list racket/function racket/format
         math/array "utilities.rkt")
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (add x w)
  (cond
    [(number? x)
     (if (number? w)
         (+ x w)
         (integer->char (+ x (char->integer w))))]
    [(char? x)
     (integer->char (+ (char->integer x) w))]))

(define (subtract x w)
  (cond
    [(char? w)
     (if (integer? x)
         (integer->char (- (char->integer w) x))
         (- (char->integer x) (char->integer w)))]
    [(number? w)
     (- w x)]))

(define plus
  (case-lambda
    [() 1]
    [(x  ) ((pv-monad conjugate) x)]
    [(x w) ((pv-dyad add) x w)]))

(define plus-undo
  (case-lambda
    [() 0]
    [(x  ) ((pv-monad conjugate) x)]
    [(x w) ((pv-dyad subtract) x w)]))

(define plus~undo
  (case-lambda
    [(x  ) ((pv-dyad (swap /) x 2))]
    [(x w) ((pv-dyad subtract) x w)]))

(define BQN+
  (bqn-func plus plus-undo plus~undo))

(define minus
  (case-lambda
    [() 0]
    [(x)   ((pv-monad -) x)]
    [(x w) ((pv-dyad subtract) x w)]))

(define (minus~undo x w)
  ((pv-dyad add) x w))

(define BQN-
  (bqn-func minus minus minus~undo))

(define (pv-equal test) 
  (pv-dyad (λ (x w) (if (test x w) 1 0))))

(define BQN=
  (case-lambda
    [() 1]
    [(x) (array-dims x)]
    [(x w) ((pv-equal equal?) x w)]))

(define BQN≡
  (case-lambda
    [(x)
     (if (array? x)
         (add1 (array-all-max (array-map BQN≡ x)))
         0)]
    [(x w) (if (equal? x w) 1 0)]))

(define BQN≠
  (case-lambda
    [() 0]
    [(x) (vector-ref (array-shape x) 0)]
    [(x w) ((pv-equal (λ (x w) (not (equal? w x)))) x w)]))

(define BQN≢
  (case-lambda
    [(x) (vector->array (array-shape x))]
    [(x w) (if (equal? x w) 0 1)]))

(define (pv-compare test)  
  (define (compare x w)
    (cond
      [(equal? w x) 0]
      [(real? x)
       (if (real? w) (- w x) 1)]
      [(char? x)
       (if (char? w) (- (char->integer w) (char->integer x)) -1)]))
  
  (pv-dyad (λ (x w) (if (test (compare x w) 0) 1 0))))

(define BQN≤ (pv-compare <=))

(define less-than
  (case-lambda
    [(x) (array x)]
    [(x w) ((pv-compare <) x w)]))

(define (undo-less-than x)
  (if (zero? (array-dims x))
      (array->list* x)
      (error  "<⁼ only works on unit arrays")))

(define BQN< (bqn-func less-than undo-less-than #f))

(define BQN>
  (case-lambda
    [() 0]
    [(x)
     (let ([inner-check (array-map array? x)])
       (cond
         [(array-all-and inner-check)
          (array-list->array (array->list x))]
         [(not (array-all-or inner-check))
          x]))]
    [(x w) ((pv-compare >) x w)]))

(define BQN≥
  (case-lambda
    [() 1]
    [(x w) ((pv-compare >=) x w)]))

(define (bqn-min x w)
  (cond
    [(real? x)
     (if (real? w)
         (min x w)
         x)]
    [(and (char? x) (char? w))
     (if (char<? x w) x w)]
    [w]))

(define BQN⌊
  (case-lambda
    [()    -inf.0]
    [(x)   ((pv-monad floor) x)]
    [(x w) ((pv-dyad bqn-min) x w)]))

(define (bqn-max x w)
  (cond
    [(real? x)
     (if (real? w)
         (max x w)
         w)]
    [(and (char? x) (char? w))
     (if (char>? x w) x w)]
    [x]))

(define BQN⌈
  (case-lambda
    [() +inf.0]
    [(x) ((pv-monad ceiling) x)]
    [(x w) ((pv-dyad bqn-max) x w)]))

(define (span [x 0] [w 0])
  ((pv-dyad (λ (x* w*)(- (add1 w*) x*))) x w))

(define (span~undo x w)
  ((pv-dyad (λ (x* w*) (sub1 (+ x* w*))))))

(define BQN¬ (bqn-func span span span~undo))

(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(define (sign x)
  (if (equal? x 0) 0 (/ x (magnitude x))))

(define times
  (case-lambda
    [() 1]
    [(x)   (sign x)]
    [(x w) ((pv-dyad *) w x)]))

(define (times-undo x w)
  ((pv-dyad /) w x))

(define times~undo
  (case-lambda
    [(x)   ((pv-monad sqrt) x)]
    [(x w) ((pv-dyad /) w x)]))

(define BQN× (bqn-func times times-undo times~undo))

(define bqn-divide
  (case-lambda
    [() 1]
    [(x)   ((pv-monad /) x)]
    [(x w) ((pv-dyad  /) w x)]))

(define bqn-divide~undo (pv-dyad *))

(define BQN÷ (bqn-func bqn-divide bqn-divide bqn-divide~undo))

(define square-root
  (case-lambda
    [(x)   ((pv-monad sqrt) x)]
    [(x w) ((pv-dyad (λ (x* w*) (expt x* (/ w*)))))]))

(define square-root-undo
  (case-lambda
    [(x)   ((pv-monad (λ (n) (* n n))))]
    [(x w) ((pv-dyad expt) w x)]))

(define (square-root~undo x w)
  (/ (log x w)))

(define BQN√ (bqn-func square-root square-root-undo square-root~undo))

(define pow
  (case-lambda
    [() 1]
    [(x)   (exp x)]
    [(x w) ((pv-dyad expt) w x)]))

(define (pow~undo x w)
  ((pv-dyad (λ (x* w*) (expt x* (/ w*))))))

(define BQN⋆ (bqn-func pow #f pow~undo))

(define BQN\|
  (case-lambda
    [(x) (magnitude x)]
    [(x w) (pv-dyad modulo) w x]))