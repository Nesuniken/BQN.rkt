#lang racket/base
(require math/array racket/provide racket/math racket/list)
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define-syntax-rule (swap f) (λ (x w) (f w x)))
(define  ∞ +inf.0)
(define ¯∞ -inf.0)

(define  π    pi)
(define ¯π (- pi))

(define (pv-monad monad)
  (lambda (x)
    (if (array? x)
        (array-map (pv-monad monad) x)
        (monad x))))

(define (pv-dyad dyad)
  (lambda (x w)
    (cond
      [(array? w)
       (array-map (pv-dyad dyad) (if (array? x) x (array x)) w)]
      [(array? x)
       (array-map (pv-dyad dyad) x (array w))]
      [(dyad x w)])))

(define (pv-func monad dyad [id #f])
  (if id
      (case-lambda
        [() id]
        [(x)   ((pv-monad monad) x)]
        [(x w) ((pv-dyad   dyad) x w)])
      (case-lambda
        [(x)   ((pv-monad monad) x)]
        [(x w) ((pv-dyad   dyad) x w)])))

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

(define BQN+
  (pv-func conjugate add 0))

(define BQN+⁼
  (pv-func conjugate (swap subtract) 0))

(define BQN-
    (pv-func - subtract 0))

(define BQN-⁼ BQN-)

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
         (+ 1 (array-all-max (array-map BQN≡ x)))
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
       (if (real? w) (exact-truncate (- w x)) 1)]
      [(char? x)
       (if (char? w) (- (char->integer w) (char->integer x)) -1)]))
  
  (pv-dyad (λ (x w) (if (test (compare x w) 0) 1 0))))

(define BQN≤ (pv-compare <=))

(define BQN<
  (case-lambda
    [(x) (array x)]
    [(x w) ((pv-compare <) x w)]))

(define (BQN<⁼ x)
  (first (array->list x)))

(define BQN>
  (case-lambda
    [() 0]
    [(x) (let ([inner-check (array-map array? x)])
           (cond
             [(array-all-and inner-check)
              (array-list->array (array->list x))]
             [(not (array-all-or inner-check))
              x]))]
    [(x w) ((pv-compare >) x w)]))

(define BQN≥
  (case-lambda
    [() 1]
    [(x w)(pv-compare >=)]))

(define BQN⌊
  (let ([bqn-min
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (min x w)
                  x)]
             [(and (char? x) (char? w))
              (if (char<? x w) x w)]
             [w]))])
    (pv-func floor bqn-min -inf.0)))

(define BQN⌈
  (let ([bqn-max
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (max x w)
                  w)]
             [(and (char? x) (char? w))
              (if (char>? x w) x w)]
             [x]))])
    (pv-func ceiling bqn-max +inf.0)))

(define BQN∧
  (pv-dyad *))

(define (BQN¬ x [w 0])
  (- (+ 1 w) x))

(define BQN¬⁼ BQN¬)

(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(define BQN×
  (let ([sign (lambda (x) (if (equal? x 0) 0 (/ 1 (magnitude x))))])
    (pv-func sign * 1)))

(define (BQN×⁼ x w)
  (pv-dyad /))

(define BQN÷ (pv-func / (swap /)))

(define BQN÷⁼ BQN÷)

(define BQN√
  (pv-func sqrt (λ (x w) (expt x (/ w)))))

(define BQN√⁼
  (pv-func (λ (x  ) (* x x))
           (λ (x w) (expt w x))))

(define BQN⋆
  (pv-func exp (swap expt) 1))

(define BQN⋆⁼
  (pv-func log log))

(define BQN-PIPE
  (pv-func magnitude (swap modulo)))
