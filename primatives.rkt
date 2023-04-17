#lang racket
(require math/array br/macro racket/provide)
(provide
 #%module-begin #%top #%app #%datum #%top-interaction
 (all-from-out math/array)
 (matching-identifiers-out #rx"^BQN" (all-defined-out))
 (matching-identifiers-out #rx"•" (all-defined-out)))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(define  ∞ +inf.0)
(define ¯∞ -inf.0)

(define  π    pi)
(define ¯π (- pi))

(define (•strict x)
  (if (array? x)
      (array-strict x)
      x))

(define (•show x)
  (display x))

(define (pv-monad monad)
  (lambda (x)
    (if (array? x)
        (array-map (pv-monad monad) x)
        (monad x))))

(define (pv-dyad dyad)
  (lambda (x w)
    (cond
      [(array? w)
       (let ([arrayX (if (array? x) x (array x))])
         (array-map (pv-dyad dyad) arrayX w))]
      [(array? x)
       (array-map (pv-dyad dyad) x (array w))]
      [(dyad x w)])))

(define (pv-func monad dyad)
  (case-lambda
    [(x)   ((pv-monad monad) x)]
    [(x w) ((pv-dyad   dyad) x w)]))

(define BQN+
  (let ([add
         (lambda (x w)
           (cond
             [(number? x)
              (if (number? w)
                  (+ x w)
                  (integer->char (+ x (char->integer w))))]
             [(char? x)
              (integer->char (+ (char->integer x) w))]))])
    (pv-func conjugate add))
  )

(define BQN-
  (let ([subtract
         (lambda (x w)
           (cond
             [(char? w)
              (if (integer? x)
                  (integer->char (+ x (- (char->integer w))))
                  (- (char->integer x) (char->integer w)))]
             [(number? w)
              (+ x (- w))]))])
    (pv-func - subtract))
  )

(define (pv-equal test) 
  (pv-dyad (λ (x w) (if (test x w) 1 0))))

(define BQN=
  (case-lambda
    [(x) (array-dims x)]
    [(x w) ((pv-equal equal?) x w)]))

(define BQN≡
  (case-lambda
    [(x)
     (if (array? x)
         (+ 1 (array-all-max (array-map BQN≡ (array-flatten x))))
         0)]
    [(x w) (if (equal? x w) 1 0)]))

(define BQN≠
  (case-lambda
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

(define BQN>
  (case-lambda
    [(x) (let ([inner-check (array-map array? x)]
               [x-list (array->list x)])
           (cond
             [(array-all-and inner-check)
              (array-append* x-list)]
             [(not (array-all-or inner-check))
              (array-append* (map BQN< x))]))]
    [(x w) ((pv-compare >) x w)]))

(define BQN≥ (pv-compare >=))

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
    (pv-func floor bqn-min)))

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
    (pv-func ceiling bqn-max)))

(define BQN∧
  (pv-dyad *))

(define BQN¬ 
  (let ([span (λ (x w) (- (+ 1 w) x))])
   (pv-func (λ (x) (span x 0)) span)))

(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(define BQN×
  (let ([sign (lambda (x) (if (real? x) (sgn x) (/ x (magnitude x))))])
    (pv-func sign *)))

(define BQN÷ (pv-func / (swap /)))

(define BQN√
  (pv-func sqrt (λ (x w) (expt x (/ w)))))

(define BQN-PIPE
  (pv-func magnitude (swap modulo)))

(define BQN⊣
  (case-lambda
    [(x)   x]
    [(x w) w]))

(define BQN⊢
  (case-lambda
    [(x)   x]
    [(x w) x]))

(define (deshape x)
  (if (array? x)
      (array-flatten x)
      (array x)))