#lang racket/base
(require math/array racket/match racket/provide racket/math racket/list BQN/prim-utils)
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define  π pi)

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

(define/match ((BQN+ #:undo? [undo? #f]) . args)
  [(_ (list)) 0]
  [(_ (list x))
   ((pv-monad conjugate) x)]
  [(#f (list x w))
   ((pv-dyad add) x w)]
  [(#t (list x w))
   ((pv-dyad subtract) x w)])

(define/match ((BQN- #:undo? [undo? #f]) . args)
  [(_ (list)) 0]
  [(_ (list x))
   ((pv-monad -) x)]
  [(_ (list x w))
   ((pv-dyad subtract) x w)])

(define (pv-equal test) 
  (pv-dyad (λ (x w) (if (test x w) 1 0))))

(define/match ((BQN= #:undo? [undo? #f]) . args)
  [(#t   _ ) (undo-error #\=)]
  [(#f '( )) 1]
  [(#f (list x  )) (array-dims x)]
  [(#f (list x w)) ((pv-equal equal?) x w)])

(define/match ((BQN≡ #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\≡)]
  [(#f (list (? array? x)))
   (+ 1 (array-all-max (array-map BQN≡ x)))]
  [(#f (list _)) 0]
  [(#f (list x w)) (if (equal? x w) 1 0)])

(define/match ((BQN≠ #:undo? [undo? #f]) . args)
  [(#t   _) (undo-error #\≠)]
  [(#f '( )) 0]
  [(#f (list x  ))
   (vector-ref (array-shape x) 0)]
  [(#f (list x w))
   ((pv-equal (λ (x w) (not (equal? w x)))) x w)]
  )

(define/match ((BQN≢ #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\≢)]
  [(#f (list x  )) (vector->array (array-shape x))]
  [(#f (list x w)) (if (equal? x w) 0 1)])

(define (pv-compare test)  
  (define (compare x w)
    (cond
      [(equal? w x) 0]
      [(real? x)
       (if (real? w) (- w x) 1)]
      [(char? x)
       (if (char? w) (- (char->integer w) (char->integer x)) -1)]))
  
  (pv-dyad (λ (x w) (if (test (compare x w) 0) 1 0))))

(define ((BQN≤ #:undo? [undo? #f]) x w)
  (if undo? (undo-error #\≤) (pv-compare <= x w)))

(define/match ((BQN< #:undo? [undo? #f]) . args)
  [(#t (list x  )) (first (array->list x))]
  [(#f (list x  )) (array x)]
  [(#f (list x w)) ((pv-compare <) x w)])

(define/match ((BQN> #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\>)]
  [(#f '()) 0]
  [(#f (list x))
   (let ([inner-check (array-map array? x)])
     (cond
       [(array-all-and inner-check)
        (array-list->array (array->list x))]
       [(not (array-all-or inner-check))
        x]))]
  [(#f (list x w)) ((pv-compare >) x w)])

(define/match ((BQN≥ #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\≥)]
  [(#f '()) 1]
  [(#f (list x w)) (pv-compare >=)])

(define/match ((BQN⌊ #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\⌊)]
  [(#f '()) -inf.0]
  [(#f (list x)) ((pv-monad floor) x)]
  [(#f (list x w))
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
     ((pv-dyad bqn-min) x w))])

(define/match ((BQN⌈ #:undo? [undo? #f]) . args)
  [(#t _) (undo-error #\⌈)]
  [(#f '()) +inf.0]
  [(#f (list x  )) ((pv-monad ceiling) x)]
  [(#f (list x w))
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
     ((pv-dyad bqn-max) x w))])

(define ((BQN¬ #:undo? [undo? #f]) [x 0] [w 0])
  ((pv-dyad (λ (x* w*)(- (+ 1 w*) x*))) x w))

(define ((BQN⍳ #:undo? [undo? #f]) x [w 0])
  (if undo?
      (undo-error #\⍳)
      (make-rectangular w x)))

(define BQN×
  (let ([sign (lambda (x) (if (equal? x 0) 0 (/ x (magnitude x))))])
    (pv-func (vector 1 sign (cons * /)))))

(define/match ((BQN÷ #:undo? [undo? #f]) . args)
  [(_ (list x))
   ((pv-monad /) x)]
  [(_ (list x w))
   ((pv-dyad (swap /)) x w)])

(define BQN√
  (pv-func
   (vector #f
           (cons sqrt (λ (x) (* x x)))
           (cons (λ (x w) (expt x (/ w))) (swap expt)))))

(define BQN⋆
  (pv-func (vector 1 (cons exp log) (cons (swap expt) log))))

(define BQN\|
  (pv-func (vector #f magnitude (swap modulo))))

(define BQN∧
  (procedure-reduce-keyword-arity BQN× 2 '() '(#:undo?)))

(define ((BQN∨ #:undo? [undo? #f]) x w)
  (if undo?
      (undo-error #\∨)
      (pv-dyad (λ (x w) (- (+ x w) (* x w))))))
