#lang racket/base
(require math/array racket/match racket/provide racket/math racket/list)
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

(define/match ((pv-func arities) #:undo? [undo? #f] . args)
  [((vector (and (not #f) ids) _ _) u? (list))
   ((pv-func ids) #:undo? u?)]
  [((vector _ (and (not #f) monads) _ ...) u? (list x))
   ((pv-func monads) x #:undo? u?)]
  [((vector _ _ (and (not #f) dyads)) u? (list x w))
   ((pv-func dyads) x w #:undo? u?)]
  
  [((cons f f-inv) u? _)
   (apply (pv-func (if u? f-inv f)) args)]

  [(id #f (list)) id]
  [(f  #f (list x))
   ((pv-monad f) x)]
  [(f  #f (list x w))
   ((pv-dyad f) x w)]
  )

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

(define/match (BQN+ #:undo? [undo? #f] . args)
  [(_ (list)) 0]
  [(_ (list x))
   ((pv-monad conjugate) x)]
  [(#f (list x w))
   ((pv-dyad add) x w)]
  [(#t (list x w))
   ((pv-dyad subtract) x w)])

(define/match (BQN- #:undo? [undo? #f] . args)
  [(_ (list)) 0]
  [(_ (list x))
   ((pv-monad -) x)]
  [(_ (list x w))
   ((pv-dyad subtract) x w)])

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
    (pv-func (vector -inf.0 floor bqn-min))))

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
    (pv-func (vector +inf.0 ceiling bqn-max))))

(define BQN∧
  (procedure-reduce-keyword-arity BQN× 2 '() '(#:undo?)))

(define BQN∨ (pv-dyad (λ (x w) (- (+ x w) (* x w)))))

(define (BQN¬ [x 0] [w 0] #:undo? [undo? #f])
  ((pv-dyad (λ (x* w*)(- (+ 1 w*) x*))) x w))

(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(define BQN×
  (let ([sign (lambda (x) (if (equal? x 0) 0 (/ x (magnitude x))))])
    (pv-func (vector 1 sign (cons * /)))))

(define/match (BQN÷ #:undo? [undo? #f] . args)
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

(define BQN-PIPE
  (pv-func (vector #f magnitude (swap modulo))))
