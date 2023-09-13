#lang racket/base
(require math/array)
(provide (all-defined-out))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(define BQN⊑
  (case-lambda
    [(x) (array-ref x (make-vector (array-dims x) 0))]
    [(x w) (array-indexes-ref
            x (array-map (λ (n) (if (array? n) (array->vector n) n)) w))]))

(define (undo-error name) (error "could not invert" name ))

(define (find-fill x)
  (cond
    [(array-all-and (array-map number? x)) 0]
    [(array-all-and (array-map char? x) #\space)]))

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

(struct bqn-func (call undo ~undo)
  #:property prop:procedure 0)

(define  undo bqn-func-undo)
(define ~undo bqn-func-~undo)
