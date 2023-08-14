#lang racket/base
(require racket/function racket/match racket/class racket/format math/array)
(provide (all-defined-out))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(define/match ((BQN⊑ [undo 0]) . args)
  [(1 _) (undo-error #\⊑)]
  [(0 (list x)) (array-ref x (make-vector (array-dims x) 0))]
  [(0 (list x w)) (array-indexes-ref
                    x (array-map (λ (n) (if (array? n) (array->vector n) n)) w))])

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

(define/match (((pv-func arities) [undo? #f]) . args)
  [((vector (and (not #f) ids) _ _) u? (list))
   ((pv-func ids) u?)]
  [((vector _ (and (not #f) monads) _ ...) u? (list x))
   ((pv-func monads) x u?)]
  [((vector _ _ (and (not #f) dyads)) u? (list x w))
   ((pv-func dyads) x w u?)]
  
  [((cons f f-inv) u? _)
   (apply (pv-func (if u? f-inv f)) args)]

  [(id #f (list)) id]
  [(f  #f (list x))
   ((pv-monad f) x)]
  [(f  #f (list x w))
   ((pv-dyad f) x w)]
  )

(struct bqn-func (call undo ~undo)
  #:property prop:procedure 0)

(define undo bqn-func-undo)
(define ~undo bqn-func-~undo)