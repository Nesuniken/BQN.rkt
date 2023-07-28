#lang racket/base
(require racket/function racket/match math/array)
(provide (all-defined-out))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(define (undo-error name) (error name "isn't invertable"))

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