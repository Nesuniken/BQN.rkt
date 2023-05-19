#lang racket/base
(require math/array racket/match racket/undefined racket/provide racket/function racket/vector racket/list)
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define ((BQN⁼ F) #:undo? [undo? #f] . args)
  (apply F args #:undo? (not undo?)))

(define BQN˙ const)

(define ((BQN˜ F) x [w undefined] #:undo? [undo? #f] )
  (if (equal? w undefined)
      (F x x #:undo? undo?)
      (F w x #:undo? undo?)))

(define ((BQN¨ F) #:undo? [undo? #f] . args)
  (apply array-map (curry F #:undo? undo?) args))

(define ((cells F) x)
  (for/array #:shape (array-shape x)
    ([major (in-array-axis x)])
    (F major)))

(define/match ((BQN˘ F) #:undo? [undo? #f] . args)
  [(_ #t _) (BQN˘ (curry F #:undo? #t) args)]
  [(_ #f (list x)) ((cells F) x)]
  [(_ #f (list x w))
   (for/array #:shape (array-shape x)
     ([major-x (in-array-axis x)] [major-w (in-array-axis w)])
     (F major-x major-w))])

(define ((BQN⌜ F) x [w undefined] #:undo? [undo? #f])
  (if (equal? w undefined)
      ((cells (curry F #:undo? (not undo?)) x))
      (for*/array #:shape (vector-append (array-shape x) (array-shape w))
       ([xn (in-array x)] [wn (in-array w)])
       (F xn wn))))

(define (BQN´ F)
  (case-lambda
    [(x  ) (array-all-fold x F)]
    [(x w) (array-all-fold x F w)]))

(define (BQN˝ F)
  (lambda (x w)
    (for/fold ([fold w]) ([cell (in-array-axis x)])
      (F fold cell))))

(define (BQN-GRAVE F)
  (lambda (x [w #f])
    (list->array
     (for/lists (scan) ([cell (in-array-axis x)])
       (cond
         [(cons? scan) (F (first scan) cell)]
         [(w) (F w cell)]
         [cell]))))
  )

