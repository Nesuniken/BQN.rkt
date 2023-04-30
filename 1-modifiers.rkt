#lang racket
(require math/array racket/provide)
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (BQN˙ F)
  (const F))

(define (BQN˜ F)
  (case-lambda
    [(x)   (F x x)]
    [(x w) (F w x)]))

(define (BQN¨ F)
  (curry array-map F))

(define (BQN˘ F)
  (case-lambda
    [(x  ) (for/array ([major (in-array-axis x)])
             (F major))]
    [(x w) (for/array ([major-x (in-array-axis x)] [major-w (in-array-axis w)])
             (F major-x major-w))]))

(define (BQN⌜ F)
  (lambda (x w)
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

