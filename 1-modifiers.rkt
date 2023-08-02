#lang racket/base
(require  racket/match racket/undefined racket/provide racket/function racket/vector racket/list
          math/array BQN/prim-utils)
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define ((BQN⁼ F) [undo 0])
  (F (not undo)))

(define BQN˙ const)

(define/match (((BQN˜ F) [undo 0]) . args)
  [(_ 0 (list x))   ((F undo) x x)]
  [(_ 0 (list x w)) ((F undo) w x)]
  [(_ _ _) (apply (F (- undo)) args)]
  )

(define (((BQN¨ F) [undo 0]) . args)
  (apply array-map (F undo) args))

(define ((cells F) x)
  (for/array #:shape (array-shape x)
    ([major (in-array-axis x)])
    (F major)))

(define (((BQN˘ F) [undo 0]) x [w undefined])
  (if (equal? w undefined)
      ((cells (F undo)) x)
      
      (for/array #:shape (array-shape x)
        ([major-x (in-array-axis x)] [major-w (in-array-axis w)])
        ((F undo) major-x major-w))
      ))

(define (((BQN⌜ F) [undo 0]) x [w undefined])
  (if (equal? w undefined)
      ((cells (F undo) x))
      (for*/array #:shape (vector-append (array-shape x) (array-shape w))
        ([xn (in-array x)] [wn (in-array w)])
        ((F undo) xn wn))))

(define ((BQN´ F) [undo 0])
  (if (zero? undo)
      (case-lambda
        [(x  ) (array-all-fold x (F))]
        [(x w) (array-all-fold x (F) w)])
      (undo-error "´")))

(define ((BQN˝ F) [undo 0])
  (if (zero? undo)
      (lambda (x w)
        (for/fold ([fold w]) ([cell (in-array-axis x)])
          ((F) fold cell)))
      (undo-error "˝")))

(define ((BQN\` F) [undo 0])
  (if (zero? undo)
   (lambda (x [w #f])
    (list->array
     (for/lists (scan) ([cell (in-array-axis x)])
       (cond
         [(cons? scan) ((F) (first scan) cell)]
         [(w) ((F) w cell)]
         [cell]))))
   (undo-error "`"))
  )

