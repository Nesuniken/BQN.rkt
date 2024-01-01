#lang racket/base
(require  racket/match racket/undefined racket/provide
          racket/function racket/vector racket/list
          math/array "utilities.rkt")
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (BQN⁼ F)
  (if (bqn-func? F)
      (match-let ([(bqn-func call inverse _) F])
        (bqn-func inverse call (thunk (error "F⁼˜⁼ is undefined"))))
      (bqn-func
       (thunk (error "Inverse not found"))
       (to-func F)
       (thunk (error "F⁼˜⁼ is undefined"))))
  )
  
(define BQN˙ const)

(define (tilde F)
  (case-lambda
      [(x)   (F x x)]
      [(x w) (F w x)]))

(define (full-tilde F)
  (match-define (bqn-func call undo ~undo) F)
  (define ~call
    (case-lambda
      [(x)   (call x x)]
      [(x w) (call w x)]))
  (define ~~undo
    (case-lambda
      [(x)   (~undo x)]
      [(x w) (undo x w)])
    )
  (bqn-func ~call ~undo ~~undo)
  )

(define (BQN˜ F)
  (if (bqn-func? F)
      (full-tilde F)
      ((apply-mod tilde) F)))

(define (BQN˜⁼ F)
  (BQN⁼ (BQN˜ F)))

(define (BQN¨ F)
  (define (each f)
    (curry array-map f))

  (bqn-func
   ((apply-mod each) F)
   (each (BQN⁼ F)) #f)
  )

(define ((1-cells F) x)
  (for/array #:shape (array-shape x)
    ([major (in-array-axis x)])
    (F major)))

(define (cells F)
  (case-lambda
    [(x) (1-cells F)]
    [(x w)
     (for/array #:shape (array-shape x)
       ([major-x (in-array-axis x)] [major-w (in-array-axis w)])
       (F major-x major-w))]))

(define (BQN˘ F)
  (bqn-func ((apply-mod cells) F) (cells (BQN⁼ F)) #f))

(define (table F)
  (case-lambda
    [(x) (1-cells F)]
    [(x w)
     (for*/array #:shape (vector-append (array-shape x) (array-shape w))
       ([xn (in-array x)] [wn (in-array w)])
       (F xn wn))]))

(define (BQN⌜ F)
  (bqn-func ((apply-mod table) F) (1-cells (BQN⁼ F)) #f))

(define (BQN´ F)
  (case-lambda
    [(x  ) (array-all-fold x (apply-mod F))]
    [(x w) (array-all-fold x (apply-mod F) w)]))

(define (BQN˝ F)
  (lambda (x w)
    (for/fold ([fold w]) ([cell (in-array-axis x)])
      ((apply-mod F) fold cell))))

(define (BQN\` F)
  (lambda (x [w (void)])
    (for/lists (scan #:result (list->array scan)) 
               ([cell (in-array-axis x)])
      (cond
        [(cons? scan)
         ((apply-mod F) (first scan) cell)]
        [((not (void? w)))
         ((apply-mod F) w cell)]
        [cell])))
  )

