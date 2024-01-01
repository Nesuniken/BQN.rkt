#lang racket/base
(require math/array racket/function racket/provide racket/list racket/match "utilities.rkt"
         (only-in "1-modifiers.rkt" BQN⁼ BQN˜ BQN˜⁼))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (BQN∘ F G)
  (bqn-func ((apply-mod compose1) F G) (compose1 (BQN⁼ G) (BQN⁼ F)) #f))

(define ((circle F G) . args)
  (apply F (map (apply-mod G) args)))

(define (undo-circle F G)
  (case-lambda
    [(x  ) ((BQN⁼ G) ((BQN⁼ F) x))]
    [(x w) ((BQN⁼ G) ((BQN⁼ F) x ((apply-mod G) w)))]))

(define (BQN○ F G)
  (bqn-func (circle F G) (undo-circle F G) #f))

(define (mmap F G)
  (if (not (procedure? F))
      (λ (x) ((to-func G) x F))
      (case-lambda
        [(x  ) ((to-func G) x (F x))]
        [(x w) ((to-func G) x (F w))])))

(define (undo-mmap F G)
  (if (not (procedure? F))
      (λ (x  ) ((BQN⁼ G) x F))
      (λ (x w) ((BQN⁼ G) x (F w)))))

(define (BQN⊸ F G)
  (bqn-func (mmap F G) (undo-mmap F G) #f))

(define (l-mmap F G)
  (if (not (procedure? G))
      (λ (x) ((to-func F) G x))
      (case-lambda
        [(x  ) ((to-func F) (G x) x)]
        [(x w) ((to-func F) (G x) w)])))

(define (undo-l-mmap F G)
  (if (not (procedure? G))
      (λ (x  ) ((BQN˜⁼ F) x G))
      (λ (x w) ((BQN⁼ G) ((BQN⁼ F) x w)))))

(define (BQN⟜ F G)
  (bqn-func (l-mmap F G) (undo-l-mmap F G) #f))

(define (BQN⊘ F G)
  (define (valences f g)
    (case-lambda
      [(x)   (f x)]
      [(x w) (g x w)]))
  (bqn-func ((apply-mod valences) F G) (valences (BQN⁼ F) (BQN⁼ G)) #f)
  )

(define (apply-choice choice)
  (if (equal? (array-size choice) 1)
      (first (array->list choice))
      (case-lambda
        [(x  ) (array-map (λ (G x  ) (G x  )) choice (array x))]
        [(x w) (array-map (λ (G x w) (G x w)) choice (array x) (array w))])))

(define (BQN◶ F g)
  (compose1 apply-choice (curry BQN⊑ g) F))

(define (BQN⎊ F G)
  (with-handlers ([exn:fail? (λ (e) (λ args (apply (to-func G) args)))])
    (λ args (apply (to-func F) args))))


(define/match ((repeat F g) . args)
  [(_ (? array?) _)
   (array-map (λ (g*) (apply (repeat F g*) args)) g)]
  [(_ (? procedure?) _)
   (apply (repeat F (apply g args)) args)]
  [(_ 0 _) (first args)]
  [(_ (? exact-positive-integer?) _)
   (letrec
       ([loop (lambda (n r)
                (if (> n 1)
                    (loop (- n 1) (apply F r (rest args)))
                    (apply F r (rest args))))])
     (loop g (first args)))]
  [(_ (? negative?) _)
   (apply (repeat (BQN⁼ F) (- g)) args)])

(define (BQN⍟ F G)
  (bqn-func (repeat F G) (repeat (BQN⁼ F) G) #f))
