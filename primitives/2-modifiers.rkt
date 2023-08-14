#lang racket/base
(require math/array racket/undefined racket/provide racket/list racket/match "utilities.rkt"
         (only-in "1-modifiers.rkt" BQN⁼ BQN˜))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (BQN∘ F G)
  (bqn-func (compose1 F G) (compose1 (undo G) (undo F)) #f))

(define ((circle F G) . args)
  (apply F (map G args)))

(define (undo-circle F G)
  (case-lambda
    [(x  ) ((undo G) ((undo F) x))]
    [(x w) ((undo G) ((undo F) x (G w)))]))

(define (BQN○ F G)
  (bqn-func circle undo-circle #f))

(define (mmap F G)
  (if (not (procedure? F))
      (λ (x) (G x F))
      (case-lambda
        [(x  ) (G x (F x))]
        [(x w) (G x (F w))])))

(define (undo-mmap F G)
  (if (not (procedure? F))
      (λ (x  ) ((undo G) x F))
      (λ (x w) ((undo G) x (F w)))))

(define (BQN⊸ F G)
  (bqn-func mmap undo-mmap #f))

(define (l-mmap F G)
  (if (not (procedure? G))
      (λ (x) (F G x))
      (case-lambda
        [(x  ) (F (G x) x)]
        [(x w) (F (G x) w)])))

(define (undo-l-mmap F G)
  (if (not (procedure? G))
      (λ (x  ) ((~undo F) x G))
      (λ (x w) ((undo G) ((undo F) x w)))))

(define (BQN⟜ F G)
  (bqn-func l-mmap undo-l-mmap #f))

(define (BQN⊘ F G)
  (define (valences f g)
    (case-lambda
      [(x)   (f x)]
      [(x w) (g x w)]))
  (bqn-func (valences F G) (valences (undo F) (undo G)) #f)
  )

(define (apply-choice choice)
  (if (equal? (array-size choice) 1)
      (first (array->list choice))
      (case-lambda
        [(x  ) (array-map (λ (G x  ) (G x  )) choice (array x))]
        [(x w) (array-map (λ (G x w) (G x w)) choice (array x) (array w))])))

(define (BQN◶ F g)
  (compose1 apply-choice BQN⊑ F))

(define (BQN⎊ F G)
  (with-handlers ([exn:fail? (λ (e) (λ args (apply G args)))])
    (λ args (apply F args))))


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
   (apply (repeat (undo F) (- g)) args)])

(define (BQN⍟ F G)
  (bqn-func (repeat F G) (repeat (BQN⁼ F) G)))
