#lang racket/base
(require math/array racket/undefined racket/provide racket/list racket/match racket/function BQN/prim-utils
         (only-in BQN/primitives BQN⊑ BQN⊢)
         (only-in BQN/1-modifiers BQN˜))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define ((BQN∘ F G) #:undo? [undo? #f] . args)
  (if undo?
      (apply (compose1 (undo G) (undo F)) args)
      (apply (compose1 F G)  args)))

(define ((BQN○ F G) #:undo? [undo? #f] . args)
  (if undo?
      ((undo G) (apply (undo F) (first args) (map G (rest args))))
      (apply F (map G args))))

(define/match ((BQN⊸ F G) #:undo? [undo? #f] . args)
  [((? procedure?) _ _ (list x w))
   (G x (F w) #:undo? undo?)]
  [((? procedure?) _ #f (list x))
   (G x (F x))]
  [((not (? procedure?)) _ _ (list x))
   (G x F #:undo? undo?)])

(define/match ((BQN⟜ F G) #:undo? [undo? #f] . args)
  [(_ (not (? procedure?)) #f (list x))
   (F G x)]
  [(_ (not (? procedure?)) #t (list x))
   ((compose1 undo BQN˜ F) x)]
  [(_ (? procedure?) #f _)
   (F (G (first args)) (last args))]
  [(_ (? procedure?) #t (list x w))
   (apply (BQN∘ F G) x w #:undo? #t)])

(define ((BQN⊘ F G) x [w undefined] #:undo? [undo? #f])
  (if (equal? w undefined)
      (F x   #:undo? undo?)
      (G x w #:undo? undo?)))

(define (apply-choice choice)
  (if (equal? (array-size choice) 1)
      (first (array->list choice))
      (case-lambda
        [(x  ) (array-map (λ (G x  ) (G x  )) choice (array x))]
        [(x w) (array-map (λ (G x w) (G x w)) choice (array x) (array w))])))

(define (BQN◶ F g)
  (compose1 apply-choice BQN⊑ F))

(define (BQN⎊ F G)
  (case-lambda
    [(x  ) (with-handlers ([exn:fail? (λ (e) (λ (x  ) (G x  )))]) (F x  ))]
    [(x w) (with-handlers ([exn:fail? (λ (e) (λ (x w) (G x w)))]) (F x w))]))

(define/match ((BQN⍟ F g) #:undo? [undo? #f] . args)
  [(_ (? array?) _ _)
   (array-map (λ (g*) (apply (BQN⍟ F g*) args)) g)]
  [(_ (? procedure?) _ _)
   (apply (BQN⍟ F (apply g args)) args)]
  [(_ 0 _ _) (first args)]
  [(_ (? integer?) #t _)
   (apply (BQN⍟ F (- g)) args)]
  [(_ (? exact-positive-integer?) #f _)
   (for/fold ([out (first args)]) ([r (in-range g)])
     (F out (rest args)))]
  [(_ (? negative?) #f _)
   (apply (BQN⍟ (curry #:undo? #t) (- g)) args)])