#lang racket/base
(require math/array racket/undefined racket/provide racket/list racket/match BQN/prim-utils
         (only-in BQN/primitives BQN⊑)
         (only-in BQN/1-modifiers BQN˜ BQN⁼))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define ((BQN∘ F G) [undo #f])
  (if undo
      (compose1 (G #t) (F #t))
      (compose1 (F) (G))))

(define (((BQN○ F G) [undo #f]) . args)
  (if undo
      ((G undo) (apply (F undo) (first args) (map (G) (rest args))))
      (apply F (map (G) args))))

(define/match (((BQN⊸ F G) [undo #f]) . args)
  [((? procedure?) _ _ (list x w))
   ((G undo) x ((F) w))]
  [((? procedure?) _ #f (list x))
   ((G) x ((F) x))]
  [((not (? procedure?)) _ _ (list x))
   ((G undo) x F)])

(define/match (((BQN⟜ F G) [undo #f]) . args)
  [(_ (not (? procedure?)) #f (list x))
   (F G x)]
  [(_ (not (? procedure?)) #t (list x))
   ((compose1 BQN⁼ BQN˜ F) x)]
  [(_ (? procedure?) #f _)
   (F (G (first args)) (last args))]
  [(_ (? procedure?) #t (list x w))
   ((BQN∘ F G #t) x w)])

(define (((BQN⊘ F G) [undo #f]) x [w undefined] )
  (if (equal? w undefined)
      ((F undo) x)
      ((G undo) x w)))

(define (apply-choice choice)
  (if (equal? (array-size choice) 1)
      (first (array->list choice))
      (case-lambda
        [(x  ) (array-map (λ (G x  ) (G x  )) choice (array x))]
        [(x w) (array-map (λ (G x w) (G x w)) choice (array x) (array w))])))

(define ((BQN◶ F g) [undo #f])
  (if (not undo)
      (compose1 apply-choice BQN⊑ (F))
      (undo-error "◶")))

(define ((BQN⎊ F G) [undo #f])
  (if (not undo)
      (case-lambda
        [(x  ) (with-handlers ([exn:fail? (λ (e) (λ (x  ) ((G) x  )))]) ((F) x  ))]
        [(x w) (with-handlers ([exn:fail? (λ (e) (λ (x w) ((G) x w)))]) ((F) x w))])
      (undo-error "⎊")))

(define/match (((BQN⍟ F g) [undo #f]) . args)
  [(_ (? array?) _ _)
   (array-map (λ (g*) (apply (BQN⍟ F g*) args)) g)]
  [(_ (? procedure?) _ _)
   (apply (BQN⍟ F (apply g args)) args)]
  [(_ 0 _ _) (first args)]
  [(_ (? integer?) #t _)
   (apply (BQN⍟ F (- g)) args)]
  [(_ (? exact-positive-integer?) #f _)
   (letrec
       ([loop (lambda (n r)
                (if (> n 1)
                    (loop (- n 1) (apply F r (rest args)))
                    (apply F r (rest args))))])
     (loop g (first args)))]
  [(_ (? negative?) #f _)
   (apply (BQN⍟ (F #t) (- g)) args)])