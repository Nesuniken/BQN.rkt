#lang racket/base
(require math/array racket/undefined racket/provide racket/list racket/match "utilities.rkt"
         (only-in "1-modifiers.rkt" BQN˜ BQN⁼))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define ((BQN∘ F G) [undo 0])
  (if (zero? undo)
      (compose1 (F) (G))
      (compose1 (G 1) (F 1))))

(define (((BQN○ F G) [undo 0]) . args)
  (if (zero? undo)
      (apply (F) (map (G) args))
      ((G undo) (apply (F undo) (first args) (map (G) (rest args))))))

(define/match (((BQN⊸ F G) [undo 0]) . args)
  [((? procedure?) _ _ (list x w))
   ((G undo) x ((F) w))]
  [((? procedure?) _ 0 (list x))
   ((G) x ((F) x))]
  [((not (? procedure?)) _ _ (list x))
   ((G undo) x F)])

(define/match (((BQN⟜ F G) [undo 0]) . args)
  [(_ (not (? procedure?)) 0 (list x))
   (F G x)]
  [(_ (not (? procedure?)) 1 (list x))
   ((compose1 BQN⁼ BQN˜ F) x)]
  [(_ (? procedure?) 0 _)
   (F (G (first args)) (last args))]
  [(_ (? procedure?) 1 (list x w))
   (((BQN∘ F G) 1) x w)])

(define (((BQN⊘ F G) [undo 0]) x [w undefined] )
  (if (equal? w undefined)
      ((F undo) x)
      ((G undo) x w)))

(define (apply-choice choice)
  (if (equal? (array-size choice) 1)
      (first (array->list choice))
      (case-lambda
        [(x  ) (array-map (λ (G x  ) (G x  )) choice (array x))]
        [(x w) (array-map (λ (G x w) (G x w)) choice (array x) (array w))])))

(define ((BQN◶ F g) [undo 0])
  (if (not undo)
      (compose1 apply-choice BQN⊑ (F))
      (undo-error "◶")))

(define ((BQN⎊ F G) [undo 0])
  (if (not undo)
      (case-lambda
        [(x  ) (with-handlers ([exn:fail? (λ (e) (λ (x  ) ((G) x  )))]) ((F) x  ))]
        [(x w) (with-handlers ([exn:fail? (λ (e) (λ (x w) ((G) x w)))]) ((F) x w))])
      (undo-error "⎊")))

(define/match (((BQN⍟ F g) [undo 0]) . args)
  [(_ (? array?) _ _)
   (array-map (λ (g*) (apply (BQN⍟ F g*) args)) g)]
  [(_ (? procedure?) _ _)
   (apply (BQN⍟ F (apply g args)) args)]
  [(_ 0 _ _) (first args)]
  [(_ (? integer?) 1 _)
   (apply (BQN⍟ F (- g)) args)]
  [(_ (? exact-positive-integer?) 0 _)
   (letrec
       ([loop (lambda (n r)
                (if (> n 1)
                    (loop (- n 1) (apply F r (rest args)))
                    (apply F r (rest args))))])
     (loop g (first args)))]
  [(_ (? negative?) 0 _)
   (apply (BQN⍟ (F 1) (- g)) args)])