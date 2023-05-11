#lang racket/base
(require math/array racket/provide racket/list racket/match racket/function
         (only-in BQN/primitives BQN⊑ BQN⊢))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out))
         (matching-identifiers-out #rx"•" (all-defined-out)))

(define (BQN∘ F G)
  (compose1 F G))

(define (BQN○ F G)
  (case-lambda
    [(x)   (F (G x))]
    [(x w) (F (G x) (G w))]))

(define (BQN⊸ F G)
  (if (procedure? F)
      (case-lambda [(x) (G x (F x))] [(x w) (G x (F w))])
      (lambda (x) (G x F))))

(define (BQN⟜ F G)
  (if (procedure? G)
      (case-lambda [(x) (F (G x) x)] [(x w) (F (G x) w)])
      (lambda (w) (F G w))))

(define (BQN⊘ F G)
  (case-lambda [(x) (F x)] [(x w) (G x w)]))

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

(define/match (•repeat F g [F-INVERSE #f])
  [(_ (? array?) _)
   (case-lambda
     [(x  ) (array-map (λ (g*) ((•repeat F g* F-INVERSE) x  )) g)]
     [(x w) (array-map (λ (g*) ((•repeat F g* F-INVERSE) x w)) g)])]
  [(_ (? procedure?) _)
   (λ (x w) ((•repeat F (g x w) F-INVERSE)))]
  [(_ (? positive?) _)
   (letrec ([loop
             (case-lambda
               [(n x  ) (if (> n 1) (F (loop (- n 1) x  )  ) (F x))]
               [(n x w) (if (> n 1) (F (loop (- n 1) x w) w) (F x w))])])
     (curry loop g))]
  [(_ (? negative?) (? procedure?))
   (•repeat F-INVERSE (- g) F)]
  [(_ 0 _) BQN⊢])