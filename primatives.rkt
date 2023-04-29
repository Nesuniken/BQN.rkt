#lang racket
(require math/array racket/provide)
(provide
 #%module-begin #%top #%app #%datum #%top-interaction
 (all-from-out math/array)
 (matching-identifiers-out #rx"^BQN" (all-defined-out))
 (matching-identifiers-out #rx"•" (all-defined-out)))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(define  ∞ +inf.0)
(define ¯∞ -inf.0)

(define  π    pi)
(define ¯π (- pi))

(define (•promote x)
  (if (array? x) x (array x)))

(define (•strict x)
  (if (array? x)
      (array-strict x)
      x))

(define (•show x)
  (display x))

(define (•exit x)
  (exit x))

(define (pv-monad monad)
  (lambda (x)
    (if (array? x)
        (array-map (pv-monad monad) x)
        (monad x))))

(define (pv-dyad dyad)
  (lambda (x w)
    (cond
      [(array? w)
       (array-map (pv-dyad dyad) (•promote x) w)]
      [(array? x)
       (array-map (pv-dyad dyad) x (array w))]
      [(dyad x w)])))

(define (pv-func monad dyad [id #f])
  (if id
      (case-lambda
        [() id]
        [(x)   ((pv-monad monad) x)]
        [(x w) ((pv-dyad   dyad) x w)])
      (case-lambda
        [(x)   ((pv-monad monad) x)]
        [(x w) ((pv-dyad   dyad) x w)])))

(define (add x w)
  (cond
    [(number? x)
     (if (number? w)
         (+ x w)
         (integer->char (+ x (char->integer w))))]
    [(char? x)
     (integer->char (+ (char->integer x) w))]))

(define (subtract x w)
  (cond
    [(char? w)
     (if (integer? x)
         (integer->char (- (char->integer w) x))
         (- (char->integer x) (char->integer w)))]
    [(number? w)
     (- w x)]))

(define BQN+
  (pv-func conjugate add 0))

(define BQN+⁼
  (pv-func conjugate (swap subtract) 0))

(define BQN-
    (pv-func - subtract 0))

(define BQN-⁼ BQN-)

(define (pv-equal test) 
  (pv-dyad (λ (x w) (if (test x w) 1 0))))

(define BQN=
  (case-lambda
    [() 1]
    [(x) (array-dims x)]
    [(x w) ((pv-equal equal?) x w)]))

(define BQN≡
  (case-lambda
    [(x)
     (if (array? x)
         (+ 1 (array-all-max (array-map BQN≡ x)))
         0)]
    [(x w) (if (equal? x w) 1 0)]))

(define BQN≠
  (case-lambda
    [() 0]
    [(x) (vector-ref (array-shape x) 0)]
    [(x w) ((pv-equal (λ (x w) (not (equal? w x)))) x w)]))

(define BQN≢
  (case-lambda
    [(x) (vector->array (array-shape x))]
    [(x w) (if (equal? x w) 0 1)]))

(define (pv-compare test)  
  (define (compare x w)
    (cond
      [(equal? w x) 0]
      [(real? x)
       (if (real? w) (exact-truncate (- w x)) 1)]
      [(char? x)
       (if (char? w) (- (char->integer w) (char->integer x)) -1)]))
  
  (pv-dyad (λ (x w) (if (test (compare x w) 0) 1 0))))

(define BQN≤ (pv-compare <=))

(define BQN<
  (case-lambda
    [(x) (array x)]
    [(x w) ((pv-compare <) x w)]))

(define (BQN<⁼ x)
  (first (array->list x)))

(define BQN>
  (case-lambda
    [() 0]
    [(x) (let ([inner-check (array-map array? x)])
           (cond
             [(array-all-and inner-check)
              (array-list->array (array->list x))]
             [(not (array-all-or inner-check))
              x]))]
    [(x w) ((pv-compare >) x w)]))

(define BQN≥
  (case-lambda
    [() 1]
    [(x w)(pv-compare >=)]))

(define BQN⌊
  (let ([bqn-min
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (min x w)
                  x)]
             [(and (char? x) (char? w))
              (if (char<? x w) x w)]
             [w]))])
    (pv-func floor bqn-min -inf.0)))

(define BQN⌈
  (let ([bqn-max
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (max x w)
                  w)]
             [(and (char? x) (char? w))
              (if (char>? x w) x w)]
             [x]))])
    (pv-func ceiling bqn-max +inf.0)))

(define BQN∧
  (pv-dyad *))

(define (BQN¬ x [w 0])
  (- (+ 1 w) x))

(define BQN¬⁼ BQN¬)

(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(define BQN×
  (let ([sign (lambda (x) (if (equal? x 0) 0 (/ 1 (magnitude x))))])
    (pv-func sign * 1)))

(define (BQN×⁼ x w)
  (pv-dyad /))

(define BQN÷ (pv-func / (swap /)))

(define BQN÷⁼ BQN÷)

(define BQN√
  (pv-func sqrt (λ (x w) (expt x (/ w)))))

(define BQN√⁼
  (pv-func (λ (x  ) (* x x))
           (λ (x w) (expt w x))))

(define BQN⋆
  (pv-func exp (swap expt) 1))

(define BQN⋆⁼
  (pv-func log log))

(define BQN-PIPE
  (pv-func magnitude (swap modulo)))

(define BQN⊣
  (case-lambda
    [(x)   x]
    [(x w) w]))

(define BQN⊢
  (case-lambda
    [(x)   x]
    [(x w) x]))

(define BQN⊣⁼ BQN⊢)

(define BQN⊢⁼ BQN⊢)

(define (deshape x)
  (if (array? x)
      (array-flatten x)
      (array #[x])))

(define (reshape x w)
  (define flat-x (deshape x))
  (define-values (left-dims other-dims)
    (splitf-at (array->list w) integer?))
  
  (if (empty? other-dims)
      (array-reshape flat-x (list->vector left-dims))
      (let*-values
          ([(var-dim right-dims)
            (split-at other-dims 1)]
           [(dim-prod)
            (* (apply * left-dims) (apply * right-dims))]
           [(var-dim-val)
            (/ (array-size flat-x) dim-prod)]
           [(make-dims)
            (λ (deshaped* v-d-v)
              (list->vector (append left-dims (list v-d-v) right-dims)))])
        (case var-dim
          [(BQN∘)
           (array-reshape flat-x (make-dims var-dim-val))]
          [(BQN⌊)
           (let* ([v-d-floor (floor var-dim-val)]
                  [resize (* v-d-floor dim-prod)])
             (array-reshape
              (array-slice-ref flat-x (list (:: #f resize)))
              (make-dims v-d-floor)))]
          [(BQN⌽)
           (let* ([v-d-ceil (ceiling var-dim-val)]
                  [resize (* v-d-ceil dim-prod)]
                  [x-loop (λ (i) (array-ref flat-x (vector (modulo (vector-ref i 0) (array-size flat-x)))))])
             (array-reshape (build-array (vector resize) x-loop) (make-dims v-d-ceil)))])))
  )

(define BQN⥊
  (case-lambda
    [(x)   (deshape x)]
    [(x w) (reshape x w)]))

(define BQN∾
  (case-lambda
    [(x) (array-all-fold x (λ (a b) (array-append* (list a b))))]
    [(x w) (array-append* (list x w))]))

(define BQN≍
  (case-lambda
    [(x) (if (array? x)
             (array-axis-insert x 0)
             (array #[x]))]
    [(x w) (BQN> x w)]))

(define BQN⋈
  (case-lambda
    [(x)   (array #[x])]
    [(x w) (array #[w x])]))

(define (pad-slices slices)
  (append slices (list ::...)))

(define (↑↓-core slice)
  (case-lambda
    [(x) (build-array
          (vector-take (array-shape x) 1)
          (λ (i) (array-slice-ref x (list (slice (vector-ref i 0)) ::...))))]
    [(x w) (array-slice-ref x (pad-slices (map slice (array->list w))))]
    ))

(define BQN↑
  (↑↓-core (λ (i) (:: #f i))))

(define BQN↓
  (↑↓-core (λ (i) (:: i #f))))

(define BQN↕
  (case-lambda
    [(x) (array-map
          vector->array
          (indexes-array
           (if (array? x) (array->vector x) (vector x))))]
    ))

(define (BQN« x w)
  (array-append* (array-slice-ref x (list (:: (BQN≠ w) #f) ::...)) w))

(define (BQN» x w)
  (array-append* w (array-slice-ref x (list (:: #f (BQN≠ w)) ::...))))

(define (rotate-range start length)
  (map (λ (n) (modulo n length)) (range start (+ start length))))

(define BQN⌽
  (case-lambda
    [(x)   (array-slice-ref x (list (:: #f #f -1) ::...))]
    [(x w)
     (let ([slices
            (vector->list
             (vector-map rotate-range (array->vector w) (array-shape x)))])
       (array-slice-ref x (pad-slices slices)))]))

(define BQN⌽⁼
  (case-lambda
    [(x)   (BQN⌽ x)]
    [(x w) (BQN⌽ x (array-map - w))]))

(define BQN⍉
  (case-lambda
    [(x) (array-axis-permute x (rotate-range 1 (array-dims x)))]
    [(x w) (let ([w-list (array->list w)])
             (array-axis-permute
              x (append w-list (remove* w-list (range (array-dims x))))))]
    ))

(define BQN⍉⁼
  (case-lambda
    [(x) (array-axis-permute x (rotate-range -1 (array-dims x)))]))

(define BQN/
  (case-lambda
    [(x) (BQN/ (index-array (vector (BQN≠ x))) x)]
    [(x w) (array-append*
          (array->vector
           (array-map
            (λ (l i) (make-array (vector l) i)) w x)))]
    ))

(define (BQN⊏ x [w (array 0)])
  (cond
    [(array-all-and (array-map array? w))
     (array-slice-ref x (pad-slices (array->list (array-map array->list w))))]
    [(array-all-and (array-map integer? w))
     (array-slice-ref x (list (array->list w) ::...))]))

(define BQN⊑
  (case-lambda
    [(x) (array-ref x (make-vector (array-dims x) 0))]
    [(x w) (array-indexes-ref
            x (array-map (λ (n) (if (array? n) (array->vector n) n)) w))]))

(define (unique x)
  (remove-duplicates (array->list x)))

(define (⊐hash seq)
  (for/hash ([k seq] [v (in-naturals)])
    (values k v)))

(define BQN⊐
  (case-lambda
    [(x) (array-map (λ (k) (hash-ref (⊐hash (unique x)) k)) x)]
    [(x w)
     (array-map
      (λ (k)
        (cond
          [(hash-ref (⊐hash (in-array-axis w) k #f))]
          [(BQN≠ w)]))
      x)]))

(define (occurence-count x)
  (define counter (make-hash))
  (for/array ([n (in-array-axis x)])
    (if (hash-has-key? counter n)
        (hash-update! counter n add1)
        (hash-set! counter n 0))
    (hash-ref counter n)))

(define (prog-index x w)
  (define stacks (make-hash))
  (for ([k (in-array-axis w)] [v (in-naturals)])
    (if (hash-has-key? stacks k)
        (hash-update! stacks k (λ (l) (cons v l)))
        (hash-set! k (list v))))
  (for/array ([k (in-array-axis x)])
    (cond
      [(hash-ref stacks k #f)
       => (lambda (v)
            (begin
              (if (empty? (rest v))
                  (hash-remove! stacks k)
                  (hash-update! stacks rest))
              v))]
      [(BQN≠ w)])))

(define BQN⊒
  (case-lambda
    [(x) (occurence-count x)]
    [(x w) (prog-index x w)]))

(define (mark-firsts x)
  (define tracker (mutable-set))
  (for/array ([n (in-array-axis x)])
    (if (set-member? tracker n)
        0
        (begin (set-add! tracker n) 1))))

(define BQN∊
  (case-lambda
    [(x) (mark-firsts x)]
    [(x w)
     (array-map
      (λ (v)
        (if (set-member? (list->set (array->list x)) v)
            1 0))
      w)]))

(define BQN⍷
  (case-lambda
    [(x) (list->array (unique x))]))

(define (group x w)
  (define w-lists
    (array-map
     (lambda (n)
       (if (array? w)
           (array->list w)
           (list w)))
     w))
  (define (max-shape a b)
    (cond
      [(empty? a) b]
      [(empty? b) a]
      [(cons (max a b) (max-shape (rest a) (rest b)))]))
  (define shape '())
  
  (define group-hash
    (for/hash ([k (in-array w)] [v (in-array-axis x)])
      (set! shape (max-shape shape k))
      (values (list->vector k) v)))
  
  (build-array
   (list->vector shape)
   (λ (i) (hash-ref group-hash i (array #[]))))
  )

(define (BQN! x [w "Assertion error"])
  (if (equal? x 1)
      (error w)
      x))

(define (BQN˙ F)
  (const F))

(define (BQN˜ F)
  (case-lambda
    [(x)   (F x x)]
    [(x w) (F w x)]))

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

(define/match (repeat F g [F-INVERSE #f])
  [(_ (? array?) _)
   (case-lambda
     [(x  ) (array-map (λ (g*) ((repeat F g* F-INVERSE) x  )) g)]
     [(x w) (array-map (λ (g*) ((repeat F g* F-INVERSE) x w)) g)])]
  [(_ (? procedure?) _)
   (λ (x w) ((repeat F (g x w) F-INVERSE)))]
  [(_ (? positive?) _)
   (letrec ([loop
             (case-lambda
               [(n x  ) (if (> n 1) (F (loop (- n 1) x  )  ) (F x))]
               [(n x w) (if (> n 1) (F (loop (- n 1) x w) w) (F x w))])])
     (curry loop g))]
  [(_ (? negative?) (? procedure?))
   (repeat F-INVERSE (- g) F)]
  [(_ 0 _) BQN⊢])