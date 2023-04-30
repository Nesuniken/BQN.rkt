#lang racket
(require math/array racket/provide
         (only-in BQN/arithmetic BQN> BQN≠))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

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