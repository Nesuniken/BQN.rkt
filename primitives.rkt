#lang racket/base
(require math/array racket/match racket/provide racket/list racket/vector racket/set BQN/prim-utils
         (only-in BQN/arithmetic BQN> BQN≠))
(provide (matching-identifiers-out #rx"^BQN" (all-defined-out)))

(define (BQN⊣ x [w #f])
  (if w w x))

(define (BQN⊢ x [w #f] #:undo? [undo? #f])
  x)

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

(define/match (BQN⥊ #:undo? [undo? #f] . args)
  [(#t  _) (undo-error #\⥊)]
  [(#f (list x  )) (deshape x)]
  [(#f (list x w)) (reshape x w)])

(define/match (BQN∾ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\∾)]
  [(#f (list x  ))
   (array-all-fold x (λ (a b) (array-append* (list a b))))]
  [(#f (list x w))
   (array-append* (list x w))])

(define/match (BQN≍ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\≍)]
  [(#f (list (? array? x)))
   (array-axis-insert x 0)]
  [(#f (list x)) (array #[x])]
  [(#f (list x w)) (array #[w x])])

(define (BQN⋈ #:undo? [undo? #f] . args)
  (if undo?
      (undo-error #\⋈)
      (array->list args)))

(define (pad-slices slices)
  (append slices (list ::...)))

(define/match ((↑↓-core char slice) #:undo? [undo? #f] . args)
  [(_ _ #t _) (undo-error char)]
  [(_ _ #f (list x))
   (build-array
    (vector-take (array-shape x) 1)
    (λ (i) (array-slice-ref x (list (slice (vector-ref i 0)) ::...))))]
  [(_ _ #f (list x w)) (array-slice-ref x (pad-slices (map slice (array->list w))))]
  )

(define BQN↑
  (↑↓-core #\↑ (λ (i) (:: #f i))))

(define BQN↓
  (↑↓-core #\↓ (λ (i) (:: i #f))))

(define/match (BQN↕ #:undo? [undo? #f] . args)
  [(#f x) (array-map
           vector->array
           (indexes-array
            (if (array? x) (array->vector x) (vector x))))]
  )

(define/match ((«»-core char) #:undo? [undo? #f] . args)
  [(_ #t _) (undo-error char)]
  [(_ #f (list x))
   ((«»-core char) x (array (find-fill x)))]
  [(#\« #f (list x w)) (array-append* (BQN↓ x 1) w)]
  [(#\» #f (list x w)) (array-append* w (BQN↑ x 1))])

(define BQN« («»-core #\«))
(define BQN» («»-core #\»))

(define (rotate-range start length)
  (map (λ (n) (modulo n length)) (range start (+ start length))))

(define (BQN⌽ x [w #f] #:undo? [undo? #f])
  (if (not w)
      (array-slice-ref x (list (:: #f #f -1) ::...))
      (if undo?
          (BQN⌽ x (array-map - w))
          (let ([slices
                 (vector->list
                  (vector-map rotate-range (array->vector w) (array-shape x)))])
            (array-slice-ref x (pad-slices slices))))))

(define (BQN⍉ x [w #f] #:undo? [undo? #f])
  (if (not w)
      (array-axis-permute x (rotate-range (if undo? -1 1) (array-dims x)))
      (let ([w-list (array->list w)])
        (array-axis-permute
         x (append w-list (remove* w-list (range (array-dims x))))))))

(define/match (BQN/ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\/)]
  [(#f (list x))
   (BQN/ (index-array (vector (BQN≠ x))) x)]
  [(#f (list x w))
   (array-append*
    (array->vector
     (array-map
      (λ (l i) (make-array (vector l) i)) w x)))]
  )

(define (BQN⊏ x [w (array 0)] #:undo? [undo? #f])
  (cond
    [undo? (undo-error #\⊏)]
    [(array-all-and (array-map array? w))
     (array-slice-ref x (pad-slices (array->list (array-map array->list w))))]
    [(array-all-and (array-map integer? w))
     (array-slice-ref x (list (array->list w) ::...))]))

(define/match (BQN⊑ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\⊑)]
  [(#f (list x)) (array-ref x (make-vector (array-dims x) 0))]
  [(#f (list x w)) (array-indexes-ref
                    x (array-map (λ (n) (if (array? n) (array->vector n) n)) w))])

(define (unique x)
  (remove-duplicates (array->list x)))

(define (⊐hash seq)
  (for/hash ([k seq] [v (in-naturals)])
            (values k v)))

(define/match (BQN⊐ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\⊐)]
  [(#f (list x)) (array-map (λ (k) (hash-ref (⊐hash (unique x)) k)) x)]
  [(#f (list x w))
   (array-map
    (λ (k)
      (cond
        [(hash-ref (⊐hash (in-array-axis w) k #f))]
        [(BQN≠ w)]))
    x)])

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
                           (hash-update! stacks k rest))
                       (first v)))]
               [(BQN≠ w)])))

(define/match (BQN⊒ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\⊒)]
  [(#f (list x)) (occurence-count x)]
  [(#f (list x w)) (prog-index x w)])

(define (mark-firsts x)
  (define tracker (mutable-set))
  (for/array ([n (in-array-axis x)])
             (if (set-member? tracker n)
                 0
                 (begin (set-add! tracker n) 1))))

(define/match (BQN∊ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\∊)]
  [(#f (list x)) (mark-firsts x)]
  [(#f (list x w))
   (array-map
    (λ (v)
      (if (set-member? (list->set (array->list x)) v)
          1 0))
    w)])

(define/match (BQN⍷ #:undo? [undo? #f] . args)
  [(#t _) (undo-error #\⍷)]
  [(#f (list x)) (list->array (unique x))])

(define/match (BQN! x [w "Assertion error"] #:undo? [undo? #f])
  [(1 _ _) 1]
  [(_ _ _) (error w)])
