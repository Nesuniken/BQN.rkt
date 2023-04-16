#lang typed/racket
(require math/array br/macro racket/provide)
(provide
 #%module-begin #%top #%app #%datum #%top-interaction
 (all-from-out math/array)
 (matching-identifiers-out #rx"^BQN" (all-defined-out))
 (matching-identifiers-out #rx"•" (all-defined-out)))

(define-type (Pv T) (U T (Array (Pv T))))
(define-type Real-Data (Pv (U Real Char)))
(define-type Bool (U Zero One))

(define-syntax-rule (swap f) (λ (x w) (f w x)))

(array-strictness #f)

(define  ∞ +inf.0)
(define ¯∞ -inf.0)

(define  π    pi)
(define ¯π (- pi))

(define (•show x)
  (display x))

(define (•strict x)
  (if (array? x)
      (array-strict x)
      x))

(define-macro-cases pervasive-func
  [(pervasive-func SELF DYAD)
   #'(lambda (x w)
       (cond
         [(array? w)
          (let ([arrayX (if (array? x) x (array x))])
            (array-map SELF arrayX w))]
         [(array? x)
          (array-map SELF x (array w))]
         [(DYAD x w)]))]
  [(pervasive-func SELF MONAD DYAD)
   #'(case-lambda
       [(x) (if (array? x)
                (array-map SELF x)
                (MONAD x))]
       [(x w)
        (cond
          [(array? w)
           (let ([arrayX (if (array? x) x (array x))])
             (array-map SELF arrayX w))]
          [(array? x)
           (array-map SELF x (array w))]
          [(DYAD x w)])])])
(: BQN-String (String -> (Array Char)))
(define (BQN-String str)
  (let* ([quote-count -2]
         [quote-removal
            (λ(c)(or
                  (not (equal? c #\"))
                  (begin
                    (set! quote-count (+ quote-count 1))
                    (equal? (remainder quote-count 2) 1))))])
    (list->array (filter quote-removal (string->list str)))))

(: BQN+
   (case->
    [(Pv Number) -> (Pv Number)]
    [(Pv Number) (Pv Number) -> (Pv Number)]
    [(Pv Char) (Pv Integer) -> (Pv Char)]
    [(Pv Integer) (Pv Char) -> (Pv Char)]
    ))
(define BQN+
  (let ([add
         : (case-> [Number Number -> Number] [Char Integer -> Char] [Integer Char -> Char])
         (lambda (x w)
           (cond
             [(number? x)
              (if (number? w)
                  (+ x w)
                  (integer->char (+ x (char->integer w))))]
             [(char? x)
              (integer->char (+ (char->integer x) w))]))])
    (pervasive-func BQN+ conjugate add))
  )
(: BQN-
   (case->
    [(Pv Number) -> (Pv Number)]
    [(Pv Number) (Pv Number) -> (Pv Number)]
    [(Pv Integer) (Pv Char) -> (Pv Char)]
    [(Pv Char) (Pv Char) -> (Pv Integer)]))
(define BQN-
  (let ([subtract
         : (case-> [Number Number -> Number] [Char Char -> Integer] [Integer Char -> Char])
         (lambda (x w)
           (cond
             [(char? w)
              (if (exact-integer? x)
                  (integer->char (+ x (- (char->integer w))))
                  (- (char->integer x) (char->integer w)))]
             [(number? w)
              (+ x (- w))]))])
    (pervasive-func BQN- - subtract))
  )

(define (pv-equal [test : (Any Any -> Boolean)]) : (Any Any -> (Pv Bool))
  (pervasive-func (pv-equal test) (λ (x w) (if (test x w) 1 0))))

(: BQN= (case-> [(Array Any) -> Index] [Any Any -> (Pv Bool)]))
(define BQN=
  (case-lambda
    [(x) (array-dims x)]
    [(x w) ((pv-equal equal?) x w)]))

(: BQN≡ :
   (case->
    [Any -> Exact-Nonnegative-Integer]
    [Any Any -> Bool]))
(define BQN≡
  (case-lambda
    [(x)
     (if (array? x)
         (+ 1 (array-all-max (array-map BQN≡ (array-flatten x))))
         0)]
    [(x w) (if (equal? x w) 1 0)]))

(: BQN≠ (case-> [(Array Any) -> Index] [Any Any -> (Pv Bool)]))
(define BQN≠
  (case-lambda
    [(x) (vector-ref (array-shape x) 0)]
    [(x w) ((pv-equal (λ (x w) (not (equal? w x)))) x w)]))

(: BQN≢ (case-> [(Array Any) -> (Array Index)] [Any Any -> Bool]))
(define BQN≢
  (case-lambda
    [(x) (vector->array (array-shape x))]
    [(x w) (if (equal? x w) 0 1)]))

(define (pv-compare [test : (Integer Zero -> Boolean)])
  : (Real-Data Real-Data -> (Pv Bool))
  
  (define (compare [x : (U Real Char)] [w : (U Real Char)])
    (cond
      [(equal? w x) 0]
      [(real? x)
       (if (real? w) (exact-truncate (- w x)) 1)]
      [(char? x)
       (if (char? w) (- (char->integer w) (char->integer x)) -1)]))
  
  (pervasive-func (pv-compare test)
                  (λ (x w) (if (test (compare x w) 0) 1 0))))

(define (BQN≤ x w)
  (pv-compare <=))

(: BQN<
   (All (A)
        (case->
         [A -> (Array A)]
         [Real-Data Real-Data -> (Pv Bool)])))
(define BQN<
  (case-lambda
    [(x) (array x)]
    [(x w) ((pv-compare <) x w)]))

(: BQN>
   (All (A)
        (case->
         [(Array (Array A)) -> (Array A)]
         [Real-Data Real-Data -> (Pv Bool)])))
(define BQN>
  (case-lambda
    [(x) (array-append* (array->list x))]
    [(x w) ((pv-compare >) x w)]))

(define BQN≥
  (pv-compare >=))

(: BQN⌊
   (case->
    [(Pv Real) -> (Pv Real)]
    [(Pv Real) Real-Data -> (Pv Real)]
    [Real-Data (Pv Real) -> (Pv Real)]
    [(Pv Char) (Pv Char) -> (Pv Char)]))
(define BQN⌊
  (let ([bqn-min
         : (case-> [Real (U Real Char) -> Real] [(U Real Char) Real -> Real] [Char Char -> Char])
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (min x w)
                  x)]
             [(and (char? x) (char? w))
              (if (char<? x w) x w)]
             [w]))])
    (pervasive-func BQN⌊ floor bqn-min)))

(: BQN⌈
   (case->
    [(Pv Real) -> (Pv Real)]
    [(Pv Char) Real-Data -> (Pv Char)]
    [Real-Data (Pv Char) -> (Pv Char)]
    [(Pv Real) (Pv Real) -> (Pv Real)]))
(define BQN⌈
  (let ([bqn-max
         : (case-> [Char (U Real Char) -> Char] [(U Real Char) Char -> Char] [Real Real -> Real])
         (lambda (x w)
           (cond
             [(real? x)
              (if (real? w)
                  (max x w)
                  w)]
             [(and (char? x) (char? w))
              (if (char>? x w) x w)]
             [x]))])
    (pervasive-func BQN⌈ ceiling bqn-max)))


(define BQN∧ : (case-> [Zero Number -> Zero] [Number Zero -> Zero] [Number Number -> Number]) 
  (pervasive-func BQN∧ *))

(define BQN¬ :
  (case->
   [(Pv One) -> (Pv Zero)]
   [(Pv Zero) -> (Pv One)]
   [(Pv Number) -> (Pv Number)]
   [(Pv Number) (Pv Number) -> (Pv Number)])
  (let ([span : (case-> [One Zero -> Zero] [Zero Zero -> One] [Number Number -> Number]) (λ (x w) (- (+ 1 w) x))])
   (pervasive-func BQN¬ (λ (x) (span x 0)) span)))

(: BQN⍳ (Real Real -> Number))
(define (BQN⍳ x [w 0])
  (make-rectangular w x))

(: BQN×
   (case->
    [(Pv Number) -> (Pv Number)]
    [(Pv Number) (Pv Number) -> (Pv Number)]))
(define BQN×
  (let ([sign (lambda ([x : Number]) (if (real? x) (sgn x) (/ x (magnitude x))))])
    (pervasive-func BQN× sign *)))

(: BQN÷
   (case->
    [(Pv Number) -> (Pv Number)]
    [(Pv Number) (Pv Number) -> (Pv Number)]))
(define BQN÷ (pervasive-func BQN÷ / (swap /)))

(: BQN√
   (case->
    [(Pv Number) -> (Pv Number)]
    [(Pv Number) (Pv Number) -> (Pv Number)]))
(define BQN√
  (pervasive-func BQN√ sqrt (λ (x w) (expt x (/ w)))))

(: BQN-PIPE
   (case->
    [(Pv Number) -> (Pv Nonnegative-Real)]
    [(Pv Integer) (Pv Integer) -> (Pv Integer)]))
(define BQN-PIPE
  (pervasive-func BQN-PIPE magnitude (swap modulo)))

(define BQN⊣
  (case-lambda
    [(x)   x]
    [(x w) w]))

(define BQN⊢
  (case-lambda
    [(x)   x]
    [(x w) x]))

(define (deshape x)
  (if (array? x)
      (array-flatten x)
      (array x)))