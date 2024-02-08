#lang racket
(require megaparsack megaparsack/text data/monad data/applicative math/array)
(provide number/p bqn-string/p)

(define (maybe/p parser)
  (many/p parser #:max 1))

;Parsing Numbers

(define negate (do [negate <- (maybe/p (char/p #\¯))]
               (pure (if (cons? negate) -1 1))))

(define int
  (do [digits <- (many/p digit/p)]
      (pure (list->string digits))))

(define decimal
  (do [+/- <- negate]
      [parts <- (many+/p
                 int #:sep (char/p #\.) #:max 2)]
      (define joined
        (string-join parts "."))
      (pure (* +/- (string->number joined)))
      )
  )

(define exponent
  (or/p
   (do
     (char-ci/p #\e)
     [+/- <- negate]
     [int <- integer/p]
     (pure (expt 10 (* +/- int))))
   (pure 1)))

(define pi/p
  (do (char/p #\π)
      (pure pi)))

(define inf
  (do (char/p #\∞)
      (pure +inf.0)))

(define real
  (do [+/- <- negate]
      [maybe-inf <- (or/p (char/p #\∞)
                          (do [mantissa <- (or/p pi/p decimal)]
                              {exp <- exponent}
                              (pure (* mantissa exp))))]
      (pure (* +/- maybe-inf))))

(define number/p
  (do [components <- (many+/p real #:sep (char-ci/p #\i) #:max 2)]
      (pure (if (empty? (rest components))
                (first components)
                (apply make-rectangular components)))))

;Parsing Srings
 
(define quote-mark
  (do (repeat/p 2 (char/p #\"))
      (pure #\")))

(define bqn-string/p
  (do (char/p #\")
      [str <- (many/p
               (or/p (char-not/p #\")
                     quote-mark))]
      (char/p #\")
      (pure (list->array str))))