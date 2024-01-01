#lang br
(require math/array)
(provide (all-defined-out))

(define (1-D? a)
  (equal? 1 (array-dims a))
  )

(define-match-expander nothing->underscore
  (lambda (stx)
    (pattern-case stx 
      [(_ (ELTS ...))
       #'(nothing->underscore () (ELTS ...))]
      [(_ (OUT ...) (· REST ...))
       #'(nothing->underscore (OUT ... _) (REST ...))]
      [(_ (OUT ...) (NEXT REST ...))
       #'(nothing->underscore (OUT ... NEXT) (REST ...))]
      [(_ (OUT ...) ())
       #'(OUT ...)])))

(define-match-expander lhsList
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(or (and (? array?) (? 1-D?)
              (app array->list
                   (nothing->underscore (list elts ...))))
             (lhsNS elts ...))])))

(define-match-expander lhsStrand
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(lhsList elts ...)])))

(define-match-expander lhsArray
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(and array? (app array->array-list
                          (nothing->underscore (list elts ...))))]
      )))

(define-match-expander lhsNS
  (lambda (stx)
    (with-pattern
        ([(_ ELTS ...) stx]
         [(BINDINGS ...)
          (pattern-case-filter #'(ELTS ...)
            [(BIND-ID KEY)
             #'((quote KEY) BIND-ID)]
            [ID-KEY
             #'((quote ID-KEY) ID-KEY)])])
      #'(hash-table BINDINGS ...))))