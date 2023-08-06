#lang racket
(require math/array br/macro)
(provide (all-defined-out))

(define (1-D? a)
  (equal? 1 (array-dims a))
  )

(define-match-expander nothing->underscore
  (lambda (stx)
    (syntax-case stx ()
      [(_ (elts ...))
       #'(nothing->underscore () (elts ...))]
      [(_ (out ...) (· rest ...))
       #'(nothing->underscore (out ... _) (rest ...))]
      [(_ (out ...) (next rest ...))
       #'(nothing->underscore (out ... next) (rest ...))]
      [(_ (out ...) ())
       #'(out ...)])))

(define-match-expander lhsList
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(and (? array?) (? 1-D?) (app array->list (list elts ...)))
       ])))

(define-match-expander lhsStrand
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(lhsList elts ...)])))

(define-match-expander lhsArray
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(and array? (app array->array-list (nothing->underscore (list elts ...))))]
      )))

(define-match-expander lhsNS
  (lambda (stx)
    (syntax-case stx ()
      [(_ (elts ...))
       #'(bqn-ns () (elts ...))]
      [(_ (binds ...) ((bind-id key) rest ...))
       #'(bqn-ns (((quote key) bind-id) binds ...) (rest ...))]
      [(_ (binds ...) ((id-key) rest ...))
       #'(bqn-ns (((quote id-key) id-key) binds ...) (rest ...))]
      [(_ (binds ...) ())
       #'(hash-table binds ...)]
      )))

(define-macro-cases select-ids
  [(select-ids PATH (IDS ...) (_ ANY) REST ...)
   #'(select-ids PATH (IDS ...) ANY REST ...)]
  
  [(select-ids PATH (IDS ...) NAME REST ...)
   #'(select-ids PATH (NAME IDS ...) REST ...)]
  [(select-ids PATH (IDS ...) (BIND-ID ORIG-ID) REST ...)
   #'(select-ids PATH ([ORIG-ID BIND-ID] IDS ...) REST ...)]

  [(select-ids PATH (IDS ...))
   #'(require (only-in PATH IDS ...))]
  )

(define-macro-cases import
  [(import (IDS ...) ⇐ (bqn-req PATH))
   #'(begin
       (import (IDS ...) ← (bqn-req PATH))
       (provide (all-from-out PATH)))]
  [(import (IDS ...) ← (bqn-req PATH))
   #'(select-ids PATH () IDS ...)]
  [(import PATH)  #'(require PATH)]
  )