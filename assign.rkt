#lang racket
(require br/macro)
(provide (all-defined-out))

(define-macro-cases select-ids
  [(select-ids PATH (IDS ...) (name NAME) REST ...)
   #'(select-ids PATH (NAME IDS ...) REST ...)]
  
  [(  select-ids PATH (IDS ...) (_ ANY) REST ...)
   #'(select-ids PATH (IDS ...) ANY REST ...)]

  [(select-ids PATH (IDS ...) (lhs-entry BIND-ID ORIG-ID) REST ...)
   #'(select-ids PATH ([ORIG-ID BIND-ID] IDS ...) REST ...)]

  [(select-ids PATH (IDS ...))
   #'(require (only-in PATH IDS ...))]
  )

(define-macro-cases def
  [(def NAME ⇐ VALUE)
   #'(begin
       (provide NAME)
       (def NAME ← VALUE))]
  [(def (_ IDS ...) ← (bqn-req PATH))
   #'(select-ids PATH () IDS ...)]
  [(def NAME ← VALUE)
   #'(define NAME (•strict VALUE))])

(define-macro (bqn-req PATH)
  #'(require PATH))

(define-macro-cases expr
  [(expr (subExpr NAME ↩ VALUE))
   #'(begin
       (set! NAME (•strict VALUE))
       NAME)]
  [(expr (_ NAME ↩ VALUE))
   #'(expr NAME ↩ VALUE)]
  [(expr (_ NAME FUNC ↩))
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(expr (_ NAME FUNC ↩ ARG))
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(expr NAME ↩ VALUE)
   #'(begin (set! NAME VALUE) NAME)]
  [(expr (_ VALUE))
   #'VALUE]
  [(expr VALUE)
   #'VALUE]
  )