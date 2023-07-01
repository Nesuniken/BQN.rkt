#lang racket
(require br/macro BQN/blocks BQN/system-values)
(provide (all-defined-out))

(define-macro-cases select-ids
  [(select-ids PATH (IDS ...) (_ ANY) REST ...)
   #'(select-ids PATH (IDS ...) ANY REST ...)]
  
  [(select-ids PATH (IDS ...) NAME REST ...)
   #'(select-ids PATH (NAME IDS ...) REST ...)]
  [(select-ids PATH (IDS ...) (lhs-entry BIND-ID ORIG-ID) REST ...)
   #'(select-ids PATH ([ORIG-ID BIND-ID] IDS ...) REST ...)]

  [(select-ids PATH (IDS ...))
   #'(require (only-in PATH IDS ...))]
  )

(define-macro-cases import
  [(import (IDS ...) ⇐ (bqn-req PATH))
   #'(begin
       (import (IDS ...) ← (bqn-req PATH))
       (all-from-out PATH)
       )]
  [(import (IDS ...) ← (bqn-req PATH))
   #'(select-ids PATH () IDS ...)]
  [(import PATH)  #'(require PATH)]
  )


(define-macro-cases subExpr
  [(subExpr NAME ↩ VALUE)
   #'(get-expr (NAME ↩ (•strict VALUE)))]
  [(subExpr NAME FUNC ↩)
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(subExpr NAME FUNC ↩ ARG)
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(subExpr NAME ASSIGN VALUE)
   #'NAME]
  [(subExpr VALUE)
   #'VALUE])


(define-macro (expr EXPR)
  #'(begin
      (get-defs EXPR)
      (get-expr EXPR)
      ))

(define-macro-cases get-defs
  [(get-defs (subExpr REST ...))
   #'(get-defs (REST ...))]
  [(get-defs (NAME ⇐ VALUE))
   #'(begin
       (provide NAME)
       (get-defs (NAME ← VALUE)))]
  [(get-defs (NAME ← VALUE))
   #'(begin
       (get-defs VALUE)
       (define NAME (get-expr VALUE)))]
  
  [(get-defs (_ (body _ ...) _ ...))
   #'(begin)]
  
  [(get-defs (ANY ...))
   #'(get-defs ANY ...)]
  [(get-defs ATOM)
   #'(begin)]
  [(get-defs FIRST REST ...)
   #'(begin
       (get-defs FIRST)
       (get-defs REST ...))]
  [(get-defs _ ...)
   #'(begin)]
  )

(define-macro-cases get-expr  
  [(get-expr (NAME ⇐ VALUE))
   #'NAME]
  [(get-expr (NAME ← VALUE))
   #'NAME]
  [(get-expr (subExpr VALUE))
   #'(get-expr VALUE)]
  [(get-expr (subExpr REST ...))
   #'(subExpr REST ...)]
  [(get-expr (NAME ↩ VALUE))
   #'(begin (set! NAME VALUE) NAME)]

  [(get-expr (BLOCK (body STMTS ...)))
   #'(BLOCK STMTS ...)]
  [(get-expr (BLOCK (body STMTS ...) REST ...))
   #'(BLOCK (STMTS ...) REST ...)]

  [(get-expr (expr REST ...))
   #'(get-expr REST ...)]

  [(get-expr ((ANY ...)))
   #'(get-expr (ANY ...))]
  
  [(get-expr (STX PARAMS ...))
   #'(get-expr STX () (PARAMS ...))]

  [(get-expr STX (PREV ...) (NEXT REST ...))
   #'(get-expr STX (PREV ... (get-expr NEXT)) (REST ...))]
  [(get-expr STX (PARAMS ...) ())
   #'(STX PARAMS ...)]

  [(get-expr ATOM)
   #'ATOM]
  )