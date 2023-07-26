#lang br
(require BQN/blocks BQN/system-values)
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

(define-macro (FuncExpr EXPR)
  #'EXPR)

(define-macro (1M-Expr EXPR)
  #'EXPR)

(define-macro (2M-Expr EXPR)
  #'EXPR)

(define-macro-cases subExpr
  [(subExpr NAME ↩ VALUE)
   #'(get-expr (NAME ↩ (•strict VALUE)))]
  [(subExpr NAME FUNC ↩)
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(subExpr NAME FUNC ↩ ARG)
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(subExpr (atom VALUE))
   #'VALUE]
  [(subExpr VALUE)
   #'VALUE])

(define-macro (stmt EXPR)
  (with-pattern
      ([((EXPORTS ...) (NAMES ...) (VALUES ...) RESULT) (extract-defs #'EXPR)])
    #'(begin
        EXPORTS ...
        (define NAMES VALUES) ...
        RESULT
        )))

(begin-for-syntax
  (require racket/list racket/match br/list)
  (define (find-defs stx)
    (match (syntax-e stx)         
      [(list _ name (app syntax-e (and (or '⇐ '←) sign)) value)
       (let*-values
           ([(exports defs vals expr)
             (find-defs value)]
            [(export)
             (if (equal? sign '⇐)
                 (cons name exports)
                 exports)])
         (values
          export
          (cons name defs)
          (cons expr vals)
          expr))]
                  
      [(list* _ (app syntax-e (list* 'body _)) _)
       (values empty empty empty stx)]
         
      [(? cons? split-stx)
       (for/fold
        ([total-export '()]
         [total-defs   '()]
         [total-values '()]
         [total-expr   '()]
         #:result (values
                   total-export
                   total-defs
                   total-values
                   (datum->syntax stx total-expr stx)
                   ))
        ([stmt (in-list (reverse split-stx))])
         (let-values
             ([(export defs vals expr)
               (find-defs stmt)])
           (values
            (append export total-export)
            (append defs   total-defs)
            (append vals total-values)
            (cons   expr   total-expr)
            )
           )
         )]
         
      [_ (values empty empty empty stx)]
      )
    )
  (define (extract-defs stmt-stx)
    (define-values (exports defs vals expr) (find-defs stmt-stx))
    (define export-stx
      (if (empty? exports)
          empty
          (list `(provide ,@exports))
          ))
    (list export-stx defs vals expr)
    )
  )