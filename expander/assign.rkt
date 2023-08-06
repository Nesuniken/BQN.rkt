#lang br
(require "blocks.rkt" "lhs.rkt"  "../primitives/primitives.rkt")
(provide (all-defined-out))

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
        (make-defs (NAMES ...) (VALUES ...))
        RESULT
        )))

(define-macro-cases make-defs
  [(make-defs (· DEF-REST ...) (VALUE VAL-REST ...))
   #'(make-defs (DEF-REST ...) (VAL-REST ...))]
  [(make-defs ((PATTERN) DEF-REST ...) (VALUE VAL-REST ...))
   #'(begin
       (match-define PATTERN VALUE)
       (make-defs (DEF-REST ...) (VAL-REST ...)))]
  [(make-defs (NAME DEF-REST ...) (VALUE VAL-REST ...))
   #'(begin
       (define NAME VALUE)
       (make-defs (DEF-REST ...) (VAL-REST ...)))]
  [(make-defs () ())
   #'(begin)])

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