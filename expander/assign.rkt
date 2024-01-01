#lang br
(require racket/stxparam "blocks.rkt" "../lhs.rkt"  "../primitives/primitives.rkt")
(provide (all-defined-out))

(define-macro (FuncExpr EXPR)
  #'EXPR)

(define-macro (1M-Expr EXPR)
  #'EXPR)

(define-macro (2M-Expr EXPR)
  #'EXPR)

(define-macro-cases subExpr
  [(subExpr NAME ↩ VALUE)
   #'(set! NAME (•strict VALUE))]
  [(subExpr NAME FUNC ↩)
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(subExpr NAME FUNC ↩ ARG)
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(subExpr (atom VALUE))
   #'VALUE]
  [(subExpr VALUE)
   #'VALUE])

(define-macro (select-ids PATH IDS)
  (with-pattern
      ([(BINDS ...)
        (pattern-case-filter #'IDS
          [(BIND-ID ORIG-ID) #'(ORIG-ID BIND-ID)]
          [NAME #'NAME])])
    #'(require (only-in PATH BINDS ...))
    ))

(define-macro-cases import
  [(import (IDS ...) ⇐ (bqn-req PATH))
   #'(begin
       (import (IDS ...) ← (bqn-req PATH))
       (provide (all-from-out PATH)))]
  [(import (IDS ...) ← (bqn-req PATH))
   #'(select-ids PATH () IDS ...)]
  [(import PATH)  #'(require PATH)]
  )

(define-macro (stmt EXPR)
  (with-pattern
      ([((EXPORTS ...) (NAMES ...) (VALUES ...) (RESULT ...)) (extract-defs #'EXPR)])
    #'(begin
        EXPORTS ...
        (make-defs (NAMES ...) (VALUES ...))
        RESULT ...
        )))

(define-macro-cases make-defs
  [(make-defs ((·) DEF-REST ...) (_ VAL-REST ...))
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
   #'(void)])

(begin-for-syntax
  (require racket/list racket/match br/list)
  
  (define (find-defs stx)
    (pattern-case stx
      [(_ NAME (def SIGN) VALUE)
       (let*-values
           ([(exports defs vals expr)
             (find-defs #'VALUE)]
            [(export)
             (pattern-case #'SIGN
               [⇐ (cons #'NAME exports)]
               [← exports])])
         (values export
                 (cons #'NAME defs)
                 (cons expr vals)
                 expr))]
      
      [(_ (body _ ...))
       (values empty empty empty stx)]
      [(_ (_ (body _ ...)) _ ...)
       (values empty empty empty stx)]

      [(EXPR ...)
       (for/fold
        ([total-export '()]
         [total-defs   '()]
         [total-values '()]
         [total-expr   '()]
         #:result
         (values total-export total-defs total-values
                 (datum->syntax stx total-expr stx)))
        ([stmt (in-list (reverse (syntax-e #'(EXPR ...))))])
         (let-values
             ([(export defs vals expr) (find-defs stmt)])
           (values
            (append export total-export)
            (append defs   total-defs)
            (append vals   total-values)
            (cons   expr   total-expr)))
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
    (define expr-stx
      (if (equal? '· (syntax-e expr))
          empty
          (list expr)))
    
    (list export-stx defs vals expr-stx)
    )
  )