#lang typed/racket
(require racket/provide math/array br/macro "primatives.rkt")
(provide #%top #%app #%datum #%top-interaction
         (rename-out [bqn-module #%module-begin])
         (matching-identifiers-out #px"^[^!].+" (all-defined-out)))

(define-macro-cases Derv
  [(Derv F) #'F]
  [(Derv F (1Mod 1M)  ) #'(1M F  )]
  [(Derv F (2Mod 2M) G) #'(2M F G)]
  )

(define-macro Fork #'Train)

(define-macro-cases Train
  [(Train   T) #'T]
  
  [(Train   T R)
   #'(case-lambda
       [(x  ) (T (R x  ))]
       [(x w) (T (R x w))])]
  
  [(Train L T R)
   #'(case-lambda
       [(x  ) (T (R x  ) (L x  ))]
       [(x w) (T (R x w) (L x w))])]
  )

(define-macro-cases arg
  [(arg   F X) #'(F X)]
  [(arg W F X) #'(F X W)]
  )

(define-macro a-list #'strand)

(define-macro (strand ELTS ...)
  #'(array #[ELTS ...]))

(define-macro (a-merge ELTS ...)
  #'(array-append* '(ELTS ...)))

(define-macro (complex REAL IMAG)
  #'(make-rectangular REAL IMAG))

(define-macro-cases real
  [(real ¯ ∞) #'-inf.0]
  [(real   ∞) #'+inf.0]
  [(real ¯ ARGS ...)
   #'(- (real ARGS ...))]
  [(real π) #'pi]
  [(real π EXP)
   #'(* pi (real 1 EXP))]
  [(real NUM EXP)
   #'(string->number (~a NUM "e" EXP))])

(define-macro (character CHAR)
  #'(first (string->list CHAR)))

(define-macro (string STR)
  #'(list->array STR))

(define-macro 2M-Expr  #'expr)
(define-macro 1M-Expr  #'expr)
(define-macro FuncExpr #'expr)
(define-macro subExpr  #'expr)

(define-macro-cases expr
  [(expr (_ NAME ↩ VALUE))
   #'(begin
       (set! NAME VALUE)
       NAME)]
  [(expr (subExpr NAME FUNC ↩))
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(expr (subExpr NAME FUNC ↩ ARG))
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(expr (_ VALUE))
   #'VALUE]
  )

(define-macro (def NAME ← VALUE)
  #'(define NAME VALUE))

(define-macro (bqn-module (program EXPR ...))
  #'(#%module-begin
     EXPR ...))
