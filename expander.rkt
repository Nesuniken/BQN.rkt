#lang racket
(require br/macro br/syntax BQN/primatives)

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

(define-macro (a-merge ELTS ...)
  #'(BQN> (strand ELTS ...)))

(define-macro (strand ELTS ...)
  #'(array #[ELTS ...]))

(define-macro-cases sub-literal
  [(sub-literal (CHARS ...))
   #'(list->array #[CHARS ...])]
  [(sub-literal VAL)
   #'VAL])

(define-macro (atom VAL) #'VAL)

(define-macro (2M-Expr ARGS ...)
  #'(expr ARGS ...))
(define-macro (1M-Expr ARGS ...)
  #'(expr ARGS ...))
(define-macro (FuncExpr ARGS ...)
  #'(expr ARGS ...))
(define-macro (subExpr ARGS ...)
  #'(expr ARGS ...))

(define-macro-cases expr
  [(expr (_ NAME ↩ VALUE))
   #'(begin
       (set! NAME (array-strict VALUE))
       NAME)]
  [(expr (subExpr NAME FUNC ↩))
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(expr (subExpr NAME FUNC ↩ ARG))
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(expr (_ VALUE))
   #'VALUE]
  [(expr VALUE)
   #'VALUE]
  )

(define-macro (def NAME ← VALUE)
  #'(define NAME (•strict VALUE)))

(define-macro (bqn-module (program EXPR ...))
  #'(#%module-begin
     (array-strictness #f)
     EXPR ...))

(provide (all-defined-out)
         (except-out (all-from-out BQN/primatives) #%module-begin)
         (rename-out [bqn-module #%module-begin]))
