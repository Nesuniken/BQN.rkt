#lang racket
(require br/macro BQN/primatives racket/undefined)
(require (for-syntax br/syntax))

(define-macro (BQN⁼ F)
  (with-pattern ([INVERSE (suffix-id #'F '⁼)])
    #'INVERSE))

(define-macro-cases Derv
  [(Derv F) #'F]
  [(Derv F 1M  ) #'(1M F  )]
  [(Derv F 2M G) #'(2M F G)]
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

(define-macro a-list  #'strand)

(define-macro subBlock #'begin)
(define-macro body     #'begin)

(define-macro (a-merge ELTS ...)
  #'(BQN> (strand ELTS ...)))

(define-macro (strand ELTS ...)
  #'(array #[ELTS ...]))

(define-macro-cases sub-literal
  [(sub-literal (CHARS ...))
   #'(array #[CHARS ...])]
  [(sub-literal VAL)
   #'VAL])

(define-macro (atom VAL) #'VAL)

(define-macro-cases FuncBlock
  [(FuncBlock BODY 𝕊1)
   #'(letrec ([𝕤 (λ (x) BODY)])
       𝕤)]
  [(FuncBlock BODY 𝕊2)
   #'(letrec ([𝕤 (λ (x w) BODY)])
       𝕤)])

(define-macro-cases 1M-block
  [(1M-block BODY 𝕤)
   #'(letrec ([𝕣 (λ (𝕗) BODY)])
       𝕣)]
  [(1M-block BODY 𝕊)
   #'(letrec ([𝕣 (λ (𝕗) (FuncBlock BODY))])
       𝕣)])

(define-macro-cases 2M-block
  [(2M-block BODY 𝕤)
   #'(letrec ([𝕣 (λ (𝕗 𝕘) BODY)])
       𝕣)]
  [(2M-block BODY 𝕊)
   #'(letrec ([𝕣 (λ (𝕗 𝕘) (FuncBlock BODY))])
       𝕣)])

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
       (set! NAME (•strict VALUE))
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

(define-macro-cases def
  [(def NAME ⇐ VALUE)
   #'(begin
       (provide NAME)
       (def NAME ← VALUE))]
  [(def NAME ← (FuncBlock (body STMTS ...) 𝕊1))
   #'(define (NAME 𝕩)
       (define 𝕤 NAME)
       STMTS ...)]
  [(def NAME ← (FuncBlock (body STMTS ...) 𝕊2))
   #'(define (NAME 𝕩 𝕨)
       (define 𝕤 NAME)
       STMTS ...)]
  [(def NAME ← (1M-block (body STMTS ...) 𝕤))
   #'(define (NAME 𝕗)
       (define 𝕣 NAME)
       STMTS ...)]
  [(def NAME ← (2M-block (body STMTS ...) 𝕤))
   #'(define (NAME 𝕗 𝕘)
       (define 𝕣 NAME)
       STMTS ...)]
  [(def NAME ← (M-BLOCK BODY RET-VAL))
   #'(def NAME ← (M-BLOCK (FuncBlock BODY RET-VAL) 𝕤))]
  [(def NAME ← VALUE)
   #'(define NAME (•strict VALUE))])

(define-macro (bqn-module (program EXPR ...))
  #'(#%module-begin
     (array-strictness #f)
     EXPR ...))

(provide (all-defined-out)
         (except-out (all-from-out BQN/primatives) #%module-begin)
         (rename-out [bqn-module #%module-begin]))
