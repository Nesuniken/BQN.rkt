#lang racket
(require math/array br/macro racket/stxparam
         BQN/primitives BQN/arithmetic
         BQN/1-modifiers BQN/2-modifiers
         BQN/system-values)
(require (for-syntax br/syntax))

(define-syntax-parameter 𝕣
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))
(define-syntax-parameter 𝕘
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))
(define-syntax-parameter 𝕗
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))
(define-syntax-parameter 𝕤
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))
(define-syntax-parameter 𝕨
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))
(define-syntax-parameter 𝕩
  (λ (stx) (raise-syntax-error #f "Special characters are illegal outside of a block" stx)))

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
   (with-syntax ([(S X) (generate-temporaries '(𝕤 𝕩))])
     #'(letrec
           ([S (lambda (X)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)])
                   BODY))])
         S))]
  [(FuncBlock BODY 𝕊2)
   (with-syntax ([(S X W) (generate-temporaries '(𝕤 𝕩 𝕨))])
     #'(letrec
           ([S (lambda (X W)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)]
                      [𝕨 (make-rename-transformer #'W)])
                   BODY))])
         S))])

(define-macro-cases 1M-block
  [(1M-block BODY 𝕊0)
   (with-syntax ([(R F) (generate-temporaries '(𝕣 𝕗))])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   BODY))])
         R))]
  [(1M-block BODY RET-TYPE)
   #'(1M-block (FuncBlock BODY RET-TYPE) 𝕊0)])

(define-macro-cases 2M-block
  [(2M-block BODY 𝕊0)
   (with-syntax ([(R F G) (generate-temporaries '(𝕣 𝕗 𝕘))])
     #'(letrec
           ([R (lambda (F G)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)]
                      [𝕘 (make-rename-transformer #'G)])
                   BODY))])
         R))]
  [(2M-block BODY RET-TYPE)
   #'(2M-block (FuncBlock BODY RET-TYPE) 𝕊0)])

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
  [(def NAME ← VALUE)
   #'(define NAME (•strict VALUE))])

(define-macro (bqn-module (program EXPR ...))
  #'(#%module-begin
     (array-strictness #f)
     EXPR ...))

(provide
 #%top #%app #%datum #%top-interaction
 (all-defined-out)
 (all-from-out BQN/primitives BQN/arithmetic BQN/1-modifiers BQN/2-modifiers BQN/system-values)
 (rename-out [bqn-module #%module-begin]))
