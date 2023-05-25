#lang racket
(require racket/match math/array br/macro racket/stxparam
         BQN/primitives BQN/arithmetic
         BQN/1-modifiers BQN/2-modifiers
         BQN/system-values BQN/prim-utils)


(define-syntax-parameter 𝕣
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))
(define-syntax-parameter 𝕘
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))
(define-syntax-parameter 𝕗
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))
(define-syntax-parameter 𝕤
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))
(define-syntax-parameter 𝕨
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))
(define-syntax-parameter 𝕩
  (λ (stx) (raise-syntax-error #f "Special characters aren't permitted outside of a block" stx)))

(define-macro (rkt NAME STMTS ...)
  #'(NAME STMTS ...))

(define-macro-cases Derv
  [(Derv F) #'F]
  [(Derv F 1M  ) #'(1M F  )]
  [(Derv F 2M G) #'(2M F G)]
  )

(define-macro Fork #'Train)

(define-macro-cases Train
  [(Train   T) #'T]

  [(Train nothing T R)
   #'(BQN∘ T R)]
  
  [(Train   T R)
   #'(BQN∘  T R)]
  
  [(Train L T R)
   #'(case-lambda
       [(x  ) (T (R x  ) (L x  ))]
       [(x w) (T (R x w) (L x w))])]
  )

(define-macro-cases arg
  [(arg nothing F X) #'(F X)]
  [(arg   F X) #'(F X)]
  [(arg W F X) #'(F X W)]
  )

(define-macro a-list  #'strand)

(define-macro subBlock #'body)
(define-macro-cases body
  [(body EXPR) #'EXPR]
  [(body STMTS ...)
   #'((thunk STMTS ...))])

(define-macro (a-merge ELTS ...)
  #'(BQN> (strand ELTS ...)))

(define-macro (strand ELTS ...)
  #'(array #[ELTS ...]))

(define-macro-cases sub-literal
  [(sub-literal (CHARS ...))
   #'(array #[CHARS ...])]
  [(sub-literal VAL)
   #'VAL])

(define-macro (atom X) #'X)
(define-macro (Func F) #'F)

(define/match ((make-block monad dyad) #:undo? [undo? #f] . args)
  [( _  _ #t  _)         (error "Block functions are not invertable")]
  [(#f  _  _ (list _  )) (error "Block is dyadic only")]
  [( _ #f  _ (list _ _)) (error "Block is monadic only")]
  [( _  _ #f (list x  )) (monad x)]
  [( _  _ #f (list x w)) (dyad  x w)])

(define-macro (FuncBlock BODY)
  (with-syntax ([(S X W) (generate-temporaries '(𝕤 𝕩 𝕨))])
    #'(letrec
          ([S (make-block
               (lambda (X)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)]
                      [𝕨 (make-rename-transformer #'void)])
                   BODY))
               (lambda (X W)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)]
                      [𝕨 (make-rename-transformer #'W)])
                   BODY)))])
        S)))

(define-macro-cases 1M-block
  [(1M-block BODY 𝕤)
   (with-syntax ([(R F) (generate-temporaries '(𝕣 𝕗))])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   BODY))])
         R))]
  [(1M-block BODY 𝕊)
   #'(1M-block (FuncBlock BODY) 𝕤)])

(define-macro-cases 2M-block
  [(2M-block BODY 𝕤)
   (with-syntax ([(R F G) (generate-temporaries '(𝕣 𝕗 𝕘))])
     #'(letrec
           ([R (lambda (F G)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)]
                      [𝕘 (make-rename-transformer #'G)])
                   BODY))])
         R))]
  [(2M-block BODY 𝕊)
   #'(2M-block (FuncBlock BODY) 𝕤)])

(define-macro (2M-Expr ARGS ...)
  #'(expr ARGS ...))
(define-macro (1M-Expr ARGS ...)
  #'(expr ARGS ...))
(define-macro (FuncExpr ARGS ...)
  #'(expr ARGS ...))
(define-macro (subExpr ARGS ...)
  #'(expr ARGS ...))

(define-macro (stmt S)
  #'S)

(define-macro-cases expr
  [(expr (subExpr NAME ↩ VALUE))
   #'(begin
       (set! NAME (•strict VALUE))
       NAME)]
  [(expr (_ NAME ↩ VALUE))
   #'(expr NAME ↩ VALUE)]
  [(expr (subExpr NAME FUNC ↩))
   #'(subExpr NAME ↩ (FUNC NAME))]
  [(expr (subExpr NAME FUNC ↩ ARG))
   #'(subExpr NAME ↩ (FUNC NAME ARG))]
  [(expr NAME ↩ VALUE)
   #'(begin (set! NAME VALUE) NAME)]
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
     (module configure-runtime racket/base
       (require BQN/setup)
       (setup!))
     (array-strictness #f)
     EXPR ...))

(define (to-func x)
  (if (procedure? x)
      x
      (const x)))

(define-macro-cases bqn-app
  [(bqn-app ID X void) #'(bqn-app ID X)]
  [(bqn-app ID ARGS ...)
   #'((to-func ID) ARGS ...)])

(provide
 #%top #%datum #%top-interaction time-apply list
 (all-defined-out)
 (all-from-out BQN/primitives BQN/arithmetic BQN/1-modifiers BQN/2-modifiers BQN/system-values)
 (rename-out [bqn-module #%module-begin] [bqn-app #%app]))
