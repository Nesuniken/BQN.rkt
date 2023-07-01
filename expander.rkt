#lang racket
(require racket/stxparam math/array br/macro 
         BQN/primitives BQN/arithmetic
         BQN/1-modifiers BQN/2-modifiers
         BQN/system-values BQN/prim-utils)

;Core-----------------------------------------------------------------------------

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

(define-macro (a-merge ELTS ...)
  #'(BQN> (strand ELTS ...)))

(define-macro (strand ELTS ...)
  #'(array #[ELTS ...]))

(define-macro (number R1 R2)
   #'(make-rectangular R1 R2)
  )

(define (pi-exp int)
  (* pi (expt 10 int))
  )

(define-macro-cases real
  [(real ¯ ∞) #'-inf.0]
  [(real   ∞) #'+inf.0]
  [(real ¯ π REST ...)
   #'(- (real π REST ...))]
  [(real π) #'pi]
  [(real π INT)
   #'(pi-exp INT)]
  )

(define-macro (atom X) #'X)
(define-macro (Func F) #'F)


(define-macro (stmt S) #'S)

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


;Assignment----------------------------------------------------------

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

;Blocks------------------------------------------------------------------------------------

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

(define-macro subBlock #'body)
(define-macro-cases body
  [(body EXPR) #'EXPR]
  [(body STMTS ...)
   #'((thunk STMTS ...))])

(define (make-func-block monad dyad)
  (lambda (x [w (void)] #:undo? [undo? #f])
    (cond
      [undo?     (error "Block functions are not invertable")]
      [(void? w) (monad x)]
      [(dyad x w)])))

(define-macro (FuncBlock STMTS ...)
  (with-syntax ([(S X W) (generate-temporaries '(𝕤 𝕩 𝕨))])
    #'(letrec
          ([S (make-func-block
               (lambda (X)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)]
                      [𝕨 (make-rename-transformer #'void)])
                   STMTS ...))
               (lambda (X W)
                 (syntax-parameterize
                     ([𝕤 (make-rename-transformer #'S)]
                      [𝕩 (make-rename-transformer #'X)]
                      [𝕨 (make-rename-transformer #'W)])
                   STMTS ...)))])
        S)))

(define-macro-cases 1M-block
  [(1M-block (STMTS ...) 𝕤)
   (with-syntax ([(R F) (generate-temporaries '(𝕣 𝕗))])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   STMTS ...))])
         R))]
  [(1M-block (STMTS ...) 𝕊)
   #'(1M-block (FuncBlock STMTS ...) 𝕤)])

(define-macro-cases 2M-block
  [(2M-block (STMTS ...) 𝕤)
   (with-syntax ([(R F G) (generate-temporaries '(𝕣 𝕗 𝕘))])
     #'(letrec
           ([R (lambda (F G)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)]
                      [𝕘 (make-rename-transformer #'G)])
                   (STMTS ...)))])
         R))]
  [(2M-block (STMTS ...) 𝕊)
   #'(2M-block (FuncBlock STMTS ...) 𝕤)])

(provide
 #%top #%datum #%top-interaction
 (all-defined-out)
 (all-from-out BQN/primitives BQN/arithmetic BQN/1-modifiers BQN/2-modifiers
               BQN/system-values)
 (rename-out [bqn-module #%module-begin] [bqn-app #%app]))
