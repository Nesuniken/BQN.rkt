#lang typed/racket
(require br/macro)
(provide #%top #%app #%datum #%top-interaction
         (rename-out [bqn-module #%module-begin]))

(define-macro-cases Derv
  [(Derv F) #'F]
  [(Derv F (1Mod 1M)  ) #'(1M F  )]
  [(Derv F (2Mod 2M) G) #'(2M F G)]
  )

(define-macro-cases Fork
  [(Fork   F) #'F]
  
  [(Fork   F R)
   #'(case-lambda
       [(x  ) (F (R x  ))]
       [(x w) (F (R x w))])]
  
  [(Fork L F R)
   #'(case-lambda
       [(x  ) (F (R x  ) (L x  ))]
       [(x w) (F (R x w) (L x w))])]
  )

(define-macro-cases arg
  [(arg   F X) #'(F X)]
  [(arg W F X) #'(F X W)]
  )

(define-macro-cases def
  [(def (TYPE NAME ⇐ VALUE))
   #'(begin
       (export NAME)
       (TYPE NAME ← VALUE))]
  [(def (TYPE NAME-1 SIGN-1 (_ NAME-2 SIGN-2 VALUE)))
     #'(begin
         (TYPE NAME-2 SIGN-2 VALUE)
         (TYPE NAME-1 SIGN-1 NAME-2))]
  [(def D) #'D])

(define-macro-cases 2M-DefExpr
  [(2M-DefExpr NAME SIGN (2M-Def 2M))
   #'(begin
       (2M-DefExpr #,@(prefix-id '0 NAME) ← 2M)
       (2M-DefExpr NAME SIGN #,@(prefix-id '0 NAME)))]
  [(2M-DefExpr NAME ← 2M)
   #'(define NAME 2M)])

(define-macro-cases 1M-DefExpr
  [(1M-DefExpr NAME SIGN (1M-Def 1M))
   #'(begin
       (1M-DefExpr #,@(prefix-id '0 NAME) ← 1M)
       (1M-DefExpr NAME SIGN #,@(prefix-id '0 NAME)))]
  [(1M-DefExpr NAME ← 1M)
   #'(define NAME 1M)])

(define-macro-cases FuncDefExpr
  [(FuncDefExpr NAME SIGN (FuncDef F))
   #'(begin
       (FuncDefExpr #,@(prefix-id '0 NAME) ← F)
       (FuncDefExpr NAME SIGN #,@(prefix-id '0 NAME)))]
  )

(define-macro (export ELTS)
  #'(set-union! •exports #,@(get-names ELTS)))

(define-macro (bqn-module (program STATEMENT ...))
  #'(#%module-begin
     (define •exports (mutable-set))
     STATEMENT ...))

(begin-for-syntax
  (require racket/list racket/set br/syntax)
  (define (get-names elts)
    (set-subtract
     (list->set
      (map
       (λ (sl) (first (syntax->datum sl)))
       (stx-flatten elts)))
     (set
      'name 'sub-custom 'func-custom '1Mod-custom '2Mod-custom
        'lhs-sub 'lhs-any 'lhs-atom 'lhs-elt 'lhs-entry
        'lhsStrand 'lhsList 'lhsArray 'lhsComp 'lhs)))
  )
