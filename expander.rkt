#lang racket
(require
  BQN/assign BQN/blocks
  racket/stxparam math/array br/macro 
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

(provide
 #%top #%datum #%top-interaction
 (all-defined-out)
 (all-from-out
  BQN/assign BQN/blocks
  BQN/primitives BQN/arithmetic BQN/1-modifiers BQN/2-modifiers
  BQN/system-values)
 (rename-out [bqn-module #%module-begin] [bqn-app #%app]))
