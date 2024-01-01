#lang racket
(require
  "assign.rkt" "blocks.rkt" "../lhs.rkt"
  (only-in "../primitives/utilities.rkt" to-func apply-mod)
  racket/stxparam math/array br/macro
  "../primitives/primitives.rkt")

(define-macro-cases Derv
  [(Derv F) #'F]
  [(Derv F 1M  ) #'(1M F  )]
  [(Derv F 2M G) #'(2M F G)]
  )

(define-macro Fork #'Train)

(define (3-train L T R)
  (case-lambda
    [(x )  (T (R x  ) (L x  ))]
    [(x w) (T (R x w) (L x w))]))

(define-macro-cases Train
  [(Train   T) #'T]

  [(Train · T R)
   #'(BQN∘ T R)]
  
  [(Train   T R)
   #'(BQN∘  T R)]
  
  [(Train L T R)
   #'((apply-mod 3-train) L T R)]
  )

(define (apply-bqn-func F x [w (void)])
  (cond
    [(void? x) (void)]
    [(void? w) (F x)]
    [(F x w)]))

(define-macro-cases arg
  [(arg   F X) #'(apply-bqn-func F X)]
  [(arg W F X) #'(apply-bqn-func F X W)]
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

(define-macro (bqn-module PROGRAM)
  #'(#%module-begin
     (module configure-runtime racket/base
       (require BQN/setup)
       (setup!))
     (array-strictness #f)
     PROGRAM))

(define-macro (program EXPR ...)
  #'(begin EXPR ...))

(define-macro (bqn-app ID ARGS ...)
  #'(#%app (to-func ID) ARGS ...)
  )

(provide
 #%top #%datum #%top-interaction
 (all-defined-out)
 (all-from-out math/array "../lhs.rkt" "assign.rkt" "blocks.rkt" "../primitives/primitives.rkt")
 (rename-out [bqn-module #%module-begin] [bqn-app #%app]))
