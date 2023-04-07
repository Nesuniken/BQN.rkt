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

(define-macro (bqn-module (program STATEMENT ...))
  #'(#%module-begin
     (define •exports (mutable-set))
     STATEMENT ...))
