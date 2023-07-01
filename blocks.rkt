#lang racket
(require br/macro racket/stxparam)
(provide (all-defined-out))

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