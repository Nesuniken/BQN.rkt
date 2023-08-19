#lang br
(require racket/stxparam)
(provide (all-defined-out))

(begin-for-syntax
  (define (special-error stx)
    (raise-syntax-error
     #f "Special characters aren't permitted outside of a block" stx)))

(define-syntax-parameter 𝕣 special-error)
(define-syntax-parameter 𝕘 special-error)
(define-syntax-parameter 𝕗 special-error)
(define-syntax-parameter 𝕤 special-error)
(define-syntax-parameter 𝕨 special-error)
(define-syntax-parameter 𝕩 special-error)

(define-macro subBlock #'body)

(define-macro-cases body
  [(body EXPR) #'EXPR]
  [(body STMTS ...)
   #'((thunk STMTS ...))])

(define (make-func-block monad dyad)
  (lambda (x [w (void)])
    (cond
      [(void? w) (monad x)]
      [(dyad x w)])))

(define-macro (headless-func STMTS ...)
  (with-syntax ([S (datum->syntax caller-stx '𝕤)]
                [X (datum->syntax caller-stx '𝕩)]
                [W (generate-temporary '𝕨)])
    #'(letrec
          ([S (make-func-block
               (lambda (X)
                 (syntax-parameterize
                     ([𝕨 (make-rename-transformer #'void)])
                   (STMTS ...)))
               (lambda (X W)
                 (syntax-parameterize
                     ([𝕨 (make-rename-transformer #'W)])
                   (STMTS ...))))])
        S)))

(begin-for-syntax
  (require racket/list racket/match)
  (define (parse-infers first-stx . rest-stx)
    (define (add-infer key)
     (define infer-dict
      (if (empty? rest-stx)
          #hash(((#\˜ #\⁼) . ())
                ((    #\⁼) . ())
                (()        . ()))
          (parse-infers rest-stx)))
      (hash-update infer-dict key (λ (l) (cons first-stx l)))
      )
    (define modifiers
      (pattern-case first-stx
        [((headW _) _ MODS ...) #'(MODS ...)]
        [(_ MODS ...) #'(MODS ...)]))
    (pattern-case modifiers
      [((swap-undo) _ ...) (add-infer '(#\˜ #\⁼))]
      [((undo)      _ ...) (add-infer '(#\⁼))]
      [((no-mod)    _ ...) (add-infer '())]
      )
    )
  
  (define (func-head-block bodies)
    (match-define
      (hash-table
       ((list #\˜ #\⁼) ~undo)
       ((list #\⁼)      undo)
       ('()             call))
      (apply parse-infers (syntax-e bodies)))
    (datum->syntax
     bodies
     `(bqn-func
       (block->lambda  call)
       (block->lambda  undo)
       (block->lambda ~undo))
     )
    )
  )

(define-macro-cases FuncBlock
  [(FuncBlock (body STMTS ...))
   #'(headless-func STMTS ...)]
;  [(FuncBlock BODIES ...)]
  )

(define-macro block->lambda)

(define-macro-cases 1M-block
  [(1M-block (STMTS ...) 𝕤)
   (with-syntax ([(R F) (generate-temporaries '(𝕣 𝕗))])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   (STMTS ...)))])
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