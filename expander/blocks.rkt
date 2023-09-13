#lang br
(require racket/stxparam)
(provide (all-defined-out))

(define-syntax-parameter 𝕣
  (λ (stx) (raise-syntax-error #f "𝕣 found outside        modifier block" stx)))
(define-syntax-parameter 𝕘
  (λ (stx) (raise-syntax-error #f "𝕘 found outside      2-modifier block" stx)))
(define-syntax-parameter 𝕗
  (λ (stx) (raise-syntax-error #f "𝕗 found outside        modifier block" stx)))
(define-syntax-parameter 𝕤
  (λ (stx) (raise-syntax-error #f "𝕤 found outside        function block" stx)))
(define-syntax-parameter 𝕨
  (λ (stx) (raise-syntax-error #f "𝕨 found outside dyadic function block" stx)))
(define-syntax-parameter 𝕩
  (λ (stx) (raise-syntax-error #f "𝕩 found outside        function block" stx)))

(define-macro subBlock #'body)

(define-macro-cases body
  [(body EXPR) #'EXPR]
  [(body STMTS ...)
   #'((thunk STMTS ...))])

(define-macro (headless-func BODY)
  (with-syntax ([(W S X) #'(𝕨 𝕤 𝕩)])
    #'(syntax-parameterize
          ([𝕤 (make-rename-transformer #'S)]
           [𝕩 (make-rename-transformer #'X)])
        (let* ([S (case-lambda
                    [(X) (syntax-parameterize
                             ([𝕨 (make-rename-transformer #'void)])
                           BODY)]
                    [(X W) (syntax-parameterize
                               ([𝕨 (make-rename-transformer #'W)])
                             BODY)])])))))

(begin-for-syntax
  (require racket/list racket/match)

  (define (process-func stx)
    (pattern-case stx
      [((F _ X) BODY)
       #`(((_ F X) BODY))]
      [((LHS) BODY)
       #'(((_ _ LHS) BODY))]
      [((F _) BODY)
       (with-syntax ([W (datum->syntax #'BODY '𝕨)])
         #'((((and (not (? void?)) W) F X)
             BODY)
            (((? void) F X)
             BODY)))]
      [((· F _ X) BODY)
       (with-syntax ([W (datum->syntax #'BODY '𝕨)])
         #'(((and (not (? void?)) W) F X)
            BODY))]
      [((W F _ X) BODY)
       (with-syntax ([WW (datum->syntax #'BODY '𝕨)])
         #'(((and (not (? void?)) W WW) F X)
            BODY))]
      )
    )
  
  (define (parse-infers first-stx . rest-stx)    
    (define (add-infer key)
     (define infer-dict
      (if (empty? rest-stx)
          #hash(((#\˜ #\⁼) . ())
                ((    #\⁼) . ())
                (()        . ()))
          (parse-infers rest-stx)))

      (define pattern-list
        (syntax-e (process-func first-stx)))
      
      (define (push l)
        (define l+ (cons (first pattern-list) l))
        (if (empty? (rest pattern-list))
            l+
            (cons (second pattern-list) l+)))
      
      (hash-update infer-dict key push)
      )
    (define modifiers
      (pattern-case (first (syntax-e first-stx))
        [((headW _) _ MODS _ ...) #'MODS]
        [(_ MODS _ ...) #'MODS]
        [ _ #'(no-mod)]
        ))
    
    (pattern-case modifiers
      [(swap-undo) (add-infer '(#\˜ #\⁼))]
      [(undo)      (add-infer '(#\⁼))]
      [(no-mod)    (add-infer '())]
      )
    )
  
  (define (func-head-block bodies)
    (match-define
      (hash-table
       ((list #\˜ #\⁼) ~undo)
       ((list #\⁼)      undo)
       ('()             call))
      (apply parse-infers (syntax-e bodies)))
    
    (cond
      [(and (empty? undo) (empty? ~undo))
       (datum->syntax bodies '(func->lambda ,call))]
      
      [else
       (datum->syntax
        bodies
        `(bqn-func
          (func->lambda  ,call)
          (func->lambda  ,undo)
          (func->lambda ,~undo))
        )])
    )
  )

(define-macro-cases FuncBlock
  [(FuncBlock BODY)
   #'(headless-func BODY)]
  [(FuncBlock BODIES ...)
   #'(func-head-block (BODIES ...))]
  )

(define-macro (func->lambda (PATTERNS ...))
  (with-syntax ([(S X) #'(𝕤 𝕩)])
    #'(letrec
          ([S (lambda (X [w? (void)])
                (syntax-parameterize
                    ([𝕤 (make-rename-transformer #'S)]
                     [𝕩 (make-rename-transformer #'X)])
                  (match/values (values w? 𝕤 𝕩)
                    PATTERNS ...)))]))))

(define-macro-cases 1M-Imm-Block
  [(_ BODY)
   (with-syntax ([(R F) #'(𝕣 𝕗)])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   BODY))])
         R))]
  [(_ BODIES ...)
   (with-syntax ([(R F) #'(𝕣 𝕗)])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   (match/values (values 𝕗 𝕣)
                     BODIES ...)))])
         R))])

(define-macro (mod-match (FRG ...) BODIES)
  (with-pattern
      ([(CASES ...)
        (pattern-case-filter #'BODIES
          [((H F) BODY)
           (with-syntax ([W (datum->syntax #'BODY '𝕨)])
             #'((H W F #f _)
                  BODY))]
          [((H · F M X) BODY)
           (with-syntax ([W (datum->syntax #'BODY '𝕨)])
             #'((H (and (not (? void?)) W) F M X)
                BODY))]
          [((H W F M X) BODY)
           (with-syntax ([WW (datum->syntax #'BODY '𝕨)])
             #'((H (and (not (? void?)) W WW) F M X)
                BODY))]
          [((H F M X) BODY)
           #'((H _ F M X) BODY)]
          )])
    (with-syntax
        ([(S X) #'(𝕤 𝕩)])
      #'(letrec
            ([S* (lambda (m)
                   (lambda (X [w? (void)])
                     (syntax-parameterize
                         ([𝕤 (make-rename-transformer #'S)]
                          [𝕩 (make-rename-transformer #'X)])
                       (match/values (values w? FRG ... m 𝕩)
                         CASES ...))))]
             [S (bqn-func (S* #f) (S* 'undo) (S* '~undo))])
          S)))
  )

(define-macro-cases 1M-Del-Block
  [(_ BODY)
   (with-syntax ([(R F) #'(𝕣 𝕗)])
     #'(letrec
           ([R (lambda (F)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)])
                   (headless-func BODY)))])
         R))]
  [(_ BODIES ...)
   (with-syntax ([(R F) #'(𝕣 𝕗)])
     #'(letrec
           ([R (lambda (F)
                    (syntax-parameterize
                        ([𝕣 (make-rename-transformer #'R)]
                         [𝕗 (make-rename-transformer #'F)])
                      (mod-match (F R) (BODIES ...))
                      ))])
         R))]
  )

(define-macro-cases 2M-Imm-Block
  [(_ BODY)
   (with-syntax ([(R F G) #'(𝕣 𝕗 𝕘)])
     #'(letrec
           ([R (lambda (F G)
                (syntax-parameterize
                    ([𝕣 (make-rename-transformer #'R)]
                     [𝕗 (make-rename-transformer #'F)]
                     [𝕘 (make-rename-transformer #'G)])))])
         R))]
  [(_ BODIES ...)
   (with-syntax ([(R F G) #'(𝕣 𝕗 𝕘)])
     #'(letrec
           ([R (lambda (F G)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)]
                      [𝕘 (make-rename-transformer #'G)])
                   (match/values (values 𝕗 𝕣 𝕘)
                     BODIES ...)))])
         R))])

(define-macro-cases 2M-Del-Block
  [(_ BODY)
   (with-syntax ([(R F G) #'(𝕣 𝕗 𝕘)])
     #'(letrec
           ([R (lambda (F G)
                 (syntax-parameterize
                     ([𝕣 (make-rename-transformer #'R)]
                      [𝕗 (make-rename-transformer #'F)]
                      [𝕘 (make-rename-transformer #'G)])
                   (headless-func BODY)))])
         R))]
  [(_ BODIES ...)
   (with-syntax ([(R F G) #'(𝕣 𝕗 𝕘)])
     #'(letrec
           ([R (lambda (F G)
                    (syntax-parameterize
                        ([𝕣 (make-rename-transformer #'R)]
                         [𝕗 (make-rename-transformer #'F)]
                         [𝕘 (make-rename-transformer #'G)])
                      (mod-match (F R G) (BODIES ...))
                      ))])
         R))]
  )

(define-match-expander else-head
  (λ (stx) #'(_ _ _)))

(define-match-expander FuncLab
  (lambda (stx)
    (pattern-case stx
      [(_ 𝕤) #'_]
      [(_ F) #'F])))

(define-match-expander headX
  (lambda (stx)
    (pattern-case stx
      [(_ 𝕩) #'_]
      [(_ X) #'X])))

(define-match-expander 1ModDelayHead
  (lambda (stx)
    (pattern-case stx
      [(_ W (F R) M X)
       #'(W F R M X)]
      [(_ W (R) M X)
       #'(W _ R M X)])))

(define-match-expander 2ModDelayHead
  (lambda (stx)
    (pattern-case stx
      [(_ W (F R G) M X)
       #'(W F R G M X)]
      [(_ W (R) M X)
       #'(W _ R _ M X)])))