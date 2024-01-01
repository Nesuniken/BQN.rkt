#lang br
(require racket/stxparam "../primitives/utilities.rkt")
(provide (all-defined-out))

(define-syntax-parameter 𝕣
  (λ (stx) (raise-syntax-error #f "𝕣 found outside modifier block" stx)))
(define-syntax-parameter 𝕘
  (λ (stx) (raise-syntax-error #f "𝕘 found outside 2-modifier block" stx)))
(define-syntax-parameter 𝕗
  (λ (stx) (raise-syntax-error #f "𝕗 found outside modifier block" stx)))
(define-syntax-parameter 𝕤
  (λ (stx) (raise-syntax-error #f "𝕤 found outside function block" stx)))
(define-syntax-parameter 𝕨
  (λ (stx) (raise-syntax-error #f "𝕨 found outside dyadic function block" stx)))
(define-syntax-parameter 𝕩
  (λ (stx) (raise-syntax-error #f "𝕩 found outside function block" stx)))

(define-macro subBlock #'body)
(define-macro branch   #'body)

(define · (void))

(define-macro-cases body
  [(body EXPR) #'EXPR]
  [(body STMTS ...)
   #'((thunk STMTS ...))])

(define-macro (ifElse IF THEN ELSE)
  #'(case IF
      [(1) THEN]
      [(0) ELSE]
      [else (error "Predicate value must be 0 or 1")]))

(define-macro-cases ifBreak
  [(_ IF THEN ELSE) #'(ifElse IF THEN ELSE)]
  [(_ IF THEN)
   (with-pattern
       ([ELSE (datum->syntax caller-stx '(failure-cont))])
     #'(ifElse IF THEN ELSE))])

(define-macro-cases headless-func
  [(_ (_ BODY))
   (with-syntax ([(W S X) #'(*𝕨 *𝕤 *𝕩)])
     #'(letrec ([S (lambda (X [W (void)])
                     (syntax-parameterize
                         ([𝕨 (make-rename-transformer #'W)]
                          [𝕤 (make-rename-transformer #'S)]
                          [𝕩 (make-rename-transformer #'X)])
                       BODY))])
         S))]
  [(_ (_ MONAD) (_ DYAD))
   (with-syntax ([(W S X) #'(*𝕨 *𝕤 *𝕩)])
     #'(letrec
           ([S (case-lambda
                 [(X) (syntax-parameterize
                          ([𝕤 (make-rename-transformer #'S)]
                           [𝕩 (make-rename-transformer #'X)])
                        MONAD)]
                 [(X W) (syntax-parameterize
                            ([𝕨 (make-rename-transformer #'W)]
                             [𝕤 (make-rename-transformer #'S)]
                             [𝕩 (make-rename-transformer #'X)])
                          DYAD)])])
         S))]
  )

(begin-for-syntax
  (require racket/list racket/match)
  
  (define (parse-infers first-stx . rest-stx)    
    (define (add-infer key)
      (define infer-dict
        (if (empty? rest-stx)
            (make-hash
             '((swap-undo . ())
               (undo      . ())
               (no-mod    . ())))
            (apply parse-infers rest-stx)))
      
      (define (push l)
        (cons first-stx l))
      
      (hash-update! infer-dict key push)
      infer-dict
      )
    (define modifiers
      (pattern-case (first (syntax-e first-stx))
        [(lhsComp    _ ...) #'no-mod]
        [(_)                #'no-mod]
        [(_   (MODS)      ) #'MODS]
        [(_ _ (MODS) _ ...) #'MODS]
        ))

    (add-infer (syntax-e modifiers))
    )
  
  (define (infer-head-block bodies)
    (match-define
      (hash-table
       ('swap-undo ~undo)
       ('undo       undo)
       ('no-mod     call))
      (apply parse-infers (syntax-e bodies)))
    
    (cond
      [(and (empty? undo) (empty? ~undo))
       call]
      
      [else (list 'invertable call undo ~undo)])
    )
  )

(define-macro (InferBlock FINAL BODIES ...)
  (pattern-case (infer-head-block #'(BODIES ...))
    [(invertable REGULAR UNDO TILDE-UNDO)
     #'(bqn-func
        (match-block FINAL REGULAR    )
        (match-block FINAL UNDO       )
        (match-block FINAL TILDE-UNDO ))]
    [BODIES
     #'(match-block FINAL BODIES)]))

(define-macro-cases FuncBlock
  [(_ (head-block) BODIES ...)
   #'(InferBlock (func->lambda) BODIES ...)]
  [(_ BODIES ...)
   #'(headless-func BODIES ...)]
  )

(define-macro (match-block (FINAL ...) (BODIES ...))
  (with-pattern
      ([(PATTERNS ...)
        (pattern-case-filter #'(BODIES ...)
          [((dyad-head) BODY)
           (with-syntax
               ([W (datum->syntax #'BODY '𝕨)])
             #'((W _ _) BODY))]
          [((monad-head) BODY)
           #'(((? void?) _ _) BODY)]
          [((else-head) BODY)
           #'((_ _ _) BODY)]
          [((LHS) BODY)
           #'((_ _ LHS) BODY)]
          [(((unbound) (F ...) M X) BODY)
           #'(((? void?) (F ... M) X) BODY)]
          [((𝕨 (F ...) M X) BODY)
           (with-syntax
               ([W (datum->syntax #'BODY '𝕨)])
             #'((W (F ... M) X) BODY))]
          [((W (F ...) M X) BODY)
           (with-syntax
               ([WW (datum->syntax #'BODY '𝕨)])
             #'(((and (not (? void?)) W WW) (F ... M) X) BODY))]
          )])
    #'(FINAL ... PATTERNS ...))
  )

(define-macro (func->lambda PATTERNS ...)
  (with-syntax ([(W S X) #'(*𝕨 *𝕤 *𝕩)])
    #'(letrec
          ([S (lambda (X [w? (void)])
                (syntax-parameterize
                    ([𝕤 (make-rename-transformer #'S)]
                     [𝕩 (make-rename-transformer #'X)])
                  (match/values (values w? S X)
                    PATTERNS ...)))])
        S))
  )

(define-macro (1M-Block (BLOCK-TYPE BODIES ...))
  (with-pattern
      ([IMM-OR-DEL
        (pattern-case #'BLOCK-TYPE
          [1M-Imm-Block #'Immediate-Mod]
          [1M-Del-Block #'Delayed-Mod])])
    (with-syntax ([(F R) #'(*𝕗 *𝕣)])
      #'(letrec
            ([R (lambda (F)
                  (syntax-parameterize
                      ([𝕗 (make-rename-transformer #'F)]
                       [𝕣 (make-rename-transformer #'R)])
                    (IMM-OR-DEL (F R) BODIES ...)))])
          R))))

(define-macro (2M-Block (BLOCK-TYPE BODIES ...))
  (with-pattern
      ([IMM-OR-DEL
        (pattern-case #'BLOCK-TYPE
          [2M-Imm-Block #'Immediate-Mod]
          [2M-Del-Block #'Delayed-Mod])])
    (with-syntax ([(F R G) #'(*𝕗 *𝕣 *𝕘)])
      #'(letrec
            ([R (lambda (F G)
                  (syntax-parameterize
                      ([𝕗 (make-rename-transformer #'F)]
                       [𝕣 (make-rename-transformer #'R)]
                       [𝕘 (make-rename-transformer #'G)])
                    (IMM-OR-DEL (F R G) BODIES ...)))])
          R))))

(define-macro (mod->lambda (FRG ...) PATTERNS ...)
  (with-syntax ([(W S X) #'(*𝕨 *𝕤 *𝕩)])
    #'(letrec
          ([S* (lambda (m)
                 (lambda (X [w? (void)])
                   (syntax-parameterize
                       ([𝕤 (make-rename-transformer #'S)]
                        [𝕩 (make-rename-transformer #'X)]
                        [𝕨 #'W])
                     (match/values (values w? (list FRG ... m) X)
                       PATTERNS ...))))]
           [S (bqn-func (S* #f) (S* 'undo) (S* '~undo))])
        S))
  )

(define-macro-cases Immediate-Mod
  [(_ (ARGS ...) (head-block) BODIES ...)
   #'(match/values (values ARGS ...)
       BODIES ...)]
  [(_ _ (_ BODY)) #'BODY])

(define-macro-cases Delayed-Mod
  [(_ ARGS (head-block) BODIES ...)
   #'(match-block (mod->lambda ARGS) (BODIES ...))]
  [(_ _ BODIES ...)
   #'(headless-func BODIES ...)])

(define-match-expander else-head
  (λ (stx) #'(_ _ _)))

(define-match-expander unbound
  (λ (stx) #'_))

(define-match-expander no-mod
  (λ (stx) #'#f))

(define-match-expander undo
  (λ (stx) #'(quote undo)))

(define-match-expander swap-undo
  (λ (stx) #'(quote ~undo)))

(define-macro (define-special-char NAME CHAR)
  #'(define-match-expander NAME
      (lambda (stx)
        (pattern-case stx
          [(_ CHAR) #'_]
          [(_ C) #'C]))))

(define-special-char headX   𝕩)

(define-special-char 1ModLab 𝕣)
(define-special-char 2ModLab 𝕣)
(define-special-char HeadF   𝕗)
(define-special-char HeadG   𝕘)
(define-special-char headW   𝕨)

(define-match-expander FuncLab
  (lambda (stx)
    (pattern-case stx
      [(_ 𝕤 _) #'_]
      [(_ S _) #'S])))

(define-match-expander FuncHead
  (lambda (stx)
    (with-pattern ([(_ ARGS ...) stx])
      #'(ARGS ...))))

(define-match-expander 1ModHeadBase
  (lambda (stx)
    (with-pattern ([(_ ARGS ...) stx])
      #'(list ARGS ...))))

(define-match-expander 2ModHeadBase
  (lambda (stx)
    (with-pattern ([(_ ARGS ...) stx])
      #'(list ARGS ...))))