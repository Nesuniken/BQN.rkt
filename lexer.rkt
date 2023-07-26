#lang racket/base
(require
  racket/string
  racket/function
  racket/match
  racket/format
  racket/list
  math/array
  (except-in brag/support ::)
  BQN/lex-utils)

(provide bqn-tokenizer)

(define (parse-num str)
  (string->number (string-replace (string-replace str "_" "") "¯" "-")))

(define (id-token str literal custom)
  (define id (string->symbol (string-downcase (string-replace str "_" ""))))
  (if (string-prefix? str "•")
      (token literal id)
      (token custom  id))
  )

(define (bqn-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)

  (define stack (box '()))
  (define number? (box #f))
  
  (thunk ((bqn-lexer stack number?) port)))

(define (lex string)
  (define stack (box '()))
  (define number? (box #f))
  (apply-port-proc (bqn-lexer stack number?) string))

(define (bqn-lexer stack number?)
  (define (add-special! lexeme)
    (define special-downcase
      (case lexeme
        [("𝕎" "𝕨") '𝕨]
        [("𝕊" "𝕤") '𝕤]
        [("𝕏" "𝕩") '𝕩]
        [("𝔽" "𝕗") '𝕗]
        [("𝔾" "𝕘") '𝕘]))

    (define role
      (case special-downcase
        [(𝕤 𝕩 𝕨) 2]
        [(𝕗)   3]
        [(𝕘)   9]))

    (match (unbox stack)
      [(list) (error (string-append "Special name" lexeme "found outside block"))]
      [(list* current-block outer-blocks)
       (set-box! stack
         (cons
          (lcm current-block role)
          outer-blocks))])
    (token (if (string-contains? "𝕎𝕊𝕏𝔽𝔾" lexeme) 'FUNC-LITERAL 'SUB-LITERAL) special-downcase))

  (define num-lexer
    (lexer-srcloc
     [(lx/+ #\_) (token '_ lexeme #:skip? #t)]
     
     [(lx/: -? int)
      (token 'INTEGER (parse-num lexeme))]

     [rkt-real
      (token 'REAL (parse-num lexeme))]

     [(lx/or #\. #\e #\i notation)
      (token lexeme)]

     [(lx/or #\E #\I)
      (token (string-downcase lexeme))]

     ["" (begin
           (set-box! number? #f)
           (main-lexer input-port))]
     ))
  
  (define main-lexer
    (lexer-srcloc
     [notation
      (begin
        (set-box! number? #t)
        (token lexeme (string->symbol lexeme)))]

     [(lx/: -? int)
      (begin
        (set-box! number? #t)
        (token 'INTEGER (parse-num lexeme)))]

     [rkt-real
      (begin
        (set-box! number? #t)
        (token 'REAL (parse-num lexeme)))]

     [rkt-number
      (token 'NUMBER (parse-num lexeme))]
   
     [func-prim
      (token 'FUNC-LITERAL (string->symbol (~a "BQN" lexeme)))]
     
     [1mod-prim
      (token '1MOD-LITERAL (string->symbol (~a "BQN" lexeme)))]
     
     [2mod-prim
      (token '2MOD-LITERAL (string->symbol (~a "BQN" lexeme)))]

     [#\@ (token 'CHARACTER #\null)]

     [nothing-dot (token 'NOTHING)]

     [#\. (token ".")]
     
     [(lx/+ newline) #\⋄]
     
     [(lx/: #\' any-char #\')
      (token 'CHARACTER (second (string->list lexeme)))]
 
     [string
      (token 'STRING (list->array (quote-filter (string->list lexeme))))]

     [(lx/: #\• string)
      (token 'RKT-STRING (substring lexeme 2 (sub1 (string-length lexeme))))]
     
     [(lx/: #\# (lx/* (lx/~ #\newline)))
      (token 'COMMENT (substring lexeme 1) #:skip? #t)]
     
     [(lx/+ white-space) (token lexeme #:skip? #t)]

     [#\{
      (begin
        (set-box! stack
          (cons 1 (unbox stack)))
        lexeme)]   

     [(lx/: (lx/? #\_) #\𝕣 (lx/? #\_))
      (begin
        (match (cons lexeme (unbox stack))
          [(list _) (error "Special name 𝕣 found outside block")]
          [(list* _ (or "𝕘" "𝔾") _) empty]
          [(list* "_𝕣_" current-block outer-blocks)
           (set-box! stack
             (cons (lcm current-block 9) rest))]
          [(list* _ current-block outer-blocks)
           (set-box! stack
             (cons (lcm current-block 3) rest))])
        (token lexeme '𝕣))]

     [(lx/or special-sub special-func) (add-special! lexeme)]

     [#\}
      (match (unbox stack)
        [(list) (token lexeme (string->symbol lexeme))]
        [(list* current-block outer-blocks)
         (begin
           (set-box! stack outer-blocks)

           (case current-block
             [(1)  (token 'SUB-BLOCK)]
             [(2)  (token 'FUNC-BLOCK)]
             [(3)  (token '1M-IMMEDIATE '𝕤)]
             [(6)  (token '1M-DELAYED   '𝕊)]
             [(9)  (token '2M-IMMEDIATE '𝕤)]
             [(18) (token '2M-DELAYED   '𝕊)]
             ))])]

     [(lx/or "•Require")
      (token (string->symbol lexeme))]

     [2mod-name (id-token lexeme '2MOD-LITERAL '2MOD-CUSTOM)]

     [rkt-2mod
      (token '2MOD-CUSTOM (string->symbol (trim-rkt lexeme)))]

     [1mod-name (id-token lexeme '1MOD-LITERAL '1MOD-CUSTOM)]

     [rkt-1mod
      (token '1MOD-CUSTOM (string->symbol (trim-rkt lexeme)))]
      
     [func-name (id-token lexeme 'FUNC-LITERAL 'FUNC-CUSTOM)]

     [rkt-func
      (token 'FUNC-CUSTOM (string->symbol (trim-rkt lexeme)))]

     [sub-name  (id-token lexeme 'SUB-LITERAL 'SUB-CUSTOM)]

     [rkt-sub
      (token 'SUB-CUSTOM (string->symbol (trim-rkt lexeme)))]

     [(lx/or brackets assign #\‿)
      (token lexeme (string->symbol lexeme))]
     ))
  
  (if (unbox number?) num-lexer main-lexer)
  )