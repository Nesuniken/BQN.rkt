#lang racket/base
(require
  racket/string
  racket/function
  racket/match
  racket/format
  racket/list
  brag/support
  BQN/lex-utils
  (prefix-in lx/ br-parser-tools/lex-sre))

(provide bqn-tokenizer)

(define (parse-num str)
  (string->number (string-replace (string-replace str "_" "") "¯" "-")))

(define (parse-id str)
  (string->symbol (string-downcase (string-replace str "_" ""))))

(define (bqn-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)

  (define specials (box '()))
  
  (thunk ((bqn-lexer specials) port)))

(define (lex string)
  (define stack (box '()))
  (apply-port-proc (bqn-lexer stack) string))

(define (bqn-lexer stack)
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
  
  (lexer-srcloc
   [func-prim
    (token 'FUNC-LITERAL (string->symbol (~a "BQN" lexeme)))]
     
   [1mod-prim
    (token '1MOD-LITERAL (string->symbol (~a "BQN" lexeme)))]
     
   [2mod-prim
    (token '2MOD-LITERAL (string->symbol (~a "BQN" lexeme)))]

   [#\@ (token 'CHARACTER #\null)]

   [nothing-dot (token 'NOTHING)]
     
   [newlines #\⋄]
     
   [(lx/: #\' any-char #\')
    (token 'CHARACTER (second (string->list lexeme)))]
 
   [string
      (token 'STRING (quote-removal (string->list lexeme)))]
     
   [(lx/: #\# (lx/* (lx/~ #\newline)))
    (token 'COMMENT (substring lexeme 1) #:skip? #t)]
     
   [(lx/+ whitespace) (token lexeme #:skip? #t)]

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

   [special (add-special! lexeme)]

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

   [(lx/: (lx/? #\•) func-name)
    (let ([defined-by (if (string-prefix? lexeme "•") 'FUNC-LITERAL 'FUNC-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) 1mod-name)
    (let ([defined-by (if (string-prefix? lexeme "•") '1MOD-LITERAL '1MOD-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) 2mod-name)
    (let ([defined-by (if (string-prefix? lexeme "•") '2MOD-LITERAL '2MOD-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) sub-name)
    (let ([defined-by (if (string-prefix? lexeme "•") 'SUB-LITERAL 'SUB-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/or brackets assign #\. #\‿)
    (token lexeme (string->symbol lexeme))]
   
   [(lx/: (lx/? #\¯) (lx/or #\∞ #\π))
    (token 'NUMBER (string->symbol lexeme))]

   [real (token 'NUMBER (parse-num lexeme))]
   
   [(lx/: real (lx/or #\I #\i) real)
    (token 'NUMBER
           (apply make-rectangular
            (map parse-num
                 (string-split
                  (string-downcase lexeme) "i"))))]
   ))