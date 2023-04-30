#lang racket
(require
  data/enumerate/lib
  brag/support
  (prefix-in lx/ br-parser-tools/lex-sre))

(provide bqn-tokenizer)

(define-lex-abbrev int (lx/* (lx/or #\_ (lx// #\0 #\9))))
(define-lex-abbrev decimal (lx/: (lx/? #\¯) (lx// #\0 #\9) int (lx/? #\. int)))
(define-lex-abbrev real (lx/: decimal (lx/? (lx/or #\E #\e) (lx/? #\¯) int)))

(define-lex-abbrev sub
  (lx/: (lx/or #\_ alphabetic) (lx/* (lx/or #\_ alphabetic numeric))))
(define-lex-abbrev func
  (lx/: (lx// #\A #\Z) (lx/* (lx/or #\_ alphabetic numeric))))
(define-lex-abbrev 1mod
  (lx/: #\_ (lx/* (lx/or #\_ alphabetic numeric))))
(define-lex-abbrev 2mod
  (lx/: 1mod #\_))

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
  (define specials (box '()))
  (apply-port-proc (bqn-lexer specials) string))

(define (bqn-lexer specials)
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
        [(𝕤 𝕩) 2]
        [(𝕨)   4]
        [(𝕗)   3]
        [(𝕘)   9]))

    (match (unbox specials)
      [(list) (error (~a "Special name " lexeme " found outside block"))]
      [(list* current-block outer-blocks)
       (set-box! specials
         (cons
          (lcm current-block role)
          outer-blocks))])
    (token (if (string-contains? "𝕎𝕊𝕏𝔽𝔾" lexeme) 'FUNC-LITERAL 'SUB-LITERAL) special-downcase))
  
  (lexer-srcloc
   [(char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!")
    (token 'FUNC-LITERAL (string->symbol (~a "BQN" lexeme)))]

   [#\| (token 'FUNC-LITERAL (string->symbol "BQN-PIPE"))]

   [#\` (token '1MOD-LITERAL (string->symbol "BQN-GRAVE"))]
     
   [(char-set "˙˘¨⌜´˝⁼˜")
    (token '1MOD-LITERAL (string->symbol (~a "BQN" lexeme)))]
     
   [(char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊")
    (token '2MOD-LITERAL (~a "BQN" lexeme))]

   [#\@ (token 'CHARACTER #\null)]

   [#\· (token 'NOTHING)]
     
   [(lx/+ (lx/or #\newline #\, #\⋄)) #\⋄]
     
   [(lx/: #\' any-char #\')
    (token 'CHARACTER (second (string->list lexeme)))]
 
   [(lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\")
    (let* ([quote-count -2]
           [quote-removal
            (λ(c)(or
                  (not (equal? c #\"))
                  (begin
                    (set! quote-count (+ quote-count 1))
                    (equal? (remainder quote-count 2) 1))))])
      (token 'STRING (filter quote-removal (string->list lexeme))))]
     
   [(lx/: #\# (lx/* (lx/~ #\newline)))
    (token 'COMMENT (substring lexeme 1) #:skip? #t)]
     
   [whitespace (token lexeme #:skip? #t)]

   [#\{
    (begin
      (set-box! specials
        (cons 1 (unbox specials)))
      lexeme)]   

   [(lx/: (lx/? #\_) #\𝕣 (lx/? #\_))
    (begin
      (match (cons lexeme (unbox specials))
        [(list _) (error "Special name 𝕣 found outside block")]
        [(list* _ (or "𝕘" "𝔾") _) empty]
        [(list* "_𝕣_" current-block outer-blocks)
         (set-box! specials
           (cons (lcm current-block 9) rest))]
        [(list* _ current-block outer-blocks)
         (set-box! specials
           (cons (lcm current-block 3) rest))])
      (token lexeme '𝕣))]

   [(char-set "𝕎𝕨𝕊𝕤𝕏𝕩𝔽𝕗𝔾𝕘")
    (add-special! lexeme)]

   [#\}
    (match (unbox specials)
      [(list) (error "Found unmatched '}'")]
      [(list* current-block outer-blocks)
       (begin
         (set-box! specials outer-blocks)

         (case current-block
           [( 1) (token 'SUB-BLOCK)]
           [( 2) (token 'FUNC-BLOCK '𝕊1)]
           [( 4) (token 'FUNC-BLOCK '𝕊2)]
           [( 3) (token '1M-BLOCK   '𝕊0)]
           [( 6) (token '1M-BLOCK   '𝕊1)]
           [(12) (token '1M-BLOCK   '𝕊2)]
           [( 9) (token '2M-BLOCK   '𝕊0)]
           [(18) (token '2M-BLOCK   '𝕊1)]
           [(36) (token '2M-BLOCK   '𝕊2)]
           ))])]

   [(lx/: (lx/? #\•) func)
    (let ([defined-by (if (string-prefix? lexeme "•") 'FUNC-LITERAL 'FUNC-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) 1mod)
    (let ([defined-by (if (string-prefix? lexeme "•") '1MOD-LITERAL '1MOD-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) 2mod)
    (let ([defined-by (if (string-prefix? lexeme "•") '2MOD-LITERAL '2MOD-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(lx/: (lx/? #\•) sub)
    (let ([defined-by (if (string-prefix? lexeme "•") 'SUB-LITERAL 'SUB-CUSTOM)])
      (token defined-by (parse-id lexeme)))]

   [(char-set "←⇐↩.;?⟨⟩[]()‿")
    (token lexeme (string->symbol lexeme))]
   
   [(lx/: (lx/? #\¯) (lx/or #\∞ #\π))
    (token 'NUMBER (string->symbol lexeme))]

   [real (token 'NUMBER (parse-num lexeme))]
   
   [(lx/: real (lx/or #\I #\i) real)
    (token 'NUMBER
           (make-rectangular
            (map parse-num
                 (string-split
                  (string-downcase lexeme) "i"))))]
   ))