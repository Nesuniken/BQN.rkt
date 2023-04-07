#lang racket
(require
  data/enumerate/lib
  brag/support
  (prefix-in lx/ br-parser-tools/lex-sre))

(provide bqn-tokenizer)

(define special-subs  (fin/e "{" "𝕨" "𝕤" "𝕩" "𝕗" "𝕘" ":"))
(define special-funcs (fin/e "{" "𝕎" "𝕊" "𝕏" "𝔽" "𝔾" ":"))

(define (bqn-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)

  (define specials (box '()))
  
  (thunk ((bqn-lexer specials) port)))

(define (lex string)
  (define specials (box '()))
  (apply-port-proc (bqn-lexer specials) string))

(define (bqn-lexer specials)  
  (lexer-srcloc
   [(char-set "+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!")
    (token 'FUNC-PRIM (string->symbol lexeme))]
     
   [(char-set "˙˘¨⌜´˝`")
    (token '1MOD-PRIM (string->symbol lexeme))]
     
   [(char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊")
    (token '2MOD-PRIM lexeme)]

   [#\@ (token 'CHARACTER #\null)]

   [#\· (token 'NOTHING)]
     
   [(lx/+ (lx/or #\newline #\, #\⋄)) #\⋄]
     
   [(lx/: #\' any-char #\')
    (token 'CHARACTER (substring lexeme 1 2))]
 
   [(lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\")
    (let* ([quote-count -2]
           [quote-removal
            (λ(c)(or
                  (not (equal? c #\"))
                  (begin
                    (set! quote-count (+ quote-count 1))
                    (equal? (remainder quote-count 2) 1))))])
      (token 'STRING (list->string (filter quote-removal (string->list lexeme)))))]
     
   [(lx/: #\# (lx/* (lx/~ #\newline)))
    (token 'COMMENT (substring lexeme 1) #:skip? #t)]
     
   [whitespace (token lexeme #:skip? #t)]

   [(lx/: (lx/or (lx/>= 2 (lx// #\A #\Z #\a #\z)) (lx/& (lx// #\A #\Z #\a #\z) (lx/~ #\E #\e #\I #\i)))
          (lx/? (lx/* (lx/or #\_ alphabetic numeric)) (lx/or alphabetic numeric)))
    (let* ([role
            (if (char-upper-case? (first (string->list lexeme)))
                'FUNC-CUSTOM 'SUB-CUSTOM)
            ])

      (token role (string->symbol (string-replace lexeme "_" ""))))]

   [(char-set "¯_EeIi∞π.•:;?⟨⟩[]()‿⁼˜") (token lexeme)]
   [(lx/: (lx// #\0 #\9) (lx/* (lx/or #\_ (lx// #\0 #\9)))) (token 'INTEGER lexeme)]
   ))