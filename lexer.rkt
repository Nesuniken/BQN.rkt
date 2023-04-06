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
  (define (add-special! enum lexeme)
    (define special-downcase
      (case lexeme
        [("𝕎" "𝕨") '𝕨]
        [("𝕊" "𝕤") '𝕤]
        [("𝕏" "𝕩") '𝕩]
        [("𝔽" "𝕗") '𝕗]
        [("𝔾" "𝕘") '𝕘]))
    
    (match (unbox specials)
      [(list) (error (~a "Special name " lexeme " found outside block"))]
      [(list* current-block outer-blocks)
       (set-box! specials
                 (cons
                  (max current-block (to-nat enum lexeme))
                  outer-blocks))])
    (token lexeme special-downcase)
    )
  
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

   [#\{
    (begin
      (set-box! specials
        (cons 0 (unbox specials)))
      lexeme)]   
     
   [(lx/: (lx/? #\_) #\𝕣 (lx/? #\_))
    (begin
      (match (cons lexeme (unbox specials))
        [(list _) (error "Special name 𝕣 found outside block")]
        [(list* _ (or "𝕘" "𝔾") _) empty]
        [(list* "_𝕣_" _ rest)
         (set-box! specials
           (cons (to-nat special-subs "𝕘") rest))]
        [(list* _ _ rest)
         (set-box! specials
           (cons (to-nat special-subs "𝕗") rest))])
      lexeme)]

   [(char-set "𝕎𝕏𝔽𝔾𝕊")
    (add-special! special-funcs lexeme)]

   [(char-set "𝕨𝕩𝕗𝕘𝕤")
    (add-special! special-subs lexeme)]

   [#\}
    (match (unbox specials)
      [(list) (error "Found unmatched '}'")]
      [(list* current-block outer-blocks)
       (begin
         (set-box! specials outer-blocks)
         
         (case current-block
           [(0) (token 'SUB-BLOCK)]
           [(1 2 3) (token 'FUNC-BLOCK)]
           [(4) (token '1M-BLOCK)]
           [(5) (token '2M-BLOCK)]
           [(6) lexeme]))])]

   [(lx/: (lx/or (lx/>= 2 (lx// #\A #\Z #\a #\z)) (lx/& (lx// #\A #\Z #\a #\z) (lx/~ #\E #\e #\I #\i)))
          (lx/? (lx/* (lx/or #\_ alphabetic numeric)) (lx/or alphabetic numeric)))
    (let* ([role
            (if (char-upper-case? (first (string->list lexeme)))
                'FUNC-CUSTOM 'SUB-CUSTOM)
            ])

      (token role (string->symbol (string-replace lexeme "_" ""))))]

   [(char-set "←⇐↩")
    (token lexeme (string->symbol lexeme))]

   [(char-set "¯_EeIi∞π.•:;?⟨⟩[](){}‿⁼˜") (token lexeme)]
   [(lx/: (lx// #\0 #\9) (lx/* (lx/or #\_ (lx// #\0 #\9)))) (token 'INTEGER lexeme)]
   ))