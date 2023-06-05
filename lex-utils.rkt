#lang racket/base
(require brag/support
         (prefix-in lx/ br-parser-tools/lex-sre))
(provide (all-defined-out))

(define-lex-abbrev int (lx/* (lx/or #\_ (lx// #\0 #\9))))
(define-lex-abbrev decimal (lx/: (lx/? #\¯) (lx// #\0 #\9) int (lx/? #\. int)))
(define-lex-abbrev real (lx/: decimal (lx/? (lx/or #\E #\e) (lx/? #\¯) int)))

(define-lex-abbrev string (lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\"))

(define-lex-abbrev special (char-set "𝕎𝕨𝕊𝕤𝕏𝕩𝔽𝕗𝔾𝕘"))
(define-lex-abbrev non-special (lx/- alphabetic special))

(define-lex-abbrev sub-name
  (lx/: (lx/or #\_ alphabetic) (lx/* (lx/or #\_ non-special numeric))))

(define-lex-abbrev func-name
  (lx/: (lx// #\A #\Z) (lx/* (lx/or #\_ non-special numeric))))
(define-lex-abbrev func-prim
  (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))

(define-lex-abbrev 1mod-name
  (lx/: #\_ (lx/* (lx/or #\_ non-special numeric))))
(define-lex-abbrev 1mod-prim
  (char-set "`˙˘¨⌜´˝⁼˜"))

(define-lex-abbrev 2mod-name
  (lx/: 1mod-name #\_))
(define-lex-abbrev 2mod-prim
  (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))

(define-lex-abbrev brackets (char-set "⟨⟩[]()"))
(define-lex-abbrev assign (char-set "←⇐↩"))