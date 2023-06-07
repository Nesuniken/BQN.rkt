#lang racket/base
(require brag/support
         (prefix-in lx/ br-parser-tools/lex-sre))
(provide (all-defined-out))

(define-lex-abbrevs
  (nothing-dot #\·)
  (newlines (lx/+ (lx/or #\newline #\, #\⋄)))
  
  (int (lx/* (lx/or #\_ (lx// #\0 #\9))))
  (decimal (lx/: (lx/? #\¯) (lx// #\0 #\9) int (lx/? #\. int)))
  (real (lx/: decimal (lx/? (lx/or #\E #\e) (lx/? #\¯) int)))
  
  (string (lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\"))
  
  (special (char-set "𝕎𝕨𝕊𝕤𝕏𝕩𝔽𝕗𝔾𝕘"))
  (non-special (lx/- alphabetic special))
  
  (sub-name (lx/: (lx/or #\_ alphabetic) (lx/* (lx/or #\_ non-special numeric))))

  (func-name (lx/: (lx// #\A #\Z) (lx/* (lx/or #\_ non-special numeric))))
  (func-prim (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))

  (1mod-name (lx/: #\_ (lx/* (lx/or #\_ non-special numeric))))
  (1mod-prim (char-set "`˙˘¨⌜´˝⁼˜"))

  (2mod-name (lx/: 1mod-name #\_))
  (2mod-prim (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))

  (brackets (char-set "⟨⟩[]()"))
  (assign (char-set "←⇐↩"))
  )