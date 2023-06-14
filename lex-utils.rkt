#lang racket/base
(require brag/support racket/list racket/string
         (prefix-in lx/ br-parser-tools/lex-sre))
(provide (all-defined-out))

(define (quote-removal list-str [counter -1])
  (cond
    [(empty? list-str) '()]
    [(equal? (remainder counter 2) 1)
     (quote-removal (rest list-str) (add1 counter))]
    [else (cons (first list-str)
                (quote-removal (rest list-str) (add1 counter)))]
    )
  )

(define (trim-rkt id)
  (string-trim id #px"[^\\.]+." #:right? #f))

(define-lex-abbrevs
  (nothing-dot #\·)
  (newlines (lx/+ (lx/or #\newline #\, #\⋄)))
  
  (int (lx/* (lx/or #\_ (lx// #\0 #\9))))
  (decimal (lx/: (lx/? #\¯) (lx// #\0 #\9) int (lx/? #\. int)))
  (real (lx/: decimal (lx/? (char-set "Ee") (lx/? #\¯) int)))
  
  (string (lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\"))
  
  (special-sub  (char-set "𝕨𝕤𝕩𝕗𝕘𝕣"))
  (special-func (char-set "𝕎𝕊𝕏𝔽𝔾"))
  (non-special (lx/- alphabetic special-sub special-func))

  (maybe-sys (lx/? #\•))
  (kt (lx/: (lx/* #\_) (char-set "Kk") (lx/* #\_) (char-set "Tt")))
  (rkt-id
   (lx/+ (lx/~ #\space #\newline #\tab #\( #\) #\[ #\] #\{ #\} #\⟨ #\⟩ #\" #\, #\' #\` #\; #\| #\\)))

  (trailing-char (lx/or #\_ non-special numeric))

  (rkt-sub (lx/: #\• (lx/? #\r kt) #\. rkt-id))
  (sub-name (lx/: maybe-sys (lx/: non-special (lx/* trailing-char))))
  (subject (lx/or sub-name special-sub rkt-sub))

  (rkt-func (lx/: "•R" kt #\. rkt-id))
  (func-name (lx/: maybe-sys (lx// #\A #\Z) (lx/* trailing-char)))
  (func-prim (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))
  (func (lx/or func-prim func-name special-func rkt-func))

  (rkt-1mod (lx/: "•_" (char-set "Rr") kt #\. rkt-id))
  (1mod-id   (lx/: #\_ (lx/+ trailing-char)))
  (1mod-name (lx/: maybe-sys 1mod-id))
  (1mod-prim (char-set "`˙˘¨⌜´˝⁼˜"))
  (1mod (lx/or 1mod-prim 1mod-name rkt-1mod))

  (rkt-2mod (lx/: "•_" (char-set "Rr") kt "_." rkt-id))
  (2mod-name (lx/: maybe-sys 1mod-id #\_))
  (2mod-prim (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))
  (2mod (lx/or 2mod-prim 2mod-name rkt-2mod))

  (brackets (char-set "⟨⟩[](){}"))
  (assign (char-set "←⇐↩"))
  )