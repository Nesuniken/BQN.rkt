#lang racket/base
(require br-parser-tools/lex racket/list racket/string
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
  (newline (lx/or #\newline #\return #\, #\⋄))
  (white-space (lx/- whitespace newline))

  
  (-? (lx/? #\¯))
  (int (lx/+ (lx/or #\_ (lx// #\0 #\9))))
  (decimal (lx/: int (lx/? (lx/: #\. int))))

  (exp (lx/? (lx/: (char-set "Ee") -? int)))
  (bqn-real (lx/: -? (lx/or #\∞ (lx/: #\π exp))))
  (rkt-real (lx/: -? decimal exp))
  (real (lx/or bqn-real rkt-real))
  
  (rkt-number (lx/: rkt-real (lx/? (lx/: (char-set "Ii") rkt-real))))
  (number (lx/: real (lx/? (lx/: (char-set "Ii") real))))
  
  (string (lx/: #\" (lx/* (lx/or (lx/~ #\") (lx/: #\" #\"))) #\"))
  
  (special-sub  (char-set "𝕨𝕤𝕩𝕗𝕘𝕣"))
  (special-func (char-set "𝕎𝕊𝕏𝔽𝔾"))
  (non-special (lx/- alphabetic #\π special-sub special-func))

  (•? (lx/? #\•))
  (kt (lx/: (lx/* #\_) (char-set "Kk") (lx/* #\_) (char-set "Tt")))
  (rkt-id
   (lx/+ (lx/~ #\space #\newline #\tab #\( #\) #\[ #\] #\{ #\} #\⟨ #\⟩ #\" #\, #\' #\` #\; #\| #\\)))

  (trailing-char (lx/or #\_ non-special numeric))

  (rkt-sub (lx/: #\• (lx/? (lx/: #\r kt)) #\. rkt-id))
  (sub-name (lx/: •? (lx/: non-special (lx/* trailing-char))))
  (subject (lx/or sub-name special-sub rkt-sub))

  (rkt-func (lx/: "•R" kt #\. rkt-id))
  (func-name (lx/: •? (lx// #\A #\Z) (lx/* trailing-char)))
  (func-prim (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))
  (func (lx/or func-prim func-name special-func rkt-func))

  (rkt-1mod (lx/: "•_" (char-set "Rr") kt #\. rkt-id))
  (1mod-id   (lx/: #\_ (lx/+ trailing-char)))
  (1mod-name (lx/: •? 1mod-id))
  (1mod-prim (char-set "`˙˘¨⌜´˝⁼˜"))
  (1mod (lx/or "_𝕣" 1mod-prim 1mod-name rkt-1mod))

  (rkt-2mod (lx/: "•_" (char-set "Rr") kt "_." rkt-id))
  (2mod-name (lx/: •? 1mod-id #\_))
  (2mod-prim (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))
  (2mod (lx/or "_𝕣_" 2mod-prim 2mod-name rkt-2mod))

  (notation (char-set "π∞¯"))
  (brackets (char-set "⟨⟩[](){}"))
  (assign (char-set "←⇐↩"))
  )