#lang racket/base
(require br-parser-tools/lex racket/list racket/string racket/match
         br-parser-tools/lex-sre)
(provide (all-defined-out) (prefix-out lx/ (all-from-out br-parser-tools/lex-sre)))

(define/match (quote-filter list-string)
  [((list* #\" #\" rest))
   (cons #\" (quote-filter rest))]
  [((list* #\" rest))
   (quote-filter rest)]
  [((list* first rest))
   (cons first (quote-filter rest))]
  [('()) empty]
  )

(define (trim-rkt id)
  (string-trim id #px"[^\\.]+." #:right? #f))

(define-lex-abbrevs
  (nothing-dot #\·)
  (newline (or #\newline #\return #\, #\⋄))
  (white-space (- whitespace newline))
  (comment (: #\# (* (~ #\newline #\return))))

  
  (-? (? #\¯))
  (int (+ (or #\_ (/ #\0 #\9))))
  (decimal (: int (? (: #\. int))))

  (exp (? (: (char-set "Ee") -? int)))
  (bqn-real (: -? (or #\∞ (: #\π exp))))
  (rkt-real (: -? decimal exp))
  (real (or bqn-real rkt-real))
  
  (rkt-number (: rkt-real (? (: (char-set "Ii") rkt-real))))
  (number (: real (? (: (char-set "Ii") real))))
  
  (string (: #\" (* (or (~ #\") (: #\" #\"))) #\"))
  
  (special-sub  (char-set "𝕨𝕤𝕩𝕗𝕘𝕣"))
  (special-func (char-set "𝕎𝕊𝕏𝔽𝔾"))
  (non-special (- alphabetic #\π special-sub special-func))

  (•? (? #\•))
  (kt (: (* #\_) (char-set "Kk") (* #\_) (char-set "Tt")))
  (rkt-id
   (+ (~ #\space #\newline #\tab #\( #\) #\[ #\] #\{ #\} #\⟨ #\⟩ #\" #\, #\' #\` #\; #\| #\\)))

  (trailing-char (or #\_ non-special numeric))

  (rkt-sub (: #\• (? (: #\r kt)) #\. rkt-id))
  (sub-name (: •? (: non-special (* trailing-char))))
  (subject (or sub-name special-sub rkt-sub))

  (rkt-func (: "•R" kt #\. rkt-id))
  (func-name (: •? (/ #\A #\Z) (* trailing-char)))
  (func-prim (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))
  (func (or func-prim func-name special-func rkt-func))

  (rkt-1mod (: "•_" (char-set "Rr") kt #\. rkt-id))
  (1mod-id   (: #\_ (+ trailing-char)))
  (1mod-name (: •? 1mod-id))
  (1mod-prim (char-set "`˙˘¨⌜´˝⁼˜"))
  (1mod (or "_𝕣" 1mod-prim 1mod-name rkt-1mod))

  (rkt-2mod (: "•_" (char-set "Rr") kt "_." rkt-id))
  (2mod-name (: •? 1mod-id #\_))
  (2mod-prim (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))
  (2mod (or "_𝕣_" 2mod-prim 2mod-name rkt-2mod))

  (notation (char-set "π∞¯"))
  (brackets (char-set "⟨⟩[](){}"))
  (assign (char-set "←⇐↩"))
  )