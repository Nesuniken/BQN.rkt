#lang racket/base
(require br-parser-tools/lex brag/support racket/list racket/string racket/match
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

(define syntax-complete?
  (lexer
   [skippable
    (syntax-complete? input-port)]
   [(char-set "\"{[⟨'#") #f]
   [any-char
    (syntax-complete? input-port)]
   [(eof) #t]
   )
  )

(define (trim-rkt id)
  (string-trim id #px"[^\\.]+." #:right? #f))

(define-lex-abbrevs
  (nothing-dot #\·)
  (newline (or #\newline #\return #\, #\⋄))
  (white-space (- whitespace newline))
  (comment (: #\# (* (~ #\newline #\return))))

  (skippable
   (or
    comment
    (: #\'    any-char  #\')
    (: #\{ (+ any-char) #\})
    (: #\[ (+ any-char) #\])
    (: #\⟨ (* any-char) #\⟩)
    (: #\" (* any-char) #\")))
  
  (-? (? #\¯))
  (int (+ (/ #\0 #\9) (or #\_ (/ #\0 #\9))))
  (decimal (: -? int (? (: #\. int))))

  (exp (? (: (char-set "Ee") -? int)))
  (bqn-real (: -? (or #\∞ (: #\π exp))))
  (rkt-real (: decimal exp))
  (real (or bqn-real rkt-real))

  (rough-number (: (or notation (/ "09")) (* (or trailing-char #\.))))
  (rkt-number (: rkt-real (? (: (char-set "Ii") rkt-real)))) 
  (number (: real (? (: (char-set "Ii") real))))
  
  (string (: #\" (* (or (~ #\") (: #\" #\"))) #\"))
  
  (special-sub  (char-set "𝕨𝕤𝕩𝕗𝕘𝕣"))
  (special-func (char-set "𝕎𝕊𝕏𝔽𝔾"))

  (system-dot #\•)
  (system? (? system-dot))
  (kt (: (* #\_) (char-set "Kk") (* #\_) (char-set "Tt")))
  (rkt-id
   (+ (~ #\space #\newline #\tab #\( #\) #\[ #\] #\{ #\} #\⟨ #\⟩ #\" #\, #\' #\` #\; #\| #\\)))

  (trailing-char (or notation #\_ (/ "AZaz09")))

  (rkt-sub (: system-dot (? (: #\r kt)) #\. rkt-id))
  (sub-id  (: (/ "az") (* trailing-char)))
  (system-sub (: system-dot sub-id))
  (subject (or sub-id system-sub special-sub rkt-sub))

  (rkt-func (: "•R" kt #\. rkt-id))
  (func-id (: (/ "AZ") (* trailing-char)))
  (system-func (: system-dot func-id))
  (func-prim (char-set "⍳+-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!"))
  (func (or func-prim func-id system-func special-func rkt-func))

  (rkt-1mod (: "•_" (char-set "Rr") kt #\. rkt-id))
  (1mod-id   (: (+ #\_) (/ "AZaz") (? (* trailing-char) (/ "AZaz09"))))
  (system-1mod (: system-dot 1mod-id))
  (1mod-prim (char-set "`˙˘¨⌜´˝⁼˜"))
  (1mod (or "_𝕣" 1mod-prim system-1mod rkt-1mod))

  (rkt-2mod (: "•_" (char-set "Rr") kt "_." rkt-id))
  (2mod-id (: (+ #\_) (/ "AZaz") (* trailing-char) #\_))
  (system-2mod (: system-dot 2mod-id))
  (2mod-prim (char-set "∘○⊸⟜⌾⊘◶⎉⚇⍟⎊"))
  (2mod (or "_𝕣_" 2mod-prim 2mod-id system-2mod rkt-2mod))

  (notation (char-set "π∞¯"))
  (brackets (char-set "⟨⟩[](){}"))
  (assign (char-set "←⇐↩"))
  )