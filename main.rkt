#lang racket/base
(require racket/class racket/format br/syntax "lexer.rkt" "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (bqn-tokenizer port path)))
  (strip-bindings
   #`(module bqn-mod BQN/expander/main
       #,parse-tree)
   )
  )

(module+ reader
  (provide read-syntax
           get-info
           ))

(define keymap
  #hash(
   (#\~ . #\¬)     (#\` . #\˜)
   (#\! . #\⎉)     (#\1 . #\˘)
   (#\@ . #\⚇)     (#\2 . #\¨)
   (#\# . #\⍟)     (#\3 . #\⁼)
   (#\$ . #\◶)     (#\4 . #\⌜)
   (#\% . #\⊘)     (#\5 . #\´)
   (#\^ . #\⎊)     (#\6 . #\˝)
                   (#\8 . #\∞)
   (#\( . #\⟨)     (#\9 . #\¯)
   (#\) . #\⟩)     (#\0 . #\•)   
   (#\_ . #\√)     (#\- . #\÷)
   (#\+ . #\⋆)     (#\= . #\×)
                   (#\q . #\⌽)
   ("s:W" . #\𝕎)   (#\w . #\𝕨)
   ("s:E" . #\⍷)   (#\e . #\∊)
   ("s:R" . #\𝕣)   (#\r . #\↑)
   ("s:T" . #\⍋)   (#\t . #\∧)
                   (#\u . #\⊔)
   ("s:I" . #\⊑)   (#\i . #\⊏)
   ("s:O" . #\⊒)   (#\o . #\⊐)
   ("s:P" . #\⍳)   (#\p . #\π)
                   (#\a . #\⍉)
   ("s:S" . #\𝕊)   (#\s . #\𝕤)
                   (#\d . #\↕)
   ("s:F" . #\𝔽)   (#\f . #\𝕗)
   ("s:G" . #\𝔾)   (#\g . #\𝕘)
   ("s:H" . #\«)   (#\h . #\⊸)
                   (#\j . #\∘)
   ("s:K" . #\⌾)   (#\k . #\○)
   ("s:L" . #\»)   (#\l . #\⟜)
   ("s:Z" . #\⋈)   (#\z . #\⥊)
   ("s:X" . #\𝕏)   (#\x . #\𝕩)
                   (#\c . #\↓)
   ("s:V" . #\⍒)   (#\v . #\∨)
   ("s:B" . #\⌈)   (#\b . #\⌊)
   ("s:M" . #\≢)   (#\m . #\≡)
   (#\{ . #\⊣)     (#\[ . #\←)
   (#\} . #\⊢)     (#\] . #\→)
   ("colon" . #\·) ("semicolon" . #\⋄)
   (#\" . #\˙)     (#\' . #\↩)
   (#\< . #\≤)     (#\, . #\∾)
   (#\> . #\≥)     (#\. . #\≍)
   (#\? . #\⇐)     (#\/ . #\≠)
   ( "space"    .      #\‿   )
   ))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(drracket:keystrokes)
       (for/list ([(in out) (in-hash keymap)])
         (list
          (~a "\\;" in)
          (lambda (text event)
            (let ([pos (send text get-start-position)])
              (send text insert out)))))]
      [else default])))

