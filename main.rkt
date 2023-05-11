#lang racket/base
(require BQN/lexer racket/class racket/format BQN/parser brag/support br/syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (bqn-tokenizer port path)))
  (strip-bindings
   #`(module bqn-mod BQN/expander
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
   (#\W . #\𝕎)     (#\w . #\𝕨)
   (#\E . #\⍷)     (#\e . #\∊)
   (#\R . #\𝕣)     (#\r . #\↑)
   (#\T . #\⍋)     (#\t . #\∧)
                   (#\u . #\⊔)
   (#\I . #\⊑)     (#\i . #\⊏)
   (#\O . #\⊒)     (#\o . #\⊐)
   (#\P . #\⍳)     (#\p . #\π)
                   (#\a . #\⍉)
   (#\S . #\𝕊)     (#\s . #\𝕤)
                   (#\d . #\↕)
   (#\F . #\𝔽)     (#\f . #\𝕗)
   (#\G . #\𝔾)     (#\g . #\𝕘)
   (#\H . #\«)     (#\h . #\⊸)
                   (#\j . #\∘)
   (#\K . #\⌾)     (#\k . #\○)
   (#\L . #\»)     (#\l . #\⟜)
   (#\Z . #\⋈)     (#\z . #\⥊)
   (#\X . #\𝕏)     (#\x . #\𝕩)
                   (#\c . #\↓)
   (#\V . #\⍒)     (#\v . #\∨)
   (#\B . #\⌈)     (#\b . #\⌊)
   (#\M . #\≢)     (#\m . #\≡)
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

