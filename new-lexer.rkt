#lang racket
(require megaparsack megaparsack/text br-parser-tools/lex brag/support "lex-utils.rkt" "parser/lexemes.rkt")

(define-tokens literal [sl Fl _m1l _m2l_])
(define-tokens custom [s F _m1 _m2_])

(define (literal-symbol primitive)
  (string->symbol (string-append "BQN" primitive)))

(define (id-token lexeme)
  (string->symbol (string-downcase (string-replace lexeme "_" ""))))
 
(define bqn-lexer
  (lexer-src-pos
   [rough-number
    (token-sl (parse-result! (parse-string number/p (string-replace lexeme "_" ""))))]

   [func-prim
    (token-Fl (literal-symbol lexeme))]

   [1mod-prim
    (token-_m1l (literal-symbol lexeme))]

   [2mod-prim
    (token-_m2l_ (literal-symbol lexeme))]

   [#\@ (token-sl #\null)]
   [nothing-dot 'Nothing]
   [(lx/+ newline) '|,|]

   [(lx/: #\' any-char #\')
    (token-sl (second (string->list lexeme)))]

   [string (token-sl (parse-result! (parse-string bqn-string/p lexeme)))]

   [(lx/or comment (lx/+ white-space))
    (return-without-pos (bqn-lexer input-port))]

   [(char-set "𝕎𝕊𝕏𝔽𝔾𝕨𝕤𝕩𝕗𝕘𝕣")
    (string->symbol lexeme)]

   [(lx/: "_𝕣" (lx/? #\_))
    (string->symbol lexeme)]

   ["•Require" 'Require]

   [sub-id
    (token-s (id-token lexeme))]
   [system-sub
    (token-sl (id-token lexeme))]

   [func-id
    (token-F (id-token lexeme))]
   [system-func
    (token-Fl (id-token lexeme))]

   [1mod-id
    (token-_m1 (id-token lexeme))]
   [system-1mod
    (token-_m1l (id-token lexeme))]

   [2mod-id
    (token-_m2_ (id-token lexeme))]
   [system-2mod
    (token-_m2l_ (id-token lexeme))]
   
   [(lx/or brackets assign (char-set ".‿:;?"))
    (string->symbol lexeme)]
   ))