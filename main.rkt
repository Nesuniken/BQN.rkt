#lang racket
(require "lexer.rkt" "parser.rkt" brag/support br/syntax)
(provide #%top #%app #%datum #%top-interaction)

(define (read-syntax path port)
  (define parse-tree (parse path (bqn-tokenizer port path)))
  (strip-bindings
   #`(module bqn-mod bqn/expander
       #,parse-tree)
   )
  )

(module+ reader
  (provide read-syntax))
