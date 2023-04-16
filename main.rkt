#lang racket
(require "lexer.rkt" "parser.rkt" br/syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (bqn-tokenizer port path)))
  (strip-bindings
   #`(module bqn-mod BQN/expander
       #,parse-tree)
   )
  )

(module+ reader
  (provide read-syntax))