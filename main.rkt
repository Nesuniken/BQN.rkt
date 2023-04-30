#lang racket/base
(require BQN/lexer BQN/parser brag/support br/syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (bqn-tokenizer port path)))
  (strip-bindings
   #`(module bqn-mod BQN/expander
       #,parse-tree)
   )
  )

(module+ reader
  (provide read-syntax))