#lang racket/base
(require BQN/lexer BQN/parser)
(provide setup!)

(define (read-bqn origin port)
  (parse #f (bqn-tokenizer port #f))
  )

(define (setup!)
  (current-read-interaction read-bqn)
  (void))