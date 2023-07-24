#lang racket/base
(require racket/port BQN/lexer BQN/parser)
(provide setup!)

(define unread? #t)

(define (read-bqn origin port)
  (begin0
    (if unread?
        (parse #f (bqn-tokenizer port))
        eof)
    (set! unread? (not unread?))))

(define (setup!)
  (current-read-interaction read-bqn)
  (void))