#lang racket/base
(require racket/port "lexer.rkt" "parser.rkt")
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