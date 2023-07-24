#lang racket/base
(require BQN/lexer BQN/parser)
(provide setup!)

(define (read-bqn-line origin port)
  (define line (read-line port))
  (if (eof-object? line)
      eof
      (parse (bqn-tokenizer (open-input-string line)))))

(define (setup!)
  (current-read-interaction read-bqn-line)
  (void))