#lang racket/base
(require BQN/lexer BQN/parser)
(provide setup!)

(define repl-parse (make-rule-parser stmt))

(define (read-bqn-line origin port)
  (define line (read-line port))
  (if (eof-object? line)
      eof
      (repl-parse (bqn-tokenizer (open-input-string line)))))

(define (setup!)
  (current-read-interaction read-bqn-line))