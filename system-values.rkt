#lang racket/base
(require racket/trace math/array racket/provide)
(provide (matching-identifiers-out #rx"•" (all-defined-out)))
(define (•promote x)
  (if (array? x) x (array x)))

(define (•strict x)
  (if (array? x)
      (array-strict x)
      x))

(define (•time x)
  (time x))

(define-syntax-rule (•trace f) (trace f))

(define (•show x)
  (display x))

(define (•exit x)
  (exit x))