#lang racket/gui
(require drracket/tool framework BQN/keymap BQN/lexer BQN/parser br/syntax)
(provide tool@)

(define bqn-mode%
  (class mode:surrogate-text%
    (super-new)
    (define/override (on-enable-surrogate txt)
      (send (send txt get-keymap) chain-to-keymap bqn-keymap #t))
    (define/override (on-disable-surrogate txt)
      (send (send txt get-keymap) remove-chained-keymap bqn-keymap))))

(define openings #(#\{ #\[ #\⟨))
(define closings #(#\} #\] #\⟩))

(define (not-equal-to e)
  (negate (curry equal? e)))

(define (complete-brackets? stack str)
  (cond
    [(empty? str) (empty? stack)]
    [(equal? (first str) #\")
     (let ([unquoted (dropf (rest str) (not-equal-to #\"))])
       (and
        (cons? unquoted)
        (complete-brackets? stack unquoted)))]
    [(vector-member (first str) openings)
     => (lambda (i)
          (complete-brackets?
           (cons i stack) (rest str)))]
    [(vector-member (first str) closings)
     => (lambda (i)
          (or
           (not (equal? i (first stack)))
           (complete-brackets? (rest stack) (rest str))))]
    [else (complete-brackets? stack (rest str))]))

(define (has-code? str)
  (and (cons? str)
       (case (first str)
         [(#\space #\newline #\tab #\, #\⋄)
          (has-code? (rest str))]
         [(#\#)
          (has-code? (dropf str (not-equal-to #\newline)))]
         [else (complete-brackets? '() str)]
         )))

(define (submit? text pos)
  (has-code? (string->list (send text get-text 0 pos))))


(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (read-syntax path port)
      (define parse-tree (parse path (bqn-tokenizer port path)))
      (strip-bindings
       #`(module bqn-mod BQN/expander
           #,parse-tree)
       )
      )

    (drracket:modes:add-mode
     "BQN mode"
     (new bqn-mode%)
     submit?
     (curry equal? '("Experimental Languages" "BQN"))
     )

    (define (phase1) (void))
    
    (define (phase2)
      (define bqn-lang%
        ((drracket:language:get-default-mixin)
         (drracket:language:module-based-language->language-mixin
          (drracket:language:simple-module-based-language->module-based-language-mixin
           (class* object% (drracket:language:simple-module-based-language<%>)
             (super-new)
             (define/public (get-language-numbers)
               '(1000 0))
             (define/public (get-language-position)
               '("Experimental Languages" "BQN"))
             (define/public (get-module)
               'BQN/expander)
             (define/public (get-one-line-summary)
               "")
             (define/public (get-reader)
               read-syntax)
             (define/public (get-language-url)
               #f))))))
      (drracket:language-configuration:add-language
       (new bqn-lang%) #:allow-executable-creation? #t))))