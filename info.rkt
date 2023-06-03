#lang info
(define license 'MIT)

(define deps '("brag-lib"
               "beautiful-racket-macro"
               "br-parser-tools-lib"))

(define drracket-tools (list (list "lang-plugin.rkt")))
(define drracket-tool-names (list "BQN Mode"))