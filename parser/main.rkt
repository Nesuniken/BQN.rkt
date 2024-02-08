#lang racket
(require megaparsack megaparsack/parser-tools/lex data/monad data/applicative math/array br/macro)

(define (maybe/p parser)
  (many/p parser #:max 1))

(define newline (==/p '|,|))
(define def
  (or/p (==/p '←) (==/p '⇐)))
(define assign
  (or/p def (==/p '↩)))

(define-macro (ns-get TYPES ...)
  #'(do (maybe/p (do atom (==/p '|.|)))
        (or/p (token/p TYPES) ...)))

(define ((brackets open close) inside)
  (do (token/p open)
      inside
      (token/p close)))

(define ((array-form open close) inside)
  ((brackets open close)
   (do (maybe/p newline)
       inside
       (maybe/p newline))))

(define parentheses
  (brackets '|(| '|)|))
(define list/p
  (array-form '|⟨| '|⟩|))
(define array/p
  (array-form '|[| '|]|))

(define program
  (do (maybe/p newline)
      (many/p line #:sep newline)
      (maybe/p newline)))

(define line
  (or/p
   ;(try/p import)
   stmt))

;; (define import
;;   (do (maybe/p lhsComp def) bqn-req))

(define stmt
  (or/p (try/p nothing)
        expr))

(define expr
  (or/p any+
        _m2Expr_
        _m1Expr
        (try/p subExpr)
        FuncExpr))

(define bqn-req
  (do (==/p 'Require)
      (token/p 's)))

(define (atomic literal custom expr)
  (or/p (token/p literal)
        (ns-get  custom)
        (parentheses expr)))

(define _mod2_ (atomic '_m2l_ '_m2_ _m2Expr_))
(define _mod1  (atomic '_m1l  '_m1  _m1Expr ))
(define  Func  (atomic 'Fl    'F    FuncExpr))
(define  atom
  (or/p arr-list
        arr-merge
        (token/p 'sl)
        (ns-get 's)
        (parentheses subExpr)))

(define arr-list
  (list/p (many/p expr #:sep newline)))
(define merge-list
  (list/p (many+/p expr #:sep newline)))

(define any
  (or/p (ns-get 's 'F '_m1 '_m2_)
        (parentheses expr)
        _mod2_
        _mod1
        Func
        atom))

(define any+
  (many+/p any #:sep (token/p '‿)))
(define strand
  (many/p  any #:sep (token/p '‿) #:min 2))

(define subject
  (or/p
   (do atom (many/p (do (token/p '‿) any)))
   strand))

(define (mExpr atomic custom)
  (or/p (do (ns-get custom)
            (maybe/p
             (do assign
                 (mExpr atomic custom))))
        atomic))

(define _m2Expr_
  (mExpr _mod2_ '_m2_))
(define _m1Expr
  (mExpr _mod1  '_m1))

(define (mods [min 0])
  (many/p
       (do (or/p _mod1
                 (do _2mod_ Operand)))
       #:min min)
  )
