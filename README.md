# BQN for Racket
BQN.rkt is a BQN compiler written in Racket, capable of converting (some) BQN code into native Racket syntax. 
While it's still too early in development to even call an alpha product, it can at least function as a rough proof of concept,
and even already has some novelties such as complex arithmetic.

**Warning:** although the default font for DrRacket appears to support BQN's character set, there are some character it can't italicize. When DrRacket tries to italicize them, such as when they appear in an error message, [the entire application bugs out](https://github.com/racket/drracket/issues/607#issue-1593118550). Switching to a font fully compatible with BQN chacaters avoids the issue.
