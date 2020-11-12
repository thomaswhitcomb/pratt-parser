# pratt-parser
## Overview

An implementation of a Pratt Parser in Clojure.

## References

Some great information on Pratt Recursive Decent parsers

[http://effbot.org/zone/simple-top-down-parsing.htm]()

[http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/]()

## Prerequisites

You will need to install Clojure and either Java 8 or 11.  You can get the details here: [https://clojure.org/guides/getting_started]().

## Build

`> make`

## Repl

`(load-file "src/lexer.clj")`
`(load-file "src/pratt.clj")`

;  Create a lexical analyzer
`(def lexer (lexer/new-lexer "1+((2*4)-3)"))`

; Start the lexical analyzer
`(lexer :start)` ; returns first token.

`(lexer :curr)` ; returns current token.

`(lexer :next)` ; returns next token.

`(lexer :curr_next)` ; returns the current token and advances.

; Create a parser
`(def parser (pratt/new-parser lexer))`



