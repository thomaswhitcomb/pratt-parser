 (ns main
    (:gen-class)
    (:require 
      [lexer :as l] 
      [pratt :as p]))

  (defn -main
    [& args]
    (let [
          lex ((l/new-lexer "123+456+789") :start)
          parser (p/new-parser lex)]
      (parser 0)))
