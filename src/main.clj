 (ns main
    (:gen-class)
    (:require
      [lexer :as l]
      [clojure.test :refer [is]]
      [pratt :as p]))

  (defn -main
    [& args]
    (prn "Parsing " (first args))
    (let [
          lex ((l/new-lexer (first args)) :start)
          parser (p/new-parser lex)]
      (prn (parser 0))))

(is (= ((pratt/new-parser ((lexer/new-lexer "1 + ( (2 * 4) - 3)") :start)) 0)
       '(:add (:literal "1") (:subtract (:multiply (:literal "2") (:literal "4")) (:literal "3")))))
