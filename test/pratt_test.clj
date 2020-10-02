(ns pratt-test
    (:require [pratt :as p]
              [lexer :as l]
              [clojure.test :as t]))

(t/deftest test1 
  (t/is 
    (= 
      '(:add (:add (:literal "123") (:literal "456")) (:literal "789")) 
      ((p/new-parser ((l/new-lexer "123+456+789") :start)) 0))))

(t/deftest test2 
  (t/is 
    (= 
      '(:subtract (:add (:literal "123") (:multiply (:literal "456") (:literal "789"))) (:divide (:literal "5") (:literal "77"))) 
      ((p/new-parser ((l/new-lexer "123+456*789-5/77") :start)) 0))))
