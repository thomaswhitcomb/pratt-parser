
(ns lexer-test
    (:require [lexer :as l]
              [clojure.test :as t]))

(t/deftest test1 
  (t/is 
    (and (l/is-alpha \a) (l/is-alpha \Z) (l/is-alpha \p) (not (l/is-alpha \1)))))

(t/deftest test2 
  (t/is 
    (and (l/is-digit \0) (l/is-digit \3) (l/is-digit \9) (not (l/is-digit \h)))))

(t/deftest test3 
  (t/is 
    (= 
      (l/remove-leading-whitespace (seq "   123")) 
      (list \1 \2 \3))))

(t/deftest test4 
  (t/is 
    (= 
      (l/remove-leading-whitespace (seq "123")) 
      (list \1 \2 \3))))

(t/deftest test5 
  (t/is 
    (= 
      (l/next-token "a123abc+") 
      '{:type :word, :value "a123abc"})))

(t/deftest test6 
  (t/is 
    (= 
      (l/next-token "123abc+") 
      '{:type :number, :value "123"})))

(t/deftest test7 
  (t/is 
    (= 
      (l/next-token "  abc+") 
      '{:type :word, :value "abc"})))

(t/deftest test8 
  (t/is 
    (= 
      (l/next-token "def  abc+") 
      '{:type :word, :value "def"})))
