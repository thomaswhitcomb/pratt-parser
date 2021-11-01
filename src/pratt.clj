(ns pratt (:gen-class)
   (:import (java.util Date TimeZone))
   (:require
     [lexer :as l]
     [clojure.test :refer [is]]
     [clojure.string :as s]
     ))

(defn decorate-literal []
  (fn [parser {value :value :as token}]
    (merge token {:nud
                  (fn [] (list :literal value ))})))

(defn decorate-right-paren []
  (fn [parser token]
    (merge token {:lbp 0})))

(defn decorate-power []
  (fn [parser token]
    (merge token {:lbp 30
                :led (fn [left]
                       (let [right (parser (- 30 1))]
                         (list :power left right )))})))

(defn decorate-left-paren []
  (fn [parser {value :value :as token}]
    (merge token {:nud (fn [] (let [e (parser 0)
                                  l (parser)]
                              (if (= (:value (l :curr)) ")")
                                (do ((l :next) :curr) e)
                                (throw (Exception. "Missing right paren")))))})))

(defn decorate-prefix-neg [f oper lbp]
  (fn [parser token]
    (let [new-token (f parser token)]
      (merge new-token { :nud (fn [] (let [e (parser lbp)] (list oper e )))}))))

(defn decorate-infix-oper [oper lbp]
  (fn [parser token]
    (merge token {:lbp lbp
                :led (fn [left]
                       (let [right (parser lbp)]
                         (list oper left right )))})))


(def decorator {
                :number  (decorate-literal)
                :word (decorate-literal)
                :left-paren (decorate-left-paren)
                :right-paren (decorate-right-paren)
                :power (decorate-power)
                :add (decorate-infix-oper :add 10)
                :multiply (decorate-infix-oper :multiply 20)
                :divide (decorate-infix-oper :divide 20)
                :subtract (decorate-prefix-neg (decorate-infix-oper :subtract 10) :neg 100)
                })

(defn decorate-token [parser {t :type value :value :as token}]
  (cond
    (contains? decorator t)
    ((t decorator) parser token)

    (= t :eof)
    (merge token {:lbp 0
                :nud (fn []
                       (list :eof))})

    :else
    token))

(defmulti new-parser (fn [& p]
                       (cond
                         (= (count p) 0)
                         :start
                         :else
                         (first p))))

(defn new-parser [lexer]
    (fn parser
      ([rbp]
       (let [t (decorate-token parser (lexer :curr_next))
             left (:nud t)]
         (loop [left1 (left)]
;          (prn "processing " (lexer :curr))
           (if (< rbp (:lbp (decorate-token parser (lexer :curr))))
             (let [t (decorate-token parser (lexer :curr_next))
                   left2 ((t :led) left1)]
               (recur left2))
             left1))))
      ([] lexer)))

(is (= '(:literal "456")
       ((new-parser ((l/new-lexer "456") :start)) 0)))

(is (= '(:add (:add (:literal "123") (:literal "456")) (:literal "789"))
       ((new-parser ((l/new-lexer "123+456+789") :start)) 0)))

(is (= '(:multiply (:add (:literal "123") (:literal  "456")) (:literal "789"))
       ((new-parser ((l/new-lexer "(123+456) *789") :start)) 0)))

(is (= '(:add (:literal "123") (:multiply (:literal  "456") (:literal "789")))
       ((new-parser ((l/new-lexer "123+ (456 *789)") :start)) 0)))

(is (= '(:add (:literal "123") (:neg (:literal  "5")))
       ((new-parser ((l/new-lexer "123 + -5") :start)) 0)))

(is (= '(:add (:literal "123") (:power (:literal  "5") (:literal "4")))
       ((new-parser ((l/new-lexer "123 + 5 ** 4") :start)) 0)))

(is (= '(:power (:add (:literal "123") (:literal  "5")) (:literal "4"))
       ((new-parser ((l/new-lexer "(123 + 5) ** 4") :start)) 0)))

(is (= '(:power (:add (:literal "123") (:literal  "5")) (:divide (:literal "4") (:literal"2")))
       ((new-parser ((l/new-lexer "(123 + 5) ** (4/2)") :start)) 0)))

(is (= '(:add (:literal "77") (:power (:add (:literal "123") (:literal  "5")) (:divide (:literal "4") (:literal"2"))))
       ((new-parser ((l/new-lexer "77 + ( (123 + 5) ** (4/2) )") :start)) 0)))

(is (= '(:subtract (:add (:literal "123") (:multiply (:literal "456") (:literal "789"))) (:divide (:literal "5") (:literal "77"))) ((new-parser ((l/new-lexer "123+456*789-5/77") :start)) 0)))
