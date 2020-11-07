(ns pratt (:gen-class)
   (:import (java.util Date TimeZone))
   (:require
     [lexer :as l]
     [clojure.test :refer [is]]
     [clojure.string :as s]
     ))

(defn decorate-number []
  (fn [{value :value :as token}]
    (merge token {:nud
                  (fn [] (list :literal value ))})))

(defn decorate-word []
  (fn [{value :value :as token}]
    (merge token {:nud (fn [] (list :literal value ))})))


(def decorator {
                :number  (decorate-number)
                :word (decorate-word)
                })

(defn decorate-token [parser {t :type value :value :as token}]
  (cond
    (contains? decorator t)
    ((t decorator) token)

    (= t :left-paren)
    (merge token {:nud (fn [] (let [e (parser 0)
                                  l (parser)]
                              (if (= (:value (l :curr)) ")")
                                (do
                                  (l :next)
                                  e)
                                (throw (Exception. "Missing right paren")))))})

    (= t :right-paren)
    (merge token {:lbp 0})

    (= t :power)
    (merge token {:lbp 30
                :led (fn [left]
                       (let [right (parser (- 30 1))]
                         (list :power left right )))})

    (= t :add)
    (merge token {:lbp 10
                :led (fn [left]
                       (let [right (parser 10)]
                         (list :add left right )))})

    (= t :subtract)
    (merge token {:lbp 10
                :nud (fn []
                         (let [e (parser 100)]
                           (list :neg e )))
                :led (fn [left]
                       (let [right (parser 10)]
                         (list :subtract left right)))})

    (= t :multiply)
    (merge token {:lbp 20
                :led (fn [left]
                       (let [right (parser 20)]
                         (list :multiply left right )))})

    (= t :divide)
    (merge token {:lbp 20
                :led (fn [left]
                       (let [right (parser 20)]
                         (list :divide left right)))})

    (= t :eof)
    (merge token {:lbp 0
                :nud (fn []
                       (list :eof))})

    :else
    token))


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
