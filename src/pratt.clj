(ns pratt (:gen-class)
   (:import (java.util Date TimeZone))
   (:require
     [lexer :as l]
     [clojure.test :refer [is]]
     [clojure.string :as s]
     ))

(defn decorate-token [parser {t :type value :value :as all}]
  (cond 
    (= t :number) 
    (merge all {:nud (fn [] (list :literal value ))}) 

    (= t :word) 
    (merge all {:nud (fn [] (list :literal value ))}) 

    (= t :left-paren) 
    (merge all {:nud (fn [] (let [e (parser 0)
                                  l (parser)] 
                              (if (= (:value (l :curr)) ")")
                                (do 
                                  (l :next) 
                                  e)
                                (throw (Exception. "Missing right paren")))))})

    (= t :right-paren) 
    (merge all {:lbp 0})
                
    (= t :power) 
    (merge all {:lbp 30 
                :led (fn [left] 
                       (let [right (parser (- 30 1))] 
                         (list :power left right )))}) 

    (= t :add) 
    (merge all {:lbp 10 
                :led (fn [left] 
                       (let [right (parser 10)] 
                         (list :add left right )))}) 

    (= t :subtract) 
    (merge all {:lbp 10 
                :nud (fn [] 
                         (let [e (parser 100)] 
                           (list :neg e )))
                :led (fn [left] 
                       (let [right (parser 10)] 
                         (list :subtract left right)))}) 

    (= t :multiply) 
    (merge all {:lbp 20 
                :led (fn [left] 
                       (let [right (parser 20)] 
                         (list :multiply left right )))}) 

    (= t :divide) 
    (merge all {:lbp 20 
                :led (fn [left] 
                       (let [right (parser 20)] 
                         (list :divide left right)))}) 

    (= t :eof) 
    (merge all {:lbp 0 
                :nud (fn [] 
                       (list :eof))}) 

    :else 
    all))
                

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

(is (= '(:add (:add (:literal "123") (:literal "456")) (:literal "789"))
       ((new-parser ((l/new-lexer "123+456+789") :start)) 0)))

(is (= '(:subtract (:add (:literal "123") (:multiply (:literal "456") (:literal "789"))) (:divide (:literal "5") (:literal "77"))) ((new-parser ((l/new-lexer "123+456*789-5/77") :start)) 0)))
