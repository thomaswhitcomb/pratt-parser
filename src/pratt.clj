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
    (merge all {:nud (fn [] value)}) 

    (= t :power) 
    (merge all {:lbp 30 
                :led (fn [left] 
                       (let [right (parser (- 30 1))] 
                         (str "(power " left " " right ")")))}) 

    (= t :add) 
    (merge all {:lbp 10 
                :led (fn [left] 
                       (let [right (parser 10)] 
                         (str "(add " left " " right ")")))}) 

    (= t :subtract) 
    (merge all {:lbp 10 
                :nud (fn [] 
                       (let [e (parser 100)]
                         (str "(neg " e ")")))
                :led (fn [left] 
                       (let [right (parser 10)] 
                         (str "(sub " left " " right)))}) 

    (= t :multiply) 
    (merge all {:lbp 20 
                :led (fn [left] 
                       (let [right (parser 20)] 
                         (str "(mult " left " " right ")")))}) 

    (= t :divide) 
    (merge all {:lbp 20 
                :led (fn [left] 
                       (let [right (parser 20)] 
                         (str "(div " left " " right)))}) 

    (= t :eof) 
    (merge all {:lbp 0 :nud (fn [] -1)}) 

    :else 
    all))
                

(defn new-parser [lexer]
    (fn parser [rbp]
      (let [t (decorate-token parser (lexer :curr_next))
            left (:nud t)]
        (loop [left1 (left)]
          (if (< rbp (:lbp (decorate-token parser (lexer :curr))))
            (let [t (decorate-token parser (lexer :curr_next))
                  left2 ((t :led) left1)]
              (recur left2))
            left1)))))
