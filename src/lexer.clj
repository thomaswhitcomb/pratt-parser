 (ns lexer (:gen-class)
    (:import (java.util Date TimeZone))
    (:require
      [clojure.test :refer [is]]
      [clojure.string :as s]
      ))

(defn to-str [cs]
  (apply str cs))

(def operators
  { "+" :add
   "-"  :subtract
   "*"  :multiply
   "/"  :divide
   "!"  :bang
   "="  :equal
   "!=" :not-equal
   "**" :power
   "~"  :tilda })

(defn is-alpha [c]
  (let [i (int c)] 
    (and (>= i (int \A)) (<= i (int \z)))))

(defn is-digit [c]
  (let [i (int c)] 
    (and (>= i (int \0)) (<= i (int \9)))))


(defn remove-leading-whitespace [[c & cs :as all]]
  (cond
    (= c \space)
    (remove-leading-whitespace cs)
    :else
    all))

(defn normalize-token [{value :value :as token}]
  (assoc token :value (to-str value)))

(defn number [cs]
  {:type :number 
   :value (take-while is-digit cs)})

(defn word [cs]
  {:type :word 
   :value (take-while (fn [c] (or (is-alpha c) (is-digit c))) cs)})

(defn operator [cs]
  (let [x (take-while (fn [c] (and (not (is-alpha c)) (not (is-digit c)))) cs)]
  {:type (operators (to-str x))
   :value x}))

(defn pop_ [tail {value :value} ]
    (drop 
      (count value) 
    (remove-leading-whitespace tail)))

(defn peek_ [[h :as all]]
  (normalize-token 
    (cond 
      (nil? h) 
      {:type :eof :value ""}

      (is-digit h)
      (number all)

      (is-alpha h)
      (word all)

      :else
      (operator all))))

(defn next-token [s]
  (peek_ s))

(defn new-lexer [s] 
  (let [stream (atom s)
        curr (atom nil)]
    (fn lexer [x] 
      (cond
        (= x :start)
        (do (lexer :next) lexer)

        (= x :next)
        (let [token (next-token @stream)]
          (do 
            (swap! stream pop_ token)
            (reset! curr token)))

        (= x :curr) 
        @curr

        (= x :curr_next)
        (let [x @curr]
          (do 
            (lexer :next)
            x))

        (= x :peek)
        (next-token @stream)))))

(is (and (is-alpha \a) (is-alpha \Z) (is-alpha \p) (not (is-alpha \1))))
(is (and (is-digit \0) (is-digit \3) (is-digit \9) (not (is-digit \h))))
(is (= (remove-leading-whitespace (seq "   123")) (list \1 \2 \3)))
(is (= (remove-leading-whitespace (seq "123")) (list \1 \2 \3)))
(is (= (next-token "a123abc+") 
      '{:type :word, :value "a123abc"}))
(is (= (next-token "123abc+") 
      '{:type :number, :value "123"}))
