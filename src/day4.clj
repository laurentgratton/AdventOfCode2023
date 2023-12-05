(ns day4
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math :as math]))

(def input (->> "../data/day4.txt"
               slurp
               str/split-lines))

(defn get-winning-numbers [line]
  (set (map #(Integer/parseInt %) (filter #(not= "" %) (str/split (subs line (inc (str/index-of line \:)) (str/index-of line \|)) #" ")))))

(defn get-chosen-numbers [line]
  (set (map #(Integer/parseInt %) (filter #(not= "" %) (str/split (subs line (inc (str/index-of line \|))) #" ")))))

(defn get-winners [winning chosen]
  (let [a (set winning)
        b (set chosen)]
    (set/intersection a b)))

(defn score-ticket
  ([winners] (cond
               (= 0 winners) 0
               (= 1 winners) 1
               (= 2 winners) 2
               :else (int (math/pow 2 (dec winners))))))

(def wins (map #(count (get-winners (get-winning-numbers %) (get-chosen-numbers %))) input))

(defn get-wins [wins]
  (map-indexed #(hash-map :index %1 :val %2 :wins (range (inc %1) (inc (+ %1 %2)))) wins))

(def visits (take (count wins) (repeat 1)))

(defn propagate-wins [state index win]
  (let [multiplier (num (nth state index))
        items (map #(* multiplier %) win)]
      (map + state items)))


(defn count-visits [wins]
  (let [win-data (->> (map-indexed #(list %1 %2) wins)
                      (map #(concat '() (take (inc (first %)) (repeat 0)) (take (second %) (repeat 1)) (take (- (count wins) (inc (first %)) (second %)) (repeat 0)))))]
    (loop [state (take (count wins) (repeat 1)) index 0 current (first win-data) remainder (rest win-data)]
      (if (empty? remainder)
        state
        (recur (propagate-wins state index current) (inc index) (first remainder) (rest remainder))))))


(defn run [& _] (do
                  (println "q1: " (reduce + (map #(score-ticket (count (get-winners (get-winning-numbers %) (get-chosen-numbers %)))) input)))
                  (println "q2: " (reduce + (count-visits wins)))))

(run 1)
