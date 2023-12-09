(ns day9
  (:require [clojure.string :as str]))
(defn make-numbers [list-of-nums]
  (map #(Integer/parseInt %) list-of-nums))
(def input (->> "../data/day9.txt"
               slurp
               str/split-lines
               (map #(str/split % #" "))
               (map make-numbers)))
(defn get-sequence [values]
  (map #(- (nth values %) (nth values (dec %))) (range 1 (count values))))

(defn build-sequences [values]
  (loop [current values parsed (list values)]
    (if (every? zero? current)
      parsed
      (recur (get-sequence current) (conj parsed (get-sequence current))))))

(defn generate-next [values]
  (reduce #(+ %1 (last %2)) 0 values))

(defn generate-first [values]
  (reduce #(- (first %2) %1) 0 values))
(defn run [& _] (do
                  (println "q1: " (reduce + (map #(generate-next (build-sequences %)) input)))
                  (println "q2: " (reduce + (map #(generate-first (build-sequences %)) input)))))
(run 1)
