(ns day12
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (->> "../data/day12.txt"
               slurp
               str/split-lines
               (map #(str/split % #" "))))
(def options (list \. \#))
(defn valid-pattern? [input pattern]
  (let [to-check (filter #(not= '"" %) (str/split input #"\."))
        pattern-steps (map #(Integer/parseInt %) (str/split pattern #","))]
    (loop [curr-pattern (first pattern-steps) remaining-pattern (rest pattern-steps) check-index 0 valid true]
          (if (>= check-index (count to-check))
            (and (nil? curr-pattern) valid)
            (recur
              (first remaining-pattern)
              (rest remaining-pattern)
              (inc check-index)
              (and
                valid
                (= curr-pattern (count (filter #(= \# %) (nth to-check check-index))))))))))

(defn count-wildcards [input]
  (count (filter #(= \? %) input)))
(defn insert-into [input values]
  (reduce #(str/replace-first %1 \? %2) input values))

(defn get-num-valids [input]
  (let [pattern (first input)
        key (second input)
        wildcards (combo/selections options (count-wildcards pattern))]
    (count (filter #(valid-pattern? (insert-into pattern %) key) wildcards))))

;;(println "q1: " (reduce + (map #(get-num-valids %) input)))

(defn quintuple [[pattern key]]
  (list (str/join (repeat 5 pattern)) (str/join '"," (repeat 5 key))))
(def q2inputs (map quintuple input))
(println (first q2inputs))

(println "q2: " (reduce + (pmap #(get-num-valids %) q2inputs)))
