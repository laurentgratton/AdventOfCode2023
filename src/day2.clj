(ns day2
  (:require [clojure.string :as str]))

(def games (-> "../data/day2.txt"
               slurp
               str/split-lines))
(def max-cubes-q1 '{:red 12 :green 13 :blue 14})

(defn get-tries [input]
  (-> input
      (str/split #":")
      second
      (str/split #";")))

(defn parse-pull [pull]
  (->> (str/split pull #",")
       (map #(str/split % #" "))
       (map #(hash-map (keyword (nth % 2)) (second %)))))

(defn is-valid-pull [pull maximums]
  (reduce #(and %1 (>= (get maximums (first (keys %2))) (Integer/parseInt (first (vals %2))))) true pull))
(defn is-valid-game [pulls maximums]
  (->> pulls
       (reduce #(and %1 (is-valid-pull %2 maximums)) true)))
(defn get-max-needed [game]
    (->> game
        flatten
        (map first)
        (sort-by #(Integer/parseInt (val %)))
        (reduce conj '{})
        vals
        (map #(Integer/parseInt %))
        (reduce *)))

(defn parse-game [game]
  (map parse-pull game))
(defn q2 [] (->> games
                 (map get-tries)
                 (map parse-game)
                 (map #(get-max-needed %))
                 (reduce +)))
(defn run [_ & _]
  (do
    (println "q1: " (reduce #(+ %1 (inc (first %2))) 0 (filter #(true? (second %)) (zipmap (range (count games)) (map #(is-valid-game (map parse-pull (get-tries %)) max-cubes-q1) games)))))
    (println "q2: " (q2))))

(run 1)
