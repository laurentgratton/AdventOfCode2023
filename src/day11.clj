(ns day11
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (-> "../data/day11.txt"
               slurp
               str/split-lines))

(def empty-rows (filter #(true? (second %)) (map-indexed #(list %1 (>= 1 (count %2))) (map #(set (chars (char-array %))) input))))
(defn get-col [inputs column]
  (map #(nth % column) inputs))
(def list2 (map str/join (map #(get-col input %) (range (count (first input))))))

(def empty-cols (filter #(true? (second %)) (map-indexed #(list %1 (>= 1 (count %2))) (map #(set (chars (char-array %))) list2))))

(defn offset [[x y val] empty-rows empty-cols per-step]
  (let [x-offsets (count (filter #(>= x (first %)) empty-cols))
        y-offsets (count (filter #(>= y (first %)) empty-rows))]
    (list (+ x (* per-step x-offsets))
          (+ y (* per-step y-offsets))
          val)))

(defn get-galaxies [index line]
  (filter #(= \# (nth % 2)) (map-indexed #(list %1 index %2) line)))

(def galaxies (set (map #(offset % empty-rows empty-cols 1) (reduce concat (list) (map-indexed get-galaxies input)))))
(defn absolute-distance [s1 s2]
  (Math/abs (- s1 s2)))
(defn manhattan [[galaxy1 galaxy2]]
  (->> (map absolute-distance (drop-last galaxy1) (drop-last galaxy2))
       (reduce +)))

(def q2galaxies (set (map #(offset % empty-rows empty-cols (dec 1000000)) (reduce concat (list) (map-indexed get-galaxies input)))))
(defn run [& _] (do
                  (println "q1: " (reduce + (map manhattan (combo/combinations galaxies 2))))
                  (println "q2: " (reduce + (map manhattan (combo/combinations q2galaxies 2))))))
(run 1)
