(ns day6
  (:require [clojure.string :as str]))

(def input (-> "../data/day6.txt"
               slurp
               str/split-lines))

(def races {:times (map #(Integer/parseInt (str/trim (first %))) (re-seq #"\s*(\d+)\s*" (first input)))
            :distances (map #(Integer/parseInt (str/trim (first %))) (re-seq #"\s*(\d+)\s*" (second input)))})
(defn distances [speeds times]
  (map * speeds times))
(def results (map #(distances (first %) (second %)) (map #(list (range %) (range % 0 -1)) (:times races))))

(defn get-winners [results target]
  (count (filter #(< target %) results)))
(defn get-first-winner [time target]
  (take-while #(< (* % (- time %)) target) (range time)))

(defn get-last-winner [time target]
  (take-while #(< (* % (- time %)) target) (range time 0 -1)))

(def q2race {:times (Long/parseLong (str/join (:times races)))
             :distances (Long/parseLong (str/join (:distances races)))})
(defn run [& _] (do
                  (println "q1: "(reduce * (map-indexed #(get-winners (nth results %1) %2) (:distances races))))
                  (println "q2: " (- (last (get-last-winner  (:times q2race) (:distances q2race)))
                                     (inc (last (get-first-winner  (:times q2race) (:distances q2race))))))))

(println (last (get-last-winner  (:times q2race) (:distances q2race))) " - " (last (get-first-winner  (:times q2race) (:distances q2race))))

(println (:distances q2race) (* (- (:times q2race) 5980954) 5980954) (< (* (- (:times q2race) 5980954) 5980954) (:distances q2race)))
(println (:distances q2race) (* (- (:times q2race) 43998540) 43998540) (< (* (- (:times q2race) 43998540) 43998540) (:distances q2race)))
(run 1)
