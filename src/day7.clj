(ns day7
  (:require [clojure.string :as str]))
(def ranks (zipmap (list \A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2) (range 13 0 -1)))
(def input (->> "../data/day7.txt"
               slurp
               str/split-lines
               (map #(str/split % #" "))))

(defn rank-hand [hand]
  (let [sorted-hand (reverse (sort-by second hand))
        primary (second (first sorted-hand))
        secondary (second (second sorted-hand))]
    (cond
      (= 5 primary) 7
      (= 4 primary) 6
      (and (= 3 primary) (= 2 secondary)) 5
      (and (= 3 primary) (= 1 secondary)) 4
      (and (= 2 primary) (= 2 secondary)) 3
      (and (= 2 primary) (= 1 secondary)) 2
      (= 1 primary) 1
      :else 0)))


(defn compare-ranks [ranksA ranksB]
  (or (first (filter #(not (zero? %)) (map #(compare (nth ranksA %) (nth ranksB %)) (range (count ranksA))))) 0))
(defn compare-hands [handA handB]
  (let [rankA (rank-hand (frequencies (first handA)))
        rankB (rank-hand (frequencies (first handB)))
        cardsA (map #(get ranks %) (first handA))
        cardsB (map #(get ranks %) (first handB))]
    (if (= rankA rankB)
      (compare-ranks cardsA cardsB)
      (compare rankA rankB))))

(def q2ranks (zipmap (list \A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J) (range 13 0 -1)))
(defn q2-rank-hand [hand]
  (let [sorted-hand (reverse (sort-by second hand))
        wildcards  (or (second (first (filter #(= (first %) \J) sorted-hand))) 0)
        filtered-hand (filter #(not= (first %) \J) sorted-hand)
        primary (+ (or (second (first filtered-hand)) 0) wildcards)
        secondary (or (second (second filtered-hand)) 0)]
    (cond
      (= 5 primary) 7
      (= 4 primary) 6
      (and (= 3 primary) (= 2 secondary)) 5
      (and (= 3 primary) (= 1 secondary)) 4
      (and (= 2 primary) (= 2 secondary)) 3
      (and (= 2 primary) (= 1 secondary)) 2
      (= 1 primary) 1
      :else 0)))
(defn q2-compare-hands [handA handB]
  (let [rankA (q2-rank-hand (frequencies (first handA)))
        rankB (q2-rank-hand (frequencies (first handB)))
        cardsA (map #(get q2ranks %) (first handA))
        cardsB (map #(get q2ranks %) (first handB))]
    (if (= rankA rankB)
      (compare-ranks cardsA cardsB)
      (compare rankA rankB))))

(defn run [& _] (do (println "q1: "(->> input
                                        (sort compare-hands)
                                        (map-indexed #(* (inc %1) (Integer/parseInt (second %2))))
                                        (reduce +)))
                    (println "q2: "(->> input
                                        (sort q2-compare-hands)
                                        (map-indexed #(* (inc %1) (Integer/parseInt (second %2))))
                                        (reduce +)))))
(run 1)
