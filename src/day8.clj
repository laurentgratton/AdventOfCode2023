(ns day8
  (:require [clojure.string :as str]))

(def input (-> "../data/day8.txt"
               slurp
               (str/split #"\n\n")))

(defn next-direction [] (lazy-seq (cycle (vec (str/trim (first input))))))

(defn parse-instruction [instruction]
  (let [items (rest (first (re-seq #"(\w+) = \((\w+), (\w+)\)" instruction)))]
    {:source (first items)
     :left (second items)
     :right (last items)}))

(def instructions (map parse-instruction (str/split-lines (second input))))

(defn traverse [instructions directions source destination]
    (loop [current-instruction (first (filter #(= (:source %) source) instructions))
           current-direction (first directions)
           steps 0]
          (if (= destination (:source current-instruction))
            steps
            (recur (first(filter #(= (:source %) ((if (= \L current-direction) :left :right) current-instruction)) instructions))
                   (nth directions (mod (inc steps) (count directions)))
                   (inc steps)))))

(defn get-direction [instruction direction instructions]
  (first (filter #(= (:source %) (direction instruction)) instructions)))

(defn q2traverse [instructions directions source destination]
  (loop [current-instructions  (filter #(= (:source %) source) instructions)
         current-direction (first directions)
         steps 0]
        (if (reduce #(and %1 (= (last (:source %2)) destination)) true current-instructions)
          steps
          (recur (map #(get-direction % (if (= \L current-direction) :left :right) instructions) current-instructions)
                 (nth directions (mod (inc steps) (count directions)))
                 (inc steps)))))

(def starts (map :source (filter #(= (last (:source %)) \A) instructions)))
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn run [& _] (do
                  (println "q1: " (traverse instructions (str/trim (first input)) '"AAA" '"ZZZ"))
                  (println "q2: " (reduce lcm (map #(q2traverse instructions (str/trim (first input)) % \Z) starts)))))

(run 1)
