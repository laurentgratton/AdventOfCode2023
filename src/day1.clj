(ns day1
  (:require [clojure.string :as str]))

(def inputs (->> "../data/day1.txt"
                slurp
                str/split-lines))

(def digits '{"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 "zero" 0})

(defn q1 [input]
  (let [digits (filter #(> % 0) (map #(Character/digit % 10) input))
        lead (first digits)
        tail (last digits)]
    (str lead tail)))

(println "q1: " (reduce #(+ %1 (Integer/parseInt %2)) 0 (map q1 inputs)))

(defn get-first-char-digits [digits input]
  (zipmap (map val digits) (map #(str/index-of input %) (keys digits))))

(defn get-last-char-digits [digits input]
  (zipmap (map val digits) (map #(str/last-index-of input %) (keys digits))))

(defn get-digits [input]
  (->> input
       (map #(Character/digit % 10))))


(defn get-first-digit [char-digits input-digits]
  (let [smallest-char (or (first (sort-by second (filter #(some? (second %)) char-digits))) [0 (count input-digits)])
        smaller-digits (take (inc (second smallest-char)) input-digits)
        char-smaller? (some? (some #(>= % 0) smaller-digits))]
    (if char-smaller? (first (filter #(>= % 0) smaller-digits)) (first smallest-char))))

(defn get-last-digit [char-digits input-digits]
  (let [largest-char (or (last (sort-by second (filter #(some? (second %)) char-digits))) [0 0])
        smaller-digits (take-last (- (count input-digits) (second largest-char)) input-digits)
        char-smaller? (some? (some #(>= % 0) smaller-digits))]
    (if char-smaller? (last (filter #(>= % 0) smaller-digits)) (first largest-char))))

(defn parse-q2 [input]
  (let [first-digit (get-first-digit (get-first-char-digits digits input) (get-digits input))
        last-digit (get-last-digit (get-last-char-digits digits input) (get-digits input))]
    (Integer/parseInt (str first-digit last-digit))))

(println "q2: " (reduce + (map parse-q2 inputs)))

