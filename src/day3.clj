(ns day3
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (-> "../data/day3.txt"
               slurp
               str/split-lines))

(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
           (lazy-seq (step))))))))
(defn get-items [line]
  (reduce conj '{} (filter #(true? (second %)) (keep-indexed vector (map #(and (not= % \.) (>= 0 (Character/digit % 10))) line)))))

(defn get-digits [line]
  (reduce conj '{} (filter #(not= -1 (second %)) (keep-indexed vector (map #(Character/digit % 10) line)))))


;; Start by converting to a sparse matrix
(def potential-numbers (map #(re-seq-pos #"([0-9]+)" %) input))
(def symbols (map #(re-seq-pos #"([^\.|\d])" %) input))

(defn get-rows-to-parse [number row]
  (let [start-row (max 0 (dec row))
        end-row (min (dec (count input)) (inc row))]
    (range start-row (inc end-row))))
(defn get-range-to-parse [number]
  {:start (max 0 (dec (:start number))) :end (min (count (first input)) (:end number))})

(defn is-part-num? [symbols row-range col-range]
  (let [potential-symbols (flatten (filter some? (map #(nth symbols %) row-range)))]
      (< 0 (count (filter #(<= (:start col-range) (:start %) (:end col-range)) potential-symbols)))))

(defn get-numbers-in-row [index row]
  (filter #(is-part-num? symbols (get-rows-to-parse % index) (get-range-to-parse %)) row))
(defn run [_] (do
                (println "q1: " (reduce #(+ %1 (Integer/parseInt %2)) 0 (map :group (flatten (map-indexed get-numbers-in-row potential-numbers)))))))
