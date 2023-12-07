(ns day5
  (:require [clojure.string :as str]))

(def input (-> "../data/day5.txt"
               slurp
               (str/split #"\n\n")))

(def seeds (map #(Long/parseLong %) (drop 1 (str/split (first input) #" "))))

(defn to-longs [items]
  (map #(Long/parseLong (str/trim (first %))) items))

(defn to-metadata [items]
  {:source (nth (first items) 1)
   :dest (nth (first items) 2)})

(def mappings (->> (rest input)
                   (map #(re-seq #"\s*([0-9]+)\s*" %))
                   (map to-longs)
                   (map #(partition 3 %))))
(def map-meta (->> (rest input)
                   (map #(vec (re-seq #"(\w+)-to-(\w+)" %)))
                   (map to-metadata)))

(def data (zipmap map-meta mappings))
(defn get-next-mapping [source data]
  (filter #(= source (:source (first %))) data))

(defn map-seed [seed mapping]
  (reduce #(cond
             (<= (second %2) seed (+ (dec (second %2)) (nth %2 2))) (+ (first %2) (- seed (second %2)))
             :else  %1) seed mapping))
(defn process-seed [data seed]
  (loop [current-mapping (first (get-next-mapping "seed" data)) current-seed seed]
    (if (empty? current-mapping)
      current-seed
      (recur (first (get-next-mapping (:dest (first current-mapping)) data)) (map-seed current-seed (second current-mapping))))))

(defn split-seeds [map-range seeds-list]
  (loop [seeds (first seeds-list) remainder (rest seeds-list) processed (list)]
    (cond
      (<= (second map-range)
          (first seeds)
          (+ (first seeds) (second seeds))
          (+ (second map-range) (nth map-range 2)))
      (- (+ (first map-range) (first seeds)) (second map-range)) ;; map range contains seeds
      (<= (first seeds)
          (second map-range)
          (+ (first seeds) (second seeds))
          (+ (second map-range) (nth map-range 2)))
      (list ()))))
(defn map-seed-range [data seeds]
  (do
    (println seeds (second data)) seeds)
  (loop [seeds-list (list seeds) current-range (first (second data)) remainder (rest (second data))]
    (if (empty? remainder)
      seeds-list
      (recur (split-seeds current-range seeds-list) (first remainder) (rest remainder)))))
(defn range-process-seed [data seeds]
       (loop [seeds-list seeds current-mapping (first (get-next-mapping "seed" data))]
          (if (empty? current-mapping)
            seeds-list
            (recur (map-seed-range current-mapping seeds-list) (first (get-next-mapping (:dest (first current-mapping)) data))))))

;; TODO Flesh out Q2

(defn run [& _]
  (do
    (println "q1: "  (apply min (map #(process-seed data %) seeds)))))


(run 1)
