(ns day10
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (-> "../data/day10.txt"
               slurp
               str/split-lines))
(def pipes (hash-map
             \| (list {:x 0 :y -1} {:x 0 :y 1})
             \- (list {:x -1 :y 0} {:x 1 :y 0})
             \L (list {:x 0 :y 1} {:x -1 :y 0})
             \J  (list {:x 0 :y 1} {:x 1 :y 0})
             \7  (list {:x 1 :y 0} {:x 0 :y -1})
             \F  (list {:x 0 :y -1} {:x -1 :y 0})
             \S (list {:x 0 :y 0})))

(def directions (hash-map
                  :top {:x 0 :y -1}
                  :bottom {:x 0 :y 1}
                  :left {:x -1 :y 0}
                  :right {:x 1 :y 0}))
(defn has-start? [row]
  (reduce #(or %1 (= %2 \S)) false row))

(def start-row (ffirst (filter #(true? (second %)) (map-indexed #(list %1 (has-start? %2)) input))))
(def start-col (str/index-of (nth input start-row) '"S"))
(defn first-connects [top bottom left right]
  (cond
    (contains? (hash-map \| 1 \F 1 \7 1) top) :top
    (contains? (hash-map \J 1 \| 1 \L 1) bottom) :bottom
    (contains? (hash-map \F 1 \L 1 \- 1) left) :left
    (contains? (hash-map \- 1 \J 1 \7 1) right) :right
    :else nil))
(defn find-start-connection [input start-row start-col]
  (let [top (get (get input (max 0 (dec start-row))) start-col)
        bottom (get (get input (min (count (first input)) (inc start-row))) start-col)
        left (get (get input start-row) (max 0 (dec start-col)))
        right (get (get input start-row) (min (count input) (inc start-col)))]
    (first-connects top bottom left right)))
(defn traverse [pipe direction]
  (let [exit (first (filter #(not= direction %) (get pipes pipe)))]
    {:x (- (:x exit))
     :y (- (:y exit))}))

(defn pipe-reducer [filtered next]
  (cond
    (= \7 (nth next 2)) (let [trimmed (drop-while #(= \- (nth % 2)) filtered)]
                          (if (= \L (nth (first trimmed) 2)) trimmed (rest trimmed)))
    (= \J (nth next 2)) (let [trimmed (drop-while #(= \- (nth % 2)) filtered)]
                          (if (= \F (nth (first trimmed) 2)) trimmed (rest trimmed)))
    :else (conj filtered next)))

(defn ray-cast-inside [pipes point]
  (if (or (= \S (nth point 2)) (> (count (filter #(= % point) pipes)) 0))
    false
    (let [line (reduce pipe-reducer (list) (sort-by first (filter #(and (= (second %) (second point)) (< (first %) (first point))) pipes)))
          other-side (reduce pipe-reducer (list) (sort-by first (filter #(and (= (second %) (second point)) (> (first %) (first point))) pipes)))]
      (and (not= 0 (count other-side)) (odd? (count line))))))


(defn q1-traverse-pipe [inputs start-row start-col]
  (let [start-direction (find-start-connection inputs start-row start-col)
        direction-values (get directions start-direction)
        first-step (list (+ (:x direction-values) start-col) (+ (:y direction-values) start-row))]
    (loop [current-x (first first-step) current-y (second first-step) steps 1 current-direction direction-values]
      (let [current-pipe (get (get inputs current-y) current-x)
            next-direction (traverse current-pipe current-direction)
            next-x (+ current-x (:x next-direction))
            next-y (+ current-y (:y next-direction))]
        (if (= \S current-pipe)
          steps
          (recur next-x next-y (inc steps) next-direction))))))

(defn traverse-pipe [inputs start-row start-col]
  (let [start-direction (find-start-connection inputs start-row start-col)
        direction-values (get directions start-direction)
        first-step (list (+ (:x direction-values) start-col) (+ (:y direction-values) start-row))]
    (loop [current-x (first first-step) current-y (second first-step) tiles (list) current-direction direction-values]
      (let [current-pipe (get (get inputs current-y) current-x)
            next-direction (traverse current-pipe current-direction)
            next-x (+ current-x (:x next-direction))
            next-y (+ current-y (:y next-direction))]
        (if (= \S current-pipe)
          tiles
         (recur next-x next-y (conj tiles (list current-x current-y current-pipe)) next-direction))))))
(defn map-rows [index input]
  (map-indexed #(list %1 index %2) input))
(defn run [& _] (do
                  (println "q1: " (/ (q1-traverse-pipe input start-row start-col) 2))
                  (let [c (reduce concat (map-indexed map-rows input))
                        pipes (traverse-pipe input start-row start-col)
                        enclosed (filter #(ray-cast-inside pipes %) c)]
                    (do
                                        (println "q2: " (count enclosed))))))



(run 1)
