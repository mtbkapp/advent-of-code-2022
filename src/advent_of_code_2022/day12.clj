(ns advent-of-code-2022.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [ubergraph.alg :as au]
            [ubergraph.core :as u]))


(def test-input 
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(def real-input (slurp (io/resource "day12.txt")))


(defn all-points 
  [vs]
  (for [y (range (count vs))
        x (range (count (first vs)))]
    [x y]))


(defn cell-at
  [vs [x y]]
  (get-in vs [y x]))


(defn read-input 
  [input]
  (mapv (fn [line]
          (mapv (fn [c]
                  (cond (= \S c) :start
                        (= \E c) :end
                        :else (int c)))
                line)) 
        (string/split-lines input)))


(defn find-ends
  [vs]
  (->> (all-points vs) 
       (map (juxt (partial cell-at vs) identity))
       (filter (fn [[c [x y]]]
                 (or (= :start c) (= :end c))))
       (into {})))


(defn replace-ends
  [vs {[sx sy] :start [ex ey] :end}]
  (-> vs
      (assoc-in [sy sx] (int \a))
      (assoc-in [ey ex] (int \z))))


(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])


(defn valid-neighbors
  [vs [x y :as p]]
  (let [w (count (first vs))
        h (count vs)
        curr-elevation (cell-at vs p)]
    (->> [[0 -1] [0 1] [-1 0] [1 0]]
         (map (partial vec+ p))
         (filter (fn [[x y]]
                   (and (< -1 x w)
                        (< -1 y h))))
         (filter (fn [np]
                   (let [neighbor-elevation (cell-at vs np)]
                     (or (= (inc curr-elevation) neighbor-elevation)
                         (<= neighbor-elevation curr-elevation))))))))


(defn build-graph
  [vs]
  (reduce (fn [g p]
            (let [neighbors (valid-neighbors vs p)]
              (u/add-directed-edges* g (map (partial vector p) neighbors))))
          (u/digraph)
          (all-points vs)))


(defn steps-in-shortest-path
  [g start end]
  (-> (au/shortest-path g start end)
      (au/edges-in-path)
      (count)))


#_(part1 test-input)
#_(part1 real-input)
(defn part1 
  [input]
  (let [vs (read-input input)
        {:keys [start end] :as ends} (find-ends vs) 
        nvs (replace-ends vs ends)
        g (build-graph nvs)]
    (steps-in-shortest-path g start end)))


(defn find-all-starts
  [nvs]
  (filter #(= (int \a) (cell-at nvs %)) 
          (all-points nvs)))


#_(part2 test-input)
#_(part2 real-input)
(defn part2
  [input]
  (let [vs (read-input input)
        {:keys [start end] :as ends} (find-ends vs) 
        nvs (replace-ends vs ends)
        g (build-graph nvs)]
    (->> (find-all-starts nvs)
         (map #(steps-in-shortest-path g % end))
         (remove #(= 0 %))
         (apply min))))
