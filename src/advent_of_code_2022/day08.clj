(ns advent-of-code-2022.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input (slurp (io/resource "day08.txt")))

(def test-input
  "30373
25512
65332
33549
35390")


(defn parse-state
  [input]
  (mapv (fn [line]
          (mapv #(Long/valueOf (str %)) line))
        (string/split-lines input)))


(defn all-points
  [size]
  (for [y (range size)
        x (range size)]
    [x y]))


(defn edge?
  [size [x y]]
  (or (= 0 x)
      (= 0 y)
      (= (dec size) x)
      (= (dec size) y)))


(defn get-height-at
  [state [x y]]
  (get-in state [y x]))


(defn points-in-dir
  [[x y] size dir]
  (map (if (contains? #{:north :south} dir)
         #(vector x %)
         #(vector % y))
       (case dir
         :north (range (dec y) -1 -1)
         :east (range (inc x) size)
         :south (range (inc y) size)
         :west (range (dec x) -1 -1))))


(defn taller-than-every?
  [state size [x y] dir]
  (let [h (get-height-at state [x y])]
    (->> (points-in-dir [x y] size dir)
         (map #(get-height-at state %))
         (every? #(< % h)))))


(defn visible?
  [state size point]
  (let [taller? (partial taller-than-every? state size point)]
    (or (taller? :north)
        (taller? :east)
        (taller? :south)
        (taller? :west))))


#_(count-visible (parse-state test-input))
#_(count-visible (parse-state real-input))
(defn count-visible
  [state]
  (let [size (count state)]
    (transduce (map (fn [point]
                      (if (or (edge? size point)
                              (visible? state size point))
                        1
                        0)))
               +
               (all-points size))))


(defn take-until
  [pred [x & xs]]
  (lazy-seq
    (if (nil? x)
      (list)
      (cons x (if (pred x) (take-until pred xs) (list))))))


(defn count-until-taller
  [state curr-height points]
  (->> points
       (take-until #(< (get-height-at state %) curr-height))
       (count)))


(defn count-in-dir
  [state size p dir]
  (count-until-taller state 
                      (get-height-at state p)
                      (points-in-dir p size dir)))


(defn scenic-score-at
  [state size [x y :as p]]
  (reduce #(* %1 (count-in-dir state size p %2))
          1
          [:north :east :south :west]))


#_(prn (max-scenic-score (parse-state test-input)))
#_(prn (max-scenic-score (parse-state real-input)))
(defn max-scenic-score
  [state]
  (let [size (count state)]
    (transduce (map (partial scenic-score-at state size))
               max
               -1 
               (all-points size))))

