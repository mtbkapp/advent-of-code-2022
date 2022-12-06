(ns advent-of-code-2022.day02
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]))


(defn read-data
  []
  (with-open [rdr (io/reader (io/resource "day02.txt"))]
    (mapv #(map keyword (string/split % #"\s"))
          (line-seq rdr))))


(def first-col
  {:A :rock
   :B :paper
   :C :scissors})


(def second-col
  {:X :rock
   :Y :paper
   :Z :scissors})


(def beats
  {:rock :scissors
   :scissors :paper
   :paper :rock})


(def loses
  (sets/map-invert beats))


(def object-scores
  {:rock 1
   :paper 2
   :scissors 3})


(defn score 
  [p1 p2]
  (cond (= p2 (beats p1)) [(+ 6 (object-scores p1)) (object-scores p2)]
        (= p1 (beats p2)) [(object-scores p1) (+ 6 (object-scores p2))]
        :else [(+ 3 (object-scores p1))
               (+ 3 (object-scores p2))]))


(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])


#_(prn (play-guide (read-data)))
#_(prn (play-guide [[:A :Y] [:B :X] [:C :Z]]))
(defn play-guide
  [input]
  (transduce (map (fn [[c1 c2]]
                    [(get first-col c1)
                     (get second-col c2)])) 
             (completing
               (fn [acc [p1 p2]]
                 (vec+ acc (score p1 p2))))
             [0 0]
             input))


(def second-col2
  {:X :lose
   :Y :draw
   :Z :win})


(defn choose 
  [opponent strategy]
  (case strategy
    :lose (get beats opponent)
    :draw opponent
    :win (get loses opponent)))

#_(prn (play-guide2 (read-data)))
#_(prn (play-guide2 [[:A :Y] [:B :X] [:C :Z]]))
(defn play-guide2
  [input]
  (transduce (map (fn [[c1 c2]]
                    [(first-col c1)
                     (second-col2 c2)]))
             (completing
               (fn [acc [opponent strategy]]
                 (vec+ acc (score opponent (choose opponent strategy)))))
             [0 0]
             input))


