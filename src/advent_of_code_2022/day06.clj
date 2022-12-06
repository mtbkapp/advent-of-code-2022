(ns advent-of-code-2022.day06
  (:require [clojure.java.io :as io]))

(def real-input (slurp (io/resource "day06.txt")))

(defn find-distinct-seq-of-size 
  ([input size]
   (find-distinct-seq-of-size (partition size 1 input) size size))
  ([[sub & ss] n size]
   (cond (nil? sub) -1
         (= size (count (distinct sub))) n
         :else (recur ss (inc n) size))))


#_(prn (find-distinct-seq-of-size real-input 4))
#_(prn (find-distinct-seq-of-size real-input 14))
