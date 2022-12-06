(ns advent-of-code-2022.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input (slurp (io/resource "day04.txt")))


(def test-input 
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")


(defn parse-input
  [input]
  (map (fn [line]
         (let [[[_ a b c d]] [] ]
           [[a b] [c d]]
           (->> (re-seq #"(\d+)-(\d+),(\d+)-(\d+)" line)
               first
               rest
               (map #(Long/valueOf %))
               (partition 2))))
       (string/split-lines input)))


(defn fully-contains?
  [[[a b] [c d]]]
  (or (and (<= c a d)
           (<= c b d))
      (and (<= a c b)
           (<= a d b))))


(defn count-if
  [input pred]
  (->> (parse-input input)
       (filter pred)
       (count)))


#_(part1 test-input)
#_(part1 (slurp (io/resource "day04.txt")))
(defn part1
  [input]
  (count-if input fully-contains?))


(defn overlaps?
  [[[a b] [c d]]]
  (or (<= c a d)
      (<= c b d)
      (<= a c b)
      (<= a d b)))


#_(part2 test-input)
#_(part2 (slurp (io/resource "day04.txt")))
(defn part2
  [input]
  (count-if input overlaps?))

