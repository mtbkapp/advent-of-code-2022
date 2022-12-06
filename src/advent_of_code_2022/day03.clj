(ns advent-of-code-2022.day03
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]))



(def test-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")


(defn common-item
  [bags]
  (->> (map set bags)
       (apply sets/intersection)
       (first)))


(defn split-bag
  [bag]
  (partition (/ (count bag) 2) bag))


(defn priority 
  [item]
  (- (int item) (if (Character/isUpperCase item) 38 96)))


#_(prn (part1 (slurp (io/resource "day03.txt"))))
#_(prn (part1 test-input))
(defn part1
  [input]
  (transduce (comp (map split-bag) 
                   (map common-item)
                   (map priority))
             +
             (string/split-lines input)))


#_(prn (part2 test-input))
#_(prn (part2 (slurp (io/resource "day03.txt"))))
(defn part2
  [input]
  (transduce (comp (map common-item)
                   (map priority))
             +
             (partition 3 (string/split-lines input))))

