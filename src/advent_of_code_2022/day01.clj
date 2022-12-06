(ns advent-of-code-2022.day01
  (:require [clojure.java.io :as io]))



(defn read-elf-snack-calories 
  []
  (with-open [r (io/reader (io/resource "day01.txt"))]
    (transduce (map #(if (not= "" %) (Long/valueOf %)))
               (completing
                 (fn [[s :as sums] num]
                   (if (nil? num)
                     (cons 0 sums)
                     (cons (+ s num) (rest sums))))
                 (fn [sums]
                   (sort-by - sums)))
               (list 0)
               (line-seq r))))


(defn solve-both-parts
  []
  (let [[a b c] (read-elf-snack-calories)]
    (println "Part 1:" a)
    (println "Part 2:" (+ a b c))))

