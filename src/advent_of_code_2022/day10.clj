(ns advent-of-code-2022.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input (slurp (io/resource "day10.txt")))

(def tiny-test-input "noop
addx 3
addx -5")

(def test-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

#_(parse-input test-input)
#_(count (parse-input real-input))
(defn parse-input 
  [input]
  (map (fn [line]
         (let [[op arg] (string/split line #"\s")]
           (cond-> [(keyword op)]
             (= op "addx") (conj (Long/valueOf arg)))))
       (string/split-lines input)))

#_(sim-x-history (parse-input test-input))
(defn sim-x-history 
  [input]
  (loop [[[op arg :as i] & is] input 
         x 1
         history [1]]
    (if (some? i)
      (case op
        :noop (recur is x (conj history x))
        :addx (let [nx (+ x arg)] 
                (recur is (+ x arg) (into history [x nx]))))
      history)))

#_(part1 test-input)
#_(part1 real-input)
(defn part1
  [input]
  (let [history (sim-x-history (parse-input input))]
    (reduce (fn [sum n]
              (+ sum (* n (nth history (dec n)))))
            0
            [20 60 100 140 180 220])))


; part 2
;
; X is the horizontal position of the center of a 3 pixel sprite
; screen is (w,h) = (40,6)
; left pixel = 0
; 

