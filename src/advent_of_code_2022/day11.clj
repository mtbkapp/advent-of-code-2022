(ns advent-of-code-2022.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def real-input (slurp (io/resource "day11.txt")))

(def test-input 
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
  ")

#_(parse-op "  Operation: new = old + 3")
(defn parse-op
  [line]
  (let [[_ op b] (->> (string/split line #"=")
                      second
                      (re-seq #"(\w+)\s([\-\+\*\/])\s(\w+)")
                      first
                      rest)
        ops {"+" + "-" - "*" * "/" /}
        b-num (if (not= "old" b) (Long/valueOf b))
        f (get ops op)]
    (if (= "old" b)
      #(f % %)
      #(f % b-num))))


(defn parse-id
  [line]
  (Long/valueOf (-> (re-seq #"Monkey (\d+):" line) first second)))


(defn parse-items
  [line]
  (mapv (comp #(Long/valueOf %) first) 
        (re-seq #"(\d+)" line)))


(defn parse-eol-long
  [line]
  (Long/valueOf (-> (re-seq #"(\d+)$" line) ffirst)))


#_(parse-input test-input)
#_(parse-input real-input)
(defn parse-input
  [input]
  (reduce (fn [monkeys section]
            (let [[l0 l1 l2 l3 l4 l5] (string/split-lines section)]
              (assoc monkeys
                     (parse-id l0)
                     {:items (parse-items l1) 
                      :items-inspected 0
                      :op (parse-op l2)
                      :test-operand (parse-eol-long l3)
                      :true-dest (parse-eol-long l4)
                      :false-dest (parse-eol-long l5)})))
          {}
          (string/split input #"\n\n")))


#_(monkey-turn (parse-input test-input) 0)
(defn monkey-turn
  [level-tamer monkeys id]
  (let [{:keys [items op test-operand true-dest false-dest items-inspected] :as m} (get monkeys id)
        nm (assoc m 
                   :items []
                   :items-inspected (+ items-inspected (count items)))]
    (reduce (fn [monkeys item]
              (let [worry-level (level-tamer (op item))
                    dest (if (= 0 (mod worry-level test-operand))
                           true-dest 
                           false-dest)]
                (update-in monkeys [dest :items] conj worry-level)))
            (assoc monkeys id nm)
            items)))


#_(play-round (parse-input test-input))
(defn play-round
  [monkeys level-tamer]
  (reduce (partial monkey-turn level-tamer) 
          monkeys
          (range (count monkeys))))


(defn play-rounds 
  [monkeys level-tamer n]
  (if (= n 0)
    monkeys
    (recur (play-round monkeys level-tamer) 
           level-tamer
           (dec n))))


(defn monkey-biz
  [monkeys]
  (->> monkeys 
       (map (comp :items-inspected val))
       (sort-by -)
       (take 2)
       (apply *)))


#_(part1 test-input)
#_(part1 real-input)
(defn part1
  [input]
  (monkey-biz (play-rounds (parse-input input) 
                           #(quot % 3)
                           20)))


; use trick by [silentw0lf](https://github.com/silentw0lf/advent_of_code_2022/blob/main/11/solve.py)
; to keep worry level in check
#_(part2 test-input)
#_(part2 real-input)
(defn part2
  [input]
  (let [monkeys (parse-input input)
        product-of-mods (transduce (map (comp :test-operand val)) * monkeys)]
    (monkey-biz (play-rounds monkeys
                             #(mod % product-of-mods)
                             10000))))


