(ns advent-of-code-2022.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def real-input (slurp (io/resource "day09.txt")))

(def test-input 
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def test-input2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (map #(let [[d m] (string/split % #"\s")]
               [(keyword d) (Long/valueOf m)]))))


(defn expand-instructions
  [input]
  (mapcat (fn [[dir mag]]
            (repeat mag dir))
          input))


(defn vec+
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])


(defn vec-
  [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])


(def dirs
  {:U [0 1]
   :D [0 -1]
   :R [1 0]
   :L [-1 0]})


(def init-state
  {:tail-history #{[0 0]}
   :head [0 0]
   :tail [0 0]})


(defn touching?
  [head tail]
  (contains? (into #{} 
                   (map (partial vec+ head))
                   (for [dx [-1 0 1]
                         dy [-1 0 1]]
                     [dx dy])) 
             tail))


(defn move-head
  [state dir]
  (update state :head vec+ (get dirs dir)))


(def relative-pos->move 
  {; straight moves
   [0 2] [0 1]
   [2 0] [1 0]
   [0 -2] [0 -1]
   [-2 0] [-1 0]
   ; diagonal moves
   [1 2] [1 1]
   [2 1] [1 1]
   [2 -1] [1 -1]
   [1 -2] [1 -1]
   [-1 -2] [-1 -1]
   [-2 -1] [-1 -1]
   [-2 1] [-1 1]
   [-1 2] [-1 1]})


(defn move-tail*
  [{:keys [head tail] :as state}]
  (let [next-tail (vec+ tail (relative-pos->move (vec- head tail)))]
    (-> state 
        (assoc :tail next-tail)
        (update :tail-history conj next-tail))))


(defn move-tail 
  [{:keys [head tail] :as state}]
  (if (touching? head tail)
    state
    (move-tail* state)))


(defn step
  [state dir]
  (-> state
      (move-head dir)
      (move-tail)))

#_(part1 test-input)
#_(part1 real-input)
(defn part1 
  [input]
  (->> (parse-input input)
       (expand-instructions)
       (reduce step init-state)
       :tail-history
       count))


(def init-state2
  (-> (zipmap (range 10)
              (repeat [0 0]))
      (assoc :tail-history #{[0 0]})))


(defn update-history
  [state]
  (update state :tail-history conj (get state 9)))


(def relative-pos->move2
  (assoc relative-pos->move
         [2 2] [1 1]
         [-2 2] [-1 1]
         [2 -2] [1 -1]
         [-2 -2] [-1 -1]))


(defn move-knots
  [state]
  (reduce (fn [state [head-id tail-id]]
            (let [h (get state head-id)
                  t (get state tail-id)]
              (if (touching? h t)
                state
                (assoc state tail-id (vec+ t (relative-pos->move2 (vec- h t)))))))
          state
          (partition 2 1 (range 10))))


(defn step2
  [state dir]
  (-> state
      (update 0 vec+ (get dirs dir))
      (move-knots)
      (update-history)))


#_(part2 test-input)
#_(part2 test-input2)
#_(part2 real-input)
(defn part2 
  [input]
  (->> (parse-input input)
       expand-instructions 
       (reduce step2 init-state2)
       :tail-history
       count))
