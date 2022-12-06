(ns advent-of-code-2022.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def real-input 
  (slurp (io/resource "day05.txt")))


(def test-input 
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-crate-line 
  [stack-indexes line]
  (transduce (map-indexed vector)
             (completing 
               (fn [stacks [stack-id str-i]]
                 (let [c (nth line str-i)]
                   (if (= \space c)
                     stacks
                     (conj stacks {:stack-index stack-id 
                                   :crate (keyword (str c))})))))
             []
             stack-indexes))


(defn init-stack-reducer
  [stacks crates]
  (reduce (fn [stacks {:keys [stack-index crate]}]
            (update stacks stack-index conj crate))
          stacks
          crates))


#_(clojure.pprint/pprint (parse-input test-input))
(defn parse-stacks
  [s]
  (let [lines (string/split-lines s)
        stack-count (-> (last lines)
                        (string/trim)
                        (string/split #"\s+")
                        (last)
                        (Long/valueOf))
        stack-indexes (take stack-count (iterate #(+ % 4) 1))]
    (transduce (map (partial parse-crate-line stack-indexes))
               (completing init-stack-reducer)
               (vec (repeat stack-count []))
               (rest (reverse lines)))))


(defn parse-script 
  [s]
  (map (fn [line]
         (let [[matches] (re-seq #"move\s(\d+)\sfrom\s(\d+)\sto\s(\d+)" line)
               [c f t] (map #(Long/parseLong %) (rest matches))]
           {:count c
            :from f 
            :to t}))
       (string/split-lines s)))


(defn parse-input 
  [input]
  (let [[stacks script] (string/split input #"\n\n")]
    {:stacks (parse-stacks stacks)
     :script (parse-script script)}))

(defn single-crate-exec 
  [stacks c f t]
  (if (zero? c)
    stacks
    (recur (-> stacks
               (update f pop)
               (update t conj (peek (nth stacks f))))
           (dec c)
           f
           t)))


(defn run-script
  ([{:keys [stacks script]} exec]
   (run-script stacks script exec))
  ([stacks [{c :count f :from t :to :as i} & is] exec]
   (if (some? i)
     (recur (exec stacks c (dec f) (dec t)) is exec)
     stacks)))


(defn get-msg
  [stacks]
  (apply str (map (comp name peek) stacks)))

#_(prn (part1 test-input))
#_(prn (part1 real-input))
(defn part1 
  [input]
  (-> input
      parse-input
      (run-script single-crate-exec) 
      get-msg))


#_(multi-crate-exec [[:A :B :X :Y :C :D] []] 3 0 1)
(defn multi-crate-exec
  [stacks c f t]
  (let [from-stack (get stacks f)
        split-index (- (count from-stack) c)
        to-move (subvec from-stack split-index)]
    (-> stacks
        (assoc f (subvec from-stack 0 split-index))
        (update t into to-move))))


#_(prn (part2 test-input))
#_(prn (part2 real-input))
(defn part2 
  [input]
  (-> input
      parse-input
      (run-script multi-crate-exec) 
      get-msg))
