(ns advent-of-code-2022.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def real-input (slurp (io/resource "day07.txt")))

(def test-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")


(defn parse-line
  [line]
  (if (= \$ (first line))
    (let [[cmd arg] (string/split (subs line 2) #"\s")]
      [:cmd {:kind (keyword cmd) :arg arg}])
    (let [[x y] (string/split line #"\s")]
      (if (= "dir" x)
        [:ls {:kind :dir :name y}]
        [:ls {:kind :file :size (Long/valueOf x) :name y}]))))


(defn process-cmd
  [state {:keys [kind arg]}]
  (if (= :cd kind)
    (if (= ".." arg)
      (update state :curr-path pop)
      (update state :curr-path conj arg))
    state))


(defn update-dirs-up-tree
  [{:keys [curr-path] :as state} size]
  (->> (iterate pop curr-path)
       (take (count curr-path))
       (reduce (fn [state p]
                 (update-in state [:dirs p] + size))
               state)))


(defn process-file 
  [{:keys [curr-path] :as state} {:keys [kind name size]}]
  (if (= :dir kind)
    (update state :dirs assoc (conj curr-path name) 0)
    (update-dirs-up-tree state size)))


(defn process-cli-history
  [input final]
  (transduce (map parse-line)
             (completing
               (fn [state [line-type line]]
                 ((if (= :cmd line-type) process-cmd process-file) state line))
               final)
             {:curr-path [] :dirs {["/"] 0}}
             (string/split-lines input)))


(defn final-part1
  [{:keys [dirs]}]
  (->> (vals dirs)
       (filter #(<= % 100000))
       (apply +)))


(defn final-part2
  [{:keys [dirs]}]
  (let [used (get dirs ["/"])
        unused (- 70000000 used)
        min-req (- 30000000 unused)]
    (some (fn [n]
            (if (<= min-req n)
              n))
          (sort (vals dirs)))))


#_(prn (process-cli-history test-input final-part1))
#_(prn (process-cli-history real-input final-part1))
#_(prn (process-cli-history test-input final-part2))
#_(prn (process-cli-history real-input final-part2))

