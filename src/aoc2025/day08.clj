(ns aoc2025.day08
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2025.util :as util]))

;;; day 8: Playground

(defn parse-input
  "Returns input as seq of all junction box coordinates as [x y z]."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(->> % (re-seq #"\d+") (mapv parse-long)))))

;; part 1

(defn dist
  "Returns the distance between the two coordinates."
  [[x y z] [x' y' z']]
  (let [dx (- x' x) dy (- y' y) dz (- z' z)]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defn find-circut-for
  "Returns the set within `circuts` that contains `p`, otherwise nil."
  [circuts p]
  (first (filter #(% p) circuts)))

(defn wire-up
  "Returns the given `circuts` (set of sets) with the two junction boxes 
  (vec with [x y z]) added, either as new circut set or into an existing
  circut set (possibly joining two existing circut sets into one)."
  [circuts [junc-box-0 junc-box-1]]
  (let [jb0-circut (find-circut-for circuts junc-box-0)
        jb1-circut (find-circut-for circuts junc-box-1)]
    (cond 
      ; no such circut so far -> create new
      (nil? (or jb0-circut jb1-circut))
      (conj circuts #{junc-box-0 junc-box-1})
      ; junc-box-0 and junc-box-1 already in the same circut -> noting to do
      (= jb0-circut jb1-circut)
      circuts
      ; at least one junction box is already in a circut -> update
      :else
      (let [new-circut (-> (set/union jb0-circut jb1-circut)
                           (conj junc-box-0 junc-box-1))]
        (-> circuts
            (disj jb0-circut jb1-circut)
            (conj new-circut))))))

(defn part-1
  []
  (time
    (->> (combo/combinations (parse-input) 2)
         (sort-by (partial apply dist))
         (take 1000)
         (reduce wire-up #{})
         (map count)
         (sort >)
         (take 3)
         (reduce *))))

;; part 2

(defn part-2
  []
  (time
    (let [input (parse-input)
          input-size (count input)]
      (->> (combo/combinations input 2)
           (sort-by (partial apply dist))
           (reduce (fn[circuts two-junc-boxes]
                     (let [circuts (wire-up circuts two-junc-boxes)]
                       ; when "any" circut contains all junction boxes it must
                       ; be the only one and we're finished
                       (if (= (count (first circuts)) input-size)
                         (reduced two-junc-boxes)
                         circuts)))
                   #{})
           (map first)
           (reduce *)))))

(comment
  (set! *warn-on-reflection* true)
)
