(ns aoc2025.day09
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2025.util :as util])
  (:import 
    [java.awt Polygon Rectangle]))

;;; day 9: Movie Theater

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(->> % (re-seq #"\d+") (mapv parse-long)))))

;; part 1

(defn area
  [[px py] [qx qy]]
  (* (inc (Math/abs (- px qx)))
     (inc (Math/abs (- py qy)))))

(defn part-1
  []
  (let [input (parse-input)]
    (->>
      (combo/combinations (parse-input) 2)
      (map #(cons (apply area %) %))
      (sort-by first >)
      first)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
