(ns aoc2025.day05
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 5:

(defn parse-input
  "Returns input as map {:ranges <seq of [low high]> :numbers <seq...>}."
  []
  (let [{ranges 2 nums 1}
        (->> (util/get-input *ns*)
             (str/split-lines)
             (map #(->> % (re-seq #"\d+") (mapv parse-long)))
             (group-by count))]
    {:ranges ranges
     :numbers (map first nums)}))

;; part 1

(defn in-ranges?
  [ranges n]
  (some (fn[[low high]] (<= low n high)) ranges))

(defn part-1
  []
  (let [{:keys [ranges numbers]} (parse-input)]
    (->> numbers
         (filter #(in-ranges? ranges %))
         count)))


;; part 2

(defn part-2
  []
  (let [{:keys [ranges]} (parse-input)]
    (->> ranges
         ; todo: overlapping ranges...
         (map (fn[[low high]] (inc (- high low))))
         (reduce +))))

(comment
  (set! *warn-on-reflection* true)
)
