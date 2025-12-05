(ns aoc2025.day05
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 5: Cafeteria

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
  "Returns if the given number is contained in any of the ranges."
  [ranges n]
  (some (fn[[low high]] (<= low n high)) ranges))

(defn part-1
  []
  (let [{:keys [ranges numbers]} (parse-input)]
    (->> numbers
         (filter #(in-ranges? ranges %))
         count)))

;; part 2

(defn add-range
  "Adds `next-range` to `ranges`, shortening or even omitting it if necessary
  to avoid overlapping."
  [ranges [next-low next-high :as next-range]]
  (let [[_ prev-high] (peek ranges)]
    (cond
      ; initial
      (nil? prev-high) [next-range]
      ; next range after prev range:
      (> next-low prev-high) (conj ranges next-range)
      ; partial overlap:
      (> next-high prev-high) (conj ranges [(inc prev-high) next-high])
      ; next range *inside* prev range:
      :otherwise ranges)))

(defn part-2
  []
  (let [{:keys [ranges]} (parse-input)]
    (->> ranges
         sort
         (reduce add-range [])
         (map (fn[[l h]] (-> h (- l) inc)))
         (reduce +))))

(comment
  (set! *warn-on-reflection* true)
)
