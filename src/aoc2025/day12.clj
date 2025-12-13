(ns aoc2025.day12
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 12: Christmas Tree Farm

(defn parse-shape
  "Returns nil."
  [lines]
  nil)

(defn parse-region
  "Returns the given region as map {:width n :length n :shapes [n ..]}."
  [line]
  (let [[w l & s#] (->> line (re-seq #"\d+") (map parse-long))]
    {:width w :length l :shapes (vec s#)}))

(defn parse-input
  "Returns input as map {:regions ({:width n :length n :shapes [n ..]} ..)}."
  []
  (let [data (->> (util/get-input *ns*)
                  #_(slurp "var/sample.txt")
                  (str/split-lines)
                  (partition-by empty?)
                  (take-nth 2)
                  )
        shapes (->> data
                    (drop-last)
                    (map parse-shape))
        regions (->> data
                     last
                     (map parse-region))]
    {:shapes shapes
     :regions regions}))

;; part 1

(defn check-region-sizes
  "Returns if the given region can fit all shapes assuming they all
  require a space of 3x3 spots."
  [{:keys [width length shapes]}]
  (let [region-size (* width length)]
    (->> shapes
         (reduce +)
         (* 9)
         (>= region-size))))

(defn part-1
  []
  (->> (parse-input)
       :regions
       (map check-region-sizes)
       (filter identity)
       count))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
