(ns aoc2025.day02
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 2: Gift Shop

(defn parse-input
  "Returns input as seq of ranges, with a range being the vec [start, end]."
  []
  (as->
    (util/get-input *ns*) $
    (str/trim $)
    (str/split $ #",")
    (map #(mapv parse-long (str/split % #"-")) $)))


;; part 1

(defn find-illegal-ids
  [regex [start end]]
  (->> (range start (inc end))
       (filter #(re-matches regex (str %)))))

(defn part-1
  []
  (->> (parse-input)
       (mapcat #(find-illegal-ids #"(.+)\1" %))
       (reduce +)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       (mapcat #(find-illegal-ids #"(.+)\1+" %))
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
