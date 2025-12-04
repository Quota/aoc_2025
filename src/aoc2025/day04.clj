(ns aoc2025.day04
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 4: Printing Department

(defn parse-input
  "Returns input as vec of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       vec))

;; part 1

(defn roll-at?
  "Returns if the input at given row and column is a 'roll' (i.e. @ sign)."
  [input r c]
  (= \@ (get-in input [r c])))

(defn get-adjacent-rolls
  "Returns seq of [r c] pairs, the adjacent rolls of the given location."
  [input r c]
  (for [r' (range (dec r) (+ r 2))
        c' (range (dec c) (+ c 2))
        :when (and
                (or (not= r r') (not= c c'))
                (roll-at? input r' c'))]
    [r' c']))

(defn part-1
  []
  (let [input (parse-input)
        height (count input)
        width (count (first input))
        rolls (set (for [c (range width)
                         r (range height)
                         :when (roll-at? input r c)
                         :let [ars (get-adjacent-rolls input r c)]
                         :when (-> ars count (< 4))]
                     [r c]))]
    (util/plot-maze [height width] #(if (rolls %) \x (get-in input %))
                    :out "var/map-5.txt")
    (count rolls)))


;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
