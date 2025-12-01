(ns aoc2025.day01
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 1: Secret Entrance

(defn parse-input
  "Returns input as seq of numbers (negative for left, positive for right)."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map (fn[^String rot]
              (let [rot-dir (.charAt rot 0)
                    rot-num (parse-long (.substring rot 1))]
              (case rot-dir
                \L (- rot-num)
                \R rot-num))))))
                               

;; part 1

(defn part-1
  []
  (->> (parse-input)
       (reduce #(conj %1 (-> %1 peek (+ %2) (mod 100))) [50])
       (filter zero?)
       count))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
