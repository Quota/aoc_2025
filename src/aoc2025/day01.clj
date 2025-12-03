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

(defn count-clicks
  [{:keys [zeroes dial]} rot]
  (let [dial-rotated (+ dial rot)
        clicks (cond
                 ; turning "above 100" - every 100 is a click
                 (>= dial-rotated 100)
                 (quot dial-rotated 100)
                 ; staying between 1 and 99 -> no clicks
                 (> dial-rotated 0)
                 0
                 ; turning "at or below 0" is a bit more complicated:
                 ; - dial was pos? -> need to add 1 after dividing by 100
                 (pos? dial) (-> dial-rotated (-) (quot 100) inc)
                 ; - dial was 0? -> new-dial must be <= -100, and don't +1
                 (<= dial-rotated -100) (-> dial-rotated (-) (quot 100))
                 ; - otherwise -> again no clicks
                 :otherwise 0)]
    {:zeroes (+ zeroes clicks)
     :dial (mod dial-rotated 100)}))

(defn part-2
  []
  (->> (parse-input)
       (reduce count-clicks {:zeroes 0 :dial 50})))

(comment
  (set! *warn-on-reflection* true)
  )
