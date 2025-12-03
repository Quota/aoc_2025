(ns aoc2025.day03
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 3: Lobby

(defn parse-input
  "Returns input as seq of vec's of longs."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(mapv Character/getNumericValue %))))

(defn max-idx
  "Returns the first max value in the given vec and its index, as `[idx val]`."
  [v]
  (reduce-kv (fn[[mx mv :as res] ; max idx and max value so far
                 ci cv] ; current index and current value
               (cond
                 ; shortcut: can't get higher than 9
                 (= 9 cv) (reduced [ci cv])
                 (> cv mv) [ci cv]
                 :else res))
             [-1 -1] ; initial max idx and value: both -1
             v))

(defn get-largest-joltage
  "Returns the `n` digits in `v` which represent the largest number, as long."
  [n v]
  (loop [res 0 ; collecting the result
         n n ; counting down to zero
         v v] ; the (rest) vector to scan
    (if (zero? n)
      res
      (let [[idx max-val] (max-idx (subvec v 0 (- (count v) (dec n))))]
        (recur (+ (* 10 res) max-val)
               (dec n)
               (subvec v (inc idx)))))))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       (map #(get-largest-joltage 2 %))
       (reduce +)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       (map #(get-largest-joltage 12 %))
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
