(ns aoc2025.day04
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 4: Printing Department

(defn parse-input
  "Returns input as set of [r c] of paper rolls (i.e. @ chars)."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (util/parse-area {:ignore-chars #{\.}})
       keys
       set))

(defn get-adjacent
  "Returns rolls adjacent to the given location, as set of [r c] pairs."
  [rolls [r c]]
  (->> ; construct locations to check...
       (for [r' (range (dec r) (+ r 2))
             c' (range (dec c) (+ c 2))
             :when (or (not= r r') (not= c c'))]
         [r' c'])
       ; ...and check them within `rolls`
       (filter rolls)))

(defn find-accessable-rolls
  "Returns all accessable rolls in `rolls`, as a seq of [r c] pairs, or
  nil if no such rolls exist.
  A roll is accessable if it has less than 4 adjacent rolls."
  [rolls]
  (->> rolls
       (filter (fn[rc] (-> rolls (get-adjacent rc) count (< 4))))
       seq))

;; part 1

(defn part-1
  []
  (-> (parse-input)
      (find-accessable-rolls)
      count))

;; part 2

(defn remove-accessable-rolls
  "From the given `rolls` set removes all accessable rolls and returns the
  resulting rolls set as well as the number of removed rolls, as map
  {:rolls <remaining rolls> :removed <num of removed rolls>}, or nil if
  there are no accessable rolls left in `rolls` (i.e nothing to remove)."
  [{:keys [rolls]}]
  (if-let [acc-rolls (find-accessable-rolls rolls)]
    {:rolls (->> rolls (remove (set acc-rolls)) set)
     :removed (count acc-rolls)}))

(defn part-2
  []
  (->> {:rolls (parse-input) :removed 0}
       (iterate remove-accessable-rolls)
       (take-while identity)
       (map :removed)
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
