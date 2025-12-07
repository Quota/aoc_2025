(ns aoc2025.day07
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 7: Laboratories

(defn parse-input
  "Returns input as seq of sets, the first of which marks the starting index,
  follow by the splitter indexes for every level.
  Example output: (#{7}, #{7} #{6 8} ...)"
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (remove (partial re-matches #"\.+"))
       (map #(keep-indexed (fn[idx ch] (if (#{\S \^} ch) idx)) %))
       (map set)))

;; part 1

(defn split-tachyons
  "Splits all `curr-tachyons` regarding `splitters` and returns the resulting
  tachyon indexes as well as the updated split-count."
  [{:keys [curr-tachyons split-count]} splitters]
  (reduce (fn[acc tach]
            (if (splitters tach)
              (-> acc
                  (update :curr-tachyons conj (dec tach) (inc tach))
                  (update :split-count inc))
              (-> acc
                  (update :curr-tachyons conj tach))))
          {:curr-tachyons #{} :split-count split-count}
          curr-tachyons))

(defn part-1
  []
  (let [[start & splitter-levels] (parse-input)]
    (->> splitter-levels
         (reduce split-tachyons {:curr-tachyons start :split-count 0})
         :split-count)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
