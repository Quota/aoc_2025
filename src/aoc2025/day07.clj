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

(defn build-split->tachyons
  "Returns a map from splitters to where the resulting tachyons continue.
  For example: (#{7} #{6 8} #{5 ..} ..) ; splitter columns
  results in:  {[0 7] #{[1 6] [1 8]}, ; splitter row/col to tachyon row/col
                [1 6] #{[2 5] ..}, ..}."
  [splitter-levels]
  (reduce-kv (fn [acc row splitter-line]
               (reduce (fn [acc splitter-col]
                         (assoc acc
                                [row splitter-col]
                                #{[(inc row) (dec splitter-col)]
                                  [(inc row) (inc splitter-col)]}))
                       acc
                       splitter-line))
             {}
             (vec splitter-levels)))

(def count-tachyon-paths
  "Returns the number of paths from the given row/col to the very bottom.
  Uses memoization for caching.
  Fn args are `[{:keys [split->tachyons height] :as teleporter} [r c :as rc]]`,
  with `split->tachyons` being a map {rc1 (rc2 ..) ..} and `rc` being the
  row/col to start counting from."
  (memoize
    (fn [{:keys [split->tachyons height] :as teleporter} [r c :as rc]]
      (if (= r height)
        1
        (->> (split->tachyons rc #{[(inc r) c]})
             (map #(count-tachyon-paths teleporter %))
             (reduce +))))))

(defn part-2
  []
  (let [[start & splitter-levels] (parse-input)]
    (count-tachyon-paths
      {:split->tachyons (build-split->tachyons splitter-levels)
       :height (-> splitter-levels count)}
      [0 (first start)])))

(comment
  (set! *warn-on-reflection* true)
)
