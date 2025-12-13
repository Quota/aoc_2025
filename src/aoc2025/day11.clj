(ns aoc2025.day11
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 11: Reactor

(defn parse-line
  [line]
  (->> (str/split line #"[: ]+")
       (map keyword)
       ((juxt first next))))

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (into {} (map parse-line))))

(defn paths-to-all-nodes
  [tree start-node]
  (->> [start-node]
       (tree-seq (fn[node] (tree (peek node)))
                 (fn[node] (map #(conj node %) (tree (peek node)))))
       (filter #(= :out (peek %)))))


;; part 1

(defn part-1
  []
  (let [tree (parse-input)]
    (->> :you
         (paths-to-all-nodes tree)
         count)))

;; part 2

(defn part-2
  []
  (let [tree (parse-input)]
    (->> :svr
         (paths-to-all-nodes tree)
         count)))

(comment
  (set! *warn-on-reflection* true)
)
