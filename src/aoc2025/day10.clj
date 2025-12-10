(ns aoc2025.day10
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2025.util :as util]))

;;; day 10: Factory

(defn parse-line
  "Parses one machine-line into a map with :lights and :buttons.
  Example: [.##.] (1) (0,2) .. {<ignored>}
  Output: {:lights #{1 2} :buttons [#{1} #{0 2} ..]}"
  [line]
  (let [[[lights] buttons [jolts]]
        (->> (str/split line #" ")
             (partition-by first))]
    {:lights (->> (rest lights)
                  (keep-indexed #(if (= \# %2) %1))
                  set)
     :buttons (->> buttons
                   (mapv #(->> % (re-seq #"\d+") (map parse-long) set)))}))

(defn parse-input
  "Returns input as seq of machine maps like
  {:lights #{n ..} :buttons [#{n ..} ..]}"
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map parse-line)))

;; part 1

(defn update-lights
  "Updates (i.e. toggles) the lights according to the button. Both lights
  and buttons are expected to be sets."
  [lights button]
  (reduce #((if (%1 %2) disj conj) %1 %2) lights button))

(defn turn-machine-lights-on
  "Turns the lights of the given machine on by trying various button
  combination. Returns the number of different buttons required to do so."
  [{:keys [lights buttons]}]
  (loop [num-btns 1]
    ; if pressing `num-btns` different buttons yield expected `lights`
    ; return `num-btns`, otherwise increment that number and try again
    (if (->> (combo/combinations buttons num-btns)
             (map #(reduce update-lights #{} %))
             (some #{lights})
             seq)
      num-btns
      (recur (inc num-btns)))))

(defn part-1
  []
  (->> (parse-input)
       (map turn-machine-lights-on)
       (reduce +)))


;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))

(comment
  (set! *warn-on-reflection* true)
)
