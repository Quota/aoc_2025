(ns aoc2025.day10
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2025.util :as util]))

;;; day 10: Factory

(defn parse-line
  "Parses one machine-line into a map with :lights, :buttons and :jolts.
  Example: [.##.] (1) (0,2) .. {3,1,5}
  Output: {:lights #{1 2} :buttons [#{1} #{0 2} ..] :jolts [3 1 5]}"
  [line]
  (let [[[lights] buttons [jolts]]
        (->> (str/split line #" ")
             (partition-by first))]
    {:lights (->> (rest lights)
                  (keep-indexed #(if (= \# %2) %1))
                  set)
     :buttons (->> buttons
                   (mapv #(->> % (re-seq #"\d+") (map parse-long) set)))
     :jolts (->> jolts
                 (re-seq #"\d+")
                 (mapv parse-long))}))

(defn parse-input
  "Returns input as seq of machine maps like
  {:lights #{n ..} :buttons [#{n ..} ..] :jolts [n ..]"
  []
  (->> #_(util/get-input *ns*)
       "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
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

(defn update-jolts
  "Updates (i.e. increments) the jolts according to the button. Jolts
  is expected to be a vec of longs, buttons a set of indexes within jolts."
  [jolts button]
  (reduce #(update %1 %2 inc) jolts button))

;(defn fx
;  [jolts buttons jolts-prev path-prev]
;  (let [res (reduce (fn[{:keys [jolts-prev path] :as state} b]
;                      (let [jolts-next (update-jolts jolts-prev b)]
;                        (if (= jolts jolts-next)
;                          (reduced (-> state
;                                       (assoc :found true)
;                                       (update :path conj b)))
;                          state)))
;                    {:jolts-prev jolts-prev
;                     :path path-prev}
;                    buttons)]
;    (if (:found res)
;      res
;      (reduce #(fx jolts buttons (update-jolts jolts 


(defn bfs
  [visitor-fn children-fn & roots]
  (if (seq roots)
    (let [roots' (map visitor-fn roots)]
      (concat roots'
              (apply bfs visitor-fn children-fn (mapcat children-fn roots'))))))


(defn bfs-lazy [c-fn tree]
  ((fn step [queue]
     (lazy-seq
       (when (seq queue)
         (let [node (peek queue)
               children (c-fn node)]
           (cons node
                 (step (into (pop queue) children)))))))
   (conj clojure.lang.PersistentQueue/EMPTY tree)))

(comment

  (let [tree [1 [2 [4] [5]] [3 [6]]]]
    (->> (bfs identity rest tree)
         (map first)))

  (let [tree {:n 1 :c [{:n 2 :c [{:n 4} {:n 5}]} {:n 3 :c [{:n 6}]}]}]
    (->> (bfs identity :c tree)
         (map :n)))

  (let [tree {:n 1 :c [{:n 2 :c [{:n 4 :c [{:n 7}]}
                                 {:n 5}]}
                       {:n 3 :c [{:n 6 :c [{:n 8}]}]}]}]
    (->> (bfs (fn visit [n]
               #_{:x (* 10 (:n n)) :p (:p n)}
               (assoc n :x (* 10 (:n n))))
             (fn children [n]
               (map #(assoc % :p (conj (or (:p n) []) (:x n)))
                    (:c n)))
             tree)
         (map #(dissoc % :n :c))))

  (let [buttons [{:v 1} {:v 2} {:v 3}]]
    (->> (apply bfs (fn visit [n]
                      (assoc n :x (* 10 (:v n))))
                (fn children [n]
                  (if 
                    (and (< (count (:p n)) 1)
                         (< (or (:x n) 0) 35))
                    (map #(assoc % :p (conj (or (:p n) []) (:v n)))
                         buttons)))
                buttons)))

  (let [tree {1 [3 4], 2 [5 6], 3 [7], 5 [8 9]}]
    (bfs #(hash-map :n %1) #(tree (:n %)) 1 2))

  (let [tree {0 [1 2], 1 [3 4], 2 [5 6], 3 [7], 5 [8 9]}]
    (bfs-lazy tree 0))

  ; hier_weiter
  (take-while #(< % 1000) (bfs-lazy #(vector (* 10 %) (inc (* 10 %))) 1))

)

(defn configure-machine-jolts
  "Configure the joltages of the given machine by trying various button
  combination. Returns the number of buttons presses required to do so."
  [{:keys [buttons jolts]}]
  (fx jolts buttons (into [] (repeat (count jolts) 0)) []))

(defn part-2
  []
  (->> (parse-input)
       (map configure-machine-jolts)
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
