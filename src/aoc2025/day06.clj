(ns aoc2025.day06
  (:require
    [clojure.string :as str]
    [aoc2025.util :as util]))

;;; day 6: Trash Compactor

(defn parse-input
  "Returns input simply as seq of lines. Parts 1 and 2 have a different
  interpretation of what the input means..."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn calc-term
  "Calculates the given term.
  Output: (reduce op args)"
  [{:keys [op args]}]
  (reduce op args))

;; part 1

(defn parse-num-or-op
  "Returns the sum or product function or a long if input is \"+\", \"*\"
  or something else (hopefully a number), respectively."
  [x]
  (case x "+" +, "*" *, (parse-long x)))

(defn interpret-input-wrong
  "Interprets the input according to part 1, numbers are read horizontally.
  Input: (\"123 6 ...\" \"45 78 ...\" ... \"+ * + ...\")
  Output: ({:op + :args [123 45 ...]} {:op * :args [6 78 ...]} ...)"
  [lines]
  (->> lines
       ; parse nums/ops, result: ((123 6 ..) (45 78 ..) (+ * ..))
       (map #(->> % (re-seq #"[\d+*]+") (map parse-num-or-op)))
       ; transpose, result: ([123 45 .. +] [6 78 .. *] ..)
       (apply map vector)
       ; transform into result map
       (map (fn[term] {:op (peek term) :args (pop term)}))))

(defn part-1
  []
  (->> (parse-input)
       (interpret-input-wrong)
       (map calc-term)
       (reduce +)))

;; part 2

(defn parse-term
  "Parses one math-problem of part 2, with the input numbers consisting of
  single digits.
  Input: ((\1 \2 \+) (\3 \4 \5) ..)
  Output: {:op + :args (12 345 ..)}"
  [term]
  (let [digits-&-op (first term)]
    {:op (-> digits-&-op last (case \+ + \* *))
     :args (->> ; "remove" operator from first item
                (cons (drop-last digits-&-op) (rest term))
                ; convert digits to longs
                (mapv #(->> % (apply str) parse-long)))}))

(defn interpret-input-right
  "Interprets the input according to part 2, numbers are read vertically.
  Input: (\"123 ..\" \" 45 ..\" \"  6 ..\" \"+   ..\")
  Output: ({:op + :args (1 24 356)}, ..)"
  [lines]
  (->> lines
       ; transpose string columns: ( (\1 \_ \_ \+) (\2 \4 \_ \_) .. )
       (apply map list)
       ; remove spaces: ((\1 \+) (\2 \4) (\3 \5 \6) () ..)
       (map #(remove #{\ } %))
       ; "group" paragraphs and remove empty lists/lines:
       ; ( ((\1 \+) (\2 \4) ..) (()) ((ch .. o) (ch ch ..) .. ) ..)
       (partition-by empty?)
       (take-nth 2)
       ; parse individual math problems: ({:op op :args (n ..)} ..)
       (map parse-term)))

(defn part-2
  []
  (->> (parse-input)
       (interpret-input-right)
       (map calc-term)
       (reduce +)))

(comment
  (set! *warn-on-reflection* true)
)
