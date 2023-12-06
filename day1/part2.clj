(ns part2
  (:require [clojure.string :as str]))


(defn input-lines [file]
  (str/split-lines (slurp file)))

(defn word-to-number [word-seq]
  (let [word (str/join word-seq)]
    (if (Character/isDigit (first word-seq))
      (Character/digit (first word-seq) 10)
      (cond
        (.startsWith word "one") 1
        (.startsWith word "two") 2
        (.startsWith word "three") 3
        (.startsWith word "four") 4
        (.startsWith word "five") 5
        (.startsWith word "six") 6
        (.startsWith word "seven") 7
        (.startsWith word "eight") 8
        (.startsWith word "nine") 9
        :else nil))))


(defn words-to-number [line]
  (let [seq-line (seq line)
        windows (partition 5 1 (concat seq-line (take 5 (repeat nil))))]
    (filter identity (map word-to-number windows))))

(defn line-value [line]
  (+ (* (first line) 10) (last line)))

(reduce + (map (comp line-value words-to-number) (input-lines "input.txt")))
