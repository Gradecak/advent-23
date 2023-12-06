(ns solution
  (:require [clojure.string :as str])
  (:require [clojure.set :as sets]))


(defn input-lines [filename]
  (str/split-lines (slurp filename)))

(defn parse-card-line [line]
  (map
   (fn [numbers-str]
     (set (map #(Integer. %1) (str/split numbers-str #"\s+"))))
   (str/split (second (str/split line #"Card\s+\d+:\s+")) #"\s+\|\s+")))

(defn line-value [line]
  (or (last (take (count (apply sets/intersection (parse-card-line line)))
                  (iterate (partial * 2) 1)))
      0))


(def part-1 (reduce + (map line-value (input-lines "input.txt"))))

(defn play-rounds [cards]
  (let [[copies, winning-numbers] (first cards)
        remainder (rest cards)]
    (concat
     (map
      (fn [[copies, win-num] extra-copies] (list (+ copies extra-copies) win-num))
      remainder
      (take winning-numbers (repeat copies)))
     (drop winning-numbers remainder))))


(def part-2 (reduce
 +
 (map
  (comp first first)
  (take-while
   (comp not empty?)
   (iterate
    play-rounds
    (map #(list %1 %2)
         (repeat 1)
         (map
          #(count (apply sets/intersection (parse-card-line %1)))
          (input-lines "input.txt"))))))))
