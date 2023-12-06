(ns part1
  (:require [clojure.string :as str]))

(defn input-lines [filename]
  (str/split-lines (slurp filename)))

(def game-max {"red" 12
               "green" 13
               "blue" 14})

(def cube-regex #"(\d+)\s(red|green|blue)")

(defn valid-round [game-round]
  (map (fn [[_whole amount colour]]
         (if (> (Integer. amount) (game-max colour))
           false
           true)) (re-seq cube-regex game-round)))

(defn valid-rounds [rounds]
  (let [validity-seq (map (comp #(every? true? %1) valid-round) rounds)]
    (reduce + (filter identity (map #(if %1 %2 nil) validity-seq (iterate inc' 1))))))

(valid-rounds (input-lines "input.txt"))

(defn min-cubes [game-round]
  (reduce * (map
   (fn [[_colour colour-amounts]]
     (apply max (map first colour-amounts)))
   (group-by
    second
    (map
     (fn [[_whole amount colour]]
       [(Integer. amount) colour])
     (re-seq cube-regex game-round))))))

(reduce + (map min-cubes (input-lines "input.txt")))
