(ns solution
  (:require [clojure.string :as str]))


(defn range-lookup-generator [ranges]
  (let [translation-fns (map
                         (fn [[dest-range-start source-range-start range-len]]
                           (let [range-max (+ source-range-start (- range-len 1))]
                             (fn [source]
                               (if (or (> source range-max) (< source source-range-start))
                                 nil ;; outside of mapped range
                                 (+ dest-range-start (- source source-range-start ))))))
                         ranges)]
    (fn [source]
      (or (first (filter identity (map #(% source) translation-fns))) source))))

(defn map-parser [inp-str]
  (let [[map-type & ranges] (str/split inp-str #"\n")
        map-key (map
                 symbol
                 (rest (re-find #"(.*?)-to-(.*?) map:" inp-str)))]
    (range-lookup-generator
     (map (comp (fn [i] (map #(Long. %) i)) #(str/split %1 #" ")) ranges))))

(defn parse-input [file-nme]
  (let [[seeds & maps] (str/split (slurp file-nme) #"\n\n")]
    (list
     ;; list of seeds
     (map #(Long. %1) (rest (str/split seeds #"\s")))
     ;; map of transformations
     (apply comp (reverse (map map-parser maps)))
     )))

(defn solution [file-name]
  (let [[seeds f] (parse-input file-name)] (reduce min (map f seeds))))

;; part 2

(defn range-map-generator [ranges]
  (let [translation-fns (map
                         (fn [[dest-range-start source-range-start range-len]]
                           (let [range-max (+ source-range-start (- range-len 1))
                                 range-min source-range-start]
                             (fn [[seed-range-start seed-total]]
                               (let [seed-range-end (+ seed-range-start (- seed-total 1))])
                               )))
                         ranges)]
    (fn [source]
      (or (first (filter identity (map #(% source) translation-fns))) source))))

(take 14 (iterate inc 79))
