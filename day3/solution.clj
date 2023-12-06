(ns solution
  (:require [clojure.string :as str]))

(defn input-lines [filename]
  (map seq (str/split-lines (slurp filename))))

(defn adjacency-mask [engine]
  (map (fn [curr-row ahead-row behind-row]
         (let [is-star #((and (not (Character/isDigit %1)) (not (= %1 \.))))]
           (map (fn [x y z] (if (any? (map is-star (list x y z))) "*" nil))
                curr-row ahead-row behind-row)))
       engine
       (concat (rest engine) (repeat (repeat \.)))
       (cons (repeat \.)  engine)))

(defn expand-symbols [engine]
  (map (fn [curr ahead behind]
         (if (reduce or (map #(= "*" %1) (list curr ahead behind))) \* nil))
       engine (concat (drop 1 engine) ".") (cons \. engine)))

(defn extract-part-numbers [engine]
  (let [flat-mask (expand-symbols (reduce concat (adjacency-mask engine)))
        flat-engine (reduce concat engine)
        extract-fn (fn [acc [symbol char]]
                     (let [[vals safe] (first acc)
                           field-safe (= symbol \*)]
                       (if (Character/isDigit char)
                         (cons (list (concat vals (list char)) (or safe field-safe)) (rest acc))
                         (if (not (= nil (first vals)))
                          ;; start new group
                           (cons (list '() false) (cons (list vals safe) (rest acc)))
                           (cons (list vals safe) (rest acc))))))]
    (reduce extract-fn (list (list '() false)) (map vector flat-mask flat-engine))))

(defn solution-1 [file-name]
  (reduce
   +
   (map
    (comp #(Integer. (apply str %1)) first)
    (filter second (extract-part-numbers (input-lines file-name))))))

(defn tag-gears [input]
  (map (fn [row] (map #(if (= char %1) (gensym) %1) row)) input))

(defn adjacency-mask [engine]
  (map (fn [curr-row ahead-row behind-row]
         (map #(first (filter symbol? (list %1 %2 %3))) curr-row ahead-row behind-row))
       engine
       (concat (rest engine) (repeat (repeat \.)))
       (cons (repeat \.)  engine)))

(defn expand-symbols [engine]
  (map #(first (filter symbol? (list %1 %2 %3)))
       engine (concat (drop 1 engine) (list nil)) (cons nil engine)))

(defn extract-gear-pairs [engine]
  (let [flat-mask (expand-symbols (reduce concat (adjacency-mask (tag-gears engine))))
        flat-engine (reduce concat engine)
        extract-fn (fn [acc [ident char]]
                     (let [[vals gear-ident] (first acc)
                           tail (rest acc)]
                       (if (Character/isDigit char)
                         (cons (list (concat vals (list char)) (or gear-ident ident)) tail)
                         (if (not (= nil (first vals)))
                          ;; start new group
                           (cons (list '() nil) (cons (list vals gear-ident) tail))
                           (cons (list vals gear-ident) tail)))))]
    (reduce extract-fn (list (list '() nil)) (map vector flat-mask flat-engine))))

(map println (expand-symbols (reduce concat (adjacency-mask (tag-gears (input-lines "dummy.txt"))))))

(println (reduce + (map (fn [[group_id vals]]
       (reduce * (map first vals)))
     (filter (fn [[group_id vals]] (> (count vals) 1))
             (group-by
              second
              (map
               (fn [[val gear-ident]] (list (Integer. (apply str val)) gear-ident))
               (filter (comp identity second) (extract-gear-pairs (input-lines "input.txt")))))))))
(solution-1 "input.txt")

(or nil (gensym))
