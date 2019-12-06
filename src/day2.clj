(ns day2
  (:require [clojure.string :as str]))

(defn parse-intcodes [s]
  (let [intcodes (-> s str/trim-newline (str/split #","))]
    (mapv #(Integer/parseInt %) intcodes)))

(defn execute-op [[ints ip] op-fn]
  (let [[a b] (mapv #(aget ints (aget ints (+ % ip))) [1 2])
        c (aget ints (+ ip 3))]
    (aset ints c (op-fn a b))
    [ints (+ ip 4)]))

(defn execute
  "Executes given 'intcodes' which is vector of ints starting at position 'ip'.
  Returns intcodes vector after execution halts (oipode 99 found)."
  ([intcodes ip] (execute [(int-array intcodes) 0]))
  ([[ints ip]]
   (condp = (aget ints ip)
     1 (recur (execute-op [ints ip] +))
     2 (recur (execute-op [ints ip] *))
     99 (into [] ints)
     (throw "Unknown intcode"))))

(defn solve1 [intcodes]
  (execute intcodes 0))

(defn nouns-verbs []
  (for [noun (range 0 100)
        verb (range 0 100)]
    [noun verb]))

(defn answer2 [intcodes]
  (+ (* 100 (nth intcodes 1)) (nth intcodes 2)))

(defn solve2 [intcodes wanted]
  (->> (map #(assoc intcodes
                    1 (first %)
                    2 (second %)) (nouns-verbs))
       (filter #(= wanted (try (first (execute % 0))
                               (catch Exception e nil))))
       first
       answer2))

(defn -main [& args]
  (let [intcodes (-> (slurp "resources/day2-input.txt")
                     parse-intcodes
                     (assoc 1 12 2 2))]
    (println "part 1:" (first (solve1 intcodes)))
    (println "part 2:" (solve2 intcodes 19690720))))

(comment
  (-> "resources/day2-input.txt" slurp parse-intcodes (assoc 1 12 2 2) solve1 first)
  ;; => 4138687
  (-> "resources/day2-input.txt" slurp parse-intcodes (solve2 19690720))
  ;; => 6635  
  )
