(ns day2
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (let [intcodes (-> s str/trim-newline (str/split #","))]
    (mapv #(Integer/parseInt %) intcodes)))

(defn execute-op [[ints pc] op-fn]
  (let [[a b] (mapv #(aget ints (aget ints (+ % pc))) [1 2])
        c (aget ints (+ pc 3))]
    (aset ints c (op-fn a b))
    [ints (+ pc 4)]))

(defn execute
  "Executes given 'intcodes' which is vector of ints starting at position 'pc'.
  Returns intcodes vector after execution halts (opcode 99 found)."
  ([intcodes pc] (execute [(int-array intcodes) 0]))
  ([[ints pc]]
   (condp = (aget ints pc)
     1 (recur (execute-op [ints pc] +))
     2 (recur (execute-op [ints pc] *))
     99 (into [] ints)
     (println "Unknown opcode"))))

(defn solve1 [input]
  (execute input 0))

(defn -main [& args]
  (let [input (-> (slurp "resources/day2-input.txt")
                  parse-input
                  (assoc 1 12 2 2))]
    (println "part 1:" (first (solve1 input)))
    (println "part 2:" (first (solve1 input)))))

(comment
  (-> "resources/day2-input.txt" slurp parse-input (assoc 1 12 2 2) solve1 first)
  ;; => 4138687

  )
