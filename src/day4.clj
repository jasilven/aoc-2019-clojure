(ns day4
  (:require [clojure.string :as str]))

(defn input->range [s]
  (let [[a b] (->> (str/split s #"-") (mapv #(Integer/parseInt %)))]
    (range a (inc b))))

(defn int->digits [n]
  (loop [n n result []]
    (if (pos-int? n)
      (recur (int (/ n 10))
             (conj result (rem n 10)))
      (into [] (reverse result)))))

(defn same-adjacents? [digits]
  (->> digits
       (reduce #(if (= %1 %2) (reduced true) %2))
       (true?)))

(defn increasing-digits? [digits]
  (apply <= digits))

(defn same-adjacents2? [digits]
  (let [freqs (frequencies digits)]
    (some (fn [[a b]] (and (= a b) (= 2 (freqs a))))
          (partition 2 1 digits))))

(defn solve [input preds]
  (->> (input->range input)
       (map int->digits)
       (filter (apply every-pred preds))
       count))

(defn solve1 [input]
  (solve input [same-adjacents? increasing-digits?]))

(defn solve2 [input]
  (solve input [same-adjacents2? increasing-digits?]))

(defn -main [& args]
  (let [input  "353096-843212"]
    (println "part 1:" (solve1 input))
    (println "part 2:" (solve2 input))))

(comment
  (time (solve1 "353096-843212"))
  ;; => 579
  (time (solve2 "353096-843212"))
  ;; => 358
  )
