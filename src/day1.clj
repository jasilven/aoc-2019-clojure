(ns day1)

(defn fuel-required [mass]
  (- (int (/ mass 3)) 2))

(defn fuel-required2
  ([mass] (fuel-required2 mass 0))
  ([mass total]
   (let [fuel (fuel-required mass)]
     (if (pos-int? fuel)
       (recur fuel (+ total fuel))
       total))))

(defn solve [fname fuel-fn]
  (with-open [rdr (clojure.java.io/reader fname)]
    (->> (line-seq rdr)
         (map #(Integer/parseInt %))
         (map fuel-fn)
         (reduce + 0))))

(defn -main [& args]
  (let [input "resources/day1-input.txt"]
    (println "part 1:" (solve input fuel-required))
    (println "part 2:" (solve input fuel-required2))))

(comment
  (solve "resources/day1-input.txt" fuel-required)
  ;; => 3394106
  (solve "resources/day1-input.txt" fuel-required2)
  ;; => 5088280
  )

