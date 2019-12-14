(ns day10
  (:require [clojure.string :as str]))

(defn parse-asteroids [fname]
  (let [data (-> (slurp fname) str/trimr )
        lines (str/split-lines data)]
    (->> (for [y (range 0 (count lines))
               x (range 0 (.length (first lines)))]
           [x y])
         (interleave (str/split (str/replace data #"\n" "") #"" ))
         (partition 2)
         (filter #(= (first %) "#"))
         (map second)
         (into #{}))))

(defn atan2 [[x1 y1] [x2 y2]] (Math/atan2 (- y2 y1) (- x2 x1)))

(defn in-line-of-sight [asteroid asteroids] 
  (->> (map (partial atan2 asteroid) asteroids) distinct count))

(defn solve1 [fname]
  (let [asteroids (parse-asteroids fname)]
    (-> (apply max-key #(in-line-of-sight % asteroids) asteroids)
        (in-line-of-sight asteroids))))

(defn -main[]
  (println "part 1:" (solve1 "resources/day10-input.txt")))

(comment
  (solve1 "resources/day10-input.txt")
  ;; => 309
  )
