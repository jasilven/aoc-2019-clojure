(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [day3]))

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
(def pi2 (/ Math/PI 2))

(defn visible-asteroids [asteroids origin] 
  (->> (disj asteroids origin)
       (map (fn [other] [other (atan2 origin other)]))
       (group-by second)
       vals
       (map #(apply min-key (partial day3/manhattan-dist origin) (map first %)))
       (into #{})))

(defn monitoring-asteroid [asteroids]
  (apply max-key (fn [a] (count (visible-asteroids asteroids a)))
         asteroids))

(defn solve1 [fname]
  (let [asteroids (parse-asteroids fname)]
    (->> (monitoring-asteroid asteroids)
         (visible-asteroids asteroids)
         count)))

(defn clockwise-compare [origin a b]
  (let [ta (atan2 origin a)
        tb (atan2 origin b)]
    (cond
      (and (>= ta (- pi2)) (>= tb (- pi2))) (compare ta tb)
      (and (< ta (- pi2)) (< tb (- pi2))) (compare ta tb)
      (and (< ta (- pi2)) (>= tb (- pi2))) 1
      :else -1 )))

(defn solve2 [fname]
  (let [asteroids (parse-asteroids fname)
        origin (monitoring-asteroid asteroids)]
    (loop [vaporized []
           asteroids (disj asteroids origin)]
      (if (empty? asteroids)
        (let [target (nth vaporized 199)]
          (+ (second target)
             (* 100 (first target))))
        (let [visible (visible-asteroids asteroids origin)]
          (recur (concat vaporized
                         (sort (partial clockwise-compare origin) visible))
                 (set/difference asteroids visible)))))))

(defn -main[]
  (let [fname "resources/day10-input.txt"]
    (println "part 1:" (solve1 fname))
    (println "part 2:" (solve2 fname))))

(comment
  (solve1 "resources/day10-input.txt")
  ;; => 309
  (solve2 "resources/day10-input.txt")
  ;; => 416
  )
