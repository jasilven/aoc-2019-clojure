(ns day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-instructions [s]
  (->> (str/split s #",")
       (map #(hash-map :direction (first %)
                       :steps (Integer/parseInt (apply str (rest %)))))))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(def direction-to-xy {\U [0 1] \R [1 0] \D [0 -1] \L [-1 0]})

(defn move [direction steps xy]
  (->> (iterate #(mapv + (get direction-to-xy direction) %) xy)
       rest
       (take steps)))

(defn build-path
  ([instructions] (build-path instructions [[0 0]]))
  ([instructions path]
   (if (empty? instructions)
     path
     (let [{:keys [direction steps]} (first instructions)]
       (recur (rest instructions)
              (concat path (move direction steps (last path))))))))

(defn intersections [path1 path2]
  (-> (set/intersection (into #{} path1) (into #{} path2))
      (into [])))

(defn instruction->path [s]
  (-> s parse-instructions build-path))

(defn solve1 [s1 s2]
  (let [[path1 path2] (mapv instruction->path [s1 s2])]
    (->> (intersections (rest path1) (rest path2))
         (map #(hash-map :distance (manhattan-dist [0 0] %)
                         :xy %))
         (sort-by :distance)
         first
         :distance)))

(defn count-steps-to [xy path]
  (reduce #(if (= xy %2)
             (reduced %1)
             (inc %1)) 0 path))

(defn solve2 [s1 s2]
  (let [[path1 path2] (mapv instruction->path [s1 s2])
        intersections (intersections (rest path1) (rest path2))]
    (-> (map +
             (map #(count-steps-to % path1) intersections)
             (map #(count-steps-to % path2) intersections))
        sort
        first)))

(defn -main [& args]
  (let [[s1 s2] (-> (slurp "resources/day3-input.txt")
                    (str/split-lines))]
    (println "part 1:" (solve1 s1 s2))
    (println "part 2:" (solve2 s1 s2))))

(comment
  (let [[s1 s2] (-> (slurp "resources/day3-input.txt") (str/split-lines))]
    (solve1 s1 s2))
  ;; => 316  
  (let [[s1 s2] (-> (slurp "resources/day3-input.txt") (str/split-lines))]
    (solve2 s1 s2))
  ;; => 16368
  )
