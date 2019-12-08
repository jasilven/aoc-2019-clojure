(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as w]))

(defn parse-pairs [fname]
  (with-open [rdr (io/reader fname)]
    (doall (for [line (line-seq rdr)]
             (str/split line #"\)")))))

(defn parse-orbits
  "Parses input file and returs map containing object as key and
  vector of its orbits as value."
  [fname]
  (let [pairs (parse-pairs fname) 
        objects-with-orbit (->> (map first pairs) (into #{}))
        all-objects (->> (map second pairs) (into #{}))
        leafs (set/difference all-objects objects-with-orbit)
        result (->> (interleave leafs (repeat (count leafs) []))
                    (apply assoc {}))]
    (->> (reverse pairs)
         (group-by first)
         (map (fn [[k vs]] [k (mapv #(second %) vs)]))
         (into {})
         (merge result))))

(defn make-orbit-tree [orbits object tier]
  (let [parent {:name object :tier tier}
        children (get orbits object)]
    (if (empty? children)
      (list parent)
      (cons parent
            (concat (map #(make-orbit-tree orbits % (inc tier)) children))))))

(defn solve1 [fname]
  (let [tree (-> (parse-orbits fname) (make-orbit-tree "COM" 0))
        result (atom [])]
    (w/postwalk #(if (some? (:tier %))
                   (swap! result conj (:tier %))
                   (identity %)) tree)
    (apply + @result)))

(defn path-to-targets [parents from targets]
  (loop [current from
         path []]
    (if (targets current)
      (drop-last path)
      (let [parent (get parents current)]
        (recur parent (conj path parent))))))

(defn solve2 [fname]
  (let [parents (->> (parse-pairs fname) (map #(into [] (reverse %))) (into {}))
        my-path (path-to-targets parents "YOU" #{"COM"})
        santa-path (path-to-targets parents "SAN" #{"COM"})
        junctions (set/intersection (set my-path) (set santa-path))]
    (+ (count (path-to-targets parents "YOU" junctions))
       (count (path-to-targets parents "SAN" junctions)))))

(defn -main [& args]
  (let [fname "resources/day6-input.txt"]
    (println "part 1:" (solve1 fname))
    (println "part 2:" (solve2 fname))))

(comment
  (solve1 "resources/day6-input.txt")
  ;; => 200001
  (solve2 "resources/day6-input.txt")
  ;; => 379
  )
