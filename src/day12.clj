(ns day12
  (:require [clojure.java.io :as io]))

(defn parse-moons [fname moons]
  (with-open [rdr (io/reader (io/file fname))]
    (->> (doall (for [line (line-seq rdr)
                      :let [matcher (re-matcher #"-?\d+" line)]]
                  {:x (Integer/parseInt (re-find matcher))
                   :y (Integer/parseInt (re-find matcher))
                   :z (Integer/parseInt (re-find matcher))
                   :vx 0 :vy 0 :vz 0}))
         (map merge moons))))

(defn update-velocity [{:keys [x y z vx vy vz] :as moon} others]
  (if (empty? others)
    moon
    (let [other (first others)]
      (-> moon
          (assoc :vx (+ vx (compare (:x other) x)))
          (assoc :vy (+ vy (compare (:y other) y)))
          (assoc :vz (+ vz (compare (:z other) z)))
          (recur (rest others))))))

(defn update-position [{:keys [x y z vx vy vz] :as moon}]
  (-> moon
      (assoc :x (+ vx x))
      (assoc :y (+ vy y))
      (assoc :z (+ vz z))))

(defn tick [moons]
  (->> moons
       (map #(update-velocity % moons))
       (mapv update-position)))

(defn moon-energy [{:keys [x y z vx vy vz]}]
  (* (reduce + (map #(Math/abs %) [x y z]))
     (reduce + (map #(Math/abs %) [vx vy vz]))))

(defn total-energy [moons ticks]
  (->> (nth (iterate tick moons) ticks)
       (map moon-energy)
       (reduce +)))

(defn solve1 [fname ticks]
  (let [moons (parse-moons fname [{:name "Io"} {:name "Europa"} {:name "Ganymede"} {:name "Callisto"}])]
    (total-energy moons ticks)))

(defn -main []
  (println "part 1:" (solve1 "resources/day12-input.txt" 1000)))

(comment
  (solve1 "resources/day12-input.txt" 1000)
  ;; => 6490
  )
