(ns day8
  (:require [clojure.string :as str]))

(defn parse-data [fname]
  (->> (str/split (str/trim-newline (slurp fname)) #"")
       (map #(Integer/parseInt %))))

(defn solve1 [fname w h]
  (->> (parse-data fname)
       (partition (* w h))
       (map frequencies)
       (sort-by #(get % 0))
       first
       (keep #(when (#{1 2} (first %)) (second %)))
       (apply *)))

(defn column [n no-of-layers arr]
  (for [i (range 0 no-of-layers)]
    (aget arr (+ n (* i (/ (alength arr) no-of-layers))))))

(defn print-data [w data]
  (with-out-str
    (doseq [row (partition w data)]
      (-> (apply str row)
          (str/replace #"1" "#")
          (str/replace #"0" " ")
          (println)))))

(defn solve2 [fname w h]
  (let [arr (->> (parse-data fname) (int-array))
        size (* w h)
        no-of-layers (/ (alength arr) size)]
    (->>  (for [col (range 0 size)]
            (column col no-of-layers arr))
          (map #(filter (fn [pixel] (#{1 0} pixel)) %))
          (mapv first)
          (print-data w))))

(defn -main [& args]
  (let [fname "resources/day8-input.txt" ]
    (println "part 1:" (solve1 fname 25 6))
    (println (format "part 2:\n%s" (solve2 fname 25 6)))))

(comment
  (solve1 "resources/day8-input.txt" 25 6)
  ;; => 1920
  (solve2 "resources/day8-input.txt" 25 6) 
  ;; ###   ##  #  # #     ##  
  ;; #  # #  # #  # #    #  # 
  ;; #  # #    #  # #    #  # 
  ;; ###  #    #  # #    #### 
  ;; #    #  # #  # #    #  # 
  ;; #     ##   ##  #### #  # 
  )
