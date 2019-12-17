(ns day11
  (:require [day9]
            [clojure.core.async :as async :as a]))

(defn turn [{:keys [pos dir]} turn-code]
  (cond
    (or (and (= dir :up) (= turn-code 0)) 
        (and (= dir :down) (= turn-code 1)))
    {:pos (map + pos [-1 0]) :dir :left}

    (or (and (= dir :up) (= turn-code 1)) 
        (and (= dir :down) (= turn-code 0)))
    {:pos (map + pos [1 0]) :dir :right}

    (or (and (= dir :left) (= turn-code 1)) 
        (and (= dir :right) (= turn-code 0)))
    {:pos (map + pos [0 -1]) :dir :up}

    (or (and (= dir :left) (= turn-code 0)) 
        (and (= dir :right) (= turn-code 1)))
    {:pos (map + pos [0 1]) :dir :down}))

(defn solve [fname input panels]
  (let [program (-> fname slurp day9/parse-program)
        in (a/chan 1)
        out (a/chan)
        runner (day9/make-runner {:mem program :ip 0 :in in :out out :base 0})]

    (a/thread (runner))

    (loop [{:keys [pos] :as location} {:pos [0 0] :dir :up}
           panels panels]
      (a/>!! in (or (get panels pos) input))
      (if-let [color (a/<!! out)]
        (recur (turn location (a/<!! out))
               (assoc panels pos color))
        panels))))

(defn solve1 [fname input]
  (count (solve fname input {})))

(defn solve2 [fname input]
  (let [panels (solve fname input {[0 0] 1})
        coords (keys panels)]
    (doseq [y (range (apply min (map second coords))
                     (inc (apply max (map second  coords))))]
      (doseq [x (range (apply min (map first coords))
                       (inc (apply max (map first coords))))]
        (print (if (= 1 (get panels [x y])) "#" " ")))
      (println))))

(defn -main []
  (println "part 1:" (solve1 "resources/day11-input.txt" 0))
  (printf  "part 2:%n%s" (with-out-str (solve2 "resources/day11-input.txt" 1))))

(comment
  (solve1 "resources/day11-input.txt" 0)
  ;; => 2322
  (solve2 "resources/day11-input.txt" 1)
  ;;   ## #  #  ##  ###  ###   ##   ##  #  #   
  ;;    # #  # #  # #  # #  # #  # #  # #  #   
  ;;    # #### #  # #  # ###  #    #    #  #   
  ;;    # #  # #### ###  #  # # ## #    #  #   
  ;; #  # #  # #  # # #  #  # #  # #  # #  #   
  ;;  ##  #  # #  # #  # ###   ###  ##   ##   
  )
