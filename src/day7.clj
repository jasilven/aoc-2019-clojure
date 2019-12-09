(ns day7
  (:require [day5]
            [day2]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as async :as a]))

(defn run-amplifiers
  ([intcodes phases] (run-amplifiers intcodes phases "0"))
  ([intcodes phases input]
   (if (empty? phases)
     input
     (let [output (with-out-str
                    (with-in-str (format "%s\n%s\n" (first phases) input)
                      (day5/execute intcodes 0)))]
       (run-amplifiers intcodes (rest phases) output)))))

(defn solve1 [program phases]
  (let [intcodes (day2/parse-intcodes program)]
    (->> phases
         (map #(run-amplifiers intcodes %))
         (map #(Integer/parseInt (str/trim-newline %)))
         (reduce #(if (> %2 %1) %2 %1) 0))))

(defn execute-input [[ints ip] [in _]]
  (->>  (a/<!! in)
        (aset ints (aget ints (inc ip))))
  [ints (+ 2 ip)])

(defn execute-output [[ints ip] {:keys [C]} [_ out]]
  (a/>!! out (day5/get-param [ints ip] C 1))
  [ints (+ 2 ip)])

(defn execute
  ([intcodes ip chan-pair name]
   (execute [(int-array intcodes) 0] chan-pair name))
  ([[ints ip] chan-pair name]
   (let [instruction (day5/parse-instruction (aget ints ip))]
     (case (:opcode instruction) 
       1 (recur (day5/execute-op [ints ip] + instruction) chan-pair name)
       2 (recur (day5/execute-op [ints ip] * instruction) chan-pair name)
       3 (recur (execute-input [ints ip] chan-pair) chan-pair name)
       4 (recur (execute-output [ints ip] instruction chan-pair) chan-pair name)
       5 (recur (day5/execute-jump [ints ip] (comp not zero?) instruction) chan-pair name)
       6 (recur (day5/execute-jump [ints ip] zero? instruction) chan-pair name)
       7 (recur (day5/execute-cmp [ints ip] < instruction) chan-pair name)
       8 (recur (day5/execute-cmp [ints ip] = instruction) chan-pair name)
       99 (doseq [ch chan-pair] (a/close! ch))))))

(defn run-amplifiers2
  ([intcodes phase]
   (let [chans (into [] (for [_ (range 0 6)] (a/chan 2)))
         chan-pairs (partition 2 1 chans)
         names ["a" "b" "c" "d" "e"]]

     ;; initialize phase for amplifier a
     (a/>!! (first chans) (first phase))

     ;; initialize phase for amplifier b-e
     (doseq [[in phase] (rest (partition 2 (interleave (map first chan-pairs) phase)))]
       (a/>!! in phase))

     ;; start amplifier executors
     (doseq [[chan-pair name] (partition 2 (interleave chan-pairs names))]
       (a/go (execute intcodes 0 chan-pair name)))

     ;; send ititial input for phase a
     (a/>!! (first chans) 0)
     
     ;; read amplifier e output and forward it to a or return result if
     ;; output channel for e is closed (e halted).
     (loop [result (a/<!! (last chans))
            prev-result nil]
       (if (nil? result)
         prev-result
         (do 
           (a/>!! (first chans) result)
           (recur (a/<!! (last chans))
                  result)))))))

(defn solve2 [program phases]
  (let [intcodes (day2/parse-intcodes program)]
    (->> phases
         (map #(run-amplifiers2 intcodes %))
         (reduce #(if (> %2 %1) %2 %1) 0))))

(defn -main [& args]
  (let [program (slurp "resources/day7-input.txt")
        phases (combo/permutations [0 1 2 3 4])
        phases2 (combo/permutations [5 6 7 8 9])]
    (println "part 1:" (solve1 program phases))
    (println "part 2:" (solve2 program phases2))))

(comment
  ;; $ lein run -m day7   
  ;; part 1: 359142
  ;; part 2: 4374895
  )
