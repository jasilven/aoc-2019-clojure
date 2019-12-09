(ns day5
  (:require [day2]
            [day4]
            [clojure.string :as str]))

(defn parse-instruction [intcode]
  (let [digits (day4/int->digits intcode)
        [a b c d e] (concat (repeat (- 5 (count digits)) 0)
                            digits)]
    {:A a :B b :C c :opcode (+ (* d 10) e)}))

(defn get-param [[ints ip] mode position]
  (case mode
    0 (aget ints (aget ints (+ position ip)))
    (aget ints (+ position ip))))

(defn parse-params [[ints ip] {:keys [B C]} n]
  (let [arg1 (get-param [ints ip] C 1)
        arg2 (get-param [ints ip] B 2)
        arg3 (when (= n 3) (aget ints (+ 3 ip)))]
    (take n [arg1 arg2 arg3])))

(defn execute-input [[ints ip]]
  (->> (Integer/parseInt (read-line))
       (aset ints (aget ints (inc ip))))
  [ints (+ 2 ip)])

(defn execute-output [[ints ip] {:keys [C]}]
  (println (get-param [ints ip] C 1))
  [ints (+ 2 ip)])

(defn execute-op [[ints ip] op-fn {:keys [B C] :as instruction}]
  (if (= 0 B C)
    (day2/execute-op [ints ip] op-fn)
    (do
      (aset ints
            (aget ints (+ ip 3))
            (apply op-fn (parse-params [ints ip] instruction 2)))
      [ints (+ 4 ip)])))

(defn execute-jump [[ints ip] test-fn instruction]
  (let [[arg1 arg2] (parse-params [ints ip] instruction 2)]
    (if (test-fn arg1)
      [ints arg2]
      [ints (+ 3 ip)])))

(defn execute-cmp [[ints ip] cmp-fn instruction]
  (let [[arg1 arg2 arg3] (parse-params [ints ip] instruction 3)]
    (aset ints arg3 (if (cmp-fn arg1 arg2) 1 0))
    [ints (+ ip 4)]))

(defn execute
  ([intcodes ip] (execute [(int-array intcodes) 0]))
  ([[ints ip]]
   (let [instruction (parse-instruction (aget ints ip))]
     (case (:opcode instruction) 
       1 (recur (execute-op [ints ip] + instruction))
       2 (recur (execute-op [ints ip] * instruction))
       3 (recur (execute-input [ints ip]))
       4 (recur (execute-output [ints ip] instruction))
       5 (recur (execute-jump [ints ip] (comp not zero?) instruction))
       6 (recur (execute-jump [ints ip] zero? instruction))
       7 (recur (execute-cmp [ints ip] < instruction))
       8 (recur (execute-cmp [ints ip] = instruction))
       99 nil))))

(defn solve [intcodes input]
  (-> (with-out-str
        (with-in-str input
          (execute intcodes 0)))
      (str/split-lines)
      last))

(defn -main [& args]
  (let [intcodes (-> (slurp "resources/day5-input.txt")
                     day2/parse-intcodes)]
    (println "part 1:" (solve intcodes "1"))
    (println "part 2:" (solve intcodes "5"))))

(comment
  (-> "resources/day5-input.txt" slurp day2/parse-intcodes
      (solve "1"))
  ;; => "16489636"
  (-> "resources/day5-input.txt" slurp day2/parse-intcodes
      (solve "5"))
  ;; => "9386583"
  )
