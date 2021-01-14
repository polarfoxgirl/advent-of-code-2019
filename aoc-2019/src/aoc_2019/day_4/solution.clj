(ns aoc-2019.day-4.solution)
(require '[clojure.string :as str])

(defn get-rev-digits [num_str]
  (reverse
   (map #(Integer/parseInt (str %)) (seq num_str))))

(defn parse-input
  [filename]
  (->> (str/split (slurp filename) #"-")
       (map get-rev-digits)))

(defn inc-password [digits]
  (let [tail (drop-while #(= 9 %) digits)]
    (cond
      (some? tail) (let [[n & others] tail]
                     (concat
                      (repeat (- 6 (count others)) (inc n))
                      others))
      :else nil)))

(defn valid-password? [digits]
  (not (apply distinct? digits)))

(defn really-valid-password? [digits]
  (boolean
   (some #{2} (vals (frequencies digits)))))

(defn get-next-desc [input_digits]
  (loop [digits input_digits]
    (if (= digits (sort > digits))
      digits
      (recur (inc-password digits)))))

(defn calc-passwords [start end]
    (loop [current start
           acc 0]
      (do
        (if (= current end)
          acc
          (recur
            (inc-password current)
            ; (if (valid-password? current) (inc acc) acc))))))
            (if (really-valid-password? current) (inc acc) acc))))))

(defn solve
  []
  (do
    (let [[start end] (parse-input "src/aoc_2019/day_4/input")]
      (let [valid_start (get-next-desc start)
            valid_end (get-next-desc end)]
        (calc-passwords
         valid_start
         (if (= valid_end end) (inc-password valid_end) valid_end))))))
