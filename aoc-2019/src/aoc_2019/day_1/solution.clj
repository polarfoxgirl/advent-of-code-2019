(ns aoc-2019.day-1.solution)

(defn calc_fuel
  [mass]
  (max 0
       (- (quot mass 3)
          2)))

(defn calc_fuel_mass
  [mass]
  (loop [m 0
         f (calc_fuel mass)]
    (if (zero? f)
      m
      (recur (+ m f) (calc_fuel f)))))

(defn solve
  []
  (do
    (def reqs (->>
               (clojure.string/split-lines (slurp "src/aoc_2019/day_1/input"))
               (map #(Integer/parseInt %))))

    (def result_1 (reduce + (map calc_fuel reqs)))
    (println (format "Result 1: %d" result_1))

    (def result_2 (reduce + (map calc_fuel_mass reqs)))
    (println (format "Result 2: %d" result_2))))
