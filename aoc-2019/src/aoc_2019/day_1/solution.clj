(ns aoc-2019.day-1.solution)

(defn process-line
  [line]
  (- (quot (Integer/parseInt line) 3) 2))

(defn solve
  []
  (do
    (->>
      (clojure.string/split-lines (slurp "src/aoc_2019/day_1/input"))
      (map process-line)
      (reduce +))))
