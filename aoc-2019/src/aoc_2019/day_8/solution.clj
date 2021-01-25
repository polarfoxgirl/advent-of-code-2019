(ns aoc-2019.day-8.solution)
(require '[clojure.string :as str])

(defn parse-input
  [filename]
  (->> (slurp filename)
       seq
       (map #(Integer/parseInt (str %)))))

(defn get-layer-frequencies
  [pixels layer_size]
  (->> pixels
      (partition layer_size)
      (map frequencies)))

(defn eval-result
  [freq_map]
  (* (freq_map 1) (freq_map 2)))

(defn solve
  [filename width height]
  (let [pixels (parse-input filename)
        layer_size (* width height)]
    (->> (get-layer-frequencies pixels layer_size)
         (sort-by #(% 0))
         first
         eval-result)))


(solve "src/aoc_2019/day_8/test_input" 3 2)

(solve "src/aoc_2019/day_8/input" 25 6)