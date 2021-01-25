(ns aoc-2019.day-8.solution-1)
(require '[clojure.string :as str])

(defn parse-input
  [filename]
  (->> (slurp filename)
       seq
       (map #(Integer/parseInt (str %)))))

(defn get-layers
  [pixels layer_size]
  (->> pixels
      (partition layer_size)))

(defn get-top-pixel
  [layer]
  (if (empty? layer)
    2
    (first layer)))

(defn combine-layers
  [layers]
  (->> (apply interleave layers)
       (partition (count layers))
       (map (partial drop-while #(= 2 %)))
       (map get-top-pixel)))

(defn print-result
  [width pixels]
  (->> pixels
       (map #(case %
               0 "_"
               1 "#"))
       (partition width)
       (map str/join)
       (map println)))

(defn solve
  [filename width height]
  (let [pixels (parse-input filename)
        layer_size (* width height)]
    (->> (get-layers pixels layer_size)
         combine-layers
         (print-result width))))


(solve "src/aoc_2019/day_8/test_input" 2 2)

(solve "src/aoc_2019/day_8/input" 25 6)