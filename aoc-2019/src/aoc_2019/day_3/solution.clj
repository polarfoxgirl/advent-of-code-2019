(ns aoc-2019.day-3.solution)
(require '[clojure.string :as str])

(defn parse-input
  [filename]
  (->> (slurp filename)
      str/split-lines
      (map (fn [line]
             (->> (str/split line #",")
                 (map #(rest (re-matches #"([UDLR])(\d+)" %)))
                 (map (fn [[dir cnt_str]] [dir (Integer/parseInt cnt_str)])))))))

(defn apply-dir
  [dir x y]
  (case dir
    "U" [x (inc y)]
    "D" [x (dec y)]
    "L" [(dec x) y]
    "R" [(inc x) y]))


(defn add-points
  [points x0 y0 [dir total_count]]
  (loop [pnts points
         x x0
         y y0
         cnt total_count]
    (if (zero? cnt)
      [pnts x y]
      (let [[x1 y1] (apply-dir dir x y)]
        (recur (conj pnts [x1 y1]) x1 y1 (dec cnt))))))

(defn get-points
  [commands]
  (loop [cmds commands
         points #{}
         x 0
         y 0]
    (if (empty? cmds)
       points
       (let [[upd_points x1 y1] (add-points points x y (first cmds))]
         (recur (rest cmds) upd_points x1 y1)))))


(defn solve
  []
  (do
    (let [[input1, input2] (parse-input "src/aoc_2019/day_3/input")]

      (->>
       (clojure.set/intersection (get-points input1) (get-points input2))
       seq
       (map (fn [[x y]]
              (+ (Math/abs x) (Math/abs y))))
       (reduce min)))))
