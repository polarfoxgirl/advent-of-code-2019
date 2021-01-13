(ns aoc-2019.day-3.solution-1)
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

(defn add-point
  [points x_y acc]
  (let [prev_acc (get points x_y)]
    (if (nil? prev_acc)
      (assoc points x_y acc)
      (update points x_y min acc))))


(defn add-points
  [points x0 y0 acc0 [dir total_count]]
  (loop [pnts points
         x x0
         y y0
         acc acc0
         cnt total_count]
    (if (zero? cnt)
      [pnts x y acc]
      (let [[x1 y1] (apply-dir dir x y)]
        (recur (add-point pnts [x1 y1] acc)
               x1
               y1
               (inc acc)
               (dec cnt))))))

(defn get-points
  [commands]
  (loop [cmds commands
         points {}
         x 0
         y 0
         acc 1]
    (if (empty? cmds)
       points
       (let [[upd_points x1 y1 acc] (add-points points x y acc (first cmds))]
         (recur (rest cmds) upd_points x1 y1 acc)))))


(defn solve
  []
  (do
    (let [[input1, input2] (parse-input "src/aoc_2019/day_3/input")]
      (let [points1 (get-points input1)
            points2 (get-points input2)]
        (->>
          (filter #(contains? points1 %) (keys points2))
          (map #(+ (get points1 %) (get points2 %)))
          (reduce min))))))
