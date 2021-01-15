(ns aoc-2019.day-6.solution)
(require '[clojure.string :as str])

(defn parse-input
  [filename]
  (->> (str/split-lines (slurp filename))
       (map #(str/split % #"\)"))))

(defn process-orbit
  [acc [sun planet]]
  (if (contains? acc sun)
    (update acc sun #(conj % planet))
    (assoc acc sun (list planet))))


(defn orbit-bfs
  [orbit_map]
  (loop [bfs_queue (conj clojure.lang.PersistentQueue/EMPTY ["COM" 0])
         depth_map {}]
    (if (empty? bfs_queue)
      depth_map
      (let [[planet depth] (peek bfs_queue)
            children (get orbit_map planet)]
        (recur
          (pop (if (nil? children)
                 bfs_queue
                 (reduce #(conj %1 [%2 (inc depth)]) bfs_queue children)))
          (assoc depth_map planet depth))))))

(defn count-orbits
  [depth_map]
  (apply + (vals depth_map)))

(defn find-min-root
  [sun_map depth_map planet1 planet2]
  (loop [p1 planet1
         p2 planet2]
    (if (= p1 p2)
      p1
      (if (< (get depth_map p1) (get depth_map p2))
        (recur p1 (get sun_map p2))
        (recur (get sun_map p1) p2)))))

(defn calc-transfers
  [depth_map p1 p2 root]
  (- (+ (get depth_map p1) (get depth_map p2) -2)
     (* 2 (get depth_map root))))

(defn solve
  []
  (let [orbit_pairs (parse-input "src/aoc_2019/day_6/input")
        orbit_map (reduce process-orbit {} orbit_pairs)
        depth_map (orbit-bfs orbit_map)]
    ; (count-orbits depth_map)
    (let [sun_map (into {} (map #(vec (reverse %)) orbit_pairs))
          you "YOU"
          san "SAN"]
      (->> (find-min-root sun_map depth_map you san)
           (calc-transfers depth_map you san)))))
