(ns aoc-2019.day-5.solution)
(require '[clojure.string :as str])

(defn parse-input [filename]
  (->>
    (slurp filename)
    (#(str/split % #","))
    (map #(Integer/parseInt %))
    vec))

(defn parse-op [n]
  [(mod n 100)
   [(quot (mod n 1000) 100)
    (quot (mod n 10000) 1000)
    (quot (mod n 100000) 10000)]])

(defn patch-op
  [[op modes]]
  [op
   (case op
     (1 2) (assoc modes 2 1)
     (3) (assoc modes 0 1)
     modes)])

(defn get-current-op [state]
  (patch-op
    (parse-op
      (nth (state :cmds) (state :pos)))))

(defn get-param
  [cmds pos modes i]
  (let [val (nth cmds (+ pos i 1))]
      ; (println (format "Got mode %d and val %d for param %d" (nth modes i) val i))
    (case (nth modes i)
      0 (nth cmds val)
      1 val)))

(defn read-op-params
  [op modes state]
  (let [cmds (state :cmds)
        pos (state :pos)
        param_cnt (if (contains? #{1 2} op) 3 1)]
     [(map #(get-param cmds pos modes %) (range param_cnt))
      (assoc state :pos (+ pos 1 param_cnt))]))

(defn write-to-pos
  [state pos val]
  (do
    ; (println (format "Writing %d to %d" val pos))
    (update state :cmds #(assoc % pos val))))

(defn append-to-output
  [state val]
  (do
    ; (println (format "Writing %d to output" val))
    (update state :output #(cons val %))))

(defn apply-op
  [op modes state]
  (let [[params upd_state] (read-op-params op modes state)]
    (case op
      (1 2) (write-to-pos upd_state
                          (nth params 2)
                          ((if (= op 1) + *) (nth params 0) (nth params 1)))
      3 (write-to-pos upd_state
                      (nth params 0)
                      (upd_state :input))
      4 (append-to-output upd_state (nth params 0)))))

(defn execute
  [init_state]
  (loop [state init_state]
    (let [[op modes] (get-current-op state)]
      ; (println (format "Op %d with modes %s" op (str modes)))
      (if (= op 99)
        state
        (recur (apply-op op modes state))))))

(defn solve
  []
  (let [cmds (parse-input "src/aoc_2019/day_5/input")]
    (execute {:cmds cmds
              :pos 0
              :input 1
              :output '()})))
