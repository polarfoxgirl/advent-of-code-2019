(ns aoc-2019.day-7.solution)
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
     (1 2 7 8) (assoc modes 2 1)
     (3) (assoc modes 0 1)
     modes)])

(defn get-current-op [state]
  (patch-op
    (parse-op
      (nth (state :cmds) (state :pos)))))

(defn get-param
  [cmds pos modes i]
  (let [val (nth cmds (+ pos i 1))]
    (case (nth modes i)
      0 (nth cmds val)
      1 val)))

(defn read-op-params
  [op modes state]
  (let [cmds (state :cmds)
        pos (state :pos)
        param_cnt (case op
                    (1 2 7 8) 3
                    (3 4) 1
                    (5 6) 2)]
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

(defn read-from-input
  [state]
  (let [[val & tail] (state :input)]
    ; (println (format "Reading %d from output" val))
    [(assoc state :input tail) val]))


(defn jump-if
  [state pred val]
  (do
    ; (println (format "Cond: if %s jump to %d" pred val))
    (if pred
      (assoc state :pos val)
      state)))

(defn apply-op
  [op modes state]
  (let [[params upd_state] (read-op-params op modes state)]
    (case op
      (1 2) (write-to-pos upd_state
                          (nth params 2)
                          ((if (= op 1) + *) (nth params 0) (nth params 1)))
      3 (let [[upd_upd_state input_val] (read-from-input upd_state)]
          (write-to-pos upd_upd_state
                        (nth params 0)
                        input_val))
      4 (append-to-output upd_state (first params))
      (5 6) (jump-if upd_state
                     ((if (= op 5) not= =) 0 (nth params 0))
                     (nth params 1))
      (7 8) (write-to-pos upd_state
                          (nth params 2)
                          (if ((if (= op 7) < =)
                               (nth params 0) (nth params 1))
                            1
                            0)))))

(defn execute
  [init_state]
  (loop [state init_state]
    (let [[op modes] (get-current-op state)]
      (if (= op 99)
        state
        (recur (apply-op op modes state))))))

(defn use-amp
  [cmds phase input]
  (do
    ; (println (format "Executing phase %d on %d" phase input))
    (let [res_state (execute {:cmds cmds
                              :pos 0
                              :input (list phase input)
                              :output '()})]
      (first (res_state :output)))))

(defn run-phase-combo
  [cmds phases]
  (let [phase_fns (map #(partial use-amp cmds %) phases)]
    (reduce #(%2 %1) 0 phase_fns)))


(defn gen-perms
  [prefix unused_vals]
  (if (empty? unused_vals)
    (list prefix)
    (mapcat (fn [x] (gen-perms (conj prefix x) (disj unused_vals x)))
            unused_vals)))

(defn solve
  []
  (let [cmds (parse-input "src/aoc_2019/day_7/input")]
    (->> (gen-perms [] #{0 1 2 3 4})
         (map #(run-phase-combo cmds %))
         (reduce max))))
