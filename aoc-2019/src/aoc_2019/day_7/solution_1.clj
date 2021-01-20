(ns aoc-2019.day-7.solution-1)
(require '[clojure.string :as str])

(defn parse-input [filename]
  (->>
    (slurp filename)
    (#(str/split % #","))
    (map #(Integer/parseInt %))
    vec))

; Intcode Computer

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
                    (5 6) 2
                    99 0)]
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
    (->
     (update state :output #(cons val %))
     (assoc :status :paused))))

(defn read-from-input
  [state]
  (let [[val & tail] (state :input)]
    ; (println (format "Reading %d from input" val))
    [(assoc state :input (vec tail)) val]))


(defn jump-if
  [state pred val]
  (do
    ; (println (format "Cond: if %s jump to %d" pred val))
    (if pred
      (assoc state :pos val)
      state)))

(defn do-next-op
  [state]
  (let [[op modes] (get-current-op state)
        [params upd_state] (read-op-params op modes state)]
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
                            0))
      99 (assoc upd_state :status :halted))))

(defn execute
  [init_state]
  (loop [state init_state]
    (if (not= (state :status) :ready)
      state
      (recur (do-next-op state)))))

; Amplifier Controller

(defn init-state
  [cmds phase]
  {:cmds cmds
   :pos 0
   :input [phase]
   :output '()
   :status :ready})

(defn init-amp-states
  [cmds phases]
  (zipmap [:A :B :C :D :E]
          (map (partial init-state cmds) phases)))

(defn pass-signal
  [amp_states amp signal]
  (do
    ; (println (format "Executing amp %s with signal %d" amp signal))
    (assoc amp_states amp (-> (amp_states amp)
                              (assoc :status :ready)
                              (update :input #(conj % signal))
                              execute))))

(defn read-signal
  [amp_states amp]
  (let [amp_state (amp_states amp)]
    ; (println (format "Reading signal from %s (amp %s)" (amp_state :output) amp))
    (first (amp_state :output))))

(defn run-amp-chain
  [cmds phases]
  (let [amp_map {:A :B, :B :C, :C :D, :D :E, :E :A}]
    (loop [amp_states (init-amp-states cmds phases)
           amp :A
           signal 0]
      (if (= ((amp_states amp) :status) :halted)
        (read-signal amp_states :E)
        (let [upd_states (pass-signal amp_states amp signal)]
          (recur upd_states
                 (amp_map amp)
                 (read-signal upd_states amp)))))))

; Problem Solution

(defn gen-perms
  [prefix unused_vals]
  (if (empty? unused_vals)
    (list prefix)
    (mapcat (fn [x] (gen-perms (conj prefix x) (disj unused_vals x)))
            unused_vals)))

(defn solve
  []
  (let [cmds (parse-input "src/aoc_2019/day_7/input")]
    (->> (gen-perms [] #{5 6 7 8 9})
         (map #(run-amp-chain cmds %))
         (reduce max))))
