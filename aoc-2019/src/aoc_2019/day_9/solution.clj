(ns aoc-2019.day-9.solution)
(require '[clojure.string :as str])
(require '[clojure.edn :as edn])

(defn parse-input [filename]
  (let [cmds (->> (slurp filename)
                  (#(str/split % #","))
                  (map edn/read-string))]
    (zipmap (range (count cmds)) cmds)))

; Intcode Computer

(defn parse-op [n]
  [(mod n 100)
   [(quot (mod n 1000) 100)
    (quot (mod n 10000) 1000)
    (quot (mod n 100000) 10000)]])

(defn get-current-op [state]
  (parse-op
    ((state :cmds) (state :pos) 0)))

(defn special-param?
  [op i]
  (case op
    (1 2 7 8) (= i 2)
    (3) (= i 0)
    false))

(defn get-param
  [state op modes i]
  (let [val ((state :cmds) (+ (state :pos) i 1) 0)]
    (if (special-param? op i)
        (case (nth modes i)
          0 val
          2 (+ val (state :base)))
        (case (nth modes i)
          0 ((state :cmds) val 0)
          1 val
          2 ((state :cmds) (+ val (state :base)) 0)))))

(defn read-op-params
  [op modes state]
  (let [pos (state :pos)
        param_cnt (case op
                    (1 2 7 8) 3
                    (3 4 9) 1
                    (5 6) 2
                    99 0)]
     [(map #(get-param state op modes %) (range param_cnt))
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
     (update state :output #(cons val %)))))

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
    ; (println (format "[%d] modes %s" op modes))
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
      9 (update upd_state :base (partial + (first params)))
      99 (assoc upd_state :status :halted))))

(defn execute
  [init_state]
  (loop [state init_state]
    (if (not= (state :status) :ready)
      state
      (recur (do-next-op state)))))

; Problem Solution

(defn solve
  [filename]
  (let [cmds (parse-input filename)]
    ((execute {:cmds cmds
               :pos 0
               :base 0
               :input [2]
               :output '()
               :status :ready}) :output)))

(solve "src/aoc_2019/day_9/test_input")

(solve "src/aoc_2019/day_9/input")