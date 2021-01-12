(ns aoc-2019.day-2.solution)
(require '[clojure.string :as str])

(defn do_op
  [op n1 n2]
  (if (= op 1)
    (+ n1 n2)
    (* n1 n2)))

(defn execute
  [commands]
  (loop [cmds commands
         pos 0]
    (let [op (nth cmds pos)]
      (if (= op 99)
        (first cmds)
        (let [n1 (nth cmds (nth cmds (+ pos 1)))
              n2 (nth cmds (nth cmds (+ pos 2)))
              t_pos (nth cmds (+ pos 3))]
          (recur
            (assoc cmds t_pos (do_op op n1 n2))
            (+ pos 4)))))))

(defn patch
  [memory noun verb]
  (-> memory
      (assoc 1 noun)
      (assoc 2 verb)))

(defn find-verb
  [value memory noun]
  (loop [verb 0]
    (cond
      (= value (execute (patch memory noun verb))) [noun verb]
      (= verb 99) nil
      :else (recur (inc verb)))))

(defn find-input
  [value memory]
  (loop [noun 0]
    (let [result (find-verb value memory noun)]
      (cond
        (some? result) result
        (= noun 99) nil
        :else (recur (inc noun))))))


(defn solve
  []
  (do

    (def cmds (->>
               (slurp "src/aoc_2019/day_2/input")
               str/trim
               (#(str/split % #","))
               (map #(Integer/parseInt %))
               vec))

    (let [patched_cmds (patch cmds 12 2)]
         (def result_1 (execute patched_cmds))
         (println (format "Result 1: %d" result_1)))

    (let [[noun verb] (find-input 19690720 cmds)]
      (def result_2 (+ verb (* noun 100)))
      (println (format "Result 2: %d" result_2)))))
