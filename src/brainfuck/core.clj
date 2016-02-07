(ns brainfuck.core
  (:import (java.io BufferedReader))
  (:gen-class))

(def commands
  #{\> \< \+ \- \. \,})

(defn parse-source [program]
  (loop [ast []
         source program]
    (let [token (first source)]
      (cond
       (nil? token) [ast ""]
       (commands token) (recur
                         (conj ast token)
                         (rest source))
       (= token \[) (let [[loop-ast unparsed] (parse-source (rest source))]
                      (recur (conj ast loop-ast) unparsed))
       (= token \]) [ast (rest source)]
       :else (recur ast (rest source))))))

(defn parse [program]
  (first (parse-source program)))

(defn evaluate-stmt [{:keys [input tape data-pointer] :as state} stmt]
  (case stmt
    \> (update state :data-pointer inc)
    \< (update state :data-pointer dec)
    \+ (update-in state [:tape data-pointer] inc)
    \- (update-in state [:tape data-pointer] dec)
    \, (let [[ch & remaining] input]
         (-> state
             (assoc :input remaining)
             (assoc-in [:tape data-pointer] ch)))))

(defn evaluate
  ([state stmts] (evaluate state stmts nil))
  ([{:keys [tape data-pointer] :as state} [stmt & remaining-stmts :as stmts] more]
    (lazy-seq
      (cond
        (nil? stmt)
          (if more
            (evaluate state more))
        (= stmt \.)
          (cons (char (nth tape data-pointer)) (evaluate state remaining-stmts more))
        (commands stmt)
          (evaluate (evaluate-stmt state stmt) remaining-stmts more)
        :else
          (if (= (nth tape data-pointer) 0)
            (evaluate state remaining-stmts more)
            (evaluate state stmt (into (vec stmts) more)))))))

(defn byte-seq [^java.io.BufferedReader rdr]
  (lazy-seq
    (let [ch (.read rdr)]
      (if (= ch -1)
        '(0)
        (cons ch (byte-seq rdr))))))

(def initial-state
  {:tape (apply vector-of :byte (repeat 30000 0))
   :data-pointer 0
   :input (byte-seq (BufferedReader. *in*))})

(defn -main
  "Reads brainfuck program from file, then executes it"
  [& args]
  (let [output (->> (first args)
                 (slurp)
                 (parse)
                 (evaluate initial-state))]
    (doseq [ch output]
      (print ch))))
