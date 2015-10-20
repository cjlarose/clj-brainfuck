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

(defn evaluate [input state stmts]
  (let [evaluate-stmt
          (fn [[input {:keys [tape data-pointer] :as state}] stmt]
            (case stmt
              \> [input (update state :data-pointer inc)]
              \< [input (update state :data-pointer dec)]
              \+ [input (update-in state [:tape data-pointer] inc)]
              \- [input (update-in state [:tape data-pointer] dec)]
              \. (do (print (char (nth tape data-pointer))) [input state])
              \, (let [[ch & remaining] input]
                   [remaining (assoc-in state [:tape data-pointer] ch)])
              (loop [[input {:keys [tape data-pointer] :as cur-state}] [input state]]
                (if (= (nth tape data-pointer) 0)
                  [input cur-state]
                  (recur (evaluate input cur-state stmt))))))]
    (reduce evaluate-stmt [input state] stmts)))

(defn byte-seq [^java.io.BufferedReader rdr]
  (lazy-seq
    (let [ch (.read rdr)]
      (if (= ch -1)
        '()
        (cons ch (byte-seq rdr))))))

(def initial-state
  {:tape (apply vector-of :byte (repeat 30000 0))
   :data-pointer 0})

(defn -main
  "Reads brainfuck program from file, then executes it"
  [& args]
  (let [in (byte-seq (BufferedReader. *in*))]
    (->> (first args)
         (slurp)
         (parse)
         (evaluate in initial-state))))
