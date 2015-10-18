(ns brainfuck.core
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

(declare evaluate)

(defn evaluate-stmt
  "Given the current machine state and a statement,
  yield new machine state"
  [{:keys [tape data-pointer] :as state} stmt]
  (case stmt
    \> (update state :data-pointer inc)
    \< (update state :data-pointer dec)
    \+ (update-in state [:tape data-pointer] inc)
    \- (update-in state [:tape data-pointer] dec)
    \. (do
         (print (char (nth tape data-pointer)))
         state)
    (loop [{:keys [tape data-pointer] :as cur-state} state]
      (if (= (nth tape data-pointer) 0)
        cur-state
        (recur (evaluate cur-state stmt))))))

(defn evaluate
  [state stmts]
  (reduce evaluate-stmt state stmts))

(def initial-state
  {:tape (apply vector-of :byte (repeat 30000 0))
   :data-pointer 0})

(defn -main
  "Reads brainfuck program from stdin, then executes it"
  [& args]
  (let [source (slurp *in*)]
    (evaluate initial-state (parse source))))
