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

(parse "><+--[++[--],.]-.,")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [source (slurp *in*)]
    (println (parse source))))
