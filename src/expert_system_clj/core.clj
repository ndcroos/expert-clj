(ns expert-system-clj.core
  (:gen-class)
)

;; Substitution algorithm
(defn Substitute [s expression]
(cond (or (nil? expression) (nil? s)) expression)
)

;;(def s ((a . x) ((f y) . z)) )

;;(def e (Q x (g z)) )

;; Investigates whether expression is a LISP atom.
(defn Singleton? [expression]
(atom expression))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  ;; Should yield (Q a (g (f y)))
  ;;(Substitute s e)
  (println (Substitute nil "azerty"))
)
