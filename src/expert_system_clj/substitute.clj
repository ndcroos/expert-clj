(defn Substitute [s expression]
(cond ((or (null expression)
(null s)) expression)
((Singleton? expression)
(let ((binding nil))
(if (Variable? expression)
(setq binding (LookUp expression s)))
(if (null binding)
expression
(Term binding))))
(t (cons (Substitute s (first expression))
(Substitute s (rest expression))))))