(defn substitute 
    [s expression]
    (cond 
        ((or (null expression)
        (null s)) expression)
    ((singleton? expression)
    (let [(binding nil)]
    (if (variable? expression)
        (setq binding (lookUp expression s)))
    (if (null binding)
        expression
    (term binding))))
    (t (cons (substitute s (first expression))
    (substitute s (rest expression))))))
