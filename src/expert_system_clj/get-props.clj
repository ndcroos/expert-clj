;;
(defn goal? 
    [var]
    (eq (get var ’class) ’goal))

;;
(defn traced? 
    [var]
    (get var ’traced))

;;
(defn get-prompt 
    [var]
    (get var ’prompt))
