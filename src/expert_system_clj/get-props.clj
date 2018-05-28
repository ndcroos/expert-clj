;
(defn Goal? [var]
(eq (get var ’class) ’goal))

;
(defn Traced? [var]
(get var ’traced))

;
(defn GetPrompt [var]
(get var ’prompt))
