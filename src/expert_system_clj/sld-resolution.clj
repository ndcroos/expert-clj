(setq *clause-set*
’(((P x z) (Q x y) (P y z))
((P x x))
((Q a b))))

(defn LookUp 
    [x environment]
    (cond ((null x) nil)
    ((variable? x)
    (let [(binding (lookup (get-binding x environment]
    environment)))
    (cond ((null binding) x)
    (t binding))))
    (t x)))

(defn get-binding 
    [var environment]
    (first (rassoc var environment :test #’equal)))

(defn add-binding 
    [var term environment]
    (cons (cons term var) environment))

;; Starts the resolution process.
(defn prove 
    [goals]
    (Resolution (list goals) ’(0) 1 nil))

(defn resolution 
    [goals level-list level environment]
    (cond ((null goals) environment)
    ((null (first goals))
    (resolution (rest goals)
    (rest level-list)
    level
    environment))
    (t (let ((goal-atom (list (first level-list)
    (first-goal goals)))
    (rest-goals (but-first goals)))
    (resolve-unit goal-atom
    rest-goals

;; Checkers wether selected atom goal is unifiable with one of the heads from the variable clause-set
(defn resolve-unit 
    [goal rest-goals level-list level environment]
    (do ((clause (first *clause-set*)
    (first rest-clauses))
    (rest-clauses (rest *clause-set*)
    (rest rest-clauses))
    (result ’FAIL)
    (env2 nil))
    ((or (null clause)
    (not (eq result ’FAIL))) result)
    (let [(head (first clause)]
    (body (rest clause)))
    (setq env2 (Unify goal
    (list level head)
    environment))
    (unless (eq env2 ’FAIL)
    (setq result (resolution (cons body rest-goals)
    (cons level level-list)
    (1+ level)
    env2))))))

;
(defn unify [x y environment-
    (let [(x (look-up x environment)]
    (y (look-up y environment)))
    (cond ((equal x y) environment)
    ((variable? x) (add-binding x y environment))
    ((variable? y) (add-binding y x environment))
    ((or (constant? x)
    (constant? y))
    (if (eq (name x)
    (name y)) environment
    ’FAIL))
    (t (setq environment
    (unify (first-expr x) (first-expr y) environment))
    (if (eq environment ’FAIL) ’FAIL
    (unify (rest-expr x)
    (rest-expr y) environment))))))

;
(defn name 
    [x]
    (cadr x))

;
(defn variable? 
    [x]
    (member (Name x) ’(u v w x y z)))

;             
(defn constant? 
    [x]
    (atom (name x)))

;            
(defn first-expr 
    [x]
    (list (first x) (caadr x)))

;             
(defn rest-expr (x)
    (list (first x) (cdadr x)))
