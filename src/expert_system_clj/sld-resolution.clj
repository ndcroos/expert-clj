(setq *clause-set*
’(((P x z) (Q x y) (P y z))
((P x x))
((Q a b))))

(defn LookUp (x environment)
(cond ((null x) nil)
((Variable? x)
(let ((binding (Lookup (GetBinding x environment)
environment)))
(cond ((null binding) x)
(t binding))))
(t x)))

(defn GetBinding (var environment)
(first (rassoc var environment :test #’equal)))

(defn AddBinding (var term environment)
(cons (cons term var) environment))

;; Starts the resolution process.
(defn Prove (goals)
(Resolution (list goals) ’(0) 1 nil))

(defn Resolution (goals level-list level environment)
(cond ((null goals) environment)
((null (first goals))
(Resolution (rest goals)
(rest level-list)
level
environment))
(t (let ((goal-atom (list (first level-list)
(FirstGoal goals)))
(rest-goals (ButFirst goals)))
(ResolveUnit goal-atom
rest-goals

;; Checkers wether selected atom goal is unifiable with one of the heads from the variable clause-set
(defn ResolveUnit (goal
rest-goals
level-list
level
environment)
(do ((clause (first *clause-set*)
(first rest-clauses))
(rest-clauses (rest *clause-set*)
(rest rest-clauses))
(result ’FAIL)
(env2 nil))
((or (null clause)
(not (eq result ’FAIL))) result)
(let ((head (first clause))
(body (rest clause)))
(setq env2 (Unify goal
(list level head)
environment))
(unless (eq env2 ’FAIL)
(setq result (Resolution (cons body rest-goals)
(cons level level-list)
(1+ level)
env2))))))

(defn Unify (x y environment)
(let ((x (LookUp x environment))
(y (LookUp y environment)))
(cond ((equal x y) environment)
((Variable? x) (AddBinding x y environment))
((Variable? y) (AddBinding y x environment))
((or (Constant? x)
(Constant? y))
(if (eq (Name x)
(Name y)) environment
’FAIL))
(t (setq environment
(Unify (FirstExpr x) (FirstExpr y) environment))
(if (eq environment ’FAIL) ’FAIL
(Unify (RestExpr x)
(RestExpr y) environment))))))

(defn Name (x)
(cadr x))

(defn Variable? (x)
(member (Name x) ’(u v w x y z)))

(defn Constant? (x)
(atom (Name x)))

(defn FirstExpr (x)
(list (first x) (caadr x)))

(defn RestExpr (x)
(list (first x) (cdadr x)))
