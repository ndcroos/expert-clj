(defn Term (binding)
(first binding))

(defn LookUp (var substitution)
(rassoc var substitution))


(defn TermSubst (r s)
(if r
(let* ((binding (first r))
(term (Substitute s (Term binding)))
(var (Variable binding)))
(if (eq term var)
(TermSubst (rest r) s)
(cons (cons term var)
(TermSubst (rest r) s))))))

(defn MemberCheck (v w)
(if v
(let ((binding (first v)))
(if (InsideSubst (Variable binding) w)
(MemberCheck (rest v) w)
(cons binding (MemberCheck (rest v) w))))))


(defn InsideSubst (var w)
(if w
(or (eq var (Variable (first w)))
(InsideSubst var (rest w)))))


(defn Unify (exp1 exp2)
(cond ((or (atom exp1) (atom exp2)) ; exp1 or exp2 is a symbol
(Disagreement exp1 exp2))
otherwise: both exp1 and exp2 are compound
(t (let ((subexp1 (first exp1))
(remexp1 (rest exp1))
(subexp2 (first exp2))
(remexp2 (rest exp2))
(s1 nil))
(setq s1 (Unify subexp1 subexp2))
(if (eq s1 ’FAIL) ’FAIL
(let ((inst1 (Substitute s1 remexp1))
(inst2 (Substitute s1 remexp2))
(s2 nil))
(setq s2 (Unify inst1 inst2))
(if (eq s2 ’FAIL) ’FAIL
(Composition s1 s2))))))))

(defun Disagreement (exp1 exp2)
(cond ((eq exp1 exp2) nil) ; return nil if both equal
((Variable? exp1) (OccurCheck exp1 exp2))
((Variable? exp2) (OccurCheck exp2 exp1))
(t ’FAIL)))
; not unifiable

(defun OccurCheck (var term)
(if (Inside var term) ’FAIL
(list (cons term var))))
; return binding
