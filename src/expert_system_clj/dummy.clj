(defn set-traced 
    [var]
    (setf (get var ’traced) t))

(defn parse-rule 
    [expr]
    (setq *rule-base* (cons expr *rule-base*)))


(defun first-rule 
    [rule-base]
    (first rule-base))

(defn rest-rules 
    [rule-base]
    (rest rule-base))

(defn antecedent 
    [rule]
    (first rule))

(defn consequent 
    [rule]
    (rest rule))

(defn first-conclusion 
    [conseq]
    (first conseq))

(defn rest-conclusions 
    [conseq]
    (rest conseq))

(defun var (assertion)
(second assertion))

(defun Consultation (var-decls)
(TraceGoals var-decls)
(PrintGoals var-decls))

(defun PrintGoals (var-decls)
(cond ((null var-decls) nil)
((Goal? (first var-decls))
(let ((var (first var-decls)))
(terpri)
(print var)
(princ (eval var)))
(PrintGoals (rest var-decls)))
(t (PrintGoals (rest var-decls)))))

;;
(defun TraceGoals (var-decls)
(if var-decls
(let ((variable (first var-decls)))
(if (Goal? variable)
; is it a goal variable?
(TraceValues variable))
(TraceGoals (rest var-decls)))))

(defun TraceValues (variable)
(if (not (Infer variable))
(Ask variable)))
(SetTraced variable))

(defun Infer (variable)
(dolist (rule (Select variable *rule-base* nil)
(eval variable))
(ApplyRule rule)))

;;
(defun Select (variable rules selected)
(cond ((null rules) selected)
(t (let ((rule (FirstRule rules)))
(if (Occurs variable (Consequent rule))
(Select variable (RestRules rules)
(cons rule selected))

;;
(defun Occurs (var conseq)
(cond ((null conseq) nil)
(t (if (eq (Var (FirstConclusion conseq)) var)
t
(Occurs var (RestConclusions conseq))))))
(Select variable (RestRules rules) selected))))))

;;
(defun ApplyRule (rule)
(if (EvalConditions? (Antecedent rule))
(EvalConclusions (Consequent rule))))

;;
(defun EvalConditions? (antecedent)
(eval antecedent))

;;
(defun EvalConclusions (consequent)
(mapc #’eval consequent))

;;
(defmacro Same (variable value)
‘(Compare #’member ’,variable ’,value))

(defmacro NotSame (variable value)
‘(and (Known ,variable)
(not (Same ,variable ,value))))

(defmacro Known (variable)
‘(not (Compare #’eq ’,variable nil)))

(defmacro LessThan (variable value)
‘(Compare #’> ’,variable ,value))
(defmacro Equals (variable value)
‘(Compare #’= ’,variable ,value))
(defmacro GreaterThan (variable value)
‘(Compare #’< ’,variable ,value))

(defun Compare (operator variable value)
(cond ((Traced? variable)
(if (or (eval variable) ; variable has a value?
(eq operator #’eq))
(funcall operator value (eval variable)))
(t (TraceValues variable)
(Compare operator variable value))))

(defmacro Add (variable value)
‘(set ’,variable (cons ’,value ,variable)))

(defun Ask (variable)
(when (GetPrompt variable)
(terpri)
(princ (GetPrompt variable))
(terpri)
(princ "-> ")
(let ((response (read)))
(unless (eq (first response) ’UNKNOWN)
(if (numberp (first response))
(set variable (first response))
(set variable response))))))
