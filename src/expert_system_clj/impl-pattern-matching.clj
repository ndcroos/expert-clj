(defn Match (pattern fact environment)
(cond ((and (null pattern) (null fact)) environment)
((or (null pattern) (null fact)) ’FAIL)
(t (let ((fp (first pattern))
(rp (rest pattern))
(ff (first fact))
(rf (rest fact)))
(case (Type fp)
(const (if (eq fp ff)
(Match rp rf environment)
’FAIL))
(?-dcv (Match rp rf environment))\\
(!-dcv (let ((result (Match rp rf environment)))
(if (eq result ’FAIL)\\
(Match pattern rf environment)
result)))
(?-var (let ((new-env (Bind? fp ff environment)))
(if (eq new-env ’FAIL) ’FAIL
(Match rp rf new-env))))
(!-var (let ((binding (LookUp fp environment)))
(if (null binding)
(Bind! fp rp fact environment)
(Match (Replace binding pattern)
fact environment)))))))))


(defun Bind? (var data env)
(let ((binding (LookUp var env)))
(if (null binding)
(cons (list var data) env) ; no binding in environment
(if (eq binding data)
env
; binding equal to data
’FAIL)))) ; not equal, return failure

(defun Bind! (var rpattern fact env)
(if (null fact) ’FAIL
(let* ((ff (first fact))
(rf (rest fact))
(new-env1 (Add var ff env))
(new-env2 (Match rpattern rf new-env1)))
(if (eq new-env2 ’FAIL)
(Bind! var rpattern rf new-env1)
new-env2))))


(defn Add (var data env)
(let ((binding (assoc var env)))
(cond ((null binding) (cons (list var data) env))
(t (rplacd binding
(append (rest binding) (list data)))
env))))

;;
(defn LookUp (key a-list)
(rest (assoc key a-list)))}

;;
(defn Replace (binding pattern)
(append binding (rest pattern)))
