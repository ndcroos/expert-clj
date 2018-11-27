;;
(def complaint
(prompt "Enter the complaints of the patient.")
(class nil))

;; Goal variable.
(def diagnosis
(prompt nil)
(class goal))

;;
(defn ConsultationSystem ( )
(terpri)
(princ "Name of the knowledge base: ")
(let ((knowledge-base (open (read-line))))
(setq *var-decls* nil
*rule-base* nil)
(Parse knowledge-base)
(close knowledge-base)
(Consultation *var-decls*)))

(defvar *var-decls*)
(defvar *rule-base*)

;;
(defn Parse (knowledge-base)
(let [(exp (ReadExpression knowledge-base))]
(cond ((eq expr ’eof) nil)
(t (case (first expr)
(def (ParseDecl (rest expr)))
(rule (ParseRule (rest expr)))
(otherwise (error "Unknown keyword: ~A" (first expr))))
(Parse knowledge-base)))))

(defn ReadExpression (stream)
(read stream nil ’eof))

;;
(defn ParseDecl (expr)
(let [(variable (first expr)]
(spec (rest expr)))
(setf (get variable ’prompt) (Prompt spec)
(get variable ’class) (Class spec)
(get variable ’traced) nil) ; not traced
(set variable nil) ; value is unknown by default
(setq *var-decls* (append *var-decls*
(list variable)))))

;;
(defn Prompt (spec)
(cadr (assoc ’prompt spec)))

;;
(defn Class (spec)
(cadr (assoc ’class spec)))
