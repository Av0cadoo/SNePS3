;;; SNePS 3: Production-System-like Rules 
;;; =====================================
;;; Stuart C. Shapiro
;;; Department of Computer Science and Engineering
;;; State University of New York at Buffalo
;;; shapiro@buffalo.edu

;;; The contents of this file are subject to the University at Buffalo
;;; Public License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the
;;; License at http://www.cse.buffalo.edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;;; the License for the specific language governing rights and limitations
;;; under the License.
;;; 
;;; The Original Code is SNePS 3.
;;; 
;;; The Initial Developer of the Original Code is Research Foundation of
;;; State University of New York, on behalf of University at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C) 2007
;;; Research Foundation of State University of New York, on behalf of
;;; University at Buffalo. All Rights Reserved.
;;; 
;;; Contributor(s): ______________________________________.

(in-package :snuser)

(defmacro withInstances (variables of pattern &body forms)
  "For each asserted substitution instance of pattern, evaluates the forms in forms,
      with each variable in variables
         taking on the term appropriate for the instance.
   Question mark variables in pattern that are not in variables
      take on the values they should have gotten in an enclosing withInstances."
  ;; For example,
  ;; (withInstances (?x ?y) of '(Isa ?x ?y) (format t "~s is an instance of ~s.~%" ?x ?y))
  ;; or
  ;; (withInstances (?x ?y) of (Isa ?x ?y)
  ;;    (format t "~s is an instance of ~s.~%" ?x ?y)
  ;;    (withInstances (?z) of (Type ?y ?z)
  ;;       (format t "~s is an instance of ~s, and also of ~s.~%" ?x ?y ?z)))
  (declare (ignore of))
  (if variables
       `(let ,variables
     (declare (special ,@variables))
     (set:loopset for sub in (nth-value 1 (find (subNonLocals ',pattern ',variables)))
	do
	,@(mapcar #'(lambda (var) `(setf ,var (subs:var-to-term ',var sub)))
			  variables)
		(unless (set:emptyp (askif (subNonLocals ',pattern nil)))
		  ,@forms)))
    `(set:when (askif (subNonLocals ',pattern ()))
       ,@forms)))

(defun subNonLocals (pattern variables)
  "Returns an instance of pattern
      in which every ?-variable not in variables
      is replaced by its value,
      assuming that that value is a sneps:term."
  (cond ((and (sneps:qVarp pattern)
	      (not (member pattern variables)))
	 (symbol-value pattern))
	((atom pattern) pattern)
	(t (mapcar #'(lambda (subpat) (subNonLocals subpat variables))
		   pattern))))

(defmacro defrule (rulename &body rulebody)
  "Defines a rule with the given name,
      and a body looking like [description-string] [lhs... =>] rhs...
   An rhs element can be any form,
      including (:subrule [lhs... =>] rhs),
      to be evaluated in the binding environment of the lhs.
   An lhs element can be
      a pattern,
      a (:for elt in list) or any other loop for clause
      a (:when predicate) clause,
      an (:unless predicate) clause."
  `(defun ,rulename ()
     ,@(multiple-value-bind (lhs rhs)
       (lhsrhs rulebody)
       (expand-rule-body lhs rhs nil))))

(defun expand-rule-body (lhs rhs prevVars)
  (if lhs 
      (case (first (first lhs)) 
	(:break `((break)
		  ,@(expand-rule-body (rest lhs) rhs prevVars)))
	(:for `((loop for ,@(rest (first lhs)) do
		     ,@(expand-rule-body (rest lhs) rhs prevVars))))
	(:unless `((set:unless ,(second (first lhs))
		   ,@(expand-rule-body (rest lhs) rhs prevVars))))
	(:when `((set:when ,(second (first lhs))
		  ,@(expand-rule-body (rest lhs) rhs prevVars))))
	(t (let ((newVars (set-difference (?varsof (first lhs))
					  prevVars)))
	     `((withInstances
		,newVars of ,(first lhs)
		,@(expand-rule-body (rest lhs) rhs
				    (append prevVars newVars))))))) 
    (expand-rhs rhs prevVars)))

(defun expand-rhs (rhs prevVars)
  (loop for action in rhs
      when (eql (first action) :subrule)
      collect (first (multiple-value-call #'expand-rule-body
		       (lhsrhs (rest action)) prevVars))
      else collect action))

(defun ?varsof (list)
  "Returns a list of all those elements of list
   that are symbols starting with the character #\?."
  (remove-if-not #'(lambda (x)
		     (and (symbolp x)
			  (char= (elt (symbol-name x) 0) #\?)))
		 list))

(defun lhsrhs (body)
  "Returns two values:
      a list of the elements of body before the first occurrence of '=>;
      a list of the elements of body after the first occurrence of '=>;
   unless there is no occurrence  of '=> in body,
      in which case the two values are: nil; and body."
  ;; Allows a description string before the first element.
  (let ((realbody (if (stringp (first body))
		      (rest body)
		    body)))
    (loop for (elt . subbody) on realbody 
	until (eql elt '=>)
	collect elt into lhs
	finally (return (if subbody (values lhs subbody)
			  (values nil lhs))))))
