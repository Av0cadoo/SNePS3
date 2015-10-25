;;; SNePS 3: SNeRE
;;; ==============
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

(defpackage :snere
  (:export #:attachPrimaction #:definePrimaction #:perform
	   #:actions))

(in-package :snere)

(defmacro definePrimaction (primaction vars &body forms)
  "Creates the function definition of the primitive action named PRIMACTION.
     VARS should be a (possibly empty) list of slot names
        that get bound to the appropriate node sets.
     However, if any VAR is enclosed in parentheses,
        it gets bound to a member of the appropriate node set.
     FORMS syntax is just as it is for `defuns'."
  (let ((act-node-var (gensym))
	(strippedvars 
	 (mapcar #'(lambda (v) (if (atom v) v (first v)))
		 vars)))
    `(prog1
	 (defun ,primaction (,act-node-var)
	   ,@(when (null vars)
	       `((declare (ignore ,act-node-var))))
	   ((lambda ,strippedvars ,@forms)
	    ,@(mapcar
	       #'(lambda (rel)
		   (if (atom rel)
		       `(sneps:findto ,act-node-var ',rel)
		     `(set:choose
		       (sneps:findto ,act-node-var ',(first rel)))))
	       vars)))
       (compile ',primaction))))

(defun attachPrimaction (term primfun)
  "Puts the function named primfun in the primaction slot
        of the Act or Action term."
  (check-type term (or sneps:Act sneps:Action))
  (cl:assert (and (symbolp primfun)
		  (fboundp primfun))
      (primfun)
    "~S is not the name of a function" primfun)
      (setf (sneps:primaction term) (symbol-function primfun)))

(defun perform (actform)
  "actform will be defined as an Act term (which might already exist).
       If there is a primitive action function for that act, it will be applied to the act;
       Else if the act has an actions slot
          and there is a primitive action function for that action, it will be applied to the act;
       Else an error will be raised."
  (let* ((act (sneps:build actform 'sneps:Act))
	 (todo (sneps:findto act 'actions))
	 (action (when (typep todo 'set:set)
		   (set:choose todo))))
    (cond ((functionp (sneps:primaction act))
	   (funcall (sneps:primaction act) act))
	  ((and action
		(functionp (sneps:primaction action)))
	   (funcall (sneps:primaction action) act))
	  (t (error "~&I don't know how to perform ~S.~%" act))
	  )))
