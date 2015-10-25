;;; SNePS 3: Assert Methods
;;; =======================
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

(in-package :context)

(defgeneric assert (expr context &key (origintag :hyp))
  (:documentation
   "Makes sure that the term expressed by expr is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
         and returns the term.")
  )

(defmethod assert (expr (context symbol) &key (origintag :hyp))
  "Makes sure that the term expressed by expr
       is asserted in the context named context,
         in a status indicated by origintag (either :hyp or :der)
    and returns the term."
  (assert expr (find-context context) :origintag origintag))

(defmethod assert ((expr symbol) (context context) &key (origintag :hyp))
  "Makes sure that the propositional atomic term expressed by expr
       is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
    and returns the term."
  (assert (sneps:build expr (find-class 'sneps:Proposition))
      context :origintag origintag)
  )

(defmethod assert ((expr number) (context context) &key (origintag :hyp))
  "Makes sure that the propositional atomic term expressed by expr
       is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
    and returns the term."
  (assert (sneps:build expr (find-class 'sneps:Proposition))
      context :origintag origintag))

(defmethod assert ((expr string) (context context) &key (origintag :hyp))
  "Makes sure that the propositional atomic term expressed by expr
       is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
    and returns the term."
  (assert (sneps:build expr (find-class 'sneps:Proposition))
      context :origintag origintag))

(defmethod assert ((expr cons) (context context) &key (origintag :hyp))
  "Makes sure that the propositional well-formed term expressed by expr
       is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
    and returns the term"
  (assert (variable-parse-and-build expr) context :origintag origintag))

(defmethod assert ((p sneps:Proposition) (context context) &key (origintag :hyp))
  "Makes sure that the proposition p
       is asserted in the given context,
         in a status indicated by origintag (either :hyp or :der)
    and returns p."
  (unless (assertedp p context)
    (case origintag
      (:hyp (util:protecting (ct-hyps context)
			     (set:add-item p (ct-hyps context))))
      (:der (util:protecting (ct-ders context)
			     (set:add-item p (ct-ders context))))))
   (if (and sneps3::*sneps-gui-ptr*
	    sneps3::*auto-refresh-graph*) 
     (sneps3::add-asserted-wft-to-graph p))
  p)

(defun unassert (prop &optional (cntxt (currentContext)))
  "Unasserts the proposition prop in the given context and all ancestor contexts."
  ;; Currently there is no belief revision,
  ;;    so propositions derived using prop might still be asserted,
  ;;    and prop, itself, might be rederivable.
  (let ((p (sneps:build prop 'sneps:Proposition)))
    (loop for context = (assertedp p cntxt)
	while context
	do (util:protecting (ct-hyps context)
			    (set:remove-item p (ct-hyps context)))
	   (util:protecting (ct-ders context)
			    (set:remove-item p (ct-ders context))))
    (if (and sneps3::*sneps-gui-ptr*
             sneps3::*auto-refresh-graph*)
        (sneps3::gui-unassert sneps3::*sneps-gui-ptr* (string (sneps3::name p))))))


(defun variable-parse-and-build  (expr)
  "Given a top-level build expression, checks that expression for
   variable terms syntax (e.g., every, some). These terms are built and 
   a new build expression is created with just the variable-labels
   (e.g., x in (every x ...)). This and a substitution between
   variable-labels and the AI and IO is provided to build. Also asserts into 
   the Base KB the restriction sets. Returns the built expression."
  (multiple-value-bind  (new-expr vars substitution)
      (sneps:check-and-build-variables expr)
    (loop 
      for var in vars
      do (set:loopset
	  for rst in (sneps:restriction-set var)
	  do (assert rst (ct:find-context 'BaseCT))))
    (sneps:build new-expr (find-class 'sneps:Proposition) substitution)))

(defun build-variable (var-expr)
  "This function should only be called when a single variable needs to be built
   indepndent of an assert. It is in assert because variable nodes need to assert 
   their restriction sets. Returns the variable node built."
  (multiple-value-bind  (new-expr vars substitution)
      (sneps:check-and-build-variables var-expr)
    (let ((var (first vars)))
      (set:loopset
       for rst in (sneps:restriction-set var)
       do (assert rst (ct:find-context 'BaseCT)))
      var)))

(defun assertedp (prop &optional (context (currentContext)))
  "If the proposition prop is asserted in the given context
        or one of its ancestors,
   returns the first context found in which it is asserted;
   else returns nil."
  (let ((p (sneps:build prop 'sneps:Entity)))
    (when (typep p 'sneps:Proposition)
      (cond ((or (set:member p (util:resource-value (ct-hyps context)))
		 (set:member p (util:resource-value (ct-ders context))))
	     context)
	    (;; Look at ancestor contexts
	     (some #'(lambda (c)
		       (assertedp p c))
		   (ct-parents context)))
	    (t nil)))))
