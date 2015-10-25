;;; SNePS 3: Ask Methods
;;; ====================
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

(defpackage :snip
  (:export
   #:*TRACE* #:*GOALTRACE*
   #:ask #:askif #:ant #:cq #:inferByForward #:pathsfrom)
  (:import-from :sneps
		#:nand #:nor #:andor #:thresh #:andorargs #:threshargs
		#:xor #:thnot #:thnor #:iff #:equivalence
		#:numericalentailment))

(in-package :snip)

(defparameter *TRACE* nil
  "If non-nil, inference will be traced when rules fire.")

(defparameter *GOALTRACE* nil
  "If non-nil, inference will be traced
         when (sub)goals are generated,
         and when (sub)goals are found asserted in the KB.")

(defgeneric askif (proposition context termstack)
  (:documentation
   "Returns a set of instances of the given proposition
         that are derivable in the given context.
         The termstack is a stack of propositions
             that this goal is a subgoal of."))

(defmethod askif ((pname symbol) (context ct:context) termstack)
  "If the proposition expressed by pname is derivable in context,
        returns a singleton set of that proposition;
        else returns the empty set.
        The termstack is a stack of propositions
            that this goal is a subgoal of."
  (askif (sneps:build pname 'sneps:Proposition) context termstack))

(defmethod askif ((p sneps:Proposition) (context ct:context) termstack)
  "If the proposition p is derivable in context,
        return a singleton set of that proposition;
        else return the empty set
         The termstack is a stack of propositions
             that this goal is a subgoal of.."
  (when *GOALTRACE* (format *trace-output* "~&I wonder if ~S~%" p))
  (cond
   ((ct:assertedp p context)
    (when *GOALTRACE* (format *trace-output* "~&I know that ~S~%" p))
    (set:new-set :items (list p)))
   (t (set:or.set
       ;; Next see if it is derivable by sort-based inference
       (sort-based-derivable p context)
       ;; Next see if it is derivable by path-based inference.
       ;; Pure path-based inference
       ;;    is a special case of slot&path-based inference.
       ;; So comment it out to save the extra effort. scs 11/6/2012
       ;; (path-based-derivable p context)
       ;; Next see if it is derivable by slot&path-based inference.
       (slot-based-derivable p context termstack)
       ;; Next see if it is derivable by subsumption-based inference.
       ;; When this is implemented, it will do derivations like
       ;;      (Property (any x (Isa x Raven)) black),
       ;;      (Isa Ralph Raven),
       ;;      |- (Property Ralph black)
     
       ;; Next see if it is derivable by Natural Deduction.
       (natural-deduction-derivable p context termstack)
       ))))

(defgeneric derivable (proposition context)
  (:documentation
   "If any instances of the given proposition are derivable in the given context,
         returns a set of those instances;
      else returns the empty set.")
  (:method ((p sneps:Proposition) (c ct:context))
	   "If no specific derivable method is available,
                     return the empty set."
	   set:*emptyset*))

(defun sort-based-derivable (p context)
  "If the categorization Proposition p
          is derivable in the given context
          accordng to sort-based inference
          returns a singleton set of that proposition;
     else returns the empty set ."
  ;; sort-based-derivable only considers the sorts of terms.
  ;; So it doesn't consider any term that logically implies p.
  ;; So it doesn't need a termstack argument.
  (unless (typep p 'sneps:categorization)
    (return-from sort-based-derivable set:*emptyset*))    
  (when *GOALTRACE* 
    (format *trace-output* "~&I will consider using Sort-Based inference.~%"))
  (set:loopset for member in (sneps3:findto p 'member)
	       do(set:loopset for class in (sneps3:findto p 'class)
			      unless (and (sneps:semantic-type-p
					   (sneps::name class))
					  (typep member (sneps::name class)))
			      do(return-from sort-based-derivable 
				  set:*emptyset*)))
  (assertTrace nil nil p "Sort-Based inference" context)
  (set::new-set :items (list p)))

(defun assertTrace (rule antecedents consequent reason context)
  "Asserts consequent in the given context,
        if appropriate,
           prints a trace
               that the consequent has been derived
               from the rule and the antecedents (a set or list)
               according to the given reason,
        and returns a singleton set of the consequent."
  ;; Assert the consequent after the "Since"s are printed
  ;; but before the "I infer" is printed.
  (when *TRACE*
    (let ((first t))
      (when rule
	(format *trace-output* "~&Since ~S~%" rule)
	(setf first nil))
      (etypecase antecedents
	(null)
	(cons 
	 (loop for ant in antecedents
	     unless (eq ant consequent)
	     when first
	     do (format *trace-output* "~&Since ~S~%" ant)
	     and do (setf first nil)
	     else
	     do (format *trace-output* "~Tand ~S~%" ant)))
	(set:set
	 (set:loopset for ant in antecedents
		      unless (eq ant consequent)
		      when first
		      do (format *trace-output* "~&Since ~S~%" ant)
		      and do (setf first nil)
		      else
		      do (format *trace-output* "~Tand ~S~%" ant))))))
  (ct:assert consequent context :origintag :der)
  (when *TRACE*
    (format *trace-output* "~&I infer ~S by ~A.~%" consequent reason))
  (set:singleton consequent))
