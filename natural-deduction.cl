l;;; SNePS 3: Natural Deduction Inference Engine
;;; ===========================================
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

(in-package :snip)

(defun natural-deduction-derivable (term context termstack)
  "If any instances of the proposition term
         are derivable by any natural deduction rules in the given context,
         return a set of those propositions;
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of."
  (check-type term sneps:Proposition)
  (check-type context ct:context)
  (set:or.set
   ;; A version of eager-beaver or should be used eventually.
   (inferByIntroduction term context termstack)
   (inferByEliminationRules term context termstack)
   ))

(defgeneric inferByIntroduction (term context termstack)
  (:documentation
      "Returns a set of instances of the given proposition
              that are derivable in the given context.
           The termstack is a stack of propositions
              that this goal is a subgoal of.")
  (:method ((term sneps:term) (context ct:context) termstack)
	   "The default case is that the term cannot be derived."  
	   (declare (ignore termstack))
	   set:*emptyset*))

(defmethod inferByIntroduction ((term sneps:conjunction) context termstack)
  "If every conjunct of the conjunction, term, is derivable,
          asserts term, and returns a singleton set containing term;
    Otherwise, returns the empty set.
    The termstack is a stack of propositions
        that this goal is a subgoal of."
  (when (onTermStack term termstack)
    (return-from inferByIntroduction set:*emptyset*))
  (push term termstack) 
  (let ((conjuncts (sneps:findto term 'and)))
    (when(set:set-some
	  #'(lambda (conjunct) (onTermStack conjunct termstack))
	  conjuncts)
      ;; If any conjunct is in termstack,
      ;; return the empty set to cut infinite recursion.
      (return-from inferByIntroduction set:*emptyset*))
    (when *GOALTRACE*
      (format *trace-output*
	      "~&I will consider using And Introduction.~%"))
    (cond (;; If any conjunct can't be derived, return the emptyset.
	   (set:set-some
	    #'(lambda (conjunct)
		(set:emptyp
		 (askif conjunct context (cons term termstack))))
	    conjuncts)
	   set:*emptyset*)
	  (t ;; assert term and return a set containing it.
	   (assertTrace nil conjuncts term "And Introduction" context)))))

(defmethod inferByIntroduction ((term sneps:negation) context termstack)
  "If the negation of every argument of term, is derivable,
          asserts term, and returns a singleton set containing term;
    Otherwise, returns the empty set.
    The termstack is a stack of propositions
        that this goal is a subgoal of."
  ;; This is really And Introduction
  ;; (not p1), ..., (not pn) |- (and (not p1), ..., (not pn))
  ;;                            = (not (or p1 ... pn))
  ;;                            = (nor p1 ... pn)
  (when (onTermStack term termstack)
    (return-from inferByIntroduction set:*emptyset*))
  (push term termstack)
  (let ((arguments (sneps:findto term 'nor)))
    (when (= (set:cardinality arguments) 1)
      (return-from inferByIntroduction set:*emptyset*))    
    (when (set:set-some
	   #'(lambda (argument) (onTermStack argument termstack))
	   arguments)
      (return-from inferByIntroduction set:*emptyset*))
    (when *GOALTRACE*
      (format *trace-output*
	      "~&I will consider using Nor Introduction.~%"))
    (let ((negatedargs
	   (set:loopset for arg in arguments
			collect (notprop arg))))
      (if (every
	   #'(lambda (narg)
	       (not 
		(set:emptyp
		 (askif narg context (cons term termstack)))))
	   negatedargs)
	  (assertTrace nil negatedargs term
		       "Nor Introduction" context)
	set:*emptyset*))))

(defmethod inferByIntroduction ((term sneps:negationbyfailure)
				context termstack)
  "If no argument of term is derivable,
          asserts term, and returns a singleton set containing term;
    Otherwise, returns the empty set.
    The termstack is a stack of propositions
        that this goal is a subgoal of."
  (when (onTermStack term termstack)
    (return-from inferByIntroduction set:*emptyset*))
  (let ((arguments (sneps:findto term 'thnor)))
    ;; Don't check if any argument is on termstack,
    ;; because may repeat goals
    ;;             that are outside negation by failure
    ;;         within negation by failure reasoning.
    (when *GOALTRACE*
      (format *trace-output*
	      "~&I will consider using Negation By Failure.~%"))
    (let ((assertedarg
	   (set:find-if #'(lambda (arg) (ct:assertedp arg context))
			arguments)))
      (when assertedarg
	;; Some argument is asserted.
	(when *GOALTRACE*
	  (format *trace-output* "~&I know that ~S~%"
		  assertedarg))
	(return-from inferByIntroduction set:*emptyset*))
      (cond (;; If any argument can be derived, return the emptyset.
	     (set:set-some
	      #'(lambda (arg)
		  (not (set:emptyp
			;; Use fresh termstack because
			;; may repeat goals
			;;     that are outside negation by failure
			;; within negation by failure reasoning.
			(askif arg context (list term)))))
	      arguments)
	     set:*emptyset*)
	    (t ;; assert term and return a set containing it.
	     (assertTrace nil nil term "Negation By Failure" context))))))

(defmethod inferByIntroduction ((term sneps:andor) context termstack)
  (inferByParam2OpIntroduction term context termstack 'andor))

(defmethod inferByIntroduction ((term sneps:thresh) context termstack)
  (inferByParam2OpIntroduction term context termstack 'thresh))

(defmethod inferByIntroduction ((term sneps:implication) context termstack)
  "If the consequents of the implication, term,
        can be derived in a new context
            in which the antecedents have been assumed,
        returns a singleton set containing term;
        Otherwise, returns the empty set.
        The termstack is a stack of propositions
            that this goal is a subgoal of."
  (when (onTermStack term termstack)
    (return-from inferByIntroduction set:*emptyset*))
  (push term termstack)
  (let ((consequents (sneps:findto term 'cq)))
    (when (set:set-some #'(lambda (cq) (onTermStack cq termstack))
			consequents)
      ;; A higher goal is to derive one of the consequents.
      ;; So don't try to use implication introduction on this implication,
      ;;    which will also require deriving that same consequent
      ;;       in an inner context.
      (return-from inferByIntroduction set:*emptyset*))
    (when *GOALTRACE*
      (format *trace-output*
	      "~&I will consider using Implication Introduction.~%"))
    (let* ((antecedent
	    (sneps:build `(and ,(sneps:findto term 'ant)) 'sneps:Proposition))
	   (consequent
	    (sneps:build `(and ,consequents) 'sneps:Proposition))
	   (newCT
	    (ct:defineContext (gensym "CT")
		:parents `(,context) :hyps `(,antecedent))))
      (when *TRACE* 
	(format *trace-output* "~&Let me assume that ~S~%" antecedent))
      (cond ((set:emptyp
	      (askif consequent newCT (cons term termstack)))
	     set:*emptyset*)
	    (t
	     (when *TRACE*
	       (format *trace-output*
		       "~&Since ~S can be derived after assuming ~S~%"
		       consequent antecedent))
	     (ct:assert term context :origintag :der)
	     (when *TRACE*
	       (format *trace-output*
		       "I infer ~S by Implication Introduction.~%"
		       term))
	     (set:singleton term))))))

(defmethod inferByIntroduction ((term numericalentailment) context termstack)
  ;; Given (i=> antecedents consequents),
  ;;    form all subsets of antecedents of size i,
  ;;    and make sure that each of them implies the consequents.
  (when (onTermStack term termstack)
    (return-from inferByIntroduction set:*emptyset*))
  (push term termstack)
  (let ((consequents (sneps:findto term 'cq)))
    (when (set:set-some #'(lambda (cq) (onTermStack cq termstack))
			consequents)
      ;; A higher goal is to derive one of the consequents.
      ;; So don't try to use numerical entailment introduction on this entailment.
      ;;    which will also require deriving that same consequent
      ;;       in an inner context.
      (return-from inferByIntroduction set:*emptyset*))
    (when *GOALTRACE*
      (format *trace-output*
	      "~&I will consider using Numericalentailment Introduction.~%"))
    (let ((implications
	   (mapcar #'(lambda (ss) 
		       (sneps:build `(if (set:setof ,@ss) ,consequents)
				    'sneps:Proposition))
		   (subsetsOfSize (sneps:minparam term)
				  (sneps:findto term 'ant)))))
      (loop for implication in implications
	  when (set:emptyp (askif implication context termstack))
	  do (return-from inferByIntroduction set:*emptyset*))
      (assertTrace nil implications term (reason term :i) context))))

(defun inferByParam2OpIntroduction (term context termstack type)
  "The term is (type (min max) args), where type is andor or thresh
           and the number of arguments is tot.
     Inference can terminate
        as soon as one of the following is determined to hold:
        (1) The number of args asserted/derived is > max
            or the number of negated args asserted/derived is > (tot-min)
        (2) The number of args asserted/derived is >= min
            and the number of negated args asserted/derived is >= (tot-max)
     If type is andor, in case (1) the derivation fails,
                       and in case (2) the derivation succeeds.
     If type is thresh, in case (1) the derivation succeeds,
                        and in case (2) the derivation fails.
     If the derivation succeeds, asserts term,
        and returns a singleton set containing it.
     If the derivation fails, returns the empty set.
     The termstack is a stack of propositions
         that this goal is a subgoal of."
  ;; Note that all termination tests use > or >=, to allow for unknowns.
  (when (onTermStack term termstack)
    (return-from inferByParam2OpIntroduction set:*emptyset*))
  (push term termstack)
  (let* ((arguments
	  (sneps:findto term
			(case type
			  (thresh 'threshargs)
			  (andor 'andorargs))))
	 (min (sneps:minparam term))
	 (max (sneps:maxparam term))
	 (tot (sneps:totparam term))
	 (tot-min (- tot min))
	 (tot-max (- tot max))
	 (reason (reason term :i)))
    (when *GOALTRACE*
      (format *trace-output* "~&I will consider using ~A.~%" reason))
    (let* ((numasserted 0)
	   (assertedargs
	    ;; Collect arguments that are already asserted.
	    (set:loopset for arg in arguments
			 until (> numasserted max)
			 when (ct:assertedp arg context)
			 collect arg
			 and do (incf numasserted)
			 and when *GOALTRACE*
			 do (format *trace-output*
				    "~&I know that ~S~%" arg))))
      (when (> numasserted max)
	(return-from inferByParam2OpIntroduction
	  (case type
	    (andor set:*emptyset*)
	    (thresh (assertTrace nil assertedargs term
				 reason context)))))
      (let* ((negatedargs
	      (set:loopset for arg in arguments
			   unless (member arg assertedargs)
			   collect (notprop arg)))
	     (numnegasserted 0)
	     (assertednegatedargs
	      ;; Collect arguments whose negations are asserted
	      (loop for narg in negatedargs
		  until (or (and (>= numasserted min)
				 (>= numnegasserted tot-max))
			    (> numnegasserted tot-min))
		  when (ct:assertedp narg context)
		  collect narg
		  and do (incf numnegasserted)
		  and when *GOALTRACE*
		  do (format *trace-output*
			     "~&I know that ~S~%" narg))))
	(cond ((> numnegasserted tot-min)
	       (return-from inferByParam2OpIntroduction
		 (case type
		   (thresh (assertTrace nil assertednegatedargs term
					reason context))
		   (andor  set:*emptyset*))))
	      ((and (>= numasserted min)
		    (>= numnegasserted tot-max))
	       (return-from inferByParam2OpIntroduction
		 (case type
		   (thresh  set:*emptyset*)
		   (andor(assertTrace
			  nil
			  (nconc assertedargs assertednegatedargs)
			  term reason context))))))
	(let* ((numderived numasserted)
	       (derivedargs
		;; Collect derivable arguments.
		(union assertedargs
		       (set:loopset for arg in arguments
				    until (or (and (>= numnegasserted
						       tot-max)
						   (>= numderived min))
					      (> numderived max))
				    unless
				    (or (member arg assertedargs)
					(member (notprop arg)
						assertednegatedargs)
					(onTermStack arg termstack)
					(set:emptyp 
					 (askif arg context
						(cons term termstack))))
				    collect arg
				    and do (incf numderived)))))
	  (cond ((> numderived max)
		 (return-from inferByParam2OpIntroduction 
		   (case type
		     (thresh (assertTrace nil derivedargs term
					  reason context))
		     (andor set:*emptyset*))))
		((and (>= numderived min)
		      (>= numnegasserted tot-max))
		 (return-from inferByParam2OpIntroduction
		   (case type
		     (thresh set:*emptyset*)
		     (andor (assertTrace
			     nil
			     (nconc derivedargs assertednegatedargs)
			     term reason context))))))
	  (let* ((numnegderived numnegasserted)
		 (derivednegatedargs
		  ;; Collect arguments whose negations are derivable
		  (union assertednegatedargs
			 (set:loopset for arg in arguments
				      for negarg = (notprop arg)
				      until (> numnegderived tot-min)
				      unless
				      (or (member arg derivedargs)
					  (member negarg assertednegatedargs)
					  (onTermStack negarg termstack)
					  (set:emptyp 
					   (askif negarg context
						  (cons term termstack))))
				      collect negarg
				      and do (incf numnegderived)))))
	    (cond ((> numnegderived (- tot min))
		   (case type
		     (andor set:*emptyset*)
		     (thresh (assertTrace nil derivednegatedargs
					  term reason context))))
		  ((and (>= numderived min)
			(>= numnegderived (- tot max)))
		   (case type
		     (andor (assertTrace
			     nil
			     (nconc derivedargs derivednegatedargs)
			     term reason context))
		     (thresh set:*emptyset*)))
		  (t set:*emptyset*))))))))

(defun inferByEliminationRules (term context termstack)
  "If any instances of the proposition term
         are derivable by any natural deduction elimination rules
                              in the given context,
         return a set of those propositions;
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of."
  (if (onTermStack term termstack)
      set:*emptyset*
    (set:or.set
     ;; A version of eager-beaver or should be used eventually.
     (inferByEliminationUsing term context termstack 'and "And"
			      #'inferByAndElimination t)
     ;; Nor Elimination is done by Slot-based inference
     ;;     and by buildCanonicalNegation
     (inferByEliminationUsing term context termstack
			      'nor "Negation"
			      #'inferNegByNegationElimination nil)
     (inferByEliminationUsing term context termstack
			      'andorargs "varieties of Andor"
			      #'inferPosByAndorElimination t)
     (inferByEliminationUsing term context termstack
			      'andorargs "varieties of Andor"
			      #'inferNegByAndorElimination nil)
     (inferByEliminationUsing term context termstack
			      'threshargs "varieties of Thresh"
			      #'inferPosByThreshElimination t)
     (inferByEliminationUsing term context termstack
			      'threshargs "varieties of Thresh"
			      #'inferNegByThreshElimination nil)
     (inferByEliminationUsing term context termstack 'cq
			      "varieties of Numericalentailment"
			      #'inferByEntailmentElimination t)
     )))

(defun inferByEliminationUsing (term context termstack
				slot methodname method posp)
  "Finds all dominating terms not on the termstack
       and in which the given term is a filler of the given slot.
       (If posp is nil, however, looks for terms
           in which the negation of the given term
           is a filler of the given slot.)
     If there are any such dominating terms,
        tries to use the given method, whose name is methodname,
        to derive term from the dominating terms.
     As soon as this is successful once, returns a singleton of term.
     If none of the dominating terms can derive term,
        returns the empty set.
     The termstack is a stack of propositions
         that this goal term is a subgoal of."
  ;; At first only ground Propositions are dealt with.
  (let ((dominators (viableDominators (if posp term (notprop term))
				      slot termstack)))
    (unless dominators
      (return-from inferByEliminationUsing set:*emptyset*))
    (when *GOALTRACE* 
      (format *trace-output* "~&I will consider using ~A Elimination.~%"
	      methodname))
    (loop for dominator in dominators
	for derived = (funcall method
			       term dominator context termstack)
	unless (set:emptyp derived)
	do (return-from inferByEliminationUsing derived))
    set:*emptyset*))

(defun inferByAndElimination (term dominatingConjunction context
			      termstack)
  "If any instances of the proposition term
         are derivable by And Elimination in the given context,
         from the term dominatingConjunction,
         return a set of those propositions;
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known
             that dominatingConjunction is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (if (set:emptyp
       (askif dominatingConjunction context (cons term termstack)))
      set:*emptyset*
    (assertTrace dominatingConjunction nil term "And Elimination"
		 context)))

(defun inferPosByAndorElimination (term dominatingAndor context
				   termstack)
  "If dominatingAndor is derivable,
         and the negations of tot-min of its arguments other than term are also derivable
         return a set of the term
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known that dominatingAndor is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (let* ((arguments (otherFillers dominatingAndor 'andorargs term))
	 (max (sneps:maxparam dominatingAndor))
	 (tot-min (- (sneps:totparam dominatingAndor)
		     (sneps:minparam dominatingAndor)))
	 (reason (reason dominatingAndor :e)))
    (multiple-value-bind (assertedargs unassertedargs)
	(assertedAndNot arguments context)
      (if (>= (length assertedargs) max)
	  ;; No other argument should be asserted
	  set:*emptyset*
	(let ((negatedargs
	       (delete-if #'(lambda (narg) (onTermStack narg termstack))
			  (loop for arg in unassertedargs
			      collect (notprop arg)))))
	  (if (< (length negatedargs) tot-min)
	      ;; Not enough negated arguments to make the inference
	      set:*emptyset*
	    (cond ((set:emptyp (askif dominatingAndor context
				      (cons term termstack)))
		   ;; dominatingAndor can't be derived.
		   ;; So can't derive term using it.
		   set:*emptyset*)
		  (t (push dominatingAndor termstack)
		   (let* ((assertednegatedargs
			     (loop for narg in negatedargs
				 when (ct:assertedp narg context)
				 collect narg
				 and when *GOALTRACE*
				 do (format *trace-output*
					    "~&I know that ~S~%" narg)))
			    (numderivednegatedargs
			     (length assertednegatedargs))
			    (derivednegatedargs
			     (loop for narg in negatedargs
				 until (>= numderivednegatedargs tot-min)
				 when (and
				       (not (member narg assertednegatedargs))
				       (not (set:emptyp
					     (askif narg context
						    (cons term termstack)))))
				 collect narg
				 and do (incf numderivednegatedargs))))
		       (if (>= numderivednegatedargs tot-min)
			   (assertTrace dominatingAndor 
					(nconc assertednegatedargs
					       derivednegatedargs)
					term reason context)
			 set:*emptyset*))))))))))

(defun inferNegByAndorElimination (term dominatingAndor context
				   termstack)
  "If dominatingAndor is derivable,
         and  max of its arguments other than the negation of term
              are also derivable
         return a set of the term
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known that dominatingAndor is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (let* ((arguments (otherFillers dominatingAndor
				  'andorargs (notprop term)))
	 (max (sneps:maxparam dominatingAndor))
	 (tot-min (- (sneps:totparam dominatingAndor)
		     (sneps:minparam dominatingAndor)))
	 (reason (reason dominatingAndor :e)))
    (multiple-value-bind (assertedargs unassertedargs)
	(assertedAndNot arguments context)
      (let ((numderivedargs (length assertedargs)))
	(if (>= numderivedargs max)
	    (cond ((set:emptyp (askif dominatingAndor context
				      (cons term termstack)))
		   set:*emptyset*)
		  (t (push dominatingAndor termstack)
		     (when *GOALTRACE*
		       (mapc #'(lambda (arg)
				 (format *trace-output*
					 "~&I know that ~S~%" arg))
			     assertedargs))
		     (assertTrace dominatingAndor assertedargs 
				  term reason context)))
	  (let ((numassertednegatedargs
		 (length (loop for arg in arguments
			     for negarg = (notprop arg)
			     when (ct:assertedp negarg context)
			     collect negarg))))
	    (if (>= numassertednegatedargs tot-min)
		set:*emptyset*
	      (if (set:emptyp (askif dominatingAndor context
				     (cons term termstack)))
		  set:*emptyset* 
		(let ((derivedargs
		       (loop for arg in unassertedargs
			   until (>= numderivedargs max)
			   unless (or (onTermStack arg termstack)
				      (set:emptyp 
				       (askif arg context
					      (cons term termstack))))
			   collect arg
			   and do (incf numderivedargs))))
		  (if (>= numderivedargs max)
		      (assertTrace dominatingAndor
				   (nconc assertedargs derivedargs)
				   term reason context)
		    set:*emptyset*))))))))))

(defun inferPosByThreshElimination (term dominatingThresh context
				    termstack)
  "If dominatingThresh is derivable,
         and at least min arguments other than term are derivable
         and the negations of tot-max-1 of its arguments
              other than term are also derivable
         return a set of the term
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known that dominatingThresh is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (when (typep dominatingThresh 'nand)
    ;; Can't use a nand to derive one of its arguments positively.
    (return-from inferPosByThreshElimination set:*emptyset*))
  (when (typep dominatingThresh 'equivalence)
    (return-from inferPosByThreshElimination
      (inferPosByEquivalenceElimination term dominatingThresh context
					termstack)))
  (let* ((arguments (otherFillers dominatingThresh 'threshargs term))
	 (min (sneps:minparam dominatingThresh))
	 (max (sneps:maxparam dominatingThresh))
	 (tot-max-1 (- (sneps:totparam dominatingThresh) max 1))
	 (reason (reason dominatingThresh :e)))
    (multiple-value-bind (derivedargs unassertedargs)
	(assertedAndNot arguments context)
      (let ((numderived (length derivedargs)))
	(if (> numderived max)
	    ;; No way to derive additional arguments
	    set:*emptyset*
	  (cond ((set:emptyp
		  (askif dominatingThresh context (cons term termstack)))
		 set:*emptyset*)
		(t ;; Dominating thresh is derivable
		 (push dominatingThresh termstack)
		 (when *GOALTRACE*
		   (mapc #'(lambda (arg)
			     (format *trace-output*
				     "~&I know that ~S~%" arg))
			 derivedargs))
		 (let ((unknownargs
			(loop for arg in unassertedargs
			    unless (onTermStack arg termstack)
			    if (set:emptyp
				(askif arg context (cons term termstack)))
			    collect arg
			    else do (push arg derivedargs)
			    and do (incf numderived))))
		   (if (or (< numderived min)
			   (> numderived max))
		       ;; No way to derive additional arguments
		       set:*emptyset*
		     (let ((negatedargs
			    (delete-if #'(lambda (narg)
					   (onTermStack narg termstack))
				       (loop for arg in unknownargs
					   collect (notprop arg)))))
		       (if (< (length negatedargs) tot-max-1)
			   ;; Not enough negated arguments to make the inference
			   set:*emptyset*
			 (let* ((assertednegatedargs
				 (loop for narg in negatedargs
				     when (ct:assertedp narg context)
				     collect narg
				     and when *GOALTRACE*
				     do (format *trace-output*
						"~&I know that ~S~%" narg)))
				(numderivednegatedargs
				 (length assertednegatedargs))
				(derivednegatedargs
				 (loop for narg in negatedargs
				     when (and
					   (not (member narg
							assertednegatedargs))
					   (not (set:emptyp
						 (askif narg context
							(cons term
							      termstack)))))
				     collect narg
				     and do (incf numderivednegatedargs))))
			   (if (= numderivednegatedargs tot-max-1)
			       (assertTrace dominatingThresh
					    (nconc derivedargs
						   assertednegatedargs
						   derivednegatedargs)
					    term reason context)
			     set:*emptyset*)))))))))))))

(defun inferNegByThreshElimination (term dominatingThresh context
				    termstack)
  "If dominatingThresh is derivable,
         and min-1 arguments other than (not term) are derivable
         and the negations of at least tot-max of its other arguments
              are also derivable
         return a set of the term
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known that dominatingThresh is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (when (typep dominatingThresh 'equivalence)
    (return-from inferNegByThreshElimination
      (inferNegByEquivalenceElimination term dominatingThresh context
					termstack)))
  (let* ((arguments (otherFillers dominatingThresh
				  'threshargs (notprop term)))
	 (min (sneps:minparam dominatingThresh))
	 (min-1 (1- min))
	 (tot (sneps:totparam dominatingThresh))
	 (tot-max (- tot (sneps:maxparam dominatingThresh)))
	 (reason (reason dominatingThresh :e)))
    (multiple-value-bind (derivedargs unassertedargs)
	(assertedAndNot arguments context)
      (let ((numderived (length derivedargs)))
	(if (>= numderived min)
	    ;; No way to derive negated arguments
	    set:*emptyset*
	  (cond ((set:emptyp
		  (askif dominatingThresh context (cons term termstack)))
		 set:*emptyset*)
		(t ;; Dominating thresh is derivable
		 (push dominatingThresh termstack)
		 (when *GOALTRACE*
		   (mapc #'(lambda (arg)
			     (format *trace-output*
				     "~&I know that ~S~%" arg))
			 derivedargs))
		 (let ((unknownargs
			(loop for arg in unassertedargs
			    unless (onTermStack arg termstack)
			    if (set:emptyp
				(askif arg context
				       (cons term termstack)))
			    collect arg
			    else do (push arg derivedargs)
			    and do (incf numderived))))
		   (if (/= numderived min-1)
		       ;; No way to derive additional arguments
		       set:*emptyset*
		     (let ((negatedargs
			    (delete-if #'(lambda (narg)
					   (onTermStack narg termstack))
				       (loop for arg in unknownargs
					   collect (notprop arg)))))
		       (if (< (length negatedargs) tot-max)
			   ;; Not enough negated arguments to make the inference
			   set:*emptyset*
			 (let* ((assertednegatedargs
				 (loop for narg in negatedargs
				     when (ct:assertedp narg context)
				     collect narg
				     and when *GOALTRACE*
				     do (format *trace-output*
						"~&I know that ~S~%" narg)))
				(numderivednegatedargs
				 (length assertednegatedargs)))
			   (if (>= numderivednegatedargs tot-max)
			       (assertTrace dominatingThresh
					    (nconc derivedargs
						   assertednegatedargs)
					    term reason context)
			     (let ((derivednegatedargs
				    (loop for narg in negatedargs
					until (>=
					       numderivednegatedargs
					       tot-max)
					when (and
					      (not
					       (member narg
						       assertednegatedargs))
					      (not (set:emptyp
						    (askif narg context
							   (cons term
								 termstack)))))
					collect narg
					and do (incf
						numderivednegatedargs))))
			       (if (>= numderivednegatedargs tot-max)
				   (assertTrace dominatingThresh
						(nconc derivedargs
						       assertednegatedargs
						       derivednegatedargs)
						term reason context)
				 set:*emptyset*)))))))))))))))

(defun inferPosByEquivalenceElimination (term dominatingIff context
					 termstack)
  "If the term dominatingIff is derivable,
       and any argument of the equivalence other than term is also derivable,
     then return a singleton set of term;
     else return the empty set.
     The termstack is a stack of propositions
         that this goal is a subgoal of,
         and it's already known
             that dominatingIff is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (when (set:emptyp
	 (askif dominatingIff context (cons term termstack)))
    (return-from inferPosByEquivalenceElimination set:*emptyset*))
  (push dominatingIff termstack)
  (let* ((arguments (otherFillers dominatingIff 'threshargs term))
	 (assertedarg (find-if #'(lambda (arg)
				   (ct:assertedp arg context))
			       arguments))
	 (reason (reason dominatingIff :e)))
    (cond (assertedarg
	   (when *GOALTRACE*
	     (format *trace-output* "~&I know that ~S~%"
		     assertedarg))
	   (assertTrace dominatingIff (list assertedarg)
			term reason context))
	  (t (push dominatingIff termstack)
	     (loop for arg in arguments
		 unless (or (onTermStack arg termstack)
			    (set:emptyp 
			     (askif arg context
				    (cons term termstack))))
		 do (return-from inferPosByEquivalenceElimination
		      (assertTrace dominatingIff (list arg)
				   term reason context)))
	     set:*emptyset*))))

(defun inferNegByEquivalenceElimination (term dominatingIff context
					 termstack)
  "If the term dominatingIff is derivable,
       and any argument of the equivalence other than (not term)
           is also derivable,
     then return a singleton set of term;
     else return the empty set.
     The termstack is a stack of propositions
         that this goal is a subgoal of,
         and it's already known
             that dominatingIff is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (when (set:emptyp
	 (askif dominatingIff context (cons term termstack)))
    (return-from inferNegByEquivalenceElimination set:*emptyset*))
  (push dominatingIff termstack)
  (let* ((arguments
	  (otherFillers dominatingIff 'threshargs (not term)))
	 (assertednarg (find-if
			#'(lambda (arg)
			    (ct:assertedp (notprop arg) context))
			arguments))
	 (reason (reason dominatingIff :e)))
    (cond (assertednarg
	   (setf assertednarg (notprop assertednarg))
	   (when *GOALTRACE*
	     (format *trace-output* "~&I know that ~S~%"
		     assertednarg))
	   (assertTrace dominatingIff (list assertednarg)
			term reason context))
	  (t (loop for arg in arguments
		 for narg = (notprop arg)
		 unless (or (onTermStack narg termstack)
			    (set:emptyp 
			     (askif narg context
				    (cons term termstack))))
		 do (return-from inferNegByEquivalenceElimination
		      (assertTrace dominatingIff (list narg)
				   term reason context)))
	     set:*emptyset*))))

(defun inferByEntailmentElimination (term entailment context termstack)
  "If the numerical entailment (i=>), is derivable
        and  if i of its antecedents
        are derivable in the given context,
        return a singleton set of term,
                  presumed to be a consequent of entailment;
        else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known
             that entailment is not on termstack."
  ;; At first only ground Propositions are dealt with.
  (if (set:emptyp (askif entailment context (cons term termstack)))
      set:*emptyset*
    (let ((antecedents (sneps:findto entailment 'ant))
	  (i (sneps:minparam entailment))
	  (reason (reason entailment :e)))
      (multiple-value-bind (assertedants unknownants)
	  (assertedAndNotset antecedents context)
	(let ((numderived (length assertedants)))
	  (when (>= numderived i)
	    (let ((triggers (subseq assertedants 0 i)))
	      (when *GOALTRACE*
		(mapc #'(lambda (ant)
			  (format *trace-output* "~&I know that ~S~%" ant))
		      triggers))
	      (return-from inferByEntailmentElimination
		(assertTrace entailment triggers term reason context))))
	  (loop for ant in unknownants
	      until (= numderived i)
	      unless (or (onTermStack ant termstack)
			 (set:emptyp 
			  (askif ant context (cons term termstack))))
	      do (push ant assertedants)
	      and do (incf numderived))
	  (return-from inferByEntailmentElimination
	    (if (>= numderived i)
		(assertTrace entailment assertedants term reason context)
	      set:*emptyset*)))))))

(defun inferNegByNegationElimination (term dominatingNegation context termstack)
  "If dominator is derivable,
         return a set of the term
         else return the empty set.
         The termstack is a stack of propositions
             that this goal is a subgoal of,
         and it's already known that dominatingNegation is not on termstack."
  ;; At first only ground Propositions are dealt with. 
  (if (or
       ;; Could have term = dominatingNegation because we are trying to derive
       ;; term by showing that the negation of ~term is derivable
       (eq term dominatingNegation)
       (set:emptyp (askif dominatingNegation context (cons term termstack))))
      set:*emptyset*
    (assertTrace dominatingNegation nil term (reason dominatingNegation :e)
		 context)))

;;; Utility Functions
(defun notprop (p)
  "Returns the negation of the proposition p."
  (sneps:build `(not ,p) 'sneps:Proposition))

(defun onTermStack (term termstack)
  "Returns t if term or its negation is on the termstack."
  (or (member term termstack)
      (member (notprop term) termstack)))

(defun viableDominators (term slot termstack)
  "Returns a list of the terms
        that are not on termstack, and
        in which the given term is a filler of the given slot."
  (set:loopset for dominator in (sneps:findfrom term slot)
	       unless (onTermStack dominator termstack)
	       collect dominator))

(defun otherFillers (term slot thisfiller)
  "Returns a list of all the fillers of the given slot
        of the given term except for thisfiller."
  (set:loopset for arg in (sneps:findto term slot)
	       unless (eq arg thisfiller)
	       collect arg))

(defun assertedAndNot (terms context)
  "Returns two values:
       a list of the terms in the list terms
          that are asserted in the given context;
       and a list of the unasserted terms."
  (loop for term in terms
      when (ct:assertedp term context)
      collect term into assertedOnes
      else collect term into unassertedOnes
      finally (return (values assertedOnes unassertedOnes))))

(defun assertedAndNotset (terms context)
  "Returns two values:
       a list of the terms in the set terms
          that are asserted in the given context;
       and a list of the unasserted terms."
  (set:loopset  for term in terms
		when (ct:assertedp term context)
		collect term into assertedOnes
		else collect term into unassertedOnes
		finally (return (values assertedOnes unassertedOnes))))

(defun reason (term type)
  "Returns the string giving a derivation reason
     based on the syntactic type of term.
     type is either :e for `Elimination' or :i for `Introduction'."
  (format nil "~:(~A~) ~:[Introduction~;Elimination~]"
	  (sneps:syntactic-type-of term) (eq type :e)))

(defun subsetsOfSize (n set)
  "Returns a list of every size n subset (represented as lists)
        of the given set (represented as a set)."
  (cond ((< (set:cardinality set) n) nil)
	((= n 1)
	 (set:loopset for x in set
		      collect (list x)))
	(t (let ((setlist (set:set-to-list set)))
	     (append 
	      (loop for ss in (subsetsOfSizelist (1- n) (rest setlist))
		  collect (cons (first setlist) ss))
	      (subsetsOfSizelist n (rest setlist)))))))

(defun subsetsOfSizelist (n set)
  "Returns a list of every size n subset (represented as lists)
        of the given set (represented as a list)."
  (cond ((< (length set) n) nil)
	((= n 1)
	 (loop for x in set
	     collect (list x)))
	(t (append 
	    (loop for ss in (subsetsOfSizelist (1- n) (rest set))
		collect (cons (first set) ss))
	    (subsetsOfSizelist n (rest set))))))
