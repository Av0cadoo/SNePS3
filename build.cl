;;; SNePS 3: Build Methods
;;; ======================
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

;;; Building a term is creating a term object
;;;    of the correct syntactic and semantic classes,
;;;    and entering it in the collection of terms.
(in-package :sneps3)

(defparameter *KRNovice* nil
  "If the value is non-null, 
        caseframes will be created automatically
        whenever the user uses a function symbol
            that doesn't already have a caseframe.")

(defun listkb ()
  "Prints the current context and all propositions asserted in it."
  (format *standard-output* "~&~S~%" (ct:currentContext))
  (loop for i from 1 to (length (format nil "~S" (ct:currentContext)))
      do (princ '- *standard-output*))
  (terpri  *standard-output*)
  (list-terms :asserted t))


(defun list-variables (&key (types nil))
  "Prints the variable nodes. First arbitraries and then indefinites. If the 
   types keyword is not nil, then it prints the types of each term."
  (set:loopset
   for arb in (util:resource-value *ARBITRARIES*)
   do (format *standard-output* "~:[~*~;<~S> ~]~S~%"
	       types (type-of arb) arb))
  (set:loopset
   for ind in (util:resource-value *INDEFINITES*)
   do (format *standard-output* "~:[~*~;<~S> ~]~S~%"
	       types (type-of ind) ind)))

(defun list-terms (&key (asserted nil) (types nil))
  "Prints a list of all the terms in the KB.
     If asserted is non-null, only asserted propositions will be printed;
        otherwise, all terms will be printed.
     If :types is non-null, also shows the type of each term."
  ;; First print atomic terms;
  ;; Then arbitrary nodes
  ;; Then indefinite nodes
  ;; Then print non-propositional molecular terms;
  ;; Then print propositional molecular terms.
  (dolist (trm
	      (loop for tm in 
		    (loop for term being the hash-value
			of (util:resource-value *TERMS*)
			if (and (typep term 'atom)
				;; There are no base nodes...
                                ;; this may be a bug. To prevent 
                                ;; variables from printing here
                                ;; we need this check
				(not (typep term 'variable))
				(or (not asserted)
				    (and (typep term 'Proposition)
					 (ct:assertedp term
						    (ct:currentContext)))))
			do (format *standard-output* "~:[~*~;<~S> ~]~S~%"
				   types (type-of term) term)
			else if (or (and 
				     (not asserted)
				     (not (typep term 'variable))) 
				    (and
				     (typep term 'Proposition)
				     (ct:assertedp term
						(ct:currentContext))))
			collect term
			finally (when (not asserted) 
				  (list-variables :types types)))
		  if (typep tm 'Proposition)
		  collect tm
		  else do (format *standard-output* "~:[~*~;<~S> ~]~S~%"
				  types (type-of tm) tm)))
    (format *standard-output* "~:[~*~;<~S> ~]~S~%"
	    types (type-of trm) trm))
  (values))


(defgeneric build (expr semtype &optional 
				substitution)
  (:documentation
   "Creates (if necessary) a term expressed by expr of the given semantic type,
        and returns it.")
  )

;;; This method is being delete to distinguish a filler term that is the symbol |nil|
;;; from the empty set of fillers, which can be specified as (setof).
;;;(defmethod build ((expr null) semtype &optional
;;;				      (substitution (subs:new-substitution)))
;;;  "Returns an empty set."
;;;  ;; To allow "nil" to indicate an empty filler.  scs 9/1/2010
;;;  (declare (ignore semtype substitution))
;;;  (set:new-set))


(defmethod build ((expr string) semtype &optional 
				(substitution (subs:new-substitution)))
  "Creates (if necessary) an atomic term expressed by
           the symbol whose name is expr
       of the given semantic type,
     and returns it."
  (build (intern expr) semtype substitution))

(defmethod build ((x number) semtype &optional 
				(substitution (subs:new-substitution)))
  "Creates (if necessary) an atomic term expressed by
           the symbol whose name looks like the integer x
       of the given semantic type,
     and returns it."
  (build (intern (format nil "~D" x)) semtype substitution))

(defmethod build ((x float) semtype &optional 
				(substitution (subs:new-substitution)))
  "Creates (if necessary) an atomic term expressed by
           the symbol whose name looks like the floating point number x,
           rounded to *PRECISION* digits
       of the given semantic type,
     and returns it."
  (build (intern (format nil "~,VF" *PRECISION* x)) semtype substitution))

(defmethod build ((term term) semtype &optional 
				(substitution (subs:new-substitution)))
  "Returns the given term,
       and if necessary, adjusting its semantic type so that
       it is of the semantic type semtype."
  (declare (ignore substitution))
  (adjustType term (semantic-type-of term) semtype))

(defmethod build ((term set:set) semtype &optional 
				(substitution (subs:new-substitution)))
  "Returns the set, assuming that it is a set of terms."
  (declare (ignore semtype substitution))
  term)

(defmethod build ((expr (eql 'T)) semtype
		  &optional (substitution (subs:new-substitution)))
  "Creates (if necessary) an atomic term expressed by T
       of the given semantic type,
     and returns it."
  (declare (ignore substitution))
  (check-type semtype semantic-type)
  (let ((term (find-term 'T)))
    (unless term 
      (util:protecting
       *TERMS*
       (setf (gethash 'T *TERMS*)
	 (setf term (make-instance (cross-class 'atom 'Proposition)
		      :name 'T)))))
    (adjustType term			; Lower its semantic type, if necessary
		(semantic-type-of term) semtype)
    (ct:assert term (ct:find-context 'ct:BaseCT))
    term))

(defmethod build ((expr symbol) semtype &optional 
				(substitution (subs:new-substitution)))
  "Creates (if necessary) an atomic term expressed by expr
       of the given semantic type,
     and returns it."
  (check-type semtype semantic-type)
  (let ((term 
	 (or 
	  (and (not (subs:emptyp substitution))
	       (subs:var-to-term expr substitution)) 
	 (find-term expr))))
    (cond ((or (wftnamep (string expr))
	       (quanttermp (string expr)))
	   (if term			; Lower its semantic type, if necessary
	       (adjustType term (semantic-type-of term) semtype)
	     (error "The name ~S is not yet associated with a term." expr)))
	  (term				; Lower its semantic type, if necessary
	   (adjustType term (semantic-type-of term) semtype))
	  (t (util:protecting
	      *TERMS*
	      (setf (gethash expr *TERMS*)
		(setf term (make-instance
			       (cross-class 'atom semtype)
			     :name expr))))
	     (when (eq expr 'F)
	       (ct:assert (build `(not ,term) (find-class 'Proposition))
		   (ct:find-context 'ct:BaseCT)))
	     term))))

(defmethod build ((expr cons) semtype &optional 
		  (substitution (subs:new-substitution)))
  "Creates (if necessary) a well-formed term expressed by expr
       of the given semantic type,
     and returns it."
  ;; expr will be of the form (fn arg ... arg)
  ;;      where each arg is an expression
  (let ((fn (first expr)))
    (case fn
      ;; Only functions with special syntax
      ;;    or which build terms of a special syntactic type
      ;;    need to be handeled specially.
      ;; All others can use the default case of buildUserTerm.
      (Isa
       ;; expr is (Isa Entities Categories)
       (unless (= (length expr) 3)
	 (error "Isa must take 2 arguments. It doesn't in ~S." expr))
       (buildMolecularNode (cf:find-frame 'Isa)
			   (list (build (second expr) (find-class 'Entity)
					substitution)
				 (build (third expr) (find-class 'Category)
					substitution))
			   'categorization semtype))

      ((and or xor nand iff not nor thnot thnor)
       ;; expr is (op a1 ... an)
       ;;      or (op (setof a1 ... an))
       (when (and (= (length expr) 2)
		  (consp (second expr))
		  (eq (first (second expr)) 'setof))
	 (setf expr (cons (first expr) (rest (second expr)))))
       (case fn
	 (and
	  ;; expr is (and a1 ... an)
	  (let ((cf (cf:find-frame 'and)))
	    (unless cf (error "There is no frame associated with and."))
	    (cond 
	     ((cddr expr)
	      (let ((fillers 
		     (build (cons 'setof (rest expr)) semtype substitution)))
		(buildMolecularNode
		 cf (list fillers) 'conjunction semtype
		 (semantic-type-of fillers))))
	     ((rest expr)
	      ;; If only one conjunct,
	      (if (typep (second expr) 'set:set)
		  ;; if the argument is a set, treat it as the set of conjuncts
		  (if (set:singletonp (second expr))
		      ;; If just one conjunct, return it
		      (set:choose (second expr))
		    ;; otherwise, build the conjunction
		    (buildMolecularNode
		     cf (rest expr) 'conjunction semtype
		     (semantic-type-of (second expr))))
		;; otherwise, just build the conjunct.
		(build (second expr) semtype substitution)))
	     (t
	      ;; A conjunction with no conjuncts is the True proposition
	      (build 'T (find-class 'Proposition) substitution)))))
      
	 (or
	  ;; expr is (or a1 ... an)
	  (let ((cf (cf:find-frame 'andor)))
	    (unless cf (error "There is no frame associated with or"))
	    (cond
	     ((cddr expr)
	      (let ((fillers 
		     (build (cons 'setof (rest expr)) semtype substitution)))
		(buildMolecularNode
		 cf (list fillers) 'disjunction semtype
		 (semantic-type-of fillers)
		 1 (set:cardinality fillers))))
	     ((rest expr)
	      ;; If only one disjunct, just build the disjunct.
	      (build (second expr) semtype substitution))
	     (t
	      ;; A disjunction with no disjuncts is the False proposition
	      (build 'F (find-class 'Proposition) substitution)))))
      
	 (xor
	  ;; expr is (xor a1 ... an)
	  (let ((cf (cf:find-frame 'andor)))
	    (unless cf (error "There is no frame associated with xor"))
	    (cond
	     ((cddr expr)
	      (let ((fillers 
		     (build (cons 'setof (rest expr)) semtype substitution)))
		(buildMolecularNode
		 cf (list fillers) 'xor semtype
		 (semantic-type-of fillers) 1 1)))
	     ((rest expr)
	      ;; If only one disjunct, just build the disjunct.
	      (build (second expr) semtype substitution))
	     (t
	      ;; A exclusive disjunction with no disjuncts is the False proposition
	      (build 'F (find-class 'Proposition) substitution)))))
      
	 (nand
	  ;; expr is (nand a1 ... an)
	  (let ((cf (cf:find-frame 'andor)))
	    (unless cf (error "There is no frame associated with nand"))
	    (cond
	     ((cddr expr)
	      (let ((fillers 
		     (build (cons 'setof (rest expr)) semtype substitution)))
		(buildMolecularNode
		 cf (list fillers) 'nand semtype
		 (semantic-type-of fillers)
		 0 (1- (set:cardinality fillers)))))
	     ((rest expr)
	      ;; If only one argument, build the negation of the argument.
	      (build `(not ,(second expr)) semtype substitution))
	     (t
	      ;; A negatedconjunction with no arguments is the False proposition
	      (build 'F (find-class 'Proposition) substitution)))))

	 (iff
	  ;; expr is (iff a1 ... an)
	  (let ((cf (cf:find-frame 'thresh)))
	    (unless cf (error "There is no frame associated with iff."))
	    (cond 
	     ((cddr expr)
	      (let ((fillers 
		     (build (cons 'setof (rest expr)) semtype substitution)))
		(buildMolecularNode
		 cf (list fillers) 'equivalence semtype
		 (semantic-type-of fillers)
		 1 (1- (set:cardinality fillers)))))
	     ((rest expr)
	      ;; If only one filler,
	      (if (typep (second expr) 'set:set)
		  ;; if the argument is a set, treat it as the set of fillers
		  (if (set:singletonp (second expr))
		      ;; If just one filler, its a tautology
		      (set:choose (second expr))
		    ;; otherwise, build the iff
		    (buildMolecularNode
		     cf (rest expr) 'equivalence semtype
		     (semantic-type-of (second expr))
		     1 (1- (set:cardinality (first (rest expr))))))
		(build 'T (find-class 'Proposition) substitution)))
	     (t
	      ;; A iff with no fillers is the True proposition
	      (build 'T (find-class 'Proposition) substitution)))))

	 ((not nor)
	  ;; expr is (nor a1 ... an) or (not a1 ... an)
	  (let ((cf (cf:find-frame 'nor)))
	    (unless cf (error "There is no frame associated with nor."))
	    (cond ((third expr)		; at least two arguments 
		   (let ((fillers 
			  (build (cons 'setof (rest expr)) semtype substitution)))
		     (buildMolecularNode
		      cf (list fillers) 'negation semtype
		      (semantic-type-of fillers))))
		  ((rest expr)		; exactly one argument
		   (buildCanonicalNegation (second expr) semtype))
		  (t			; (not) = (nor) = T
		   (build 'T (find-class 'Proposition) substitution)))))
      
	 ((thnot thnor)
	  ;; expr is (thnor a1 ... an) or (thnot a1 ... an)
	  (let ((cf (cf:find-frame 'thnor)))
	    (unless cf (error "There is no frame associated with thnor."))
	    (cond ((rest expr)		; at least one argument 
		   (let ((fillers 
			  (build (cons 'setof (rest expr)) semtype substitution)))
		     (buildMolecularNode
		      cf (list fillers) 'negationbyfailure semtype
		      (semantic-type-of fillers))))
		  (t			; (thnot) = (thnor) = T
		   (build 'T (find-class 'Proposition) substitution)))))))

      (andor
       ;; expr is (andor (i j) a1 ... an)
       ;;      or (andor (i j) (setof a1 ... an))
       (if (and (= (length expr) 3)
		(consp (third expr))
		(eq (first (third expr)) 'setof))
	   (buildAndor (cons (second expr) (rest (third expr)))
		       semtype)
	 (buildAndor (rest expr) semtype)))
      
      (thresh
       ;; expr is  (thresh (i j) a1 ... an)
       ;;      or (thresh (i j) (setof a1 ... an))
       (if (and (= (length expr) 3)
		(consp (third expr))
		(eq (first (third expr)) 'setof))
	   (buildThresh (cons (second expr) (rest (third expr)))
		       semtype)
	 (buildThresh (rest expr) semtype)))
       
      (if
	  ;; expr is (if a1 a2)
	  (let ((cf (cf:find-frame 'if)))
	    (unless cf (error "There is no frame associated with if"))
	    (checkarity 'if expr cf)
	    (let ((fillers1 (build (second expr) semtype substitution))
		  (fillers2 (build (third expr) semtype substitution)))
	      (buildMolecularNode
	       cf (list fillers1 fillers2) 'implication semtype
	       semtype
	       (if (typep fillers1 'set:set)
		   (set:cardinality fillers1)
		 1)))))
      
      (setof
       ;; expr is (setof term ... term)
;;;     An empty set (empty set of fillers) is now allowed. scs 9/1/2010
;;;     (unless (rest expr)
;;;	 (error "The setof operator must be given at least one argument."))
       (let ((set (set:new-set
		   :items (loop for arg in (rest expr)
				collect (build arg semtype substitution)))))
	 (if (set:singletonp set)
	     (set:choose set)
	   set)))
      
      (t (cond ((ientailsymbp fn)
		;; expr is (i=> ant cq)
		(buildNumericalEntailmant 
		 (ientaili fn) (second expr) (third expr) semtype))
	       (t (buildUserTerm fn expr semtype substitution)))))))



(defun buildAndor (args semtype)
  "Build a term for andor
       args is the original expression after 'andor'."
  (let ((cf (cf:find-frame 'andor)))
    (unless cf (error "There is no frame associated with andor."))
    (unless (consp (first args))
      (error "andor must be followed by a list, (i j) in ~S." (cons 'andor args))) 
    (let* ((min (first (first args)))
	   (max (second (first args)))
	   (tot (length (rest args))))
      (unless (and (integerp min) (<= 0 min tot))
	(error
	 "The min parameter of andor must be an integer ~
              between 0 and the number of arguments, ~D, in ~S."
	 tot (cons 'andor args)))
      (unless (and (numberp max) (<= min max tot))
	(error
	 "The max parameter of andor must be an integer ~
              between ~D and ~D, in ~S."
	 min tot (cons 'andor args)))
      (cond
       ;; Canonical transformations 
       ((and (= min 0) (= max tot)) (build 'T semtype)) 
       ((= tot min max)
	(build `(and ,@(rest args)) semtype))
       ((= 0 min max)
	(build `(nor ,@(rest args)) semtype)) 
       (t (let* ((fillers (build `(setof ,@(rest args)) semtype))
		 (term (buildMolecularNode
			cf (list fillers) 'andor semtype
			(semantic-type-of fillers)
			min max))) 
	    term))))))

(defun buildThresh (args semtype)
  "Build a term for thresh.
       args is the original expression after 'thresh'."
  (let ((cf (cf:find-frame 'thresh)))
    (unless cf (error "There is no frame associated with thresh."))
    (unless (consp (first args))
      (error
       "thresh must be followed by a list, (i) or (i j), in ~S."
       (cons 'thresh args)))
    (let* ((min (first (first args)))
	   (tot (length (rest args)))
	   (tot-1 (1- tot))
	   (max (or (second (first args)) tot-1)))
      (unless (and (integerp min) (<= 0 min tot))
	(error
	 "The min parameter of thresh must be an integer ~
              between 0 and ~D, in ~S."
	 tot-1 (cons 'thresh args)))
      (unless (and (numberp max) (<= min max tot))
	(error
	 "The max parameter of thresh must be an integer ~
              between ~D and ~D, in ~S."
	 min tot (cons 'thresh args)))
      (cond
       ;; Canonical transformations
       ((and (= min 0) (= max tot)) (build 'F semtype))
       ((and (= min 0) (= max (1- tot))) (build `(and ,@(rest args)) semtype))
       ((= min max 0) (build `(or ,@(rest args)) semtype))
       ((and (= min 1) (= max tot)) (build `(nor ,@(rest args)) semtype))
       ((= min tot) (build `(nand ,@(rest args)) semtype))
       (t (let* ((fillers (build `(setof ,@(rest args)) semtype))
		 (term (buildMolecularNode
			cf (list fillers) 'thresh semtype
			(semantic-type-of fillers)
			min max))) 
	    term))))))

(defun buildNumericalEntailmant (i ant cq semtype)
  "Builds the term for `(i=> ant cq)."
  (when (cl:atom ant) (setf ant `(setof ,ant)))
  (when (cl:atom cq) (setf cq `(setof ,cq)))
  (let ((cf (cf:find-frame 'if)))
    (unless cf (error "There is no frame associated with ~D=>." i))
    (unless (and ant cq)
      (error "~D=> must be given two arguments, ~
                         the antecedent(s) and the consequent(s), in (~D=> ~S ~S)."
	     i i ant cq)) 
    (let ((tot (length ant)))
      (unless (<= 0 i tot)
	(error
	 "The number in ~D=> must be between 0 ~
                and the number of antecedents, ~D, in (~D=> ~S ~S)."
	 i i ant cq))
      (cond
       ;; Canonical transformations 
       ((= i 0) (build `(and ,(build cq semtype)) semtype))
       (t (let ((term (buildMolecularNode
			cf (list (build ant semtype) (build cq semtype))
			'numericalentailment semtype
			semtype i)))
	    term))))))

(defgeneric buildCanonicalNegation (arg semtype)
  (:documentation 
   "Creates (if necessary) a canonical term
         expressed by (not arg) of the given semantic type, 
         and returns it."
   ))

(defmethod buildCanonicalNegation ((arg t) semtype)
  "Creates (if necessary) a term
        expressed by (not arg) of the given semantic type, 
        and returns it."
  (let ((filler (build arg semtype)))
    (buildMolecularNode (cf:find-frame 'nor) (list filler)
			'negation semtype (semantic-type-of filler))))

(defmethod buildCanonicalNegation ((arg cons) semtype)
  "Creates (if necessary) a canonical term
        which is the negation arg, of the given semantic type, 
        and returns it."
  (case (first arg)
    (and (build `(nand ,@(rest arg)) semtype))
    (or (build `(nor ,@(rest arg)) semtype))
    (nand (build `(and ,@(rest arg)) semtype))
    ((thnot thnor) (build `(or ,@(rest arg)) semtype))
    ((nor not) (build `(or ,@(rest arg)) semtype))
    (xor (build `(thresh (1 1) ,@(rest arg)) semtype))
    (andor (build `(thresh ,@(rest arg)) semtype))
    (thresh (build `(andor ,@(rest arg)) semtype))
    (iff (build `(andor (1 ,(1- (length (rest arg)))) ,@(rest arg))
		semtype))
    (t
     (let ((filler (build arg semtype)))
       (buildMolecularNode (cf:find-frame 'nor) (list filler)
			   'negation semtype (semantic-type-of filler))))))

(defmethod buildCanonicalNegation ((arg disjunction) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given disjunction,
        of the given semantic type, 
        and returns it." 
  (build `(nor ,@(set:set-to-list (sneps:findto arg 'andorargs)))
	 semtype))

(defmethod buildCanonicalNegation ((arg conjunction) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given conjunction,
        of the given semantic type, 
        and returns it." 
  (build `(nand ,@(set:set-to-list (sneps:findto arg 'and)))
	 semtype))

(defmethod buildCanonicalNegation ((arg nand) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given negated conjunction,
        of the given semantic type, 
        and returns it."
  (build `(and ,@(set:set-to-list (sneps:findto arg 'andorargs)))
	 semtype))

(defmethod buildCanonicalNegation ((arg negation) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given negation,
           of the given semantic type, 
        and returns it."
  (build `(or ,@(set:set-to-list (sneps:findto arg 'nor)))
	 semtype))

(defmethod buildCanonicalNegation ((arg negationbyfailure) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given negationbyfailure,
            of the given semantic type, 
        and returns it."
  (build `(or ,@(set:set-to-list (sneps:findto arg 'thnor)) )
	 semtype))

(defmethod buildCanonicalNegation ((arg andor) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given andor.
        of the given semantic type, 
        and returns it."
  (build `(thresh (,(minparam arg) ,(maxparam arg))
		  ,@(set:set-to-list (sneps:findto arg 'andorargs)))
	 semtype))

(defmethod buildCanonicalNegation ((arg thresh) semtype)
  "Creates (if necessary) a canonical term
        which is the negation of the given thresh.
        of the given semantic type, 
        and returns it."
  (build `(andor (,(minparam arg) ,(maxparam arg))
		 ,@(set:set-to-list (sneps:findto arg 'threshargs)))
	 semtype))

(defun buildUserTerm (fn expr semtype
		      &optional (substitution (subs:new-substitution)))
  "Build a term for the expression expr, whose function is fn,
       and whose contextual semantic type is to be semtype."
  ;; fn = (first expr)
  (unless (cl:atom fn)
    (setf fn (build fn 'Thing substitution)))	; Should the type be function?
  (let ((cf (or
	     (cf:find-frame 
	      (typecase fn
		(symbol 
		 (if (wftnamep (string fn))
		     (caseframe (find-term fn))
		   fn))
		((and term atom) (name fn))
		(molecular (caseframe fn))
		(t (error
		    "The function \"symbol\", ~S, is not an acceptable function \"symbol\"."
		    fn))))
	     (and  *KRNovice*
		   (cf:defineNoviceCaseframe fn expr)))))
    (unless cf 
      (error "There is no frame associated with ~S." fn))
    (if (and ;; Allow (fn a1 ... an) to mean (fn (setof a1 ... an))
	 (cf:hasOneArgumentSlot cf)
	 (rest expr)
	 (not (and (consp (second expr))
		   (eq (first (second expr)) 'setof))))
	(setf expr (list fn (cons 'setof (rest expr))))
      (checkarity fn expr cf))
    (let ((fillers (loop for arg in (if (cf:quotedpp cf)
					(rest expr)
				      expr)
		       for rel across (cf:caseframe-slots cf)
		       collect (build arg (slot:slot-type rel) substitution))))
      (buildMolecularNode cf fillers 'molecular semtype))))

(defun checkarity (fn expr cf)
  "Raises an error
       if the use of the function fn in the expression, expr,
       has the wrong arity according to fn's caseframe, cf.
    Otherwise, just returns nil."
  (let ((correctlength
	 (+ (length (cf:caseframe-slots cf))
	    (if (cf:quotedpp cf) 1 0))))
    (unless (= (length expr) correctlength)
      (error "~S is used with incorrect arity in ~S.~%~
                         It should be given ~D arguments instead of ~D."
	     fn expr (1- correctlength) (1- (length expr))))))
  

(defun buildMolecularNode (cf dcs syntype semtype 
			   &optional (fsemtype semtype)
				     (min 0 mingiven) (max 0 maxgiven))
  "Builds a molecular node with:
       the caseframe cf;
       the down-cableset dcs represented as a list;
       the syntactic type syntype;
       the contextually determined semantic type semtype;
       the semantic type, fsemtype,
           determined by the fillers of its slots
           (only used by rule nodes,
            the semantic types of whose arguments
            are to be used instead of the type proposition;
       the min parameter, given for andor and thresh;
       the max parameter, given for andor and thresh."
  ;; make every element in dcs a set if it's not already
  (loop for i from 0 to (- (length dcs) 1)
      do (setf (elt dcs i) (set::make-set-if-not-set (elt dcs i))))
  (map nil #'check-min-max dcs (cf:caseframe-slots cf))
  (let ((term 
	 (or
	  (cond (maxgiven (find-exact syntype cf dcs min max))
		(mingiven (find-exact syntype cf dcs min))
		(t (find-exact syntype cf dcs)))
	  (let* ((wft (make-instance
			  (cross-class syntype
					(cf:caseframe-type cf))
			:caseframe cf
			:down-cableset (coerce dcs '(simple-array set:set))
			:min min
			:max max))
		 (counter
		  (util:protecting (counter wft)
				   (incf (counter wft)))))
	    (setf (slot-value wft 'name)
	      (intern (format nil "~A~D" :wft counter)))
	    (util:protecting
	     *TERMS*
	     (setf (gethash (name wft) *TERMS*) wft))
	    (cf:add-caseframe-term wft cf)
	    wft))))
    (adjustType term (cf:caseframe-type cf) fsemtype)))

(defun check-min-max (fillers slot)
  "Raises an error if fillers is a set
        that doesn't satisfy the min and max restrictions of slot."
  (let ((numfillers
	 (etypecase fillers
	   (set:set (set:cardinality fillers))
	   (null 0)
	   (cl:atom 1)))
	(min (slot:slot-min slot))
	(max (slot:slot-max slot)))
    (when (and (numberp min) (cl:< numfillers min))
      (error
       "The set of fillers, ~S, is too few for the minimum restriction on the slot ~S."
       fillers slot))
    (when (and (numberp max) (cl:> numfillers max))
      (error
       "The set of fillers, ~S, is too many for the maximum restriction on the slot ~S."
       fillers slot))))


(defun cross-class (syn sem)
  "Given the names of syntactic and semantic types, return the class
       for the cross product.  This should be called on the fly."
  ;; Written: AEC & DTB 6/10/1998
  ;; Modified: SCS 6/5/07

  (cl:assert (typep (find-class syn nil) 'sneps3::syntactic-type)
      (syn)
    "Cannot make a cross-class of ~S and ~S because ~S is not a syntactic type."
    syn sem syn)
  (check-type sem semantic-type)
  (assert (or (not (symbolp syn))
	      (find-class syn nil)) (syn)
    "~A is not a defined class." syn)
  (assert (or (not (symbolp sem))
	      (find-class sem nil)) (sem)
    "~A is not a defined class." sem)
  (let ((typename (intern (format nil "~A-~A" syn (find-type-name sem)))))
    (cond ((find-class typename nil))
	  (t (eval `(defclass ,typename (,syn ,sem)
		      ()))))))


(defun wftnamep (ic)
  "Returns True if the string ic looks like a indname;
     else returns False."
  (and (> (length ic) 3)
       (string= (subseq ic 0 3) "wft")
       (every #'digit-char-p (subseq ic 3))))

(defun quanttermp (ic)
  "Returns True if the string ic looks like arbi or indi;
     else returns False."
  (and (> (length ic) 3)
       (member (subseq ic 0 3) '("arb" "ind") :test #'string=)
       (every #'digit-char-p (subseq ic 3))))

(defun roundf (x)
  "Returns a rounding of x according to *PRECISION*
       for use as a term name."
  (declare (special *PRECISION*))
  (float (/ (round (* x (expt 10 *PRECISION*)))
	    (expt 10 *PRECISION*))))

a(defun adjustType (term oldtype newtype)
  "Adjusts the type of term, if possible,
       from its old semantic type, oldtype,
       to its new semantic type, newtype,
       while keeping its syntactic type, syntype,
    and return the term."
  (let ((syntype (syntactic-type-of term))
	gcsub)
    (cond ((typep term newtype)
	   ;;(format t "~S is already of type ~S.~%" term newtype)
	   )

	  ;; If newtype is a subtype of oldtype,
	  ((subtypep newtype oldtype)
	   ;; change the type of term to newtype
	   ;;(format t "Changing the type of ~S from ~S to ~S.~%"
	   ;;term oldtype (find-type-name newtype))
	   (change-class term (cross-class syntype newtype)))
	
	  ;; If new type and oldtype have a common subtype,
	  ;;    change the type to be the greatest common subtype.
	  ;; Could there be more than 1 greatest common subtype?
	  ((setf gcsub (gcsubtype newtype
				  (typecase oldtype
				    (symbol (find-class oldtype))
				    (standard-class oldtype))))
	   (if (rest gcsub)
	       (let ((gcsubchoice
		      (util:menuChooseFromList
				(format nil "Choose a type for ~S."
					term)
				gcsub)))
		 (if gcsubchoice
		     (change-class term 
			     (cross-class syntype gcsubchoice))
		 (error "No semantic type for ~S." term)))
	     ;;(format t "Changing the type of ~S from ~S to ~S.~%"
	     ;;term oldtype ctype)
	     (change-class
	      term
	      (cross-class syntype (first gcsub)))))

	  ;; Otherwise this attempted use of term is an error.
	  (t (error
	      "The attempt to use ~S in a context ~
                        that requires it to be of type ~S ~
                        conflicts with its current type of ~S."
	      term (find-type-name newtype) oldtype))
	  ))
  term)

(defun ientailsymbp (symbol)
  "Returns t if the symbol name is v=> or i=>, for some positive integer i;
    nil otherwise."
  (when (symbolp symbol)
    (let* ((fname (symbol-name symbol))
	   (len (length fname)))
      (and (> len 2)
	   (string= (subseq fname (- len 2)) "=>")
	   (let ((i (subseq fname 0 (- len 2))))
	     (when (or (string= i "v")
		       (every #'digit-char-p i))
	       (return-from ientailsymbp t)))))))

(defun ientaili (i=>)
  "Assuming that i=> satisfies #'ientailsymbp,
       returns the number represented by i."
  (let* ((fname (symbol-name i=>))
	 (len (length fname))
	 (i (subseq fname 0 (- len 2))))
    (if (string= i "v") 1 (read-from-string i))))


(defun pre-build-var (quant var-label rsts &optional arb-rsts ind-rsts)
  "This function creates a variable without a restriciton set. This is
   needed to handle cases of mutual dependencies. quant indicates the
   type of variable and is either the symbol 'some or
   'every. var-label is the label the user used to denote this
   variable. rsts are the restriction set specifier for the variable,
   but are not actually constructed here. Simple error checking is
   done at this stage, as is a search of the KB for pre-existing
   arbitrary nodes with this restriction set. The substitution is a mapping 
   between these labels and the
   variables they build. This function returns the varibale node built
   and modifies the substitution accordingly. Arbitrary individuals may
   have a resriction set slot filled in already if they existed in the KB
   previously. Otherwise, resrtiction sets still need to be built."
   (loop for rst in rsts
      if (not (member var-label rst))
      do (error
	  "The variable label, ~S, is not part of the restriction proposition, ~A."
	  var-label rst)
      end)
   (or (and (eq quant 'every) (catch 'old-vars
	 (find-old-arb-node var-label rsts arb-rsts ind-rsts)))
      (let* ((var (make-instance 
		      (cross-class 
		       (case quant (every 'arbitrary) (some 'indefinite))
		       'Entity))))
	(setf (slot-value var 'name)
	  (intern (format nil "~A~D" 
			  (case quant (every :arb) (some :ind)) 
			  (util:resource-value (counter var)) var)))
	(setf (slot-value var 'var-label) var-label)
	(util:protecting (counter var) 
			 (incf (counter var)))
	var)))
  
  
(defun pre-build-vars (arb-rsts ind-dep-rsts substitution)  
  "Loops through the arbitrary individuals and builds an initial
   structure, but not the restriction sets. The same is done for the 
   indefinite objects."
  (loop 
      for key being the hash-key using (hash-value value) of arb-rsts
      do (subs:add-var-term-pair 
	  key 
	  (pre-build-var 'every key value arb-rsts ind-dep-rsts) 
	  substitution))
  (loop
      for key being the hash-key using (hash-value value) of ind-dep-rsts
      do (subs:add-var-term-pair
	  key
	  (pre-build-var 'some key (second value))
	  substitution)))

(defun build-var (quant var-label rsts substitution &optional dependencies)
  "Build a variable node of type quant with label var-label and
   restrictions rsts. Substitution contains all the variable nodes
   that should be needed to build the restrictions. Dependencies is only
   needed for building indefinite objects."
  (let ((var (subs:var-to-term var-label substitution)))
    (loop 
	for rst in rsts
	do (set:add-item 
	    (build rst (find-class 'Proposition) substitution)
	    (slot-value var 'restriction-set)))
    (when (and (eq quant 'some) (not (endp dependencies)))
      (loop 
	  for v-label in dependencies
	  do (set:add-item (subs:var-to-term v-label substitution)
			   (slot-value var 'dependencies)))) 
    (util:protecting *TERMS*
		     (setf (gethash (name var) *TERMS*) var))
    (case quant
      (every (util:protecting *ARBITRARIES*
			      (set:add-item var *ARBITRARIES*)))
      (some (util:protecting *INDEFINITES*
			     (set:add-item var *INDEFINITES*))))
    var))


(defun build-vars (arb-rsts ind-dep-rsts substitution)
  "Loops through the arbs and inds. and builds their restriction
   sets. In the case of indenfinite-objects, their dependency lists 
   are populated." 
  (let ((built-vars '()))
   (loop 
      for key being the hash-key using (hash-value value) of arb-rsts
       do 
	 (push (build-var 'every key value substitution) built-vars))
   (loop 
      for key being the hash-key using (hash-value value) of ind-dep-rsts
       do (push (build-var 'some key (second value) 
			       substitution (first value)) built-vars))
  built-vars))


(defun check-and-build-variables (expr)
   "Given a top-level build expression, checks that expression for
   variable terms syntax (e.g., every, some). These terms are built and 
   a new build expression is created with just the variable-labels
   (e.g., x in (every x ...)). This and a substitution between
   variable-labels and the AI and IO is provided to build.
   Returns three values. The first is the new build expression, the
   second the the built nodes, and the third the substitution between
   var-labels and the AI/IO nodes."
  (let* ((arb-rsts (make-hash-table ))
	 (ind-dep-rsts (make-hash-table))
         (new-expr (parse-vars-and-rsts expr arb-rsts ind-dep-rsts))
	 (substitution (subs:new-substitution))
	 (built-vars '()))
    (setf *arb-rsts* arb-rsts)
    (setf *ind-dep-rsts* ind-dep-rsts)
    (pre-build-vars arb-rsts ind-dep-rsts substitution)
    (setf *subs* substitution)
    (setf built-vars 
      (append built-vars 
	      (build-vars arb-rsts ind-dep-rsts substitution)))
    (values new-expr built-vars substitution)))

(defun parse-vars-and-rsts (assertion-spec arb-rsts ind-deps-rsts)
  "Helper function: assertion-spec is a specifier for building a proposition
   in SNePS 3 (e.g., '(Isa (every x (Dog x) (Black x)) Mammal)). This
   function loops through assertion-spec and returns the expression
   with variable specifiers replaced with the variable label 
   (e.g. (Isa x Mammal)). This function also takes in two empty hash-tables,
   arb-rsts and ind-deps-rsts. These are modified to contain a mapping
   between var-labels and their restriciton sets and given dependencies
   if they are indefinite objects. For example the proposition 
   (Beats (every x (Isa x Farmer) 
                   (Owns x (some y(x) (Isa y Donkey)))) 
          y) 
   results in:

  Ex. ind-deps-rsts: [y -> ((x) (Isa y Donkey))]
      arb-rsts:  [x ->  ((Isa x Farmer) (Owns x y))]"
  (if (consp assertion-spec)
      (cond 
       ((eq (first assertion-spec) 'some) 
	(let ((rsts (rest (gethash (second assertion-spec) ind-deps-rsts))))
	  (cond
	   ((and rsts (not (set-difference rsts (subseq assertion-spec 3))))
	    (error "Restriction sets: ~A and ~A cannot be applied to ~
                      the same variable." rsts (cddr assertion-spec)))  
	   (t (setf 
		  (gethash (second assertion-spec) ind-deps-rsts)
		(list (third assertion-spec)
		      (parse-vars-and-rsts (subseq assertion-spec 3) 
					   arb-rsts ind-deps-rsts)))
	      (second assertion-spec))))) 
       ((eq (first assertion-spec ) 'every)
	(let ((rsts (second (gethash (second assertion-spec) arb-rsts))))
	  (cond
	   ((and rsts (not (set-difference rsts (subseq assertion-spec 2))))
	    (error "Restriction sets: ~A and ~A cannot be applied to ~
                      the same variable." rsts (cddr assertion-spec)))  
	   (t (setf (gethash (second assertion-spec) arb-rsts)
		(parse-vars-and-rsts (subseq assertion-spec 2) 
				     arb-rsts ind-deps-rsts))
	      (second assertion-spec))))) 
       (t (mapcar #'(lambda (arg)
		      (parse-vars-and-rsts arg arb-rsts ind-deps-rsts))
		  assertion-spec)))
    assertion-spec))
