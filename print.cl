;;; SNePS 3: Print & Description Methods
;;; ====================================
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

(in-package :sneps3)

;;; Print-object Methods
;;; ====================
(defmethod print-object ((term atom) stream)
  (format stream "~@<~S~:[~;!~]~:>"
	  (name term)
	  (and (typep term 'Proposition)
	       (ct:assertedp term (ct:currentContext)))))

(defmethod print-object ((term molecular) stream)
  (top-level-named-molecular-term term stream))		  
    
(defmethod print-object ((term indefinite) stream)
  (top-level-named-variable-term term 'some stream))

(defmethod print-object ((term arbitrary) stream)
  (top-level-named-variable-term term 'every stream))


(defmethod print-object ((ct ct:context) stream)
  (format stream "~<:~S~:>" (ct:ct-name ct)))

;;; Utilities for print-object methods
;;; ==================================


(defun top-level-named-variable-term (term quant stream)
  "Prints to the stream the variable term
        preceded only at the top level  by its wft name
           and an indication of its assert status."

  ;; Does this by first replacing its own code by the unnamed version,
  ;;;   and then restoring it.
  (setf (symbol-function 'top-level-named-variable-term)
	(symbol-function 'print-unnamed-variable-term))

  ;; This needs to be done with the other top-level-functions
  (setf (symbol-function 'top-level-named-molecular-term)	
	(symbol-function 'print-unnamed-molecular-term))

  ;; Also initializes and desconstructs a structures used for
  ;; printing variable nodes fully the first time, and the label
  ;; otherwise

  (setf *PRINTED-VARIABLES* (set:new-set))

  (unwind-protect
      (print-named-variable-term term quant stream)
    (setf (symbol-function 'top-level-named-variable-term)
	  (symbol-function 'top-level-named-variable-term-code))
    (setf (symbol-function 'top-level-named-molecular-term)
	  (symbol-function 'top-level-named-molecular-term-code))
    (setf *PRINTED-VARIABLES* nil)))

(defun top-level-named-variable-term-code (term quant stream)
  "Function repository for top-level-named-variable-term."
  (setf (symbol-function 'top-level-named-variable-term)
    (symbol-function 'print-unnamed-variable-term))
  (setf (symbol-function 'top-level-named-molecular-term)	
	(symbol-function 'print-unnamed-molecular-term))
  (setf *PRINTED-VARIABLES* (set:new-set))
  (unwind-protect
      (print-named-variable-term term quant stream)
    (setf (symbol-function 'top-level-named-variable-term)
	  (symbol-function 'top-level-named-variable-term-code))
    (setf (symbol-function 'top-level-named-molecular-term)
	  (symbol-function 'top-level-named-molecular-term-code))
    (setf *PRINTED-VARIABLES* nil)))

(defun top-level-named-molecular-term (term stream)
  "Prints to the stream the molecular term
        preceded only at the top level  by its wft name
           and an indication of its assert status."
  ;; Does this by first replacing its own code by the unnamed version,
  ;;;   and then restoring it.
  (setf (symbol-function 'top-level-named-molecular-term)
    (symbol-function 'print-unnamed-molecular-term))

  ;; This needs to be done with the other top-level-functions

  (setf (symbol-function 'top-level-named-variable-term)
	(symbol-function 'print-unnamed-variable-term))

  ;; Also initializes and desconstructs a structure used for
  ;; printing variable nodes fully the first time, and the label
  ;; otherwise

  (setf *PRINTED-VARIABLES* (set:new-set))

  (unwind-protect
      (print-named-molecular-term term stream)
    (setf (symbol-function 'top-level-named-molecular-term)
	  (symbol-function 'top-level-named-molecular-term-code))
    (setf (symbol-function 'top-level-named-variable-term)
	  (symbol-function 'top-level-named-variable-term-code))
    (setf *PRINTED-VARIABLES* nil)))

(defun top-level-named-molecular-term-code (term stream)
  "Function repository for top-level-named-molecular-term."
  (setf (symbol-function 'top-level-named-molecular-term)
    (symbol-function 'print-unnamed-molecular-term))
  (setf (symbol-function 'top-level-named-variable-term)
	(symbol-function 'print-unnamed-variable-term))
  (setf *PRINTED-VARIABLES* (set:new-set))
  (unwind-protect
      (print-named-molecular-term term stream)
    (setf (symbol-function 'top-level-named-molecular-term)
	  (symbol-function 'top-level-named-molecular-term-code))
    (setf (symbol-function 'top-level-named-variable-term)
	  (symbol-function 'top-level-named-variable-term-code))
    (setf *PRINTED-VARIABLES* nil)))

(defun print-unnamed-molecular-term (term stream)
  "Prints to the stream
        the molecular term not preceded by its wft name
        nor an indication of its assert status."
  (typecase term
    (negation
     (print-negation (elt (down-cableset term) 0) stream))
    (negationbyfailure
     (print-negationbyfailure (elt (down-cableset term) 0) stream))
    (conjunction
     (print-nary 'and (elt (down-cableset term) 0) stream))
    (disjunction
     (print-nary 'or (elt (down-cableset term) 0) stream))
    (equivalence
     (print-nary 'iff (elt (down-cableset term) 0) stream))
    (xor
     (print-nary 'xor (elt (down-cableset term) 0) stream)) 
    (nand
     (print-nary 'nand (elt (down-cableset term) 0) stream))
    (andor
     (print-param2op 'andor (minparam term) (maxparam term)
		     (elt (down-cableset term) 0) stream))
    (thresh
     (print-param2op 'thresh (minparam term) (maxparam term)
		     (elt (down-cableset term) 0) stream))
    (implication
     (format stream "~@<(if ~W~_ ~W)~:>"
	     (elt (down-cableset term) 0)
	     (elt (down-cableset term) 1)))
    (numericalentailment
     (format stream "~@<(~A=> ~_ ~S~_ ~S)~:>"
	     (if (= (minparam term) 1) :v (minparam term))
	     (elt (down-cableset term) 0)
	     (elt (down-cableset term) 1))) 
    (t
     (let ((cf (caseframe term)))
       (cond ((cf:hasOneArgumentSlot cf)
	      (let ((fsymbol (first (cf:caseframe-print-pattern cf))))
		(if (and (consp fsymbol)
				     (eq (first fsymbol) 'quote))
		(print-nary (second fsymbol)
			    (elt (down-cableset term) 0)
			    stream)
		(print-nary (elt (down-cableset term) 0)
			    (elt (down-cableset term) 1)
			    stream))))
	     (t (print-molecular (cf:caseframe-print-pattern cf)
				 (cf::caseframe-slots cf)
				 (down-cableset term)
				 stream)))))))

(defun print-unnamed-term (term stream)
  "Prints the given term, without wft-names on any level
   to the given stream."
  (setf (symbol-function 'top-level-named-molecular-term)
    (symbol-function 'print-unnamed-molecular-term)
    (symbol-function 'top-level-named-variable-term)
    (symbol-function 'print-unnamed-variable-term))
  (setf *PRINTED-VARIABLES* (set:new-set))
  (unwind-protect
      (print-unnamed-molecular-term term stream)
    (setf (symbol-function 'top-level-named-molecular-term)
      (symbol-function 'top-level-named-molecular-term-code)
      (symbol-function 'top-level-named-variable-term)
      (symbol-function 'top-level-named-variable-term-code)
      *PRINTED-VARIABLES* nil)))

;;; Methods for printing various types of molecular terms
;;; =====================================================

(defun print-named-variable-term (term quant stream)
  "Prints the variable term with unique identifier in front."
  (format stream "~A: " (name term))
  (print-unnamed-variable-term term quant stream))

(defun print-unnamed-variable-term (term quant stream)
  "Prints the variable term without unique identifier in front."
  (cond 
   ((and *PRINTED-VARIABLES* (set:member term *PRINTED-VARIABLES*))
    (format stream "~A" (var-label term)))
   ((and *PRINTED-VARIABLES* (eq quant 'some)) 
    (set:add-item term *PRINTED-VARIABLES*)
    (format stream "(some ~A(~{~A~^ ~}) ~{~A~^ ~})" 
	    (var-label term)
	    (set:set-to-list (dependencies term))
	    (set:set-to-list (restriction-set term))))
   ((and *PRINTED-VARIABLES* (eq quant 'every)) 
    (set:add-item term *PRINTED-VARIABLES*)
    (format stream "(every ~A ~{~A~^ ~})" 
	    (var-label term) 
	    (set:set-to-list (restriction-set term))))))



(defun print-molecular (pattern slots cs stream)
    "Prints to the stream
           the molecular term containing
               the given print-pattern,caseframe slots, and down-cableset.
        Prints it not preceded by its wft name
           nor an indication of its assert status."
    ;; Default method for printing a molecular term.
  (format stream "~@<(~{~S~^~_ ~})~:>"
	  (loop for p in pattern
	      if (and (consp p) (eq (first p) 'quote))
	      collect (second p)
	      else if (symbolp p)
	      collect (elt cs
			   (position p slots
				     :key #'slot:slot-name))
	      else do (error
		       "Bad pattern part ~S in the pattern ~S."
		       p pattern))))

(defun print-negation (args stream)
  "Prints to the stream
        the negation (not or nor) whose arguments are args.
     Prints it not preceded by its wft name
        nor an indication of its assert status."
  ;; This semi-pretty prints because the form is translated into a list.
  ;; It would be better
  ;;    if the Lisp pretty printing features were used directly.
  (format stream "~@<~W~:>"
	  (cons (if (set:singletonp args) 'not 'nor)
		(set:set-to-list args))))

(defun print-negationbyfailure (args stream)
  "Prints to the stream
        the negationbyfailure (thnot or thnor) whose arguments are args.
     Prints it not preceded by its wft name
        nor an indication of its assert status."
  ;; This semi-pretty prints because the form is translated into a list.
  ;; It would be better
  ;;    if the Lisp pretty printing features were used directly.
  (format stream "~@<~W~:>"
	  (cons (if (set:singletonp args) 'thnot 'thnor)
		(set:set-to-list args))))

(defun print-nary (fn args stream)
  "Prints to the stream
        the term whose function symbol is fn, and whose arguments are args,
        without using setof for the args.
     Prints it not preceded by its wft name
        nor an indication of its assert status."
  ;; This semi-pretty prints because the form is translated into a list.
  ;; It would be better
  ;;    if the Lisp pretty printing features were used directly.
  (format stream "~@<~W~:>" (cons fn (set:set-to-list args))))
 

(defun print-param2op (fn min max args stream)
  "Prints to the stream
        the param2op term whose function symbol is fn,
        whose parameters are (min max),
        and whose arguments are args,
        without using setof for the args.
     Prints it not preceded by its wft name
        nor an indication of its assert status."
  ;; This semi-pretty prints because the form is translated into a list.
  ;; It would be better
  ;;    if the Lisp pretty printing features were used directly.
  (format stream "~@<~W~:>" `(,fn (,min ,max) ,@(set:set-to-list args))))

(defun print-named-molecular-term (term stream)
  "Prints to the stream
        the molecular term preceded by its wft name
        and an indication of its assert status."
  (format stream "~@<~W~:[~*~;~:[?~;!~]~]: ~W~:>"
	  (name term)
	  (typep term 'Proposition)
	  (ct:assertedp term (ct:currentContext)) term))

;;; Description Methods
;;; ===================

(defgeneric description (term)
  (:documentation
   "Returns a description string for term."))

(defmethod description ((term atom))
  (format nil "~<~S~:>" term))

(defmethod description ((term molecular))
  (funcall (cf:caseframe-descfun (caseframe term)) term))

(defmethod description ((termset set:set))
  (let ((desc ""))
    (set:loopset for trm in termset
		 do (setf desc
		      (concatenate 'string
			desc ", and " (description trm))))
    (subseq desc 6)))

;;; Functions for Writing to Files
;;; ==============================

(defun writeKBToTextFile (file &optional headerfile)
  "Writes the KB to the given text file,
   so that when that file is loaded,
   all the propositions asserted in the current KB
   will be asserted in the new KB.
   If the headerfile is included,
      a load of that file will be written before any of the asserts."
  ;; Assumes that all required Types, Contexts, Slots, and Caseframes
  ;;    will be defined before the first assert in the file is performed.
  (with-open-file (opstream file
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (format opstream ";;; SNePS 3 KB~%~
                      ;;; ==========~%")
    (multiple-value-bind (sec min hr date mon year) (get-decoded-time)
      (format opstream ";;; ~d/~d/~d ~d:~d:~d~%"
	      mon date year hr min sec))
    (when headerfile (format opstream "~&(load ~S)~%" headerfile)) 
    (format opstream ";;; Assumes that all required Contexts, Types, Slots, ~
                           and Caseframes have now been loaded.~%~
                      (in-package :snuser)~%")
    (let ((*package* (find-package :snuser)))
      (maphash
       #'(lambda (cname context) 
	   (set:loopset for hyp in (util:resource-value (ct::ct-hyps context))
			do 
			(format opstream "~&(ct:assert '")
			(if (typep hyp 'atom)
			    (print-object hyp opstream)
			  (print-unnamed-term hyp opstream))
			(format opstream " '~S :origintag :hyp)" cname))
	   (set:loopset for der in (util:resource-value (ct::ct-ders context))
			do 
			(format opstream "~&(ct:assert '")
			(if (typep der 'atom)
			    (print-object der opstream)
			  (print-unnamed-term der opstream))
			(format opstream " '~S :origintag :der)" cname))
	   )
       ct:*CONTEXTS*))))
