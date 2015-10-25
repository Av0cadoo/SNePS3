;;; SNePS 3: Definition of the sneps3-user package
;;; ==============================================
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

(defpackage :sneps3-user
  (:nicknames :snuser)
  (:shadow cl:assert)
  
  (:import-from
   :sneps3-set

   ;; Reserved words
   #:setof)

  (:shadowing-import-from :sneps3 #:find)
			  
  (:import-from
   :sneps3
   
   ;; Reserved words
   #:Isa #:nor #:nand #:andor #:thresh #:xor #:iff #:thnot #:thnor
   
   ;; Slot names
   #:andorargs #:threshargs
   
   ;; Constant Propositions
   #:T #:F

   ;; Globals
   #:*KRNovice*
   #:*PRECISION*
   
   ;; Classes
   #:Entity #:Proposition #:Act #:Policy #:Thing
   #:Category #:Action

   ;; Functions
   #:box #:unbox
   #:.+. #:.-. #:.*. #:./. #:.<. #:.<=. #:.>. #:.>=. #:.=. #:./=.
   #:find-term #:find-terms #:listkb #:list-terms #:showTypes #:startGUI
   #:writeKBToTextFile
   
   ;; Macros
   #:defineType)

  (:import-from
   :sneps3-slot

   ;; Functions
   #:list-slots

   ;; Slot-based inference regimes
   #:reduce #:expand #:none
   )
  
  (:import-from
   :sneps3-caseframe
   #:list-caseframes #:sameFrame)
  
  (:import-from
   :context

   ;; Symbols
   #:BaseCT #:DefaultCT
   
   ;; Functions
   #:currentContext #:defineContext #:list-contexts
   #:setCurrentContext
   #:unassert
   )
  
  (:import-from
   :snip
   
   ;; Symbol used to define slots
   #:ant #:cq
   #:definePath

   ;; Functions
   #:pathsfrom
   
   )
  (:import-from
   :snere
   #:attachPrimaction #:definePrimaction #:perform
   #:actions)
  )

(in-package :sneps3-user)

;;; Global varibles

(defconstant *demo-index-file*
  #+mswindows "Demo\\index.cl"
  #-mswindows "Demo/index.cl"
  "The demo index file to used based on operating system.")

;;; User Functions

(defun assert (expr)
  "Asserts the term expressed by expr in the current context."
  (ct:assert expr (currentContext)))

(defun assert-all (exprs)
  "Asserts all the expressions in the list exprs."
  (loop
      for expr in exprs
      do (assert expr)))

(defun assert! (expr)
  "Asserts the term expressed by expr in the current context
      and then do forward inference."
  (let*  (
	  ;; First, assert the term        
	  (term (ct:assert expr (currentContext)))

	  ;; Second, do forward chaining
          (results (snip:inferByForward term (currentContext) nil))) 

	  (if (and sneps3::*sneps-gui-ptr*
                   sneps3::*auto-refresh-graph*)
              (sneps3::add-asserted-wft-to-graph term))
         
         ;; Show all the terms which are asserted by forward chaining
         (loop for result in results  
               until (eq result term)
               do (block eval 
			(if (and sneps3::*sneps-gui-ptr*
             			 sneps3::*auto-refresh-graph*)
        		    (sneps3::add-asserted-wft-to-graph result))
		       (format t "~S~%" result)))
         term))

(defun ask (exprpat)
  "Returns a set of instances of the term pattern exprpat or its negation
        that are derivable in the current context;
        or the empty set if there are none."
  (let ((ret (set:or.set    ; Until exprpat can be non-ground --- then set:union 
	      (askif exprpat) 
	      (askifnot exprpat))))
    (if (and sneps3::*sneps-gui-ptr* 
	     sneps3::*auto-refresh-graph*)
        (sneps3::generate-graph)) ;;Added by [DRS] 5/22/2010, modified 5/26
    ret))

(defun askif (exprpat)
  "Returns a set of instances of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  (when snip:*GOALTRACE*
    (format *trace-output* "~&~%I will try to derive ~S~%" exprpat))
  (snip:askif (sneps3:build exprpat (find-class 'Proposition))
	      (currentContext) nil))

(defun askifnot (exprpat)
  "Returns a set of instances of the negation of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  (when snip:*GOALTRACE*
    (format *trace-output* "~&~%I will try to derive the negation of ~S~%"
	    exprpat))
  (snip:askif
   (sneps3:build `(not ,exprpat) (find-class 'Proposition))
   (currentContext)
   nil))

(defun showproofs (&key (goals nil))
  "Prints the proofs of derived terms.
     If goals is non-null, messages are printed
        when goals are generated, and and when inference methods are tried."
  (declare (special snip:*TRACE* snip:*GOALTRACE*))
  (setf snip:*TRACE* t
	snip:*GOALTRACE* goals))

(defun noshowproofs ()
  "Turns off printing of proofs, goals, and methods to be tried."
  (declare (special snip:*TRACE* snip:*GOALTRACE*))
  (setf snip:*TRACE* nil
	snip:*GOALTRACE* nil))

(defun allTerms (&key (test #'(lambda (x)
				(declare (ignore x))
				t)))
  "Returns a set of all the terms in the knowledge base that satisfy the test."
  (set:new-set
   :items (loop for trm being the hash-value
	      of (util:resource-value sneps:*TERMS*)
	      if (funcall test trm) 
	      collect trm)))

(defun remove-from-context (trm ctx)
  "Removes the term (trm) from the context (ctx)."
  (ct:remove-from-context trm ctx))

(defun erase-term (trm)
  "Erases a term (trm) from the network completely, if
   possible. Returns trm if successful, nil otherwise."
  (sneps3:erase-term trm))

;;; User Macros

(defmacro describe-terms (&rest wftnames)
  "Prints a description of all the given terms."
  `(progn
     (loop for tname in ',wftnames
	 do (format *standard-output* "~A~%"
		    (sneps:description (sneps:find-term tname))))
     (values)))

(defmacro defineSlot (name &rest args)
  "Defines the slot."
  `(prog1
       (slot:defineSlot ',name
       ,@(loop for (keyword value) on args by #'cddr
	     collect keyword
	     collect `(quote ,value)))
     (when (member :path ',args)
       (definePath ',name (second (member :path ',args))))))

(defun defineCaseframe (type frame &key (docstring "") (fsymbols ()))
  "Defines a caseframe.
      where <type> is the name of a SNePS semantic type,
            <frame> is either (slot1 ... slotn)
                           or ('function-symbol slot1 ... slotn)
            <docstring> is a caseframe documentation string;
            <fsymbols> is a list of function symbols
                       given if first of the <frame> is not quoted."
  (cf:defineCaseframe
       type
       (cond ((and (consp frame) (symbolp (first frame)))
		frame)
	       ((and (consp (first frame))
		    (eq (first (first frame)) 'quote))
		     (rest frame))
		    (t (error
			"The frame, ~S, must be a list of slot names or ~
                                    a list containing a quoted atomic constant followed by slot names."
			frame)))
     :docstring docstring
     :print-pattern frame
     :fsymbols fsymbols))

(defun defineTerm (term &optional (semtype 'Entity))
  "Finds or builds the given term,
       assures that is of the given semantic type,
       and returns the term."
  (let ((ret 
    (cond 
     ((and (typep term 'cons) (or (eq (first term) 'some)
				(eq (first term) 'every)))
      (ct:build-variable term))
     ((typep term 'cons) 
      (ct:variable-parse-and-build term))
     (t (sneps:build term (find-class semtype))))))
   (if (and sneps3::*sneps-gui-ptr*
	     sneps3::*auto-refresh-graph*)
	(sneps3::add-asserted-wft-to-graph ret)) ;;[DRS-4/18/2011]
   ret))

