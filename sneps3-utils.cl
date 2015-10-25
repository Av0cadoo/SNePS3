;;; Utility Functions for SNePS 3
;;; =============================
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

(defpackage :sneps3-utility
  (:nicknames :util)
  (:use 
   :common-lisp 
   #+ltk :ltk
   )
  (:export #:make-resource #:menuChoice #:menuChooseFromList
	   #:protecting #:resource-value)
  )

(in-package :util)

#+ltk
(defun menuChooseFromList (&rest msgs&choices)
  "Takes a sequence of alternating message strings and lists of choices.
     Presents each list of choices to the user preceded by its message string,
        and solicits the user's choice for each list of choices.
    If the user finally approves, multiple values are returned.
    Each value is the user's choice from a list of choices.
    Or the user can cancel the request,
       in which case nil is returned."
  (let ((choices (multiple-value-list (apply #'menuChoice msgs&choices))))
    (when (< (first choices) (length (second msgs&choices)))
      (values-list
       (loop for possibleChoices in (rest msgs&choices) by #'cddr
	   for i in choices
	   collect (nth i possibleChoices))))))

#+ltk
(defun menuChoice (&rest msgs&choices)
  "Takes a sequence of alternating message strings and lists of choices.
     Presents each list of choices to the user preceded by its message string,
        and solicits the user's choice for each list of choices.
    If the user finally approves, multiple values are returned.
    Each value is the number of the choice from a list of choices, zero-based.
    Or the user can cancel the request,
       in which case the length of the first list of choices is returned."
  (with-ltk ()
    (let* ((of (make-instance 'frame :master nil))
	   (f (make-instance 'frame :master of))
	   (bf (make-instance 'frame :master nil))
	   (valueButtons 
	    (loop for msg in msgs&choices by #'cddr
		for choices in (rest msgs&choices) by #'cddr
		for numset from 1
		do (pack (make-instance 'label
			   :master f
			   :text msg))
		collect (loop for choice in choices
			    for choicenum from 0
			    for rb = (make-instance 'radio-button
				       :master f :background "white"
				       :activebackground "orange"
				       :selectcolor "green"
				       :text (format nil "~A" choice) :anchor :w
				       :variable (format nil "choice~D" numset)
				       :value choicenum)
			    do (pack rb :fill :x :anchor :e)
			       (configure rb :indicatorOn 0)
			    finally (return rb)))))
      (pack of) (pack f) (pack bf)
      (pack (make-instance 'button
	      :master bf :text "OK"
	      :command
	      (lambda ()
		(return-from menuChoice
		  (values-list (mapcar #'value valueButtons)))))
	    :side :left)
      (pack (make-instance 'button
	      :master bf :text "Cancel"
	      :command (lambda ()
			 (return-from menuChoice
			   (length (second msgs&choices)))))
	    :side :left))))

#-(or common-graphics ltk)
(defun menuChooseFromList (msgString choices &rest rest)
  "Uses menuChoice to present the msgString and the choices.
       returns the choice chosen, or nil if the user chooses 'cancel'."
  (nth (menuChoice msgString choices) choices))

#-(or common-graphics ltk)
(defun menuChoice (msgString choices &rest rest)
  "Presents the msgString,
        the set of choices, plus an added \"cancel\" choice,
        as a numbered list;
     reads the number of the user's choice;
     and returns that number minus one."
  (unless choices
    (error "chooseFromMenu called with no choices."))
  (let ((len (1+ (length choices))))
    (format *query-io* "~%~A~2%" msgString)
    (loop for i from 1
	for item in choices
	do (format *query-io* "~&~D. ~A" i item)
	finally (format *query-io* "~&~D. ~A~2%" i "Cancel"))
    (loop 
	for choice = (read *query-io*)
	until (and (numberp choice)
		   (<= 1 choice len))
	do (format *query-io*
		   "~%Please enter a number between 1 and ~A. " len)
	finally (return (1- choice)))))

#+(and common-graphics (not ltk))
(defun menuChooseFromList (msgString choices &rest rest)
  "Presents the msgString and the choices.
     Returns the choice chosen, or nil if the user chooses 'cancel'."
  (cg:ask-user-for-choice-from-list msgString choices))

#+(and common-graphics (not ltk))
(defun menuChoice (msgString choices &rest rest)
  "Presents the msgString,
        and the set of choices, plus an added \"cancel\" choice,
        as a numbered list;
     reads the number of the user's choice;
     and returns that number minus one."
  (multiple-value-bind (choice chose)
      (cg:ask-user-for-choice-from-list msgString choices)
    (if chose
	(position choice choices :test 'equal)
      (length choices))))

(defstruct resource
  "An easily protected resource to be shared among multiple threads.
    Should be used as the value of any non-local variable that needs such protection."
  (lock					; The process lock for protecting this resource
   (mp:make-process-lock :name (string (gensym "Semaphore")))
   :read-only t)
  (value)				; The values stored in this resource
  (:documentation
   "An easily protected resource to be shared among multiple threads."))

(defmacro protecting (resource &body forms)
  "Evaluate forms while protecting resource from corruption via
      multiple threads.
    The resource argument must be a variable whose value is a util:resource."
  `(mp:with-process-lock ((resource-lock ,resource))
     ,@(subst `(resource-value ,resource) resource forms :test 'equal)))
