;;; SNePS 3: Demo
;;; =============
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

(in-package :sneps3-user)

(defun demo (&key (file nil) (pause nil))
  "Echoes and evaluates the forms in the file.
   If pause is t, will pause after echoing each form,
   but before evaluating it.
     If the file is omitted, a menu will be presented of available demos."
  ;; Written by SCS 7/19/01
  ;; Modified by SCS 6/26/07
  ;; SNePS 2 made the following options available at pause points:
  ;; "~%The following commands are available at pause points:~
  ;;  ~%  h,?            Print this help message~
  ;;  ~%  l,^            Enter Lisp read/eval/print loop~
  ;;  ~%  s,%            Enter SNePS toplevel loop~
  ;;  ~%  o,:            Enter SNePSLOG~
  ;;  ~%  c              Continue without pausing~
  ;;  ~%  p              Set pause control~
  ;;  ~%  q              Quit this demo~
  ;;  ~%  a              Quit all demos~
  ;;  ~%  any other key  Continue the demo~
  ;;  ~%"
  (declare (special *DEMOQUIT*))
  (check-type file (or null string))
  (unless file
    (multiple-value-setq (file pause)
      (chooseDemoFile&PauseControl)))
  (when file
    (with-open-file
	(stream file) :direction :input
	(setf stream
	  (make-echo-stream stream *standard-output*))
	(format *standard-output*
		"~&Beginning of \"~A\" demonstration.~%"
		file)
	(when pause
	  (format *standard-output*
		  "~%  The demo will pause between commands,~
                              ~%at that time press RETURN to continue~
                              ~%or h for a list of options."))
	(prog (input value (donesymb (gensym)))
	 loopTop
	  (format *standard-output* "~%input:~%")
	  (when (eql (setf input (read stream nil donesymb))
		     donesymb)
	    (return))
	  (when pause
	    (loop
	      (format *query-io* "~&--- pause ---")
	      (let ((pauseInput (read-line *query-io*))
		    (stay nil))
		(cond ((string-equal pauseInput ""))
		      ((string-equal pauseInput "c")
		       (setf pause nil))
		      ((string-equal pauseInput "q")
		       (return-from demo (values)))
		      ((or (string-equal pauseInput "l")
			   (string-equal pauseInput "^"))
		       (break "Demo Interrupted.")
		       (setf stay t))
		      ((or (string-equal pauseInput "h")
			   (string-equal pauseInput "?"))
		       (format *standard-output*
			       "~%The following commands are available at pause points:~
                              ~%  h,?            Print this help message~
                              ~%  l,^            Enter Lisp read/eval/print loop~
                              ~%  c              Continue without pausing~
                              ~%  q              Quit the demo~
                              ~%  RETURN         Continue the demo~
                              ~%")
		       (setf stay t)))
		(unless stay (return)))))
	  (unwind-protect
	      (progn (format *standard-output* "~&output:~%")
		     (setf value
		       (multiple-value-list (eval input)))
		     ;; value is nil if the input form returns no value,
		     ;; but (nil) if it returns nil.
		     ;; If the input form returns no value,
		     ;; none will be printed by the following format.
		     (format  *standard-output* "~&~{~S~^~%~}~&" value))
	    (if *DEMOQUIT*
		(setf *DEMOQUIT* nil)
	      (go loopTop)))))
    (format *standard-output* "~%End of \"~A\" demonstration.~%" file)
    (values)))

(defparameter *DEMOQUIT* nil)

(defun quit ()
  "Call this function from an error-break of demo
        to quit the demo."
  (declare (special *DEMOQUIT*))
  (setf *DEMOQUIT* t))

(defun chooseDemoFile&PauseControl ()
  "Returns the name of a demo file to demo,
       and a pause control to use;
    or nil if the user cancels the demo."
  (let* ((menu (with-open-file
		   (stream (concatenate 'string
			     cl-user::*sneps-directory*
			     *demo-index-file*))
		 :direction :input
		 (read stream))))
    (multiple-value-bind (choice pausep)
	(util:menuChoice "Choose a file to demo:" (mapcar #'car menu)
			 "Pause?" '(:Yes :No))
      (when (< choice (length menu))
	(values (concatenate 'string
		  cl-user::*sneps-directory*
		  "Demo/"
		  (nth choice (mapcar #'cdr menu)))
		(when pausep (nth pausep '(t nil))))))))
