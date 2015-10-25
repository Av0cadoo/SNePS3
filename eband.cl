;;; Eager-beaver and
;;; an example of multiprocessing in ACL
;;; by Stuart C. Shapiro

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


(defmacro eband (&rest args)
  "Evaluates its arguments in parallel, in separate processes.
   Returns True if all its arguments evaluate to True.
   Returns nil if, and as soon as, any of its arguments evaluate to nil."
  ;; Written by Stuart C. Shapiro
  (let ((flag (gensym))			; to get final answer
	(childProcessCount (gensym))	; number of child processes
	(childProcessCountSemaphore (gensym))
        (localresult (gensym)))		; local to each child process
    `(let ((,flag t)
	   (,childProcessCount ,(length args))
	   (,childProcessCountSemaphore
	    (mp:make-process-lock :name "childProcessCountSemaphore")))
       ,@(mapcar
	  ;; Spawn a child process for each argument.
          #'(lambda (arg i)
              `(mp:process-run-function
		   (list
		    :name (format nil "ChildProcess ~D" ,i)
		    :resume-hook
		    ;; Evaluated whenever process is resumed.
		    #'(lambda ()
			;;(format t "~A is resuming.~%" mp:*current-process*)
			(when (null ,flag)
			  ;; As soon as flag is set to nil,
			  ;; decrement count and quit.
			  (mp:with-process-lock (,childProcessCountSemaphore)
			    (decf ,childProcessCount))
			  (mp:process-kill mp:*current-process*))))
		 ;; Function run by each child process.
		 #'(lambda () 
		     ;;(format t "~&Processing ~A" mp:*current-process*)
		     ;; See if it can quit before doing any work.
		     (cond ((null ,flag)
;;;			    (format t "~&Process ~A is quitting without doing anything.~%"
;;;				    mp:*current-process*)
			    ;; As soon as flag is set to nil, quit.
			    (mp:with-process-lock (,childProcessCountSemaphore)
			      (decf ,childProcessCount)))
			   (t 
			    ;; Evaluate the argument
			    (let ((,localresult ,arg)) ; Here's what takes time.  
			      ;; Contribute the value of its arg
			      ;;    to the final value.
			      (cond (,localresult
				     ;; Check out,
				     ;; but let others continue.
				     (mp:with-process-lock
					 (,childProcessCountSemaphore)
				       (decf ,childProcessCount)))
				    (t
				     ;; Register result
				     (setf ,flag nil)
				     (mp:with-process-lock
					 (,childProcessCountSemaphore)
				       (decf ,childProcessCount))))))))))
          args
	  (loop for i from 1 to (length args)
	      collect i))
       ;; Wait until some child process sets the answer to nil,
       ;;   or until all child processes are done.
       (mp:process-wait 
	"Checking wait status"
	#'(lambda ()
	    (or (null ,flag)
		(zerop ,childProcessCount))))
       ;; Return the value the children have computed.
       ,flag)))

#| Test

(defun cnt (n count)
  (format t "~&Starting loop ~D.~%" count)
  (loop for i from 1 to n
      finally 
	(format t "~&Finished with loop ~D.~%" count)
	(return t)))

;;; and has to do one loop entirely before terminating.
;;; eband can terminate before finishing one loop.
;;; and took over 100 times as long as eband on 6/7/07
(time (eband (cnt 1000000000 1) (print nil) (cnt 1000000000 2)))

;;; Not so good test
;;; Makes a long list of long lists of random integers.
;;; Returns nil if one of the doesn't have a 3 in it.
;;; Member is so fast that if an early list is missing a 3
;;;    eband is not worth the overhead.
(defun longlist (length)
  (loop for i from 1 to length
      collect (random (* length 3))))

(defun longlists (length)
  (loop for lst in (loop for i from 1 to length
		       collect (longlist (1+ (random (* length 3)))))
      when (< (random 10) 9)
      do (setf (cdr (last lst)) (list 3))
	 collect lst))

(setf ll (longlists 500)
      args (mapcar #'(lambda (l)
		       `
		       (print
			 (member 3 ',l)
			 )
		       ) ll))

(time (eval `(eband ,@args)))

;;; Conclusion:
;;;    eband is worth while if at least some argument takes a very long time to evaluate,
;;;    but it's not worth it for a lot of arguments all of which are quick to evaluate.
|#
