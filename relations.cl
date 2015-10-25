;;; SNePS 3: Definition of Slots
;;; ================================
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

(defpackage :sneps3-slot
  (:nicknames :slot)
  (:export 
   ;; Global variables
   *SLOTS*
   ;; Types
   #:slot
   ;; Functions
   #:defineSlot #:find-slot #:list-slots #:slot-p
   #:slot-type #:slot-max #:slot-min #:slot-name
   #:slot-negadjust #:slot-posadjust
   #:slot-docstring
   ;; Slot-based inference regimes
   #:reduce #:expand #:none
   ;; Paths
   #:slot-path #:slot-f-pathfn #:slot-b-pathfn
   ))

(in-package :sneps3-slot)

(defparameter *SLOTS* (make-hash-table)
  "Map from slot name to slot object.")

(defstruct slot
  "A SNePS slot"
  ;; (slotname option) ..
  (name (gensym "rel") ; the name of the slot
	:type symbol  :read-only t)
  (type (find-class 'sneps3:Entity) ; the type of terms it points to
	:read-only t)
  (docstring "" ; A documentation string for the slot
	     :type string :read-only t)
  (posadjust 'reduce ; for slot-based inference: reduce, expend, or none
	     :type symbol :read-only t)
  (negadjust 'expand ; for slot-based inference of negative instances
	     :type symbol :read-only t)
  (min 1 ; minimum number of slot fillers
       :type integer :read-only t)
  (max nil ; maximum number of slot fillers, nil means infinite
       :read-only t)
  (path nil) ; the path that implies this slot
  (f-pathfn nil) ; "forward" path function 
  (b-pathfn nil)) ; "backward" path function 


(defmethod print-object ((rel slot) stream)
  (print-unreadable-object
      (rel stream)
    (format stream
	    "~<name: ~W ~_ docstring: ~W ~_ type: ~W ~_ min: ~W max: ~W   posadjust: ~W negadjust: ~W~:>"   
	    (list (slot-name rel) (slot-docstring rel)
	    (slot-type rel) (slot-min rel)
	    (slot-max rel) (slot-posadjust rel)
	    (slot-negadjust rel)))))

(defun defineSlot (name &key (type 'sneps3:Entity typegiven)
				 (docstring "")
				 (posadjust 'reduce posadjustgiven)
				 (negadjust 'expand negadjustgiven)
				 (min 1 mingiven)
				 (max nil maxgiven)
				 (path nil))
  "Defines the slot."
  (assert (symbolp name) (name) "~S is not a symbol." name)
  (when (find-slot name nil)
    (cerror "Use the existing definition."
	    "~S is already a slot defined as ~S" name (gethash name *SLOTS*))
    (return-from defineSlot (gethash name *SLOTS*)))
  (assert (or (not typegiven)
	      (sneps:semantic-type-p type)) (type)
    "~S is not a semantic type" type)
  (assert (stringp docstring) (docstring)
    "Docstring, ~S, is not a string." docstring)
  (assert (or (not posadjustgiven)
	      (member posadjust '(reduce expand none))) (posadjust)
    "posadjust, ~S, is not one of 'reduce 'expand or 'none" posadjust)
  (assert (or (not negadjustgiven)
	      (member negadjust '(reduce expand none))) (negadjust)
    "negadjust, ~S, is not one of 'reduce 'expand or 'none" posadjust)
  (assert (or (not mingiven)
	      (and (integerp min) (not (minusp min)))) (min)
    "min, ~S, must be a positive integer." min)
  (assert (or (not maxgiven)
	      (or (null max)
		  (and (integerp max)
		       (>= max min)))) (max)
    "max, ~S, must be nil or an integer greater than or equal to min (~D)."
    max min)
  (setf (gethash name *SLOTS*)
    (make-slot :name name
		   :type (find-class type)
		   :docstring docstring
		   :posadjust posadjust
		   :negadjust negadjust
		   :min min
		   :max max
		   :path path))
  ;; Added by DRS 4/1/2010
  (if sneps3::*sneps-gui-ptr* (sneps3::send-one-slot (gethash name *SLOTS*)))
  (gethash name *SLOTS*))

  
(defun find-slot (rname &optional (errorp t))
  "If rname is a slot, returns it;
     if it is the name of a slot, returns the slot object;
     else if error p is True, raises an errorr
          else returns nil."
  (declare (special *SLOTS*))
  (if (slot-p rname)
      rname
    (let ((slot (gethash rname *SLOTS*)))
      (cond (slot)
	    (errorp (error "There is no slot named ~S." rname))
	    (t nil)))))

(defun list-slots ()
  "Prints a list of all the SNePS slots."
  (loop for r being each hash-value of *SLOTS*
      do (print r)))
