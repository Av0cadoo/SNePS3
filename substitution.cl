;;; SNePS 3: Substitution
;;; ====================
;;; Michael Kandefer
;;; Department of Computer Science and Engineering
;;; State University of New York at Buffalo
;;; mwk3@buffalo.edu

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




(defpackage :sneps3-substitution
  (:nicknames :subs)
  (:export 
   #:add-var-term-pair #:add-var-term-pairs #:binds-term
   #:copy-substitution #:emptyp #:equal-bound-term #:join
   #:new-substitution #:size #:substitution #:variables #:var-to-term))

(in-package :sneps3-substitution)

(defstruct (substitution (:print-function print-substitution))
  "Substitution constructor definition. 
   Substitutions have a hash table that has as a key
   that is a var and a terms that
   can substitute for the var. "
  (table (make-hash-table :test #'eq)))

(defun new-substitution (&key (size 3 size-given) (var-term-pairs '()))
  "Return a new substitution.
   If size is provided, the underlying hash-table representation is
   set to that initial size;
   else, and var-term-pairs is, then the size of the hash-table is set to the
        length of var-term-pairs;
   else, a default is used.
   If var-term-pairs is a non-empty list of the form 
       (var1 term1 var2 term2 ...),
   then the elements of that list are added to the substitution."
  (let ((new-subst
	 (make-substitution 
	  :table (make-hash-table
		      :test #'eq
		      :size (cond
			     (size-given size)
			     ((consp var-term-pairs) (length var-term-pairs))
			     (t size))))))
    (when (consp var-term-pairs)
      (add-var-term-pairs var-term-pairs new-subst))
    new-subst))

(defmethod print-substitution (substitution stream depth)
  "Print the hash set"
  (declare (ignore depth))
  (if (emptyp substitution)
      (format stream "()")
    (let ((*print-length* nil))
      (format stream "[ ")
      (loop for e being each hash-key of (substitution-table substitution)
	  do (format stream "(~A . ~A) " e (gethash e (substitution-table substitution))))
      (format stream "]"))))

(defun add-var-term-pair (var term substitution)
  "Desctructively adds term-set, a set of terms, to the substitution
   for var."
  (check-type substitution substitution)
  (setf (gethash var (substitution-table substitution)) term)) 

(defun add-var-term-pairs (var-term-pairs substitution)
  "Destructively adds var-term-pairs to the substitution."
  (check-type substitution substitution)
  (when (consp var-term-pairs)
    (loop 
	for (var term) on var-term-pairs by #'cddr
	do (add-var-term-pair var term substitution))))

(defun size (substitution)
  "Returns the number of vars in the substitution"
  (check-type substitution substitution)
  (hash-table-count (substitution-table substitution)))

(defun variables (substitution)
  "Returns a list of the variables in this substitution."
  (check-type substitution substitution)
  (loop 
      for key being the hash-keys of (substitution-table substitution)
      collect key))

(defun emptyp (substitution)
  "Returns true if the substitution is the empty"
  (check-type substitution substitution)
  (= (size substitution) 0)) 

(defun var-to-term (var substitution)
  "Returns the value bound to this variable in this substitution."
  (check-type substitution substitution)
  (gethash var (substitution-table substitution)))


;;; We've been using loops across hash tables, and maphash. Which is prefered?
(defun binds-term (term substitution)
  "Returns the variable the term is bound in this substitution, 
   else returns nil."
  (check-type substitution substitution)
  (catch 'bound-term
    (maphash 
     #'(lambda (k v) 
	 (when (eq term v)
	   (throw 'bound-term k)))
     (substitution-table substitution))
    nil))

(defun equal-bound-term (first second)
  "Determines if two substitutions bind the same term, 
   regardless of variable. Returns nil if they don't."
  (check-type first substitution)
  (check-type second substitution)
  (let ((result 
	 (loop 
	     for key being the hash-keys of (substitution-table first)
	     if (binds-term (var-to-term key first) second)
	     do (return t))))
    result))
  

(defun copy-substitution (substitution)
  "Returns a copy of the passed in substitution."
  (check-type substitution substitution)
  (let ((subs-copy (new-substitution)))
    (maphash #'(lambda (k v)
		 (add-var-term-pair k v subs-copy))
	     (substitution-table substitution))
    subs-copy))

(defun join (first second)
  "Combines two substitutions into one."
  (check-type first substitution)
  (check-type second substitution)
  (let ((new-subs (copy-substitution first)))
    (loop 
	for key being the hash-keys of (substitution-table second)
	if (not (var-to-term key new-subs))
	do (add-var-term-pair key (var-to-term key second) new-subs) 
	end)
    new-subs))
                  
    
    
