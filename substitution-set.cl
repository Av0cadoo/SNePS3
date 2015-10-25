;;; SNePS 3: Substitution-Set
;;; ====================
;;;  Michael Kandefer
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

(defpackage :sneps3-substitution-set
  (:nicknames :subs-set)
  (:export 
   #:copy-all
   #:union #:union-into-first
   #:add-binding
   #:copy-substitution-set
   #:get-all-terms-from-var #:join #:join-into-first
   #:new-substitution-set #:remove-if
   #:remove-if-contains-vars #:remove-if-contains-value 
   #:substitution-set
   #:*emptysubset*)
  (:shadow cl:remove-if cl:append cl:union))

(in-package :sneps3-substitution-set)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (substitution-set 
	      (:include set:set (set:type 'subs:substitution))
	      (:print-function set:print-set))
    "Substitution-set definition. Like regular sets in structure, except
     every element is a substitution.")
  
  (defun new-substitution-set (&key (items '()) (size 12 size-given))
    "Returns a new substitution-set."
    (let ((new-subs-set
	   (make-substitution-set
	    :table (make-hash-table 
		    :test #'equalp
		    :size (cond 
			   (size-given size)
			   (items (length items))
			   (t size))
		    :rehash-size 2.0))))
      (when (consp items)
	(set:add-items items new-subs-set))
      new-subs-set)))

(eval-when (:load-toplevel :execute)
  (defconstant *emptysubset* (new-substitution-set :size 0)
    "A constant empty set
        to be used whenever an empty set is to be returned from some function.
        It must never be destructively changed."))


(defun copy-substitution-set (subs-set)
  "Returns a copy of the given substitution-set"
  (check-type subs-set substitution-set)
  (let ((subs-set-copy (new-substitution-set)))
    (maphash #'(lambda (k v) 
		 (declare (ignore v))
		 (set:add-item k subs-set-copy))
	     (substitution-set-table subs-set))
    subs-set-copy))
    
    
(defmethod get-all-terms-from-var (var (subs-set substitution-set))
    "Returns the set of terms bound to var in this substitution set."
    (let ((result (set:new-set)))
      (set:loopset 
       for subs in subs-set
       for term = (subs:var-to-term var subs)
       when term
          do (set:add-item term result))
      result))



(defun remove-if (pred set)
  "Returns a new set without those elements in set that pass the pred test."
  (check-type set substitution-set) 
  (let ((result (new-substitution-set)))
    (set:loopset for subs in set
	     do (unless (funcall pred subs)
		  (set:add-item subs result)))
    result))

(defmethod join  ((first substitution-set) (second substitution-set))
  "Joins two substitution sets together. Results in a permuation of
   the elements between them, such that each substitution has the same
   number of bindings, and the same variables. Bound terms differ
   between substitutions. 

   This function was written for creating substitution-sets for matching 
   term-sets. As such some assumptions are made that wouldn't be used for 
   joining normal substitution sets together." 
  (cond ((set:emptyp first) second)
	((set:emptyp second) first)
	(t 
	 (let ((result (new-substitution-set)))
	   (set:loopset 
	    for subs1 in first
	    do (set:loopset
		for subs2 in second
		when (not (subs:equal-bound-term subs1 subs2)) 
		do (set:add-item (subs:join subs1 subs2) result)))
	   result))))

(defmethod join-into-first ((first substitution-set) (second substitution-set))
  "Same as join, except modifies the first parameter instead of
   creating a new substitution."
  (cond ((set:emptyp first) 
	 (set:loopset for subs in second
		      do (set:add-item subs first))
	 first)
	((set:emptyp second) first)
	(t 
	 (set:loopset 
	  for subs1 in first
	  do (set:remove-item subs1 first) 
	     (set:loopset
	         for subs2 in second
	         when (not (subs:equal-bound-term subs1 subs2)) 
	         do (set:add-item (subs:join subs1 subs2) first)))
	   first)))

(defmethod union ((s1 substitution-set) (s2 substitution-set))
  "Returns a substitution set that is the union of the two argument sets."
  (let ((s3 (new-substitution-set :size (+ (set:cardinality s1) (set:cardinality s2)))))
    (loop for item being each hash-key of (substitution-set-table s1)
	do (set:add-item item s3))
    (loop for item being each hash-key of (substitution-set-table s2)
	do (set:add-item item s3))
    s3))


(defmethod union-into-first ((s1 substitution-set) (s2 substitution-set))
  "Returns a substitution set that is the union of the two argument
  sets. The first set gets any new values, as such, this function is destructive."
  (loop for item being each hash-key of (substitution-set-table s2)
	do (set:add-item item s1))
    s1)

(defmethod add-binding (var term (subs-set substitution-set))
  "Creates a new substitution from subs-set and adds a var-term binding to each 
   substitution in a new substitution set."
  (let ((new-subs (subs:new-substitution :var-term-pairs (list var term)))
	(new-subs-set (new-substitution-set)))
    (if (set:emptyp subs-set)
	(set:add-item new-subs new-subs-set)  
      (set:loopset 
       for subs in subs-set
       do (set:add-item (subs:join subs new-subs) new-subs-set)))
    new-subs-set)) 

(defmethod copy-all ((old-subs substitution-set) 
		     (new-subs substitution-set))
  "Copies all elements from the second supstitution into the first,
   after clearing the first. Allows forreferences to be retained."
  (setf (substitution-set-table old-subs) 
    (make-hash-table 
     :test #'equalp
     :size (set:cardinality new-subs)
     :rehash-size 2.0))
  (union-into-first old-subs new-subs)
  old-subs)
