;;; SNePS 3: Sets
;;; =============
;;; Written by Michael Kandefer
;;;    with additional/modifications by Stuart C. Shapiro
;;; Implements sets in lisp using hash-tables
;;; Methods should be more efficient
;;;    than the corresponding list methods for "set" operations.

;;; The contents of this file are subject to the University at Buffalo
;;; Public License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the
;;; License at http://www.cse.buffalo.edu/sneps/Downloads/ubpl.pdf
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


(defpackage :sneps3-set
  (:nicknames :set)
  (:export
   #:*show-singleton-parens*
   #:add-item #:add-items #:and #:copy-add-item #:cardinality #:choose #:copy-set
   #:difference #:emptyp
   #:*emptyset* #:equal #:find-if #:if #:intersection #:intersection* #:member #:new-set
   #:or.set #:print-set #:remove-if #:remove-item  #:remove-items #:set
   #:setp
   #:set-every #:set-some #:set-to-list #:setof #:singleton
   #:singletonp #:subset-of #:type #:union #:union* #:unless
   #:make-set-if-not-set #:loopset #:when)
  (:shadow
   cl:and cl:equal cl:find-if cl:if cl:intersection cl:member cl:remove-if cl:set
   cl:union cl:unless cl:when))

(in-package :sneps3-set)


(defparameter *show-singleton-parens* nil
  "Set to t to indicate that sets should be printed with brackets,
   even when singleton. This makes for easier debugging.") 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (set 
	      (:print-function print-set)
	      (:copier nil))
    "Set constructor definition.
       Sets have a test function (test), hash table (the-set), and
       element type (type)."
    (table (make-hash-table))
    (test #'eq)
    (type t))

  (defun new-set (&key (items '()) (test #'eq) (size 12 size-given)
		       (type t))
    "Returns a new set.
     If size is provided, the underlying hash-table representation is
        set to that initial size;
     else, and items is, then the size of the hash-table is set to the
        length of items;
     else, a default is used.
     
     If items is a non-empty list,
     then the elements of that list are added to the set."
      
    (let ((new-set
	   (make-set
	    :test test
	    :type type
	    :table (make-hash-table 
		    :test test
		    :size (cond 
			   (size-given size)
			   (items (length items))
			   (t size))))))
      (cl:when (consp items)
	(add-items items new-set))
      new-set)))

(eval-when (:load-toplevel :execute)
  (defconstant *emptyset* (new-set :size 0)
    "A constant empty set
        to be used whenever an empty set is to be returned from some function.
        It must never be destructively changed."))

(defmethod print-set (set stream depth)
  "Print the hash set"
  (declare (ignore depth))
  (cl:if (emptyp set)
      (format stream "()")
    (let ((*print-length* nil))
      (cl:if (cl:and (not *show-singleton-parens*) (singletonp set))
	  (format stream "~S" (choose set))
	(format stream "~S"
		(cons 'setof
		      (loop for e being each hash-key of (set-table set)
			  collect e)))))))

(defun copy-set (set)
  "Returns a copy of the given set."
  (check-type set set)
  (let* ((set-copy (new-set :test (set-test set) :type (set-type set)
			    :size (cardinality set)))
	 (set-table-copy (set-table set-copy)))
    (maphash #'(lambda (k v) 
		 (setf (gethash k set-table-copy) v))
	     (set-table set))
    set-copy))
    

(defun add-item (item set)
  "Desctructively adds item to the set, if item is of the appropriate type."
  (check-type set set)
  (cl:unless (typep item (set-type set))
    (error "~A is not of type ~A, which is required for candidacy in ~
              this set.~%" item (set-type set)))
  (setf (gethash item (set-table set)) t)
  set)


(defun copy-add-item (item set)
  "Non-desctructively adds item to the set and returns the new set."
  (check-type set set)
  (let ((new-set (copy-set set)))
    (add-item item new-set)
    new-set))


(defun add-items (items set)
  "Destructively adds a list of items to the set."
  (check-type set set)
  (cl:when (consp items)
    (loop 
	for item in items
	do (add-item item set))))

(defun singleton (item &key (test #'eq) (type t))
  "Creates a new set containing only item, and returns it."
  (let ((s (new-set :size 1 :test test :type type)))
    (add-item item s)
    s))

(defun remove-item (item set)
  "Destructively removes item from the set. 
   Returns the item if removal was successful."
  (check-type set set)
  (cl:when (remhash item (set-table set))
    item))


(defun copy-remove-item (item set)
  "Destructively removes item from the set. 
   Returns the item if removal was successful."
  (check-type set set)
  (cl:when (remhash item (set-table set))
    item))

(defun remove-items (items set)
  "Destructively remove items from the set."
  (check-type set set)
  (cl:when (consp items)
    (loop 
	for item in items
	if (remove-item item set)
	collect item)))

(defun check-compatible (s1 s2 operation)
  "Checks if two sets have compatible equality operators."
  (check-type s1 set)
  (check-type s2 set)
  (cl:unless (eq (set-test s1) (set-test s2))
    (warn (format nil
		  "Performing ~A on incompatible elements. Sets have ~
                              different element equality functions ~A ~
                              and ~A. ~
                              Consider changing (set-test <set>) of ~
                              one or both sets."
		  operation (set-test s1) (set-test s2))))
  (cl:unless (eq (set-type s1) (set-type s2))
    (warn (format nil
		  "Performing ~A on incompatible elements. Sets have ~
                              different element types ~A ~
                              and ~A. ~
                              Consider changing (set-type <set>) of ~
                              one or both sets."
		  operation (set-type s1) (set-type s2)))))

(defun union (s1 s2)
  "Returns a set that is the union of the two argument sets."
  (check-type s1 set)
  (check-type s2 set)
  (check-compatible s1 s2 "union")
  (let ((s3 (copy-set s1)))
    (loop for item being each hash-key of (set-table s2)
	do (add-item item s3))
    s3))

(defun union* (&rest sets)
  (cl:if sets
      (reduce #'union sets)
    (new-set)))

(defun cardinality (set)
  "Returns the set cardinality"
  (check-type set set)
  (hash-table-count (set-table set)))

(defun member (item set)
  "Returns True if item is a member of the set;
     else False."
  (gethash item (set-table set)))

(defun intersection (s1 s2)
  "Returns a set that is the intersection of the two argument sets."
  (check-type s1 set)
  (check-type s2 set)
  (check-compatible s1 s2 "intersection")
  (let ((s3 (new-set :test (set-test s1))))
    (multiple-value-bind (small-set large-set) 
	(cl:if (< (cardinality s1) (cardinality s2))
	    (values s1 s2)
	  (values s2 s1)) 
      (loop for item being each hash-key of (set-table small-set)
	  when (member item large-set)
	  do (add-item item s3)))
    s3))

(defun intersection* (&rest sets)
  (cl:if sets 
      (reduce #'intersection sets)
    (new-set)))

(defun difference (s1 s2)
  "Returns the set which contains
       the elements of the set s1 that are not elements of the set s2."
  (check-type s1 set)
  (check-type s2 set)
  (check-compatible s1 s2 "difference")
  (let ((s3 (new-set :test (set-test s1))))
    (loop for item being each hash-key of (set-table s1)
	unless (member item s2)
	do (add-item item s3))
    s3))

(defun equal (s1 s2)
  "Returns t if two sets are equal, nil otherwise."
  (check-type s1 set)
  (check-type s2 set)
  (cl:and (= (cardinality s1) (cardinality s2))
       (loop for item being each hash-key of (set-table s1)
	   unless (member item s2)
	   do (return nil)
	   finally (return t))))

(defun emptyp (set)
  "Returns true if the set is the empty set"
  (check-type set set)
  (= (cardinality set) 0)) 

(defun subset-of (s1 s2)
  "Returns t
        if the set s1 is a subset (not necessarily proper) of the set s2;
     nil otherwise."
  (check-type s1 set)
  (check-type s2 set)
  (cl:and (<= (cardinality s1) (cardinality s2))
       (loop for item being each hash-key of (set-table s1)
	   unless (member item s2)
	   do (return nil)
	   finally (return t))))

(defmacro loopset (for elt in set &body forms)
  "Iterates the forms as a loop body,
     with var taking on successive elements
     of the set which is the value of setform.
     Returns what the loop macro returns."
  (declare (ignore for in))
  `(loop for ,elt being each hash-key of (set::set-table ,set) 
				      ,@forms))

(defun singletonp (set)
  "If the set has exactly one member, returns True;
      else returns nil."
  (check-type set set)
  (= (cardinality set) 1))

(defun choose (set)
  "Returns an arbitrary member of the given set."
  (check-type set set)
  (loop for e being each hash-key of (set-table set)
      do (return e)))

(defun remove-if (pred set)
  "Returns a new set without those elements in set that pass the pred test."
  (let ((result (new-set :test (set-test set))))
    (loopset for item in set
	     unless (funcall pred item)
	     do (add-item item result))
    result))

(defun find-if (pred set)
  "Returns an item in set that satisfies the pred test,
        or nil if there is none."
  (loopset for item in set
	   when (funcall pred item)
	   do (return-from find-if item)))

(defun set-to-list (set)
  "Returns a list contianing the elements of this set."
  (let ((result '()))
    (maphash #'(lambda (k v) 
		 (declare (ignore v))
		 (push k result))
	     (set-table set))
    result))

(defun setp (set?)
  "Returns t if set? is a set"
  (typep set? 'set))
  
(defun make-set-if-not-set (element)
  "Returns the argument if it is a set, and the set containing only that argument otherwise"
  (cl:if (setp element)
      element
    (new-set :items 
	     (cl:if (not (consp element))
		 (list element)
	       element))))

(defmacro or.set (&rest setexprs)
  "Returns the evaluation of the first set expression
       that evaluates to a non-empty set;
    or the empty set if none do."
  (cl:if (endp setexprs)
      '*emptyset*
    (let ((expr (gensym)))
      `(let ((,expr ,(first setexprs)))
	 (cl:if (emptyp ,expr)
	     (or.set ,@(rest setexprs))
	   ,expr)))))

(defun set-every (pred set)
  "Given a predicate and a set, returns true 
     if the predicate holds for every element"
  (loopset for item in set
	  unless (funcall pred item)
	    do (return-from set-every nil))
  (return-from set-every t))

(defun set-some (pred set)
  "Given a predicate and a set, returns true 
     if the predicate holds for some element"
  (loopset for item in set
	  when (funcall pred item)
	    do (return-from set-some item)))

(defmacro when (expr &body forms)
  "If expr evaluates to a non-empty set or a non-null non-set,
      then the forms are evaluated;
   otherwise the forms are not evaluated.
   Returns the value of the last form evaluated."
  (let ((exprval (gensym)))
    `(let ((,exprval ,expr))
       (cl:unless (or (cl:and (typep ,exprval 'set)
			   (set:emptyp ,exprval))
		      (null ,exprval))
	 ,@forms))))

(defmacro unless (expr &body forms)
  "If expr evaluates to an empty set or to nil,
      then the forms are evaluated;
   otherwise the forms are not evaluated.
   Returns the value of the last form evaluated."
  (let ((exprval (gensym)))
    `(let ((,exprval ,expr))
       (cl:when (or (cl:and (typep ,exprval 'set)
			 (set:emptyp ,exprval))
		    (null ,exprval))
	 ,@forms))))

(defmacro if (expr ifform elseform)
  "If expr evaluates to a non-empty set or a non-null non-set,
      then the first form is evaluated;
   otherwise the second form is evalulated.
   Returns the value of the last form evaluated."
  (let ((exprval (gensym)))
    `(let ((,exprval ,expr))
       (cl:if (or (cl:and (typep ,exprval 'set)
			   (set:emptyp ,exprval))
		      (null ,exprval))
	 ,elseform
         ,ifform))))

(defmacro and (&rest exprs)
  "Returns nil if any expr evaluates to nil or to an empty set;
      otherwise returns t."
  (cl:if (endp exprs)
      t
    (let ((expr (gensym)))
      `(let ((,expr ,(first exprs)))
	 (cl:if (cl:or (null ,expr)
		    (cl:and (typep ,expr 'set)
			 (emptyp ,expr)))
	     nil
	   (and ,@(rest exprs)))))))
