;; SNePS 3: Definition of Semantic Types
;;; =====================================
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

;;; Initial Semantic Types
;;; ======================

(defclass Entity ()
  ((typeName :allocation :class
	     :initform 'Entity
	     :reader semantic-type-of
	     :documentation
	     "The semantic type of this node."))
  (:documentation "The top of the semantic hierarchy"))

(defparameter *TopSemanticType* (find-class 'Entity)
  "The root of the semantic type hierarchy.")

(defclass Proposition (Entity)
  ((typeName :allocation :class
	     :initform 'Proposition
	     :reader semantic-type-of
	     :documentation
	     "The semantic type of this node.")
   (support-set :initarg :supports
		:initform (set:new-set)
		:accessor supports
		:documentation "The set of supports of the proposition.") 
   (supported-nodes :initarg :supported-nodes
		    :initform (set:new-set)
		    :accessor supported-nodes
		    :documentation "The set of nodes that this one supports."))
  (:documentation "An Entity that can be believed,and whose negation can be believed."))

(defclass Act (Entity)
  ((typeName :allocation :class
	     :initform 'Act
	     :reader semantic-type-of
	     :documentation "The semantic type of this node.")
   (primaction :accessor primaction
	       :initform nil
	       :documentation
	       "An optional implementation for this act."))
  (:documentation "An Entity that can be performed.."))

(defclass Policy (Entity)
  ((typeName :allocation :class
	     :initform 'Policy
	     :reader semantic-type-of
	     :documentation
	     "The semantic type of this node."))
  (:documentation "An Entity that relates propositions to acts."))

(defclass Thing (Entity)
  ((typeName :allocation :class
	     :initform 'Thing
	     :reader semantic-type-of
	     :documentation
	     "The semantic type of this node."))
  (:documentation "An Entity other than a Proposition, Act, or Policy."))

(defclass Category (Thing)
  ((typeName :allocation :class
	     :initform 'Category
	     :reader semantic-type-of
	     :documentation
	     "The semantic type of this node."))
  (:documentation "A Category of Entities."))

(defclass Action (Thing)
  ((typeName :allocation :class
	     :initform 'Action
	     :reader semantic-type-of
	     :documentation "The semantic type of this node.")
   (primaction :accessor primaction
	       :initform nil
	       :documentation
	       "The primitive action implementation for this action."))
  (:documentation
   "An action that can be performed on one or more argument entities.")) 

;;; Functions on Semantic Types
;;; ===========================
(defun list-types ()
  "Lists all the semantic types."
  ;; Default for showTypes
  (loop for type in  (subtypes *TopSemanticType*)
      do (format *standard-output* "~S~%"
		 (slot-value type 'excl::name)))
  (values))

(defun semantic-type-p (name)
  "Returns t if name is a SNePS semantic type
      or the name of a SNePS semantic type;
     else returns nil."
  (and (or (find-class name nil) (typep name 'standard-class))
       (subtypep name *TopSemanticType*)
       (not (subtypep name *TopSyntacticType*))))

(deftype semantic-type ()
  "The type of symbols that name semantic types."
  '(satisfies semantic-type-p))

(defun find-type-name (type)
  "If type is the name of a SNePS semantic type, returns it;
     if type is a SNePS semantic type, returns its name;
     else returns nil."
  ;; This definition uses an ACL-implementation-specific feature.
  (when (semantic-type-p type)
    (typecase type
      (symbol type)
      (standard-class (slot-value type 'excl::name)))))

(defmacro defineType (newtype supers &optional docstring)
  "Defines newtype to be a SNePS semantic type,
       and a subtype of the types listed in the list supers.
     If docstring is given,
        it is set as the documentation string of the new type.
     Returns a string-message, either of success or what the problem was."
  ;; Errors to worry about:
  ;;   newtype is already a Lisp type or a SNePS type;
  ;;   a member of supers is not a SNePS semantic type.
  ;;
  ;; In the previous version, I didn't want defineType to issue an error in these cases,
  ;;    just return error messages as strings.
  (cond ((not (symbolp newtype))
	 (format nil "~S is not a symbol." newtype))
	((typep newtype 'semantic-type)
	 (format nil "~S is already a SNePS semantic type." newtype))
	((typep newtype 'syntactic-type)
	 (format nil "~S is already a SNePS syntactic type." newtype))
	((find-class newtype nil)
	 (format nil "~S is already a Common Lisp class." newtype))
	((find-if #'(lambda (tn) (not (typep tn 'semantic-type)))
		  supers)
	 (format nil
		 "Listed super types are not SNePS semantic types: ~{~S~#[~; and ~:;, ~]~}"
		 (remove-if #'(lambda (tn) (typep tn 'semantic-type))
			    supers)))
	(t 
	 `(progn
	    (setf newclass (defclass ,newtype ,supers
	      ((typeName :allocation :class
			 :initform ',newtype
			 :reader semantic-type-of
			 :documentation
			 "The semantic type of this node."))
	      (:documentation ,docstring)))
            (if *sneps-gui-ptr* (sneps3::send-one-type newclass))
	    (format nil "~S defined as a subtype of ~{~S~#[~; and ~:;, ~]~}"
		    ',newtype ',supers)))))

(defmethod semantic-type-of ((s set:set))
  "Returns the name of a least common supertype of the terms in s
     as the semantic type of the set s."
  (let ((lcstypes (list (find-class (semantic-type-of (set:choose s))))))
    (set:loopset for term in s
		 do (setf lcstypes 
		      (delete-duplicates
		       (loop for lcst in lcstypes
			   append (lcsupertype
				   lcst 
				   (find-class
				    (semantic-type-of term)))))))
    (if (rest lcstypes)
	(let ((choice (util:menuChooseFromList
		       (format nil "Choose a type for ~S."
			       s)
		       lcstypes)))
	  (if choice (find-type-name choice)
	    (error "No chosen type for ~S." s)))
      (find-type-name (first lcstypes)))))

(defun subtypes (type)
  "Returns a list of all the subtypes of the given type."
  ;; Uses ACL-implementation-specific feature.
  (let ((children (slot-value type 'excl::direct-subclasses)))
    (if children
	(cons type
	      (remove-duplicates (mapcan #'subtypes children)))
      (list type))))

(defun supertypes (type)
  "Returns a list of all the supertypes of the given type."
  ;; Uses ACL-implementation-specific feature.
  (let ((parents (slot-value type 'excl::direct-superclasses)))
    (if parents
	(cons type
	      (remove-duplicates (mapcan #'supertypes parents)))
      (list type))))

(defun gcsubtype (t1 t2)
  "Returns a list of the greatest common subtypes of types t1 and t2."
  (if (eq t1 t2) (list t1)
    (let ((common (intersection (subtypes t1) (subtypes t2))))
      (loop for sub in common
	  unless (member sub common
			 :test #'proper-subtypep)
	  collect sub))))

(defun proper-subtypep (t1 t2)
  "Returns t if t1 is a proper subtype of t2;
    Else returns nil."
  (and (not (eq t1 t2))
       (subtypep t1 t2)))

(defun lcsupertype (t1 t2)
  "Returns a list of the least common supertypes of types t1 and t2."
  (cond ((eq t1 t2) (list t1))
	((eq t1 *TopSemanticType*) (list t1))
	((eq t2 *TopSemanticType*) (list t2))
	(t (let ((common
		  (intersection (supertypes t1) (supertypes t2))))
	     (loop for super in common
		 unless (member super common
				:test #'proper-supertypep)
		 collect super)))))

(defun proper-supertypep (t1 t2)
  "Returns t if t1 is a proper supertype of t2;
    Else returns nil."
  (and (not (eq t1 t2))
       (subtypep t2 t1)))

#+composer
(defun showTypes ()
  "Displays all the defined types, breadth-first."
  (cond ((find-package :composer)
	 (unless (composer:composer-initialized-p)
	   (composer:start-composer))
	 (when (and (find-package :common-windows)
		    (not (common-windows:common-windows-initialized-p )))
	   (common-windows:initialize-common-windows))
	 (composer:graph-class *TopSemanticType*))
	(t (list-types)))
  (values))

#-composer
(defun showTypes ()
  "Lists all the semantic types."
  (list-types))
