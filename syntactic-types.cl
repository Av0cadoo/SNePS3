;;; SNePS 3: Definition of Syntactic Types
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

;;; Syntactic-class Metaclass
;;; =========================
(defclass syntactic-type (standard-class)
  ()
  (:documentation "Metaclass for all syntactic types"))


;;; Data structures for Syntactic Types
;;; ====================================



(defparameter *TERMS*
    (util:make-resource 
     :value (make-hash-table)		;  A map from term names to the actual terms.
     ))

(defparameter *ARBITRARIES* 
    (util:make-resource 
     :value (set:new-set))
  "The set of all arbitrary individual nodes.")

(defparameter *INDEFINITES*
    (util:make-resource
     :value (set:new-set))
  "The set of all indefinite objects.")

;;; Syntactic Types
;;; ===============

(defparameter *TopSyntacticType* 'term 
  "The root of the syntactic type hierarchy.")

(defclass term ()
  ((name :initarg :name
	 :initform nil
	 :reader name
	 :documentation "every term has a name")
   (activation-value :initarg :activation-value
		     :initform 0.0
		     :accessor activation-value
		     :documentation "Every term (node) has an 
                                     activation-value")
   (fired :initarg :fired
	  :initform nil
	  :accessor fired
	  :documentation "Whether this node has fired.")
   (recorded-firing :initarg :recorded-firing
		    :initform nil
		    :accessor recorded-firing
		    :documentation "Whether the node is recorded for firing.")
   (activation-marker :initarg :activation-marker
		      :initform nil
		      :accessor activation-marker
		      :documentation "In some versions of spreading
                                      activation. An activation path is
                                      used to choose a path through the
                                      network.")
   (up-cablesetw
    :initform (util:make-resource
	       :value (make-hash-table)) ; map from relation to nodeset
    :reader up-cableset
    :documentation
    "The slots this term is a value in, and the moledular terms they are slots in."))
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm term))
  "Returns the name of the syntactic type of trm."
  'term)

(defclass atom (term)
  ()
  (:documentation
   "An atomic term has only a name and an up-cableset and has no structure.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm atom))
  "Returns the name of the syntactic type of trm."
  'atom)

(defclass base (atom)
  ()
  (:documentation "An individual constant.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm base))
  "Returns the name of the syntactic type of trm."
  'base)


(defclass variable (atom)
  ((restriction-set 
   :initarg :restriction-set
   :initform (set:new-set)
   :reader restriction-set
   :documentation "every variable has a restriction set, which is a
                   set of propositional terms that restrict the arbitrary
                   individual/indefinite object to a certain class")
   (var-label 
    :initarg :var-label
    :reader var-label
    :documentation "every variable is given a label by the user on creation
                    this stores that label for printing purposes."))
  (:documentation 
   "A variable term.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm variable))
  "Returns the name of the syntactic type of trm."
  'variable)

;;;  We might include this class
;;; (defclass queriable (variable) ())

(defclass indefinite (variable)
  ((ind-counter :allocation :class	; The number of built indefinite nodes.
		:accessor counter
		:initform  (util:make-resource 
			    :value 1)
		:documentation "A counter for the number 
                              of indefinite objects.")
   (dependencies :initarg :dependencies 
		 :initform (set:new-set)
		 :reader dependencies
		 :documentation "every indefinite node may has a set
                                 of arbitrary node dependencies."))
  (:documentation 
   "A indefinite object.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm indefinite))
  "Returns the name of the syntactic type of trm."
  'indefinite)

(defclass arbitrary (variable)
  ((arb-counter :allocation :class	; The number of built arbitrary nodes.
		:accessor counter
		:initform  (util:make-resource 
			  :value 1)))
  (:documentation 
   "An arbitrary individual.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm arbitrary))
  "Returns the name of the syntactic type of trm."
  'arbitrary)



(defclass molecular (term)
  ((wftcounter				; The number of built well-formed non-atomic terms.
    :allocation :class			; Used to form wft names.
    :accessor counter
    :initform (util:make-resource :value 0))
   (caseframe :initarg :caseframe
	      :reader caseframe
	      ;; :type caseframe
	      :documentation "holds this molecular node's case frame")
   (down-cableset :initarg :down-cableset
		  :initform '()
		  :accessor down-cableset
		  ;; a vector of the slot fillers
		  ;;    in order of the slots
		  ;;    in the caseframe
		  )
;;;   (dominate-set  :initarg :dominate-set
;;;		  :initform nil
;;;		  :accessor dominate-set)
   (down-weights :initarg :down-weights
		 :initform '()
		 :accessor down-weights
		 :documentation "a vector of the 'edge' weights
                                 between a molecular node and its fillers"
   ))
  (:documentation
   "A molecular term is a functional term with zero or more arguments.
          equivalently, it is a frame with slots and fillers.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm molecular))
  "Returns the name of the syntactic type of trm."
  'molecular)

(defmethod initialize-instance :after ((n molecular) &rest args)
  "Update the up-cablesets of all the directly dominated terms."
  (loop for rel across (cf:caseframe-slots (caseframe n))
      for ns across (down-cableset n)
      if (typep ns 'set:set)
      do (set:loopset for m in ns
		      do (install-in-upcset n rel m)) 
      else
      do (install-in-upcset n rel ns)))

(defclass param2op (molecular)
  ((min :initarg :min
	 :accessor minparam
	 :documentation "The minimum number of arguments that are True.")
   (max :initarg :max
	 :accessor maxparam
	 :documentation "The maximum number of arguments that are True."))
  (:documentation "The andor or thresh of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm param2op))
  "Returns the name of the syntactic type of trm."
  'param2op)

(defmethod totparam ((term param2op))
  "Returns the tot of the given param2op term."
  (set:cardinality (elt (down-cableset term) 0)))

(defclass andor (param2op)
  ()
  (:documentation "The andor of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm andor))
  "Returns the name of the syntactic type of trm."
  'andor)

(defmethod initialize-instance :after ((term andor) &rest args)
  "Demotes a andor
        with min=1 and max=tot to a disjunction;
             min=max=1 to an xor;					;
             min=0 and max=tot-1 to a nand."
  (when (eq (syntactic-type-of term) 'andor)
    (cond ((= 1 (minparam term) (maxparam term))
	   (change-class term 'xor))
	  ((and (= (minparam term) 1)
		(= (maxparam term) (totparam term)))
	   (change-class term 'disjunction))
	  ((and (zerop (minparam term))
		(= (maxparam term) (1- (totparam term))))
	   (change-class term 'nand)))))

(defclass disjunction (andor)
  ()
  (:documentation "The disjunction of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm disjunction))
  "Returns the name of the syntactic type of trm."
  'disjunction)

(defmethod initialize-instance :after ((term disjunction) &rest args)
  "Initialize the max slot of the disjunction to be the number of arguments."
  (setf (slot-value term 'max) (totparam term)))

(defclass xor (andor)
  ()
  (:documentation "The xor of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm xor))
  "Returns the name of the syntactic type of trm."
  'xor)

(defclass nand (andor)
  ()
  (:documentation "The nand of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm nand))
  "Returns the name of the syntactic type of trm."
  'nand)

(defmethod initialize-instance :after ((term nand) &rest args)
  "Initialize the max slot of the disjunction
         to be one less than the number of arguments."
  (setf (slot-value term 'max) (1- (totparam term))))

(defclass thresh (param2op)
  ()
  (:documentation "The thresh of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm thresh))
  "Returns the name of the syntactic type of trm."
  'thresh)

(defmethod initialize-instance :after ((term thresh) &rest args)
  "Demotes a thresh 
        with min=1 and max=tot-1 to an equivalence."
  (when (eq (syntactic-type-of term) 'thresh)
    (cond ((and (= (minparam term) 1)
		(= (maxparam term) (1- (totparam term))))
	   (change-class term 'equivalence)))))

(defclass equivalence (thresh)
  ()
  (:documentation "An equivalence proposition")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm equivalence))
  "Returns the name of the syntactic type of trm."
  'equivalence)

(defclass conjunction (molecular)
  ()
  (:documentation "The conjunction of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm conjunction))
  "Returns the name of the syntactic type of trm."
  'conjunction)

(defclass negation (molecular)
  ()
  (:documentation "The negation of one, or the generalized nor of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm negation))
  "Returns the name of the syntactic type of trm."
  'negation)

(defclass negationbyfailure (molecular)
  ()
  (:documentation
   "The negation (by failure) of one,
          or the generalized thnor of some proposition(s)")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm negationbyfailure))
  "Returns the name of the syntactic type of trm."
  'negationbyfailure)

(defclass numericalentailment (molecular)
  ((min :initarg :min
	:accessor minparam
	:documentation "The minimum number of arguments that are True."))
  (:documentation "A numerical entailment")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm numericalentailment))
  "Returns the name of the syntactic type of trm."
  'numericalentailment)

(defmethod totparam ((term numericalentailment))
  "Returns the tot of the given numericalentailment term."
  (set:cardinality (elt (down-cableset term) 0)))

(defmethod initialize-instance :after ((term numericalentailment)
				       &rest args)
  "Demotes a numerical entailment with a minparameter of 1
       to an orentailment."
  (when (eq (syntactic-type-of term) 'numericalentailment)
    (cond ((= (minparam term) 1)
	   (change-class term 'orentailment))
	  ((= (minparam term) (totparam term))
	   (change-class term 'implication)))))

(defclass orentailment (numericalentailment)
  ()
  (:documentation "The consequents are implied by any antecedent.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm orentailment))
  "Returns the name of the syntactic type of trm."
  'orentailment)

(defclass implication (numericalentailment)
  ()
  (:documentation "A conditional proposition")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm implication))
  "Returns the name of the syntactic type of trm."
  'implication)

(defclass categorization (molecular)
  ()
  (:documentation "A Proposition that says that some Entities are instances of some Categories.")
  (:metaclass syntactic-type))

(defmethod syntactic-type-of ((trm categorization))
  "Returns the name of the syntactic type of categorization."
  'categorization)

;;; Functions on Syntactic Types
;;; ============================

(defun install-in-upcset (n r m)
  "Installs n in the up-cableset of m for relation r."
  (util:protecting 
   (up-cableset m)
   (unless (gethash r (up-cableset m))
     (setf (gethash r (up-cableset m)) (set:new-set)))
   (set:add-item n
		 (gethash r (up-cableset m)))))
