;;; SNePS 3: Slot-Based Inference
;;; =============
;;; Written by Jonathan P. Bona
;;;    
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

(in-package :snip)
(defgeneric slot-based-entails (source target)  )

;;; Slot-based entailment never applies to an atom and anything
(defmethod slot-based-entails ((source sneps3::atom ) (target sneps3:molecular )))
(defmethod slot-based-entails ((source sneps3:molecular ) (target sneps3::atom)))
(defmethod slot-based-entails ((asource sneps3::atom) (atarget sneps3::atom)))

(defmethod slot-based-entails ((source sneps::negation) (target sneps::nand))
  ;; nor -> nand
  ;; (nor (and P Q) R) |= (nand P Q)
  ;; if any of the source's fillers is a conjunction X s.t. 
  ;; X's fillers are a subset of the target's fillers, 
  ;; then the source entails the target so return it.
  (set:loopset for arg in (sneps:findto source 'sneps:nor) do
	       (when (and (typep arg 'sneps::conjunction) 
			  (set::subset-of (sneps::findto arg 'and) (sneps::findto target 'andorargs)))
		 (return-from slot-based-entails target))))

(defmethod slot-based-entails ((source sneps::implication) (target  sneps::implication))
  (let ((src_ant (elt (sneps::down-cableset source) 0))
	(src_cq (elt (sneps::down-cableset source) 1))
	(tgt_ant (elt (sneps::down-cableset target) 0))
	(tgt_cq (elt (sneps::down-cableset target) 1)))
    (if (and (set:subset-of src_ant tgt_ant) (set:subset-of tgt_cq src_cq))
	(return-from slot-based-entails target))))


(defmethod slot-based-entails ((source sneps3::andor) (target sneps3::andor))
  (let ((i (sneps3:minparam source))
	(j (sneps3:maxparam source))
	(src_set (elt (sneps3::down-cableset source) 0))
	(tgt_set (elt (sneps3::down-cableset target) 0)))
    (let ((k (- (set:cardinality src_set) (set:cardinality tgt_set))))
      (when (or 
	     (and (>= k 0)
		  (set:subset-of tgt_set src_set)
		  (eq (max (- i k) 0)  (sneps3:minparam target))
		  (eq (min j (set:cardinality tgt_set)) (sneps3:maxparam target)))
	     (and (set:subset-of src_set tgt_set)
		  (eq i (sneps3:minparam target))
		  (eq (- j k) (sneps3:maxparam target))))
	(return-from slot-based-entails target)))))

(defmethod slot-based-entails ((source sneps3::thresh) (target sneps3::thresh))
  (let ((i (sneps3:minparam source))
	(j (sneps3:maxparam source))
	(src_set (elt (sneps3::down-cableset source) 0))
	(tgt_set (elt (sneps3::down-cableset target) 0)))
    (let ((k (- (set:cardinality src_set) (set:cardinality tgt_set))))
      (when (or
	     (and (>= k 0)
		  (set:subset-of tgt_set src_set)
		  (eq (min i (set:cardinality tgt_set))  (sneps3:minparam target))
		  (eq (max (- j k) i) (sneps3:maxparam target)))
	     (and (set:subset-of src_set tgt_set)
		  (eq (- i k) (sneps::minparam target))
		  (eq j (sneps::maxparam target))))
	(return-from slot-based-entails target)))))      
      
      



(defmethod slot-based-entails ((negsource sneps3:negation ) (negtarget sneps3:negation ))
  "Returns the target if the source entails the target by slot-based inference"
  (when (not (eq negsource negtarget))	; if they're eq, slot-based doesn't apply
    (let ((sourceset (sneps:findto negsource 'sneps:nor))
	  (targetset  (sneps:findto negtarget 'sneps:nor)))
      ;; If the source and target sets are singletons, perform normal slot-based inference
      (if  (and (set:singletonp sourceset) (set:singletonp targetset))
	  (let ((source (set:choose sourceset))
		(target (set:choose targetset)))
	    ;; If source and target are molecular, and have compatible frames
	    ;;TODO     should this "adjustable" check be eliminated?
	    (if (and (typep target 'sneps:molecular) 
		     (typep source 'sneps:molecular)
		     (cf::adjustable (sneps::caseframe source)
				     (sneps::caseframe target)))
		;; Return targetset if the fillers of every source slot can 
		;;     be validly adjusted to the fillers of the corresponding target slot
		(when (every   
		       #'(lambda (x) (valid-adjust 
				      (slot:slot-negadjust x) 
				      (slot:slot-min x)
				      (slot:slot-max x)
				      (snip::pb-findtos (set:singleton source) 
							x)
				      (snip::pb-findtos (set:singleton target)
							x)))
		       (cf:caseframe-slots (sneps:caseframe source)))
		  targetset)))
	;; Else (not singletons;  special nor case): 
	;;      return targetset if the source set covers the target set
	(when (covers sourceset targetset)  targetset)))))

(defmethod slot-based-entails ((source sneps3:molecular)  (target sneps3:molecular))
  "Returns the target if the source entails it by slot-based inference"
  (when 
      (and (cf::adjustable (sneps::caseframe source) (sneps::caseframe target))
	   (every 
	    #'(lambda (x) (valid-adjust (slot:slot-posadjust x)
					(slot:slot-min x)
					(slot:slot-max x)
					(snip::pb-findtos (set:singleton source) 
							  x)
					(snip::pb-findtos (set:singleton target)
							  x)))
	    (cf:caseframe-slots (sneps:caseframe source))))
    target))

(defmethod covers (source target)
  "Returns true if target and source are sets of propositions
        such that every proposition in target is eq to,
             or is slot-based-entailed by some proposition in source"
  (set:set-every 
   #'(lambda (tgt) 
       (set::set-some
	#'(lambda (src) (or (eq src tgt) (slot-based-entails src tgt))) 
	source))
   target))

(defun valid-adjust (adj min max sourcefillers targetfillers)
  "Returns t if the sourcefillers can be adjusted
        via the adjust type adj to the targetfillers"
  (unless (or (set:emptyp sourcefillers) (set:emptyp targetfillers))
    (and (<= min (set:cardinality targetfillers))
	 (or (null max)
	     (<= (set:cardinality targetfillers) max))
	 (ecase adj
	   (slot:reduce			; reduce:  sourcefillers must be a superset of target fillers
	    (or (eq targetfillers sourcefillers) 
		(set:subset-of targetfillers sourcefillers)))
	   (slot:expand			; expand: sourcefillers to be a subset of target fillers
	    (or (eq sourcefillers targetfillers)  
		(set:subset-of sourcefillers targetfillers)))
	   (slot:none			; none: sourcefillers and target fillers must be the same
	    (or (eq sourcefillers targetfillers) 
		(set:equal sourcefillers targetfillers)))))))

(defun slot-based-derivable (target context termstack)
  "If the term [target] is entailed in the given context
          via slot-based-inference
          assert it,
          and return a set containing it;
     else return the empty set.
     The termstack is a stack of propositions
         that this goal is a subgoal of."
  (when (typep target 'sneps:molecular) ;;Look at the terms stored in the target's
    ;; caseframe
    (when *GOALTRACE* 
      (format *trace-output* "~&I will consider using Slot&Path-Based inference.~%"))
    (set:loopset for trm in (cf:get-caseframe-terms (sneps:caseframe target))
		 when (sb-derivable-test trm target context termstack)
		 do (assertTrace trm nil target
				 "Slot&Path-Based inference" context)
		 and do (return-from slot-based-derivable     
			  (set:new-set :items (list target))))
    ;; For each caseframe cf that can be adjusted to the target's frame, 
    ;;  look at each of cf's stored terms
    (set:loopset for cf in (cf:caseframe-adj-from (sneps:caseframe target))
		 do (set:loopset for trm in (cf:get-caseframe-terms cf)
				 when (sb-derivable-test trm 
							 target 
							 context 
							 termstack)
				 do (assertTrace trm nil target
						 "Slot-Based inference"
						 context)
				 and do (return-from slot-based-derivable     
					  (set:new-set
					   :items (list target))))))
  set:*emptyset*)

(defun sb-derivable-test (term target context termstack)
  "Returns t if target is slot-based-entailed by term 
       and if term is asserted in context;
     Else, returns nil."
  (and
   (not (eq term target))
   (not (member term termstack))
   (slot-based-entails term target)
   (not (set:emptyp
	 (askif term context (cons target termstack))))))




