
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
;;; Contributor(s): Michael W. Kandefer

(in-package :sneps3)


(defmethod erase-ucs-rel ((trm term) (mol molecular))
  "Erases the relation between mol and trm from trm's up-cableset as mol
   is going to be removed."
  (util:protecting 
   (up-cableset trm)
   (loop 
       for k being the hash-key using (hash-value v) of (up-cableset trm)
       ;;; The following is a check to see if the value of this relationship
       ;;; connects to the molecular node that is being removed							
       if (eql (set:choose v) mol)
       do (remhash k (up-cableset trm))
       end)))
    
(defmethod erase-term ((trm term) &optional removing-descendant)
  "Erases a term  from the SNePS network. Returns the
   term erased, or nil if the term cannot be erased."
  (let ((ucs-size (hash-table-count (util:resource-value (up-cableset trm)))))
    (cond 
     ((typep trm 'variable) 
      (when (= ucs-size (set:cardinality (restriction-set trm))) 
	(set:loopset 
	 for rst in (restriction-set trm)
	 do (erase-term rst trm))
	(cond
	 ((typep trm 'arbitrary) 
	    (util:protecting 
	     *ARBITRARIES*
	     (remhash (name trm) *ARBITRARIES*))
	    (util:protecting 
	     *TERMS*
	     (remhash (name trm) *TERMS*)))
	 ((typep trm 'indefinite)  
	  (util:protecting 
	     *INDEFINITES*
	     (remhash (name trm) *INDEFINITES*))
	  (util:protecting 
	   *TERMS*
	   (remhash (name trm) *TERMS*))))) 
      trm)
     (t 
      (when (< ucs-size 1) 
	(when (and (typep trm 'molecular)
		   (typep trm 'Proposition))
	  (loop 
	      for dcs-term-set across (down-cableset trm)
	      do (set:loopset 
		  for dcs-term in dcs-term-set
		  do (erase-ucs-rel dcs-term trm) 
		     (cond 
		      ((and removing-descendant
			    (not (eq removing-descendant dcs-term-set)))
		       (erase-term dcs-term))
		      ((not removing-descendant) 
		       (erase-term dcs-term))))
		 (let ((cf (caseframe trm)))
		   (util:protecting 
		    (cf:caseframe-terms cf)
		    (set:remove-item trm (cf:caseframe-terms cf)))))
	  (loop for ctx being the hash-value of ct:*CONTEXTS*
	   do (ct:remove-from-context trm ctx)))
	(util:protecting 
	 *TERMS*
	 (remhash (name trm) *TERMS*)) 
	trm)))))