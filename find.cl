;;; SNePS 3: Find
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

(in-package :sneps3)

(defvar *PRECISION* 5
  "A floating point number will be rounded to this number of
       decimal places before being converted to a term.")

(defgeneric findto (n r))

(defmethod findto (n (relname symbol))
  "Returns the set of nodes to which a slot named relname goes from n, including possibly the empty set."
  (findto n (slot:find-slot relname)))
  
(defmethod findto (n (r slot:slot))
  "Returns the set of nodes to which a slot named r goes from n, including possibly the empty set."
  (check-type n term)
  (when (typep n 'molecular)
    (let ((pos (position r (cf:caseframe-slots (caseframe n)))))
      (if pos 
	  (elt (down-cableset n) pos)
	(set:new-set)))))
    
(defun findfrom (m r)
  "Returns the set of nodes
        from which a slot r, or a slot named r, goes to m."
  (check-type m term)
  (gethash 
   (etypecase r
     (symbol (slot:find-slot r))
     (slot:slot r))
   (util:resource-value (up-cableset m)) (set:new-set)))

;;; When given a term already, return it
(defun find-term (name)
  "Returns the term named name;
      or nil if there isn't one."
  (if (subtypep (type-of name) 'term)
      name
    (let ((symbol
	   (cond ((stringp name)
		  (intern name))
		 ((integerp name)
		  (intern (format nil "~D" name)))
		 ((floatp name)
		  (intern (format nil "~,VF" *PRECISION* name)))
		 (t name))))
      (gethash symbol (util:resource-value *TERMS*)))))

(defmethod find-terms (term-list)
  "Returns a set of terms found from a list of term identifiers (term-list)."
  (let((term-set (set:new-set )))
    (loop 
      for trm in term-list
      for term = (find-term trm)
      if term
      do (set:add-item term term-set))
    term-set))

(defmethod find ((expr cons) &optional (var-list '()))
  "Given an cons expression (expr) possibly containing variables
  (specified in list var-list or with a leading #\?),
  return all nodes that match the expression,
  and the substitution-set built during the find."
  (let ((cf (cf:find-frame (first expr))))
    (if cf
	(find-in expr (filter (cf:get-caseframe-terms cf) expr cf var-list)
		 var-list)
      (values (set:new-set) (subs-set:new-substitution-set)))))

(defun filter (termset expr cf var-list)
  "Filters from the termset those terms that couldn't possibly match expr,
   and returns the result.
   var-list is a list of symbols in expr that are variables."
  ;; Loop in parallel through the slots, slot, in cf and the corresponding
  ;; subexpressions, subex, of expr.
  ;; For each subex that is a symbol and not a variable
  ;;    intersect termset with the set of terms with subex in their slot slot.
  ;;     Note, the slot variable can be an actual slot or its name.
  ;; until done or termset is the empty set.
  (loop for subex in (if (cf:quotedpp cf) (rest expr) expr)
      for slot across (cf:caseframe-slots cf)
      until (set:emptyp termset)
      when (and (symbolp subex)
		(not (member subex var-list))
		(not (qVarp subex))
		(find-term subex))
      do ;; (format t "~&Filter by ~S on the slot ~S~%" subex slot)
	(setf termset (set:intersection termset 
					(findfrom (find-term subex) slot)))
	 )
  termset)


(defmethod find-in ((expr cons) term-set var-list)
  "Given a cons expression (expr) possibly containing variables
  (specified in list var-list), return all nodes in term-set
  that match the expression, and the substitution-set built 
  during the find."
   (let ((result-terms (set:new-set))
        (result-subs-set (subs-set:new-substitution-set)))
     (set:loopset 
      for term in term-set
      do (multiple-value-bind (new-subs-set matched)
	     (catch 'matched-results
	       (pattern-term-match expr (down-cableset term)
				   var-list))
	   (when matched
	     ;; Associate substitution with term 
	     (setf result-subs-set 
	       (subs-set:union result-subs-set new-subs-set))
	     (set:add-item term result-terms))))
     (values result-terms result-subs-set)))


(defun pattern-term-match (pat-args dcs var-list 
			     &optional (subs (subs-set:new-substitution-set)))
  "Given a list of arguments (pat-args), which can be either symbols or lists,
   determine if that list can build the vector of nodes representing
   the down-cableset of a molecular node (dcs). pat-args may contain
   variables, and these variables are specified in a list of symbols
   (var-list).  subs is used to retain bound variables between
   recursive calls of this function. Returns two values, the resulting
   subs and t if the results match. Returns subs nil otherwise." 
  ;; Separated pattern-term-match and pattern-term-match2
  ;;    so the first is only called on a whole parenthesized expression.
  ;; This is a kludgy way of doing it.
  (cond 
   ((and (endp pat-args) (= (length dcs) 0 ))
    (throw 'matched-results (values subs t)))
   ((endp pat-args) (throw 'matched-results (values subs nil)))
   ((= (length dcs) 0) (throw 'matched-results (values subs nil)))
   (t
    (let ((pat-arg (pop pat-args))
	  (term-arg (elt dcs 0)))
      (cond 
       ;; If the pat-arg is a variable
       ((or (member pat-arg var-list)
	    (and (qVarp pat-arg)
		 (push pat-arg var-list)))
	;; Check if the variable has been encountered before
	(let ((var-values (subs-set:get-all-terms-from-var pat-arg subs)))
	  (cond 
	   ((not (set:emptyp var-values ))
	    ;; if it has, and the current term doesn't match the
	    ;; any of the terms bound to the variable, then return not 
	    ;; matched
	    ;; else continue onward in the matcher, and eliminate
	    ;; any possibilities that don't match the term in the
	    ;; substitution set.
	    (cond
	     ((not (or (and (not (set:singletonp term-arg))
			    (set:set-some #'(lambda (set) 
					      (and (set:setp set)  
						   (set:equal term-arg set))) 
					  var-values))
		       (set:member (set:choose term-arg) var-values)))
	      (throw 'matched-results (values subs nil)))
	     (t 
	      (pattern-term-match2 pat-args 
				  (subseq dcs 1) 
				  var-list 
				  (subs-set:remove-if 
				   #'(lambda (sub) 
				       (not (eq
					     (if (set:singletonp term-arg)
						 (set:choose term-arg)
					       term-arg)
					     (subs:var-to-term pat-arg sub))))
				   subs)))))
	   ;; if the variable hasn't been bound before, bind it to
	   ;; the current term, and continue on in the matcher
	   (t 
	    (pattern-term-match2 pat-args 
				(subseq dcs 1)
				var-list 
				(subs-set:add-binding pat-arg 
						      (if (set:singletonp term-arg)
							  (set:choose term-arg)
							term-arg)
						      subs))))))
       ;; If pat-arg is a symbol, find the term or caseframe it 
       ;; represents, if it is a term
       ;; compare it against term-arg. if they aren't the same,
       ;; return non-match. if it's a quoted caseframe 
       ;; then make a recursive call as the pattern is off from the
       ;; term-set by one (e.g., we're comparing (Isa Dog Mammal) to 
       ;; the term-set (Dog Mammal).
       ((or (symbolp pat-arg)
	    (when (stringp pat-arg)
	      (setf pat-arg (intern pat-arg)))
	    (when (typep pat-arg 'fixnum)
	      (setf pat-arg (intern (format nil "~D" pat-arg))))
	    (when (typep pat-arg 'float)
	      (setf pat-arg (format nil "~,VF" *PRECISION* pat-arg)))
	    (when (typep pat-arg 'term)
	      (setf pat-arg (name pat-arg))))
	(let ((cf (cf:find-frame pat-arg))) 
	  (cond
	   ((and cf (cf:quotedpp cf)) 
	    (pattern-term-match2 pat-args dcs var-list subs))
	   (t (if (eq (set:choose term-arg) (find-term pat-arg))
		  (pattern-term-match2 pat-args (subseq dcs 1) 
				      var-list subs)
		(throw 'matched-results (values subs nil)))))))
       ;; If pat-arg is a cons, it represents a molecular node or set. Recursively
       ;; call this function as if the terms match. If a set, perform set matching.  
       ((consp pat-arg)
	(cond 
	 ((eq (first pat-arg) 'setof)
	  (cond 
	   ((and (eq (type-of term-arg) 'set:set)
		 (= (set:cardinality term-arg) (1- (length pat-arg))))
	    (multiple-value-bind (returned matched)
		(pattern-term-set-match 
		 (remove-duplicates (rest pat-arg)) 
		 term-arg var-list subs)
		(if matched
		    (pattern-term-match2 pat-args (subseq dcs 1) var-list 
					returned)
		  (throw 'matched-results (values subs nil)))))
	   (t
	    (throw 'matched-results (values subs nil)))))
	 (t 
	  (let ((mol-term (set:choose term-arg)))
	    (if (typep mol-term 'molecular)
		(multiple-value-bind (new-subs matched) 
		    (catch 'matched-results
		      (pattern-term-match 
		       (rest pat-arg) 
		       (down-cableset mol-term) 
		       var-list subs))
		  (if matched
		    (pattern-term-match2 pat-args (subseq dcs 1) 
					  var-list new-subs)
		  (throw 'matched-results (values subs nil)))) 
	      (throw 'matched-results (values subs nil))))))))))))

(defun pattern-term-match2 (pat-args dcs var-list 
			     &optional (subs (subs-set:new-substitution-set)))
  "Given a list of arguments (pat-args), which can be either symbols or lists,
   determine if that list can build the vector of nodes representing
   the down-cableset of a molecular node (dcs). pat-args may contain
   variables, and these variables are specified in a list of symbols
   (var-list).  subs is used to retain bound variables between
   recursive calls of this function. Returns two values, the resulting
   subs and t if the results match. Returns subs nil otherwise." 
  ;; Separated pattern-term-match and pattern-term-match2
  ;;    so the first is only called on a whole parenthesized expression.
  ;; This is a kludgy way of doing it.
  (cond 
   ((and (endp pat-args) (= (length dcs) 0 ))
    (throw 'matched-results (values subs t)))
   ((endp pat-args) (throw 'matched-results (values subs nil)))
   ((= (length dcs) 0) (throw 'matched-results (values subs nil)))
   (t
    (let ((pat-arg (pop pat-args))
	  (term-arg (elt dcs 0)))
      (cond 
       ;; If the pat-arg is a variable
       ((or (member pat-arg var-list)
	    (and (qVarp pat-arg)
		 (push pat-arg var-list)))
	;; Check if the variable has been encountered before
	(let ((var-values (subs-set:get-all-terms-from-var pat-arg subs)))
	  (cond 
	   ((not (set:emptyp var-values ))
	    ;; if it has, and the current term doesn't match the
	    ;; any of the terms bound to the variable, then return not 
	    ;; matched
	    ;; else continue onward in the matcher, and eliminate
	    ;; any possibilities that don't match the term in the
	    ;; substitution set.
	    (cond
	     ((not (or (and (not (set:singletonp term-arg))
			    (set:set-some #'(lambda (set) 
					      (and (set:setp set)  
						   (set:equal term-arg set))) 
					  var-values))
		       (set:member (set:choose term-arg) var-values)))
	      (throw 'matched-results (values subs nil)))
	     (t 
	      (pattern-term-match2 pat-args 
				  (subseq dcs 1) 
				  var-list 
				  (subs-set:remove-if 
				   #'(lambda (sub) 
				       (not (eq
					     (if (set:singletonp term-arg)
						 (set:choose term-arg)
					       term-arg)
					     (subs:var-to-term pat-arg sub))))
				   subs)))))
	   ;; if the variable hasn't been bound before, bind it to
	   ;; the current term, and continue on in the matcher
	   (t 
	    (pattern-term-match2 pat-args 
				(subseq dcs 1)
				var-list 
				(subs-set:add-binding pat-arg 
						      (if (set:singletonp term-arg)
							  (set:choose term-arg)
							term-arg)
						      subs))))))
       ;; If pat-arg is a symbol, find the term or caseframe it 
       ;; represents, if it is a term
       ;; compare it against term-arg. if they aren't the same,
       ;; return non-match. if it's a quoted caseframe 
       ;; then make a recursive call as the pattern is off from the
       ;; term-set by one (e.g., we're comparing (Isa Dog Mammal) to 
       ;; the term-set (Dog Mammal).
       ((or (symbolp pat-arg)
	    (when (stringp pat-arg)
	      (setf pat-arg (intern pat-arg)))
	    (when (typep pat-arg 'fixnum)
	      (setf pat-arg (intern (format nil "~D" pat-arg))))
	    (when (typep pat-arg 'float)
	      (setf pat-arg (format nil "~,VF" *PRECISION* pat-arg)))
	    (when (typep pat-arg 'term)
	      (setf pat-arg (name pat-arg))))
	  (if (eq (set:choose term-arg) (find-term pat-arg))
		  (pattern-term-match2 pat-args (subseq dcs 1) 
				      var-list subs)
		(throw 'matched-results (values subs nil))))
       ;; If pat-arg is a cons, it represents a molecular node or set. Recursively
       ;; call this function as if the terms match. If a set, perform set matching.  
       ((consp pat-arg)
	(cond 
	 ((eq (first pat-arg) 'setof)
	  (cond 
	   ((and (eq (type-of term-arg) 'set:set)
		 (= (set:cardinality term-arg) (1- (length pat-arg))))
	    (multiple-value-bind (returned matched)
		(pattern-term-set-match 
		 (remove-duplicates (rest pat-arg)) 
		 term-arg var-list subs)
		(if matched
		    (pattern-term-match2 pat-args (subseq dcs 1) var-list 
					returned)
		  (throw 'matched-results (values subs nil)))))
	   (t
	    (throw 'matched-results (values subs nil)))))
	 (t 
	  (let ((mol-term (set:choose term-arg)))
	    (if (typep mol-term 'molecular)
		(multiple-value-bind (new-subs matched) 
		    (catch 'matched-results
		      (pattern-term-match 
		       (rest pat-arg) 
		       (down-cableset mol-term) 
		       var-list subs))
		  (if matched
		    (pattern-term-match2 pat-args (subseq dcs 1) 
					  var-list new-subs)
		  (throw 'matched-results (values subs nil)))) 
	      (throw 'matched-results (values subs nil))))))))))))



(defun pattern-term-set-match (set-pattern term-set var-list subs)
  "This is a helper function, and not meant to be called independent
   of pattern-term-match. Given set-pattern a set specifier without
   the 'setof symbol as the first element 
   (e.g., (Dog Cat Elephant)), and term-set, a set of terms, determine 
   if set-spec builds
   term-pat.  set-pattern may contain variables (var-list), that may or
   may not be bound (subs, contains this information). It is assumed
   that each variable indicates a unique individual, and thus the expression:
   (x y Cat), where x and y are variables,will only match sets
   of  three elements with one of those elements being Cat. This
   function will only return two values, one is the substitution-set
   resulting from the pattern matcher, the other is whether or not the
   matching was  success (t is so, nil otherwise). If the matching was
   unsuccessful the contents of the first parameter are not useful. "
  (let ((term-pat (first set-pattern)))
    (cond 
     ;; cond-0: If the element is a variable...
     ((null term-pat) (values subs t))
     ((or (member term-pat var-list)
	  (qVarp term-pat))
      ;; Check to see if it is bound
      (let* ((bound-terms 
	      (subs-set:get-all-terms-from-var term-pat subs)))
	(cond
	 ;; cond-1: If the variable is bound in bound-terms 
	 ;; only, check to see if  matches
	 ;; anything in the term-set. If so, modify
	 ;; bound-terms to include only those entries from
	 ;; current-bound-terms with that binding. Else, this
	 ;; binding cannot match anything in the term set, and
	 ;; thus the matcher fails
	 ((not (set:emptyp bound-terms))
	  (let ((matched-bindings 
		 (set:intersection bound-terms term-set)))
	    (cond
	     ;; cond-2: The intersection  of term set with 
	     ;; bound-terms is empty,
	     ;; meaning they have no terms in common. Return empty
	     ;; subset and nil 
	     ((set:emptyp matched-bindings) 
	      (values subs nil))
	     ;; cond-2: Else there was at least one match found. Loop
	     ;; through the substitution set, and remove any
	     ;; entries that do not bind the current variable to
	     ;; one of the terms matched, or that bind one of the
	     ;; matched terms to another variable, then continue.
	     (t 
	      (let ((result-subs (subs-set:new-substitution-set))
		    (match-found nil))
		(set:loopset  
		 for term in matched-bindings
		 do (multiple-value-bind (returned matched)
			(pattern-term-set-match 
			 (rest set-pattern) 
			 (set:difference term-set (set:singleton term))
			 var-list 
			 (subs-set:remove-if 
			  #'(lambda (sub) 
			      (not 
			       (eq (subs:var-to-term term-pat sub) term)))
			  subs))
		      (when matched
			(setf match-found t)
			(subs-set:union-into-first result-subs returned))))
		(values result-subs match-found))))))
	 ;; cond-1: Else it not bound.  Bind the variable with the
	 ;; first term in the subset, recursively call the
	 ;; function. Then  bind it to the next and recursively call
	 ;; the function. Do this until all combinations have been
	 ;; tried. Merge the resulting substitutions.
	 (t 
	  (let ((result-subs (subs-set:new-substitution-set))
		(match-found nil))
	    (set:loopset  
	     for term in term-set
	     do
	     (multiple-value-bind (returned matched)
		 (pattern-term-set-match  
		  (rest set-pattern)
		  (set:difference term-set (set:singleton term))
		  var-list
		  (subs-set:add-binding term-pat term subs))
	       (when matched
		 (setf match-found t)
		 (subs-set:union-into-first result-subs returned))))
	    (values result-subs match-found))))))
       ;; cond-0: If the term-pat is a symbol...
     ((symbolp term-pat)
       ;; Find the term the term-pat builds
       (let ((term (find-term term-pat)))
	 (cond
	  ;; cond-3: If it is a member of the term-set under 
	  ;; comparison, Remove that element from the 
	  ;; term-set and continue, unless the term was bound
	  ;; previously in the substitution. If this is the case, fail.
	  ((set:member term term-set)
	   (pattern-term-set-match (rest set-pattern)
				   (set:difference term-set (set:singleton term))
				   var-list subs))
	  ;; cond-3: Else this term is not in term-set, but could be
	  ;; bound 
	  (t (values subs nil)))))
     ((consp term-pat)  
      (let ((result-subs (subs-set:new-substitution-set))
	    (match-found nil))
	    (set:loopset  
	     for term in term-set
	     do (when (typep term 'molecular)
		  (multiple-value-bind (mol-returned mol-matched)
		      (catch 'matched-results 
			(pattern-term-match term-pat 
					    (down-cableset term) 
					    var-list subs))
		    (when mol-matched
		      (multiple-value-bind (set-returned set-matched) 
			  (pattern-term-set-match 
			   (rest set-pattern)
			   (set:difference term-set (set:singleton term))
			   var-list mol-returned)
			(when set-matched
			  (setf match-found t)
			  (subs-set:union-into-first result-subs set-returned)))))))
	    (values result-subs match-found))))))

	
(defun find-exact (syntactic-type cf dcs
		   &optional (min 0 mingiven) (max 0 maxgiven))
  "If there is a molecular term with the case frame cf
        and the given syntactic type,
        and the down-cableset dcs (represented as a list of sets),
     returns that term;
     else returns nil."
  (let ((result (set:new-set)))
    (loop for r across (cf:caseframe-slots cf)
	for ns in dcs
	do (set:loopset for m in ns
			;; Find all frames in which m is a filler of the r slot,
			;;    and which use cf as their caseframe.
			for terms = (set:remove-if
				     #'(lambda (trm)
					 (not (eq (caseframe trm) cf)))
				     (findfrom m r))
			do (setf result
			     (if (set:emptyp result)
				 terms
			       (set:intersection result terms)))
			(when (set:emptyp result)
			  (return-from find-exact nil)))
	when (set:emptyp result)
	do (return-from find-exact nil))
    ;; result is now a set of nodes that might have filler sets that are too large
    (set:loopset for n in result
		 ;; as soon as find a good one, return it
		 ;; if never do, return nil
		 when (and (typep n syntactic-type)
			   (or (not mingiven)
			       (= min (minparam n)))
			   (or (not maxgiven)
			       (= max (maxparam n)))
			   (eqfillersets (down-cableset n) dcs))
		 do (return-from find-exact n)) 
    ;; Every result has some filler set that was too big.
    nil))

(defun eqfillersets (dcs1 dcs2)
  "Returns True if the corresponding elements
        of the vector dcs1 and the list dcs2 are
       are either both non-sets,
       or are sets of the same size;
     else returns False."
  (loop for fs1 across dcs1
      for fs2 in dcs2
      unless (or (and (typep fs1 'set:set)
		      (typep fs2 'set:set)
		      (= (set:cardinality fs1) (set:cardinality fs2)))
		 (and (not (typep fs1 'set:set))
		      (not (typep fs2 'set:set))))
	     do (return-from eqfillersets nil))
  t)




(defun find-variable-node (quant var-label restrictions deps var-list 
			   &optional (subs (subs-set:new-substitution-set)))
  "Finds an existing indefinite or arbitrary node based on:
      quant: The type of quantifier symbol 'some 'every
      var-label: The label for the variable
      restricitons: The restrictions for the variable
      deps: Indefinite dependencies, nil if this is an arbitrary,      
      var-list: List of variables in the find expression
      subs: The current substitution-set
   Returns the substitution-set and t if this found a node that
   matched the parameters, or nil otherwise."
  (loop for rst in restrictions
      do (when (contains-new-term-or-cf var-label rst)
	   (throw 'matched-results (values subs nil))))
  (remove-duplicates restrictions :test #'equal)
  
  ;;; Next determine what nodes are possible candidates by comparing
  ;;; the number of restrictions and semantic types
  (let* ((num-restrictions (length restrictions))
	 (num-dependencies (length deps))
         (possibles
	  (if (eq quant 'every)
	      (set:remove-if  
	       #'(lambda (n)
		   (not (= (set:cardinality (restriction-set n))
			   num-restrictions)))
	       (util:resource-value *ARBITRARIES*))
	    ;; For Indefinites they should have the same number of
	    ;; dependencies
	    (set:remove-if
	     #'(lambda (n)
		 (and 
		  (not (= (set:cardinality (restriction-set n))
			  num-dependencies))
		  (not (= (set:cardinality (restriction-set n))
			  num-restrictions))))
	     (util:resource-value *INDEFINITES*))))
	 (new-var-list (cons var-label var-list)))
	     
    ;; Possibles is now a set of arbitrary or indefinite variables 
    ;; with the correct number of restrictions and dependencies
    (when (set:emptyp possibles)
      (throw 'matched-results (values subs nil)))
    
    ;; Find if one of the possibles has the same restriction set using
    ;; find on the restrictions sets with variables, then checking if
    ;; possibles intersects any of the results. This also must ensure
    ;; that if the variable found is of the appropriate type.
    (loop 
	for rst in restrictions
	do (multiple-value-bind 
	       (found new-subs) 
	       (find rst new-var-list)
	     (cond 
	      ;; If no term is found with this restriciton set
	      ;; this node is unique
	      ((set:emptyp found) (throw 'matched-results (values subs nil)))
	    
	      ;; Else 
	      (t 
	       (setf possibles 
		 (set:intersection 
		  possibles 
		  (subs-set:get-all-terms-from-var var-label new-subs)))
	       (cond 
		;; If the intersection of the possibles with those
		;; found terms that bind the variable label for the variable
		;; under consideration is empty, this arbitrary doesn't
		;; exist yet
		((set:emptyp possibles)
		 (throw 'matched-results (values subs nil)))
		;; Else we can eliminate some possibles by looking
		;; at the other variables in the expression.
		;; 
		;; Incomplete? Is this possible? Hasn't been
		;; encountered yet.
		(t
		 
		 ))))))
    (throw 'matched-results (values subs t))))
 
  
				     
  

;;; Remove quant and dependencies, not needed
;;; Remove caseframe check, just look for terms
(defun find-old-arb-node (var-label restrictions arb-rsts ind-dep-rsts)
  "If an existing arbitrary node can be found such that:
      it is an arbitrary
      it is of the given semantic type;
      it has the given restrictions,
         where var-label stands for the variable node label in the 
         restrictions;
   that existing node is thrown or returned.
   Otherwise nil is thrown or returned."
  
  ;;; First check the restriction sets for any symbols that aren't
  ;;; nodes, or terms (i.e. symbols that reference existing terms) 
  ;;; If there is a new symbol, it will construct a new term,
  ;;; as such it is known there is no existing arbitrary node.
  (loop for rst in restrictions
      do (when (contains-new-term-or-cf var-label rst)
	   (throw 'old-vars nil)))
  
  (remove-duplicates restrictions :test #'equal)
  
  ;;; Next determine what nodes are possible candidates by comparing
  ;;; the number of restrictions and semantic types
  (let* ((num-restrictions (length restrictions))
         (possibles
	  (set:remove-if  
	   #'(lambda (n)
	        (not (= (set:cardinality (restriction-set n))
		       num-restrictions)))
	   (util:resource-value *ARBITRARIES*))))
    ;; Possibles is now a set of arbitrary variables 
    ;; with the correct number of restrictions
    (when (set:emptyp possibles)
      (throw 'old-vars nil))
    ;; Find if one of the possibles has the same restriction set using
    ;; find on the restrictions sets with variables, then checking if
    ;; possibles intersects any of the results. This also must ensure
    ;; that if the variable found is of the appropriate type.
    
    
    ;; Create the variable list from the arb-rsts and ind-dep-rsts keys
    (let ((var-list '())) 
      (maphash #'(lambda (key value) (push key var-list)) arb-rsts)
      (maphash #'(lambda (key value) (push key var-list)) ind-dep-rsts)
      
      (loop 
	  for rst in restrictions
	  do (multiple-value-bind 
		 (found subs) 
		 (find rst var-list)
	       (cond 
		;; If no term is found with this restriciton set
		;; this node is unique
		((set:emptyp found) (throw 'old-vars nil))
		
		;; Else 
		(t 
		 (setf possibles 
		   (set:intersection 
		    possibles 
		    (subs-set:get-all-terms-from-var var-label subs)))
		 (cond 
		  ;; If the intersection of the possibles with those
		  ;; found terms that bind the variable label for the arbitrary
		  ;; under consideration is empty, this arbitrary doesn't
		  ;; exist yet
		  ((set:emptyp possibles)
		   (throw 'old-vars nil))
		  ;; Else we can eliminate some possibles by looking
		  ;; at the other variables in the expression.
		  ;; 
		  ;; Incomplete? Is this possible? Hasn't been
		  ;; encountered yet.
		  (t
		   
		    )))))))
      (throw 'old-vars (set:choose possibles))))
  
(defun contains-new-term-or-cf (var-label restriction)
  "Returns t if a resriction contains new terms not already in the KB."
  (let ((found nil))
      (loop 
	  for arg in (rest restriction)
	  do (cond
	      ((consp arg) (setf found (contains-new-term-or-cf var-label arg)))
	      ((not (or 
		     (eq var-label arg)
		     (typep arg 'term)
		     (find-term arg))) 
	       (setf found t))))
      found))

(defun qVarp (x)
  "Returns True if x is a symbol whose name begins with a question mark."
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))
