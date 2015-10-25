
;;; SNePS 3: Path-Based
;;; ===================
;;; Written by Jonathan P. Bona
;;;            and Stuart C. Shapiro
;;;
;;; Functions for path-based reasoning

(in-package :snip)

(defun definePath (slotname pathexpr)
  "Given a slot name and a path expression, 
   generate the functions that will compute that path and its converse, 
   and store them in the slot."
  (let ((fwd (build-path-fn pathexpr))
	(bwd (build-path-fn (converse pathexpr)))
	(aslot  (slot:find-slot slotname)))
    (setf 
	(slot:slot-path aslot) pathexpr
	(slot:slot-b-pathfn aslot) bwd
	(slot:slot-f-pathfn aslot) fwd)
    (values)))

(defun asserted-members (termset ctx)
  "Given a set of terms, return the set 
    containing only those that are asserted in the given context"
  (set::new-set :items 
		(set:loopset for term in termset
			     when (ct::assertedp term ctx)
			     collect term)))
	       
(defgeneric pb-findfroms (terms slot))
(defmethod pb-findfroms ((terms symbol) slotname)
  (pb-findfroms (sneps:find-term terms) slotname))
(defmethod pb-findfroms ((terms sneps:term) slotname)
  (pb-findfroms (set:singleton terms) slotname))
(defmethod pb-findfroms ((terms set:set) (slotname symbol))
  (pb-findfroms terms (slot::find-slot slotname)))
(defmethod pb-findfroms ((terms set:set) (slot slot:slot))
  "Returns the set of nodes
   from which the given slot, or a path for the slot, goes to term."
  (let ((fn (slot::slot-b-pathfn slot)))
    (if fn (funcall fn terms)
      (get-froms terms slot))))

(defgeneric pb-findtos (terms slot))
(defmethod pb-findtos ((terms symbol) slotname)
  (pb-findtos (sneps:find-term terms) slotname))
(defmethod pb-findtos ((terms sneps:term) slotname)
  (pb-findtos (set:singleton terms) slotname))
(defmethod pb-findtos ((terms set:set) (slotname symbol))
  (pb-findtos terms (slot::find-slot slotname)))
(defmethod pb-findtos ((terms set:set) (slot slot:slot))
  "Returns the set of nodes to which the given slot goes from 
   each of the terms, or to which the path for the slot goes"
  (let ((fn (slot::slot-f-pathfn slot)))
    (if fn (funcall fn terms)
      (get-tos terms slot))))

(defgeneric pathsfrom (terms path)
  (:documentation
   "Returns the set of nodes
    from which the given path goes from the terms."))
(defmethod pathsfrom ((terms null) path)
  (declare (ignore path))
  (set:new-set))
(defmethod pathsfrom ((terms symbol) path)
  (pathsfrom (sneps:find-term terms) path))
(defmethod pathsfrom ((terms cons) path)
  (pathsfrom (set:new-set
	      :items (delete nil (mapcar #'sneps:find-term terms)))
	     path))
(defmethod pathsfrom ((terms sneps:term) path)
  (pathsfrom (set:singleton terms) path))
(defmethod pathsfrom ((terms set:set) path)
  (funcall (build-path-fn path) terms))

(defun path-keywordp (s)
  "Returns nil if the argument is not a path keyword"
  (when (symbolp s)
    (member (intern s :snip)
	    '(or and compose kstar kplus not relative-complement
	      irreflexive-restrict restrict converse))))

(defun converse (path)
  "Given a path expression, returns its converse"
  (when path
    (if (atom path)			; unitpath
	(if (equal '! (intern path :snip))
	    path
	  (let ((revname (rev-slotname path)))
	    (if revname 
		revname 
	      (intern (concatenate 'string (symbol-name path) "-")))))
      (if (path-keywordp (first path))
	  (if (string= (symbol-name (first path)) "restrict")
	      path
	    (cons (first path)
		  (reverse 
		   (loop for elt in (rest path) 
			 collect (converse elt)))))
	(reverse (loop for elt in path 
		       collect (converse elt)))))))

(defun memberOrVar (symbol termSet)
  "Returns t if either symbol is `?'and termSet is non-empty,
      of f the term named `symbol' is a member of the termSet."
  (or (and (symbolp symbol)
	   (string= (symbol-name symbol) "?")
	   (not (set:emptyp termSet)))
      (set:member (sneps:find-term symbol) termSet)))

(defun build-path-fn (path)
  "Given a path expression, returns the function that will traverse that path"
  (compile 
   nil
   (if (consp path) 
       (case (intern (first path) :snip) ; complex paths
	 (compose   `(lambda (x) ,(compose-helper (reverse (rest path)))))
	 (or  `(lambda (x) ,(or-helper (rest path))))
	 (and `(lambda (x) ,(and-helper (rest path))))
	 (kstar
	  (assert (null (cddr path)) (path)
	    "kstar must have only one path argument in ~S" path)
	  `(lambda (x) (f* x ,(build-path-fn (second path)))))
	 (kplus
	  (assert (null (cddr path)) (path)
	    "kplus must have only one path argument in ~S" path)
	  `(lambda (x) (f+ x ,(build-path-fn (second path)))))
	 (converse (build-path-fn (converse (second path))))
	 ;;don't need	 (not '())
	 ;;don't need	 (relative-complement '())
	 (irreflexive-restrict 
	  `(lambda (x) (set:difference  (funcall ,(build-path-fn (second path)) x) x)))
	 (restrict
	  ;; (restrict path symbol-or-?)
	  (assert (and (= (length path) 3)) (path)
	    "restrict must have two arguments, a path, and an atomicwft in ~S" path)
	  `(lambda (x)
	     (set:new-set
	      :items
	      (set:loopset for trm in x
			   if (memberOrVar ',(third path)
					   (funcall ,(build-path-fn (second path))
						    (set:singleton trm)))
			   collect trm))))
	 (t (error "Unrecognized path expression operator: ~S" (first path))))
     (if (equal '! (intern path :snip))
	 ;; handle asserted 
       	 `(lambda (x) (asserted-members x (ct::currentContext)))
       (let ((rev (rev-slotname path)))	; unit path
	 (if rev 
	     `(lambda (x) (get-froms x (quote ,rev)))
	   `(lambda (x) (get-tos x (quote ,path))))))))) 

(defun and-helper (path-elts)
  "Given a list of path elements, return an expression that, when 
   evaluated, gives the intersection of the sets that result 
   from calling the path function for each of those elements"
  (append '(set:intersection*)
	  (loop for elt in path-elts
	      collect `(funcall ,(build-path-fn elt) x))))

(defun or-helper (path-elts)
  "Given a list of path elements, return an expression that, when 
   evaluated, gives the union of the sets that result 
   from calling the path function for each of those elements"
  (append '(set:union*)
	  (loop for elt in path-elts
	      collect `(funcall ,(build-path-fn elt) x))))



(defun compose-helper (path-elts)
  "Given a list of path elements (in the reverse of their original order), 
    return a function that will traverse a path in the original order"
  (if (rest path-elts)			; if there are more after this:
      `(funcall ,(build-path-fn (first path-elts)) 
		,(compose-helper (rest path-elts)))
    ;; else this is the only path element
    `(funcall ,(build-path-fn (first path-elts)) x)))


(defun get-froms (nodes slotname)
  "Given a set of nodes and a symbol that names a slot/arc, 
    returns all the nodes that have that arc going from them 
    to one or more of the input nodes"
  (let ((result '()))
    (apply #'set::union*
	   (set:loopset for n in nodes 
			do (setf result (sneps::findfrom n slotname))
			when (and result (not (set:emptyp result)))
			collect result))))


(defun get-tos (nodes slotname)
  "Given a set of nodes and a symbol that names a slot/arc, 
    returns all the nodes that have that arc going to them 
    from one or more of the input nodes"
  (let ((result '()))
    (apply #'set::union*
	   (set:loopset for n in nodes 
			do (setf result (sneps::findto n slotname))
			when (and result (not (set:emptyp result)))
			collect result))))

(defun rev-slotname (symbol)
  "Given a symbol that names a slot (either 'forward' or 'backward'):
      if the symbol is the backward name of a slot, return the forward name 
      otherwise, return nil"
  (let ((sym-name (symbol-name symbol)))
    (when (equal (subseq sym-name (1- (length sym-name))) "-")
      (intern (subseq sym-name 0 (1- (length sym-name ))) :snuser))))


(defun f+ (nodeset fn)
  "Given a nodeset and a function, return the nodeset that results 
    from repeately applying the function to the nodeset one or more times"
  (let ((res (funcall fn nodeset))
	(retval (set:new-set)))
    (loop while (not (set:emptyp res))
	do (setf retval (set:union retval res))
	   (setf res (set:difference (funcall fn res) retval)))
    retval))


(defun f* (nodeset fn)
    "Given a nodeset and a function, return the nodeset that results 
    from repeately applying the function to the nodeset zero or more times"
  (set:union nodeset (f+ nodeset fn)))

(defun path-based-derivable (p context)
  "If the proposition p is derivable in the given context by path-base-inference,
      return a singleton set of that proposition;
   else return the empty set."
  (when *GOALTRACE* 
    (format *trace-output* "~&I will consider using Path-Based inference.~%"))
  (if (typep p 'sneps:molecular)
      (let* ((cf (sneps:caseframe p))
	     (dcs (sneps::down-cableset p))
	     (ldcs (coerce dcs 'list))
	     (firstTime t)
	     (results (set:new-set)))
	(loop for slot across (cf:caseframe-slots cf)
	    for fillers across dcs
	    do (set:loopset for arg in fillers
			    if firstTime
			    do (setf results (pb-findfroms arg slot))
			    and do (setf firstTime nil)
			    else
			    do (setf results (set:intersection results
							       (pb-findfroms arg slot))))
	    finally (setf results
		      (set:remove-if #'(lambda (wft) (not (ct:assertedp wft context)))
				     results)))
	(cond ((and
		(not (set:emptyp results))
		(set:set-some
		 #'(lambda (result) (sneps::eqfillersets
				     (sneps::down-cableset result)
				     ldcs))
		 results))
	       (assertTrace nil (set:set-to-list results) p "Path-Based inference" context)
	       (set:singleton p))
	      (t (set:new-set))))
    (set:new-set)))
