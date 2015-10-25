;;; SNePS 3: Definition of Caseframes
;;; =================================
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

(defpackage :sneps3-caseframe
  (:nicknames :cf)
  (:export
   #:*CASEFRAMES*
   #:*FN2CF* 
   #:add-caseframe-term 
   #:caseframe
   #:caseframe-adj-to
   #:caseframe-adj-from
   #:caseframe-equal
   #:caseframe-descfun 
   #:caseframe-docstring
   #:caseframe-name
   #:caseframe-print-pattern
   #:caseframe-slots 
   #:caseframe-type
   #:caseframe-terms
   #:defineCaseframe
   #:defineNoviceCaseframe
   #:find-frame
   #:get-caseframe-terms
   #:hasOneArgumentSlot
   #:list-caseframes #:make-caseframe
   #:*NoviceCaseframes* #:sameFrame
   #:quotedpp
   ))

(in-package :sneps3-caseframe)

(defparameter *CASEFRAMES* (set:new-set)
  "A set of all the caseframes.")

(defparameter *FN2CF* (make-hash-table)
  "Map from function name to caseframe.")

(defparameter *NoviceCaseframes* (make-hash-table)
  "Map from the number of slots of a novice caseframe
           to the novice caseframe.")

(defstruct caseframe
  (type                 ; the semantic type of the frame
   )
  (docstring            ; a documentation string in documentation format.
   )
  (descfun              ; a function of a node that returns its description
   )
  (slots                ; a vector of slots
   )
  (print-pattern        ; a pattern for printing in functional format
   )
  (adj-to               ; set of frames to which this is adjustable
   (set:new-set))
  (adj-from             ; set of frames that are adjustable to this one
   (set:new-set))
  (terms                ; a set of terms that have this as their caseframe
   (util:make-resource :value (set:new-set))
   :accessor get-caseframe-terms))

;; shouldn't this be a part of the caseframe struct? -- Mike P.
;;This returns the name of the cf, but if the cf doesn't have a name
;;(ie. it has a slot in its first position), it returns the name of
;;that slot. We should think about changing this though at some point
;;and explicitly requiring a name in those cases (for the sake of the GUI
;;mostly).
(defun caseframe-name (cf)
  "Returns the name of the caseframe"
  (if (listp (first (caseframe-print-pattern cf)))
        (symbol-name (second (first (caseframe-print-pattern cf))))
        (symbol-name (first (caseframe-print-pattern cf)))))

(defmethod print-object ((cf caseframe) stream)
  "Prints a caseframe in a human-readable format"
  (print-unreadable-object 
      (cf stream)
    (format stream "caseframe:~15T~S~%~2Ttype:~15T~S~%~2Tslots:" 
	    (first (caseframe-print-pattern cf))
	    (caseframe-type cf))
    (loop for i from 0 below (length (caseframe-slots cf))
	do (format stream "~15T~S~%" (slot:slot-name
                                      (elt (caseframe-slots cf) i))))))

(defun make-description-function (desc-string)
  "Parses the docstring of the caseframe cf,
     and creates a function that returns a description for the
     case-frame arcs."
  ;; For SNePS 2.7 by Michael W. Kandefer 9/13/06
  ;; Modified by Stuart C. Shapiro 7/3/07
  (let* ((desc-string-copy desc-string)
	 (arc-list 
	  (loop  
	      for lb-pos = (position #\[ desc-string-copy)
	      for rb-pos = (position #\] desc-string-copy)
	      until (not lb-pos)
	      collect (slot:find-slot
		       (intern (read-from-string (subseq desc-string-copy 
				       (1+ lb-pos) rb-pos)) :snuser)) 
	      do (setf desc-string-copy 
		   (concatenate 'string 
		     (subseq desc-string-copy 0 lb-pos) 
		     "~A"
		     (subseq desc-string-copy (1+ rb-pos)))))))
    (lambda (n) 
      (apply #'format nil desc-string-copy
	     (loop 
		 for arc in arc-list
		 collect
		   (sneps:description (sneps:findto n arc)))))))

#|

Molecular Terms, Caseframes, Caseframe Print-Patterns, Parsing, and
Generating.

The user types and sees a molecular term in KIF format: (f a1 ... an).
The issues are:
    finding the caseframe to use to represent the term (parsing);
    reconstructing the expression from the stored term (generating).

Generation is easy.
    The stored term is essentially represented as an instance,:
        (slot1 filler1 ... slotn fillern)
    of a caseframe that has a sequence of slots (slot1 ... slotn)
    and a print-pattern, a list of pattern elements: (pe1 ... pen),
        where each pei is a slot name, or a quoted symbol.	
    The generated expression is (pe1' ... pen'),
        where pei' is pei itself if pei is quoted,
                   and is the generated term(s) which fill the pei
                   slot otherwise.

Parsing is more difficult.
It requires retrieving the caseframe from the function symbol, f.		   
There are 3 cases:
1)  The function symbol doesn't occur in the represented term, but is represented by the caseframe.
    An example is Isa, represented by the (member class) caseframe.
    In this case the function symbol is mapped to the caseframe in *FN2CF*,
                 and appears quoted in the print-pattern.

2) The function symbol occurs in the represented term,
       because it is one of several instances
       of a more general function, which is represented by the case frame.
   Examples are (senseFor smell) and (go left),
       which might both be represented by the (action object) caseframe.
   In this case, the print-pattern will have all non-quoted pe's.
                 and every function symbol that uses this caseframe
                     needs to be mapped to the caseframe in *FN2CF*.

3) The function symbol is a molecular term.
   For example, for any binary relation R,
       e(R) could be the reflexive closure of R
       defined by the axiom
           (forall R (=> (binaryRelation R) (forall (x y) (iff ((e R) x y) (or (R x y) (= x y))))))
   Generation is easily done:
       I'll assume that pe1 of the print pattern will be an unquoted slot name,
       and the function-symbol slot will be filled by a molecular term
   but for parsing, we need a mapping from e to the caseframe that
   will be used for representing (e R),
   and then a mapping from that term to the caseframe that will be
   used for the representation of ((e R) a b).
   We can do that by having a caseframe as the key in the mapping *FN2CF*.
   Map e to the caseframe for (e R), 
       and that caseframe to the caseframe for ((e R) a b), etc.
   In general the function symbol slot could be filled by a molecular term,
      whose function symbol slot is filled by a molecular term, etc.
      E.g. (((e1 R) a b) c d), and so on.
      Map e1 to its caseframe, that caseframe to the next, etc.
   The problems are now:
      How does the user tell SNePS what that sequence is?
      How does the user specify which slot gets the function "symbol"?
   Idea:  The user should use defineCaseframe as for when pe1 is a slot name
          and the fsymbols argument should include the primitive function symbol
               enclosed in the appropriate number of lists.
          So for (senseFor smell) and (go left)
	   the fsymbols are senseFor and go
             for ((e R) a b) the fsymbol is (e)
             for (((e1 R) a b) c d) the fsymbol is ((e1))
|#

(defun defineCaseframe (typename slots &key (docstring "")
					    print-pattern fsymbols)
  (assert (sneps:semantic-type-p typename) (typename)
    "~S is not a semantic type" typename)
  (checkNewCaseframe typename slots)
  (assert (stringp docstring) (docstring)
    "Docstring, ~S, is not a string." docstring)
  (assert (listp print-pattern) (print-pattern)
    "~S is not a list" print-pattern)
  (assert (or (null fsymbols) (consp fsymbols)) (fsymbols)
      "Function symbol ~S must be nil or a list." fsymbols)
  (assert (or (and (listp (car print-pattern))
		   (eq (caar print-pattern) 'quote))
	      fsymbols) (fsymbols)
    "A list of function symbols is needed.")
  (mapc #'(lambda (rel)
	    (when (not (listp rel))
	      (unless (or (member rel slots)
			(member (slot:find-slot rel) slots))
		(error
		 "The print-pattern slot ~S is not among the list of slots ~S."
		 rel slots))))
	print-pattern)
  (let ((cf (make-caseframe
	     :type (find-class typename)
	     :docstring docstring
	     :descfun
	     (if (string= docstring "")
		 (lambda (trm)
		   (sneps:print-unnamed-molecular-term
		    trm *standard-output*))
	       (make-description-function docstring))
	     :print-pattern print-pattern 
	     :slots (map
			'(simple-array set:set)
		      #'(lambda (r)
			  (let ((rel (slot:find-slot r)))
			    (if rel
				rel
			      (error
			       "~S is not a slot nor the name of a slot"
			       r))))
		      slots))))
    (cond ((and (listp (car print-pattern))
		(eq (caar print-pattern) 'quote))
	   ;; Store the caseframe in the function-name to caseframe map
	   (let ((fname (cadar print-pattern)))
	     (when (and (find-frame fname)
			(not (equalp (find-frame fname) cf)))
	       (warn "~S being redefined from ~S~%"
		     fname (find-frame fname)))
	     (setf (gethash fname *FN2CF*) cf))
	   (when fsymbols
	     (warn
	      "Function symbols ~S being ignored ~
                        because the print-pattern starts with a quoted symbol."
	      fsymbols)))
	  (t (mapc #'(lambda (fs)
		       (add-fn-cf-map fs cf))
		   fsymbols)))
    (set:loopset for cf2 in *CASEFRAMES* do 
		 ;; Look at all existing caseframes, check whether they are adjustable to or 
		 ;; from this one. If so, store that information in the frames.
		 (when (not (eq cf cf2))
		   (when (adjustable cf cf2)
		     (add-adj-to cf cf2))
		   (when (adjustable cf2 cf)
		     (add-adj-to cf2 cf))))
    (set:add-item cf *CASEFRAMES*)
    ;;;Added by DRS 4/1/2010
    (if sneps3::*sneps-gui-ptr* (sneps3::send-one-caseframe cf fsymbols))
    cf))

(defun checkNewCaseframe (typename slots)
  "If there is already a caseframe with the given typename
        and slots (order doesn't matter),
    raises an error;
    Otherwise, returns."
  (set:loopset for oldcf in *CASEFRAMES*
	       for oldslots = (caseframe-slots oldcf)
	       for numnewslots = (length slots)
	       with newtype = (find-class typename) 
	       when (and 
		     (eq newtype (caseframe-type oldcf))
		     (= (length oldslots) numnewslots)
		     (every
		      #'(lambda (os)
			  (member (slot:slot-name os) slots))
		      oldslots))
	       do (error "A caseframe with type = ~S and slots = ~S already exists."
			 typename slots)))

(defun add-fn-cf-map (fn cf)
  "Adds the map from the function name fn to the caseframe cf.
     If fn is a list, (f), the map is from the caseframe f is mapped to to cf."
  (typecase fn
    (atom (setf (gethash fn *FN2CF*) cf))
    (list (unless (find-frame (first fn))
	    (error "~S does not have a caseframe defined for it."
		   (first fn)))
     (setf (gethash (find-frame (first fn)) *FN2CF*) cf))))

(defun sameFrame (newf oldf)
  "Associates the same frame associated with the function symbol oldf
       with the symbol, or list of symbols, newf."
  (check-type newf (or (and symbol (not null)) cons))
  (check-type oldf symbol)
  (assert (find-frame oldf) (oldf)
	 "~S does not have a caseframe defined for it." oldf)
  (if (atom newf)
      (setf (gethash newf *FN2CF*) (find-frame oldf))
    (mapc #'(lambda (fs)
	      (setf (gethash fs *FN2CF*) (find-frame oldf)))
	  newf))) 

(defun find-frame (fname)
  "Returns the caseframe associated with the given function symbol."
  (gethash fname *FN2CF*))

(defun list-caseframes ()
  "Print all the caseframes."
  (set:loopset for cf in *CASEFRAMES*
	       do (print cf)))

(defun quotedpp (cf)
  "Returns True if the caseframe cf
        has a print-pattern with a quited first symbol;
     False otherwise."
  (and (consp (first (caseframe-print-pattern cf)))
       (eq (caar (caseframe-print-pattern cf)) 'quote)))

(defun add-caseframe-term (term &optional cf)
  "Adds a term to a caseframe's list of terms that use it.
   If the caseframe cf is given, add the term to that caseframe.
   Else, add the term to the caseframe that term uses."
  (let ((cfterms (caseframe-terms (if cf cf (sneps:caseframe term)))))
    (util:protecting cfterms
		     (set:add-item term cfterms))))
  
(defun adjustable (srcframe tgtframe)
  "Returns t if srcframe is a caseframe that is adjustable 
         to the caseframe tgtframe"
  (or (pos-adj srcframe tgtframe)
      (neg-adj srcframe tgtframe)
      (pseudo-adjustable srcframe tgtframe)))

(defun pseudo-adjustable (srcframe tgtframe)  
  "Returns t if srcframe is 'pseudo-adjustable' to tgtframe. 
       Pseudo-adjustability allows slot-based inference to operate on frames
       that are not actually adjustable, e.g. nor and andor"
  (cond    
   ;; nor pseudo-adjusts to andor
   ((eq srcframe (find-frame 'sneps::nor))  (eq tgtframe (find-frame  'sneps::andor )))))

(defun pos-adj (srcframe tgtframe)
  "Returns t if srcframe is a caseframe
         that is pos-adjustable to the caseframe tgtframe"
  (or (eq srcframe tgtframe)
      (let ((srcslots  (cf:caseframe-slots srcframe))
	    (tgtslots  (cf:caseframe-slots tgtframe)))
	;; CF <C_src,R_src> is pos-adjustable to case frame <C_tgt,R_tgt> iff:
	(and     
	 ;; 1) C_src is the same as, or a subtype of, C_tgt
	 (subtypep (type-of  (cf:caseframe-type srcframe)) 
		   (type-of  (cf:caseframe-type tgtframe)))
	 ;; 2) Every slot in R_src - R_tgt is posadjust reducible and has min = 0
	 (every #'(lambda (r)
		    (or (find r tgtslots)
			(and (= (slot:slot-min r) 0)
			     (eq (slot:slot-posadjust r)
				 'slot:reduce))))
		srcslots)
	 ;; 3) Every slot in R_tgt - R_src is posadjust expandable and has min = 0     
	 (every #'(lambda (r)
		    (or  (find r srcslots)
			 (and (= (slot:slot-min r) 0) 
			      (eq (slot:slot-posadjust r) 'slot:expand))))
		tgtslots)))))

(defun neg-adj (srcframe tgtframe)
  "Returns t if srcframe is a caseframe
        that is neg-adjustable to the caseframe tgtframe"
  (or (eq srcframe tgtframe)
      (let ((srcslots  (cf:caseframe-slots srcframe))
	    (tgtslots  (cf:caseframe-slots tgtframe)))
	;; Case frame <C_src,R_src> is neg-adjustable to case frame <C_tgt,R_tgt> iff:
	(and
	 ;; 1) C_src is the same as, or a subtype of, C_tgt
	 (subtypep (type-of  (cf:caseframe-type srcframe)) 
		   (type-of  (cf:caseframe-type tgtframe)))
	 ;; 2) Every slot in R_src - R_tgt is negadjust reducible and has min = 0
	 (every #'(lambda (r)
		    (or (find r tgtslots)
			(and (eq (slot:slot-min r) 0) 
			     (eq (slot:slot-negadjust r)
				 'slot:reduce))))
		srcslots)
	 ;; 3) Every slot in R_tgt - R_src is nrgadjust expandable and has min = 0
	 (every #'(lambda (r) 
		    (or (find r srcslots)
			(and (eq (slot:slot-min r) 0) 
			     (eq  (slot:slot-negadjust r)
				  'slot:expand))))
		tgtslots)))))

(defun add-adj-to (source target)
  "Given that source and target are caseframes:
    Adds target to the list of frames source is adjustable to, and 
    Adds source to the list of frames target is adjustable from."
  (set:add-item target (caseframe-adj-to source))
  (set:add-item source (caseframe-adj-from target)))

(defun get-caseframe-terms (cf)
  "Gets the value of the caseframe-terms resource for the caseframe cf"
  (util:resource-value (caseframe-terms cf)))

(defun hasOneArgumentSlot (cf)
  "Returns t if the caseframe, cf, specifies a single argument slot."
  (= (- (length (cf:caseframe-slots cf))
	(if (cf:quotedpp cf) 0 1))
     1))

(defun caseframe-equal (cf1 cf2)
  "Returns t if both arguments are equivalent caseframes. 
   Two caseframes are equivalent when:
      1. They have the same type
      2. They have the same slots (slot order is disregarded)"
  (and (eq (caseframe-type cf1) (caseframe-type cf2))
       (every #'(lambda (x) 
		  (some #'(lambda (y) (eq x y)) 
			(caseframe-slots cf2)))
	      (caseframe-slots cf1))
       (every #'(lambda (x) 
		  (some #'(lambda (y) (eq x y)) 
			(caseframe-slots cf1)))
	      (caseframe-slots cf2))))

(defun defineNoviceCaseframe (fn expr)
  "Defines a caseframe for the novice user
     for the function named fn, which is known not to already have a caseframe.
     fn is a symbol or a molecular node used as a functional-term function.
     expr is the term the user is trying to build, (fn arg1 arg2 ...)."
  (let ((numslots (length expr)))
    (if (gethash numslots *NoviceCaseframes*)
	;; Already a novice caseframe for this number of slots
	(setf (gethash fn *FN2CF*) (gethash numslots *NoviceCaseframes*))
      ;; Need to make a new novice caseframe for this number of slots
      (setf (gethash numslots *NoviceCaseframes*)
	(let* ((slotnames
		(cons (intern "fn" :snuser)
		      (loop for slotno from 1 to (1- numslots)
			  collect (intern (format nil "arg~d" slotno) :snuser))))
	       (slots
		(loop for slotname in slotnames
		    collect (or (slot:find-slot slotname nil)
				(slot:defineSlot slotname)))))
	  (defineCaseframe 'sneps3:Entity slots
	    ;; the docstring is what is used by describe-terms.
	    :docstring (format nil "(~{[~S]~^ ~})" slotnames)
	    :print-pattern slotnames
	    :fsymbols (list fn)))))))
