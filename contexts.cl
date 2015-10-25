;;; SNePS 3: Definition of Contexts
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

;;; A context is a named structure that contains a set of hypotheses
;;;    and a set of asserted derived propositions.
;;; The set of contexts form a rooted dag, with the root named DefaultCT.

(defpackage :context
  (:nicknames :ct)
  (:shadow cl:assert)
  (:export
   ;; Context class
   #:context
   
   ;; Global
   #:*CONTEXTS*
   
   ;; Root context name
   #:BaseCT
   
   ;; Default current context name
   #:DefaultCT
   
   ;; Functions
   #:assert #:assertedp #:currentContext
   #:defineContext #:find-context #:list-contexts #:setCurrentContext
   #:remove-from-context
   #:unassert
   #:variable-parse-and-build
   #:build-variable
  
  ;; Methods
  #:ct-name))

(in-package :context)

(defvar *CONTEXTS* (make-hash-table)
  "A map from context name to context.")

(defparameter *CurrentContext* nil
  "The current context.")

(defstruct (context (:conc-name ct-))
  "A SNePS 3 Context"
  (name (gensym "ct")			; the name of this context
	:type symbol :read-only t)
  (docstring ""				; A documentation string for this context
	     :type string :read-only t)
  (parents nil				; this context's parent contexts
	   :type list :read-only t)
  (hyps					; the set of hypotheses of this context
   (util:make-resource :value (set:new-set))
   )
  (ders			; the set of derived asserted terms of this context
   (util:make-resource :value (set:new-set))
	)
  (kinconsistent nil			; t if this context is known to be inconsistent
		 )
  (:documentation "The class of contexts."))

(defun find-context (ctx)
  "If ctx is a context, returns it.
     If ctx is a symbol, returns the context named ctx
      or nil if there isn't any."
  (typecase ctx
    (context ctx)
    (symbol (gethash ctx *CONTEXTS*))))

(defun currentContext ()
  "Returns the current context."
  *CurrentContext*)

(defun setCurrentContext (ctx)
  "If ctx is a context name,
         makes the context named ctx the current context.
     If ctx is a context, makes it the current context.
     Else raises an error."
  (cl:assert (or (typep ctx 'context)
		 (and (symbolp ctx) (ct:find-context ctx)))
      (ctx) "~S is neither a context nor the name of a context." ctx )
  (typecase ctx
    (symbol (setf *CurrentContext* (find-context ctx)))
    (context (setf *CurrentContext* ctx)))
  ;;Added by DRS 11-30-2009
;  (if (and sneps3::*sneps-gui-ptr* sneps3::*auto-refresh-graph*) 
;    (sneps3::generate-graph))
  (if sneps3::*sneps-gui-ptr* (sneps3::gui-select-current-context))
  *CurrentContext*)

(defun defineContext (name &key (docstring "")
				(parents '(BaseCT))
				(hyps nil))
  "Defines a new context with the given name, docstring,
        parent contexts, and initial hypotheses."
  (check-type name symbol)
  (check-type docstring string)
  (check-type parents list)
  (check-type hyps list)
  (if (find-context name)
      (error "A context named ~S already exists." name)
    (setf (gethash name *CONTEXTS*)
      (make-context
       :name name :docstring docstring
       :parents (mapcar #'find-context parents )
       :hyps
       (util:make-resource
	:value (set:new-set
		:items (loop for p in hyps
			   collect (sneps:build
				    p (find-class 'sneps:Proposition))))))))
  ;;Added by DRS 11-30-2009, modified 05-02-2010
  (if sneps3::*sneps-gui-ptr* (sneps3::send-one-context (gethash name *CONTEXTS*)))
  (gethash name *CONTEXTS*))

(defun list-contexts ()
  "Prints a list of all the contexts."
  (loop for ctx being the hash-key of *CONTEXTS*
      do (format *standard-output* "~S~%" (find-context ctx)))
  (values))


(defmethod remove-from-context ((trm sneps:molecular) (ctx context))
  "Removes the term from the context (ctx) hyps or ders."
  (util:protecting 
   (ct-hyps ctx) 
   (set:remove-item trm (ct-hyps ctx)))
  (util:protecting
   (ct-ders ctx)
   (set:remove-item trm (ct-ders ctx)))) 

(defmethod add-to-context (trm ctx)
  (set:add-item (sneps:build trm (find-class 'sneps:Proposition)) (util:resource-value (ct-hyps ctx))))
