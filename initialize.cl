;;; SNePS 3: Clearkb --- Initializing SNePS
;;; =======================================
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

(in-package :sneps3-user)

(defun clearkb (&optional (clearall nil))
  "Reinitializes the SNePS knowledge base.
    If clearall is non-nil, also reinitializes all slots, and caseframes,
    but not the semantic types."

  (if (and (not clearall) sneps3::*sneps-gui-ptr*) (sneps3::reinit-gui sneps3::*sneps-gui-ptr* nil))
  (if (and clearall sneps3::*sneps-gui-ptr*) (sneps3::reinit-gui sneps3::*sneps-gui-ptr* t))

  ;; Initialize contexts
  (setf ct:*CONTEXTS* (make-hash-table))
  
  (defineContext 'BaseCT
      :parents nil
      :docstring "The root of all the contexts.")

  (setCurrentContext 
   (defineContext 'DefaultCT
       :parents '(BaseCT)
       :docstring "The default current context."))

  ;; Initialize the set of terms.
  (setf
      (sneps:counter (make-instance 'sneps:molecular
			:caseframe (cf:make-caseframe)))
    (util:make-resource :value 0)
    (util:resource-value sneps:*TERMS*)
    (make-hash-table))
  
  
  ;; Initialize the set of arbitraries.
  (setf
      (sneps:counter (make-instance 'sneps:arbitrary))
    (util:make-resource :value 1)
    (util:resource-value sneps:*ARBITRARIES*)
    (set:new-set))
  
  
  ;; Initialize the set of indefinites.
  (setf
      (sneps:counter (make-instance 'sneps:indefinite))
    (util:make-resource :value 1)
    (util:resource-value sneps:*INDEFINITES*)
    (set:new-set))
  
  
  
    ;; Needs to erase pointers to terms in the caseframes themselves
    (set:loopset for cf in cf:*CASEFRAMES*
		 do (setf (cf::caseframe-terms cf)
		      (util:make-resource :value (set:new-set))))
  
  (when clearall

    ;; Reinitialize slots
    (setf slot:*SLOTS* (make-hash-table))

    ;; Slots for built-in Propositions
    ;; ===================================
    (defineSlot class :type Category
		:docstring "Points to a Category that some Entity is a member of."
		:negadjust reduce)
    (defineSlot member :type Entity
		:docstring "Points to the Entity that is a member of some Category."
		:negadjust reduce) 
    (defineSlot equiv :type Entity
		:docstring "All fillers are coreferential."
		:min 2 :negadjust reduce
	    :path (compose ! equiv (kstar (compose equiv- ! equiv))))

    ;; Slots for Rules
    ;; ===================
    (defineSlot and :type Proposition
		:docstring "Fillers are arguments of a conjunction."
		:min 2 :posadjust reduce :negadjust expand)
    (defineSlot nor :type Proposition
		:docstring "Fillers are arguments of a nor."
		:min 1 :posadjust reduce :negadjust expand) 
    (defineSlot andorargs :type Proposition
		:docstring "Fillers are arguments of an andor."
		:min 2 :posadjust none :negadjust none)
    (defineSlot threshargs :type Proposition
		:docstring "Fillers are arguments of a thresh."
		:min 2 :posadjust none :negadjust none)
    (defineSlot thnor :type Proposition
		:docstring "Fillers are arguments of a thnor."
		:min 1 :posadjust reduce :negadjust reduce)

    
    ;;(not (if (and A B) (and C D))) 
    ;;implies (not (if A (and C D)))
    ;;but not (not (if (and A B Z) (and C D)),
    ;;therefore neg-adjust for ant is reduce.
    ;;
    ;;(if (and A B) (and C D)) 
    ;;implies (if (and A B Z) (and C D))
    ;;but not (if (and A (and C D))).
    ;;therefore pos-adjust for ant is expand.
    (defineSlot ant :type Proposition :posadjust expand :negadjust
		reduce
		:docstring "antecedent for a set."
		:min 1 :posadjust expand :negadjust reduce)
    
    
    ;;(not (if (and A B) (and C D))) 
    ;;implies (not (if (and A B) (and C D E)))
    ;;but not (not (if (and A B) C)),
    ;;therefore the neg-adj for cq is expand (default).
    ;;
    ;;(if (and A B) (and C D))
    ;;implies (if (and A B) C)
    ;;but not (if (and A B) (and C D E)),
    ;;therefore the pos-adj for cq is reduce (default).
    (defineSlot cq :type Proposition
		:docstring "consequent for a set."
		:min 1 :posadjust reduce :negadjust expand)
    
    ;; Slots for SNeRE
    ;; ===================
    (defineSlot actions :type Action
		:docstring "The actions of an act."
		:min 1 :max 1
		:posadjust none :negadjust none)
    
    ;; Reinitialize Caseframes
    ;; =======================
    (setf cf:*CASEFRAMES* (set:new-set)
	  cf:*FN2CF* (make-hash-table)
	  cf:*NoviceCaseframes* (make-hash-table))
    (defineCaseframe 'Proposition  '('Isa member class)
      :docstring "[member] is a [class]")
    (defineCaseframe 'Proposition '('Equiv equiv)
      :docstring "[equiv] are all co-referential")
    (defineCaseframe 'Proposition '('and and)
      :docstring "it is the case that [and]")
    (defineCaseframe 'Proposition '('nor nor)
      :docstring "it is not the case that [nor]")
    (defineCaseframe 'Proposition '('thnor thnor)
      :docstring "I don't know that it is the case that [thnor]")
    (defineCaseframe 'Proposition '('andor andorargs))
    (defineCaseframe 'Proposition '('thresh threshargs))
    (defineCaseframe 'Proposition '('if ant cq)
      :docstring "if [ant] then [cq]")

    (concatenate 'string 
      (format nil "Knowledge Base cleared.  Contexts reinitialized.")
      (if clearall
	  " Slots and caseframes reinitialized; types remain as they were."
	""))))
;    (if sneps3::*sneps-gui-ptr* (sneps3::clear-graph))) ;;Added by [DRS] 5/22/2010
