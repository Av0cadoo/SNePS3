;;; SNePS 3: Definition of the sneps3 package
;;; =========================================
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

(defpackage :sneps3
  (:nicknames :sneps)
  (:shadow cl:atom cl:variable cl:find)
  (:export
   ;; Metaclasses

   ;; Semantic Types
   #:Entity #:Proposition #:Act #:Policy #:Thing
    #:Action #:Category
   
   ;; Meta-Types
   #:semantic-type

   ;; Syntactic Types
   #:term #:atom #:arbitrary #:indefinite #:molecular #:categorization 
   #:negation #:negationbyfailure #:conjunction #:disjunction #:implication
   #:equivalence #:negatedconjunction #:numericalentailment
   
   ;; Syntactic Types accessors
   #:contexts
   
   ;; Reserved words
   #:Isa #:nor #:nand #:andor #:thresh #:xor #:iff #:thnot #:thnor

   ;; Slot names
   #:andorargs #:threshargs
   
   ;; Constant Propositions
   #:T #:F

   ;; Globals
   #:*ARBITRARIES*
   #:*INDEFINITES*
   #:*KRNovice*
   #:*PRECISION*
   #:*TERMS*

   ;; Methods
   #:caseframe #:counter #:description #:erase-term #:semantic-type-of
   #:minparam #:maxparam #:syntactic-type-of #:totparam
   
   ;; Functions
   #:box #:unbox
   #:.+. #:.-. #:.*. #:./. #:.<. #:.<=. #:.>. #:.>=. #:.=. #:./=.
   #:clearkb #:find-term #:findfrom #:findto #:listkb #:list-terms
   #:make-caseframe #:name
   #:primaction #:print-unnamed-molecular-term
   #:qVarp
   #:restriction-set
   #:semantic-type-p #:showTypes
   #:startGUI #:subtypes #:supertypes
   #:writeKBToTextFile
   
   ;; Generic Functions
   #:build #:print-named-and-return #:check-and-build-variables
   
   ;; Macros
   #:defineType
   )

  (:import-from :sneps3-set
	       #:setof))

(in-package :sneps3)

;;; Global Variables
;;; ================
(defconstant *INITIAL-SEMANTIC-TYPE-NAMES*
    '(Entity Proposition Act Policy Thing Category)
  "List of semantic types when SNePS is initialized.")

(defparameter *SEMANTIC-TYPES* '()
  "Assoc list of (semantic-type-name . semantic-type).") 

(defparameter *PRINTED-VARIABLES* nil
  "A hashtable of variable nodes that have been printed.")

(defparameter *sneps-gui-ptr* nil)
