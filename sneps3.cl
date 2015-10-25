;;; Top-Level File for Loading SNePS 3
;;; ==================================
;;; Stuart C. Shapiro
;;; Department of Computer Science and Engineering
;;; State University of New York at Buffalo
;;; shapiro@buffalo.edu
;;;
;;; This version will use facilities of ACL.
;;; The intention is to make delivery versions for distribution to non-ACL sites.

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

(in-package :cl-user)

;;; Installation Settable Parameters
;;; ================================

(defvar *sneps-directory* 
    ;; Installer:  Change the path on the following line
    (if (sys:getenv "Sneps3Home") 
	(concatenate 'string (sys:getenv "Sneps3Home") "\\") 
	"C:\\Program Files\\Sneps3\\")
  "Root of the SNePS 3 directory tree")

    ;"/projects/snwiz/Sneps3/"
    ;"./"

;;; Developer Settable Parameters
;;; =============================
(defvar *developing-sneps* nil
  "t means SNePS is being run for development/debugging purposes.
     nil means SNePS is being run in production mode.")

(defvar *ltk-gui*
  (concatenate 'string *sneps-directory* "ltk\\ltk")
  "Startup for LTK (TK toolkit for Lisp)")

;;; lifts restriction on list members printed (default: 10)
;;;    and list depth printed (default: 5)
(setf top-level:*print-length* nil
      top-level:*print-level* nil)

;;; Streams Used
;;; ============
;;; *query-io*         To output a query to the user, and receive a response.
;;; *standard-output*  For output explicitly requested by user.
;;; *error-output*     For output from error and warn.
;;; *trace-output*     For printing traces in inference and other procedures.

;;; To make sure that delivery versions include ACL's Defsystem facility.
(require :defsys)

;;; To use Ltk as the GUI builder.
;;; This line will change when ltk is included in the SNePS 3 repository.
;;; Meanwhile, it will have to be changed for systems running
;;;    on platforms that do not mount /projects.
(load *ltk-gui*)

;;; If Ltk is not being used,
;;; include common-graphics, where available
;;; It's also available on Windows---Add the feature
#+(and linux86 (not ltk)) (require :cg)
#+common-graphics (cg:initialize-cg)

;;; Define the SNePS 3 System
;;; =========================

(let (;; Set the default pathname for defsystem
      (*default-pathname-defaults* (pathname *sneps-directory*)))

  (defsystem :sneps3
      (
       ;; System options
       ;; ==============
       )
    ;; System modules
    ;; ==============
    ;; Short-form until the long-form is needed.
    (:serial (:parallel "sneps3-utils"
			"sets")
	     "substitution"
	     "substitution-set"
	     "sneps3-package"
	     "relations"
	     "caseframes"
	     "syntactic-types"
	     (:parallel
	      "contexts"
	      "semantic-types")
	     "find"
	     "build"
	     "arithmetic"
	     "assert"
	     "snere"
	     "ask"
	     "natural-deduction"
	     "forward"
	     "slot-based"
	     "path-based"
	     "erase"
	     "sneps3-user-package"
	     (:parallel
	      "snepsml" 
	      "rules"
	      "print"
	      "demo")
	     "initialize"
	     ))


  ;; Update the compiled files

  ;; First set the compiler switches
  ;;   to emphasize speed for production
  ;;   but debugging information for development.
  (if *developing-sneps*
      (proclaim '(optimize (safety 1) (space 1) (speed 2) (debug 3)))
    (proclaim '(optimize (safety 1) (space 0) (speed 3) (debug 0))))

  ;; Compile files that need compiling.
  (compile-system :sneps3 :silent (not *developing-sneps*))

  ;; Load SNePS 3
  (load-system :sneps3 :silent (not *developing-sneps*)))

(load (concatenate 'string *sneps-directory* "sneps3-gui"))

;;; Initialize SNePS 3
(in-package :snuser)
(clearkb t)

(print "Change package to snuser.")
