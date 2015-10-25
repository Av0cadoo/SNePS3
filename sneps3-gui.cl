;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;


;;; Having this here will allow for users/developers to create their own custom
;;; load SNePS-GUI files without need to change all of the variables
(defvar *sneps-directory* "C:\\Program Files\\Sneps3")
;(defvar *sneps-directory* "./")

(defvar *sneps-load-file*
    (concatenate 'string *sneps-directory* "sneps3.cl")
  "Location of the SNePS load file.")


(defvar *sneps-gui-directory* 
    (concatenate 'string *sneps-directory* "GUI\\")
  "Location of files for the SNEPS GUI")

(defvar *sneps-gui-jar*
    (concatenate 'string *sneps-gui-directory* "gui.jar")
  "Location of the Sneps3GUI.jar file")

(defvar *jung-directory* 
    (concatenate 'string *sneps-gui-directory* "jung2")
  "Location of the Jung2 files")

(defvar *freehep-directory*
    (concatenate 'string *sneps-gui-directory* "freehep")
  "Location of the freeHEP files")


(defvar *config-file*
    (concatenate 'string *sneps-gui-directory* "sneps_gui_config.lisp"))


(export '(*sneps-gui-directory*))
(export '(*sneps-gui-jar*))
(export '(*jung-directory*))
(export '(*freehep-directory*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Required files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (find-package :mp))
    (require :process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Force SNePS output to the gui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sneps-msg-port* nil
  "Port to send messages over.")
(defparameter *sneps-msg-stream* nil
  "Stream to send messages to.")

(defvar *default-output-stream* 
    nil
  "New variable to add to SNePS. Outunit will be set to this rather than t")

(export '(*sneps-msg-port* *sneps-msg-stream*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load SnePS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add :sneps-gui to the features list so SNePS doesn't load
;;; conflicting features (i.e. the Jung version of the show command
(push :sneps-gui *features*)

(unless (find-package :sneps)
  (load *sneps-load-file*))

(defun start-message-listener ()
  (let ((new-socket (socket:make-socket :connect :passive)))
    (setf *sneps-msg-port* (socket:local-port new-socket))
    (setf *sneps-msg-stream* 
      (socket:accept-connection new-socket))
    (setf *default-output-stream* *sneps-msg-stream*)
;;    (setf sneps:outunit user:*default-output-stream*)
    (format t "Connection made...~%")))
  
(mp:process-run-function "Message listener" #'start-message-listener)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial jlinker and GUI setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :sneps)
;;(export '(load-sneps-text eval-query end generate-tree generate-sneps-graph
;;	  generate-sneps-graph-term-set))
(require :jlinker)
(use-package :javatools.jlinker)

(defun start-jlinker ()
        (if (boundp 'cl-user::*remote-gui*)
            (block remote 
              (jlinker-init :lisp-advertises  :lisp-file nil :lisp-port 4321)
              (sleep 1)
            )
            (block local
              (cl:load (concatenate 'string
                   cl-user:*sneps-gui-directory*
                   "jl-config"))
              (setf (sys:getenv "CLASSPATH")
                (concatenate 
                    'string
                    "."
                    ";" cl-user:*sneps-gui-jar* "\\"
                    ";" cl-user:*sneps-gui-directory* "\\" "multiscroll.jar"
                    ";" cl-user:*jung-directory* "\\" "*.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-export-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphics2d-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-emf-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-java-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-pdf-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-ps-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-svg-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-swf-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-graphicsio-tests-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-io-2.0.2.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-swing-2.0.3.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-util-2.0.2.jar"
                    ";" cl-user:*freehep-directory* "\\" "freehep-xml-2.1.1.jar"
                    ";" cl-user:*freehep-directory* "\\" "jas-plotter-2.2.jar"
                    ";" cl-user:*freehep-directory* "\\" "jdom-1.0.jar"
                    ";" cl-user:*freehep-directory* "\\" "junit-3.8.2.jar"
                    ";" cl-user:*freehep-directory* "\\" "openide-lookup-1.9-patched-1.0.jar"
                    ";" cl-user:*sneps-gui-directory* "\\" "Jlinker" "\\" "jlinker.jar"))

		;;; Open a Lisp => Java connection

                ;;If you want debug support, use this version.
		;(jlinker-init :verbose t :java-args
		;	'((:options "-Xdebug" "-Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n")))

                (jlinker-init :verbose nil)

	))) ;;End jlinker startup


;(print "Before connection...")
;
;(defparameter *java-port* 5630)
;(jlinker-init :mode :java-advertises
;	      :verbose t
;	      :java-file nil
;	      :java-host "128.205.58.123"
;	      :java-port *java-port*)

;(print "After connection...")

;;; Create SNePSGUIConstructor
(def-java-class (sneps-gui "edu.buffalo.cse.sneps3.gui.GUI2")
    () () () ())

;;; Create and show GUI
(def-java-constructor new-sneps-gui (sneps-gui))

;(defparameter *sneps-gui-ptr* (new-sneps-gui))
(defvar *sneps-gui-ptr* nil)


;(def-java-method (set-visible "setVisible") (sneps-gui "boolean"))
;(set-visible *sneps-gui-ptr* t)

;;; Open the connection going the other way
(defparameter *lisp-port* nil
  "Port to connect lisp to")


 (let ((temp-socket  (socket:make-socket :connect :passive)))
   (close temp-socket)
   (setf *lisp-port* (socket:local-port temp-socket)))

(defun start-lisp-server ()
  "Starts a lisp server that the Java class LispConnection will send
  commands to"
    (jlinker-init :lisp-advertises
		  :lisp-file nil
		  :lisp-port *lisp-port*
		  :timeout nil))

;(mp:process-run-function "Lisp connector" #'start-lisp-server)

;;; Open the SNePS message port, used to send text from a SNePS(log)
;;; interaction  to the  Java GUI

(def-java-method (connect-to-lisp "connectToLisp") (sneps-gui "int" "int"))
(format t "lisp-port ~a" *lisp-port*)
(format t "sneps-msg-port ~a" user:*sneps-msg-port*)
;;;(setf cl:*standard-output* user:*sneps-msg-stream*)


;(connect-to-lisp *sneps-gui-ptr* *lisp-port* user:*sneps-msg-port*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create and describe the classes jlinker needs to work with
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Jung Initial Setup Stuff
(def-java-class 
    (directed-sparse-graph "edu.uci.ics.jung.graph.DirectedSparseMultigraph<String, String>")
    () () () ())
(def-java-constructor new-directed-sparse-graph (directed-sparse-graph))

;;(def-java-class (vertex "java.lang.Integer"))
;;(def-java-constructor new-vertex (vertex))

;;(def-java-method (add-vertex  "addVertex") 
;;    (directed-sparse-graph "edu.uci.ics.jung.graph.Vertex"))
;;(def-java-method (add-edge  "addEdge") 
;;    (directed-sparse-graph "edu.uci.ics.jung.graph.Edge"))

(def-java-method (add-vertex  "addVertex") 
    (directed-sparse-graph "java.lang.String" "java.lang.String"))

;;;From the SNePS2 Interface
(def-java-class (array-list "java.util.ArrayList")
    () () () ())
(def-java-constructor new-array-list
    (array-list "int"))
(def-java-method (add-to-array "add") (array-list "java.lang.Object")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alist-from-list (l)
  (setf alist (new-array-list 10))
    (loop for item in l
      do (add-to-array alist (string item)))
  alist)


;(defun cf-for-term (term)
;  (set:loopset for cf in cf:*CASEFRAMES*
;    do (set:loopset for tm in (cf::get-caseframe-terms cf)
;      do (if (eq (sneps3::name term) (sneps3::name tm)) (return-from cf-for-term (string (cf::caseframe-name cf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Java classes for some of the SNePS constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;These are new for SNePS3
(in-package :sneps)

(setf *auto-refresh-graph* t)
(setf *auto-relayout-graph* t)

(def-java-method (disable-graph-refresh-int "disableGraphRefresh") (sneps-gui))
(def-java-method (enable-graph-refresh-int "enableGraphRefresh") (sneps-gui))

(def-java-method (reinit-gui "reinitialize") (sneps-gui "boolean"))

(defun disable-graph-refresh ()
  (disable-graph-refresh-int *sneps-gui-ptr*)
)

(defun enable-graph-refresh ()
  (enable-graph-refresh-int *sneps-gui-ptr*)
)

(def-java-method (disable-graph-relayout-int "disableGraphRelayout") (sneps-gui))
(def-java-method (enable-graph-relayout-int "enableGraphRelayout") (sneps-gui))

(defun disable-graph-relayout ()
  (disable-graph-relayout-int *sneps-gui-ptr*)
)

(defun enable-graph-relayout ()
  (enable-graph-relayout-int *sneps-gui-ptr*)
)

;;Representing caseframes
(def-java-class (jcaseframe "edu.buffalo.cse.sneps3.gui.Caseframe")
    () () () ())

;;new Caseframe(String name, String type)
(def-java-constructor new-jcaseframe
    (jcaseframe "java.lang.String" "java.lang.String" "java.util.ArrayList" "java.util.ArrayList"))

;;Add a slot to the caseframe
(def-java-method (add-slot-to-jcf "addSlot") (jcaseframe "edu.buffalo.cse.sneps3.gui.Slot"))

;;Send a caseframe to the GUI
(def-java-method (send-caseframe "addCaseframe") (sneps-gui "edu.buffalo.cse.sneps3.gui.Caseframe"))


;;Representing slots
(def-java-class (jslot "edu.buffalo.cse.sneps3.gui.Slot")
    () () () ())

;;new Slot(String name, String type, String posadjust, String negadjust, int min, int max)
(def-java-constructor new-jslot
    (jslot "java.lang.String" "java.lang.String" "java.lang.String" "java.lang.String" "int" "int"))

;;Send the new slot to the GUI
(def-java-method (send-slot "addSlot") (sneps-gui "edu.buffalo.cse.sneps3.gui.Slot")) 


;;Representing semantic types
(def-java-method (send-type "addSemanticType") (sneps-gui "java.lang.String" "java.util.ArrayList"))


(defun repl-output (str)
  (format nil "~a" (javatools.jlinker:eval-from-string str)))

;(defun alist-slots ()
;  (setf slot-arr (new-array-list 10))
;  (loop for r being each hash-value of slot:*SLOTS*
;      do (add-to-array slot-arr (slot::slot-name r))))

;(defun alist-caseframes ()
;  (setf frame-arr (new-array-list 10))
;  (set:loopset for cf in cf:*CASEFRAMES*
;               do (add-to-array (cf::caseframe-name cf))))

;;;Sending the slots to the GUI
(defun send-one-slot (s)
  (setf max (slot::slot-max s))
  (if (eq max nil) (setf max -1))
  (setf js (new-jslot (string (slot::slot-name s)) (string (class-name (slot::slot-type s))) (string (slot::slot-posadjust s))
			(string (slot::slot-negadjust s)) (slot::slot-min s) max))
  (send-slot *sneps-gui-ptr* js))

(defun send-all-slots ()
  (loop for r being each hash-value of slot:*SLOTS*
    do (send-one-slot r)))

;;;Sending the caseframes to the GUI
(defun alist-slot-names (cframe)
  (setf sname-arr (new-array-list 10))
  (loop for i from 0 below (length (cf::caseframe-slots cframe))
    do (add-to-array sname-arr (string (slot::slot-name (aref (cf::caseframe-slots cframe) i)))))
  sname-arr)

(defun send-one-caseframe (cframe &optional fsyms)
  (when fsyms
    (let ((alist (new-array-list (length fsyms))))
      (loop for i in fsyms
        do (typecase i
              (atom (add-to-array alist (string i)))
              (list (add-to-array alist (concatenate 'string "(" (string (first i)) ")")))
              (t (add-to-array alist (string i)))))
      (let ((jcf (new-jcaseframe (cf::caseframe-name cframe) (string (slot-value (cf::caseframe-type cframe) 'excl::name)) (alist-slot-names cframe) alist)))
        (send-caseframe *sneps-gui-ptr* jcf)
        (return-from send-one-caseframe jcf))))
  (let ((jcf (new-jcaseframe (cf::caseframe-name cframe) (string (slot-value (cf::caseframe-type cframe) 'excl::name)) (alist-slot-names cframe) nil)))
    (send-caseframe *sneps-gui-ptr* jcf)))


(defun send-all-caseframes ()
  (set:loopset for cf in cf:*CASEFRAMES*
    do (send-one-caseframe cf)))



;;;Array list of all the semantic types.
(defun alist-types ()
  (setf context-arr (new-array-list 10))
  (loop for type in  (subtypes *TopSemanticType*)
      do (string (slot-value type 'excl::name)))
  context-arr)

(defun send-one-type (type)
  (send-type *sneps-gui-ptr* (string (slot-value type 'excl::name)) 
    (alist-from-list (mapcar #'(lambda (x) (slot-value x 'excl::name))
      (let ((parents (slot-value type 'excl::direct-superclasses)))
    	(if parents
          parents))))))

(defun send-all-types ()
  (loop for type in  (subtypes *TopSemanticType*)
    do (send-one-type type)))

(def-java-method (startup "startup") (sneps-gui))

(def-java-method (send-updated-types "updateTypes") (sneps-gui "java.util.ArrayList"))

;;Representing contexts
(def-java-class (jcontext "edu.buffalo.cse.sneps3.gui.Context")
    () () () ())

(def-java-method (send-context "addContext") (sneps-gui "java.lang.String" "java.util.ArrayList"))

(def-java-method (send-current-context-name "selectCurrentContext") (sneps-gui "java.lang.String"))

;;new Context(String name, ArrayList parents)
(def-java-constructor new-jcontext
    (jcontext "java.lang.String" "java.util.ArrayList"))

(defun ctx-parent-alist (l)
  (setf alist (new-array-list 10))
    (loop for item in l
      do (add-to-array alist (string (ct::ct-name (ct::find-context item)))))
  alist)

;;Reverse the list order so we send in proper order so we ensure we have the parents already!
(defun send-all-contexts ()
  (setf revlist nil)
  (loop for ctx being the hash-key of ct:*CONTEXTS*
      do (setf revlist (cons (ct::find-context ctx) revlist)))
  (loop for ctx in revlist
      do (send-one-context ctx)))

(defun send-one-context (ctx)
  ;(format t "Sending ~a ~%" ctx)
  (setf ctxt (ct::find-context ctx))
  (send-context *sneps-gui-ptr* (string (ct::ct-name ctxt)) (ctx-parent-alist (ct::ct-parents ctxt))))

(defun gui-select-current-context ()
  (send-current-context-name *sneps-gui-ptr* (string (ct::ct-name (ct::currentContext)))))

(def-java-method (send-updated-contexts "updateContexts") (sneps-gui "java.util.ArrayList"))
;;To use: (alist-contexts) (send-updated-contexts *sneps-gui-ptr* context-arr)

(defun alist-contexts ()
  (setf context-arr (new-array-list 10))
  (loop for ctx being the hash-key of ct:*CONTEXTS*
    do (add-to-array context-arr (string (ct::ct-name (ct::find-context ctx)))))
  context-arr)

;;;Find graph:
(def-java-method (send-find-results "findGraphResults") (sneps-gui "java.util.ArrayList"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Graphing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;These hack around jlinker not supporting java generics.
(def-java-method (create-new-graph "createNewGraph") (sneps-gui))
(def-java-method (add-vertex "addCustomVertex") 
    (sneps-gui
     "java.lang.String"
     "java.lang.String"
     "java.lang.String"
     "java.util.ArrayList"
     "boolean"
     "double"))
(def-java-method (add-edge "addStringEdge") (sneps-gui "java.lang.String" "java.lang.String" "java.lang.String"))
(def-java-method (add-arb-res-edge "addArbRestrictionEdge") (sneps-gui "java.lang.String" "java.lang.String"))
(def-java-method (add-ind-res-edge "addIndRestrictionEdge") (sneps-gui "java.lang.String" "java.lang.String"))
(def-java-method (add-dep-res-edge "addDepRestrictionEdge") (sneps-gui "java.lang.String" "java.lang.String"))
(def-java-method (display-graph "displayGraph") (sneps-gui))
(def-java-method (set-add-mode "setAddMode") (sneps-gui "boolean"))
(def-java-method (set-show-new-assertions "setShowNewAssertions") (sneps-gui "boolean"))
(def-java-method (set-scale-level "setScaleLevel") (sneps-gui "int"))
(def-java-method (set-hide-find-mode "setHideFindMode") (sneps-gui "boolean"))
(def-java-method (display-nodes "displayNodes") (sneps-gui "java.util.ArrayList"))
(def-java-method (gui-unassert "unassert") (sneps-gui "java.lang.String"))


;;Function to set the scale of the graph
(defun set-graph-scale (amt)
  (set-scale-level *sneps-gui-ptr* amt))

(defun enable-hide-find-mode (b)
  (set-hide-find-mode *sneps-gui-ptr* b))

(defun generate-graph (&optional term-set)
  ;(create-new-graph *sneps-gui-ptr*)
  (update-graph term-set)
  (display-graph *sneps-gui-ptr*))

(defun clear-graph ()
  (create-new-graph *sneps-gui-ptr*)
  (display-graph *sneps-gui-ptr*))

;;This attempts to add the nodes and edges. On the java end they are tested for existance.
(defun update-graph (&optional term-set)
  (cond 
   ((typep term-set 'set:set) 
    (set:loopset for term in term-set
	do (add-vertex *sneps-gui-ptr* (string (sneps3::name term)) (string (type-of term))
                       (if (typep term 'sneps3:molecular) (string (cf::caseframe-name (sneps3::caseframe term))) nil)
                       (if (typep term 'sneps3:molecular) (alist-slot-names (sneps3::caseframe term)) nil)
		       (ct::assertedp term (ct::currentContext))
		       (activation-value term)))
    (set:loopset for term in term-set
	do (loop for relation being the hash-key using (hash-value node) of (util:resource-value (sneps3::up-cableset term))
	       do (loop for e being each hash-key of (sneps3-set::set-table node)
						  
		      do (when (and (set:member e term-set) 
				    (set:member term term-set))    
			  (add-edge *sneps-gui-ptr* (string (sneps3::name e)) (string (sneps3::name term)) 
				    (string (slot::slot-name relation))))))))
   (t 
    (loop for term being each hash-value of (util:resource-value sneps3:*TERMS*)
	do (add-vertex *sneps-gui-ptr* (string (sneps3::name term)) (string (type-of term)) 
                       (if (typep term 'sneps3:molecular) (string (cf::caseframe-name (sneps3::caseframe term))) nil)
                       (if (typep term 'sneps3:molecular) (alist-slot-names (sneps3::caseframe term)) nil)
                       (ct::assertedp term (ct::currentContext))
		       (activation-value term))) ;;added typeof term, cf.
    (loop for term being each hash-value of (util:resource-value sneps3:*TERMS*)
	do (loop for relation being the hash-key using (hash-value node) of (util:resource-value (sneps3::up-cableset term))
	       do (loop for e being each hash-key of (sneps3-set::set-table node)
		      do (block add
			   ;;Build the arcs, including "every", "depends" and "some".
			   (if (and (typep term 'arbitrary) (set::member e (restriction-set term))) 
  				(add-arb-res-edge *sneps-gui-ptr* (string (sneps3::name term)) (string (sneps3::name e))))
			   (if (and (typep term 'indefinite) (set::member e (restriction-set term))) 
  				(add-ind-res-edge *sneps-gui-ptr* (string (sneps3::name term)) (string (sneps3::name e))))
			   ;;Depends arcs:
			   (if (and (typep term 'indefinite) (set::member e (restriction-set term)))
                                (set:loopset for dep in (dependencies term)
				     do (add-dep-res-edge *sneps-gui-ptr* (string (sneps3::name term)) (string (sneps3::name dep)))))
			   ;;Normal arcs:
			   (add-edge *sneps-gui-ptr* (string (sneps3::name e)) (string (sneps3::name term)) 
				   (string (slot::slot-name relation))))))))))
  

(defun add-asserted-wft-to-graph (term)
  "Adds a wft which has been added to the KB to the GUI graph."
  ;;Add the nodes
  (if *auto-relayout-graph* (set-add-mode *sneps-gui-ptr* t))
  (dolist (e (terms-to-update term))
    (add-vertex *sneps-gui-ptr* (string (sneps3::name e)) (string (type-of e)) 
                        (if (typep e 'sneps3:molecular) (string (cf::caseframe-name (sneps3::caseframe e))) nil)
                        (if (typep e 'sneps3:molecular) (alist-slot-names (sneps3::caseframe e)) nil)
                        (ct::assertedp e (ct::currentContext)) (activation-value e)))
  ;;Add the edges
  (dolist (e (terms-to-update term))
    (block 'a
	;;Figure out the relations.
        (loop for relation being the hash-key using (hash-value node) of (util:resource-value (sneps3::up-cableset e))
          do (loop for e1 being each hash-key of (sneps3-set::set-table node)
            do (block add 
              (add-edge *sneps-gui-ptr* (string (sneps3::name e1)) (string (sneps3::name e)) (string (slot::slot-name relation)))
              (if (and (typep e 'arbitrary) (set::member e1 (restriction-set e))) 
  	           (add-arb-res-edge *sneps-gui-ptr* (string (sneps3::name e)) (string (sneps3::name e1))))
	      (if (and (typep e 'indefinite) (set::member e1 (restriction-set e))) 
  		   (add-ind-res-edge *sneps-gui-ptr* (string (sneps3::name e)) (string (sneps3::name e1))))
              ;;Depends arcs
	      (if (and (typep e 'indefinite) (set::member e1 (restriction-set e)))
                   (set:loopset for dep in (dependencies e)
			do (add-dep-res-edge *sneps-gui-ptr* (string (sneps3::name e)) (string (sneps3::name dep))))))))))
  (if *auto-relayout-graph* (set-add-mode *sneps-gui-ptr* nil)))

(defun terms-to-update (term &aux l)
  "Returns a list of terms containing the term given and any terms found by
    recursing it's down cableset."
  (setf l (append (list term) l))
  (if (typep term 'molecular)
    (loop for x across (sneps3::down-cableset term)
      do (loop for e being each hash-key of (sneps3-set::set-table x) ;;I think this is always a singleton set.
        do (block 'a 
             (if (not (member e l)) (setf l (append (terms-to-update e) l)))))))
  (if (or (typep term 'arbitrary) (typep term 'indefinite))
    (set::loopset for res in (restriction-set term)
      do (block add 
        (setf l (append (list res) l))
        (loop for x across (sneps3::down-cableset res)
   	  do (loop for e being each hash-key of (sneps3-set::set-table x)
    	    do (if (not (member e l)) (setf l (append (terms-to-update e) l))))))))
  l)

;;;Query example: (perform-search '(Isa x GreatLake) '(x))
(defun perform-search (expr var)
  (setf search-term-set (car (multiple-value-list (sneps::find expr var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send initial state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro startGUI (&rest terms)
  "Starts the SNePS 3 GUI. Takes a variable number of terms to display on the
    graph. The term is either found or defined using defineTerm. If no terms are
      given, the entire graph will be displayed."
  `(progn
    (if *sneps-gui-ptr*
      (format t "~S~%" "The GUI is already running!")
      (block nil
        ;;Start up the GUI and connect it to the port where SNePS messages are sent.
        (start-jlinker)
        (setf *sneps-gui-ptr* (new-sneps-gui))
        (mp:process-run-function "Lisp connector" #'start-lisp-server)
        (connect-to-lisp *sneps-gui-ptr* *lisp-port* user:*sneps-msg-port*)

        ;;Send initial values to the GUI.
        (startup *sneps-gui-ptr*)
        (create-new-graph *sneps-gui-ptr*)
        (display-graph *sneps-gui-ptr*)
        (send-all-contexts)
        (send-all-types)
        (send-all-slots)
        (send-all-caseframes)
        (gui-select-current-context)

        ;;We always send the entire graph to the GUI, but only the part which is requested
        ;;Is shown to the user.
        (if ',terms
          (set-show-new-assertions *sneps-gui-ptr* nil))
        (generate-graph)
        (if ',terms
          (set-show-new-assertions *sneps-gui-ptr* t))

        ;;Now we make the GUI show the terms we want
        (if ',terms
          (loop for x in ',terms
            do (snuser::defineTerm x 'Entity)))))))

;;;Additional snuser code:

(in-package snuser)

(defun graph-find (find-expr)
  "This function takes as an argument a form which when evalulated calls the
    SNePS 3 'find' function. The results of this function are sent to the
      GUI to be displayed on the graph."
  (setf s (eval find-expr)) ;;;This is the set of results
  (setf l '())
  (set:loopset for term in s
    do (block eval
      (setf l (cons (string (sneps3::name term)) l))
      (loop for x across (sneps3::down-cableset term)
        do (setf l (cons (string (sneps3::name (first (set::set-to-list x)))) l)))))
  (sneps::send-find-results sneps::*sneps-gui-ptr* (sneps::alist-from-list (delete-duplicates l))))
