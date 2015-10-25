(defpackage :snepsml
  (:documentation "SnepsML is an XML dialect used for saving, loading, and
                   importing SNePS knowledge bases.")
;; ***TODO*** import: separate context?
;; ***TODO*** support OWL
  (:nicknames :snepsml)
  (:export
   #:importkb
   #:loadkb
   #:print-xml-object
   #:savekb
   #:*SNEPS-SAVE*
   #:write-with-xml-tag
   ))

(in-package :snepsml)

;; Is this CL or Allegro-specific?
(require :sax)
(use-package :net.xml.sax)

(defparameter *SNEPS-SAVE* nil)

(defun savekb (kbname &optional (format 'xml))
  "SNePS Knowledge Base dump into file.
   The named knowledge base determines the file name
   (ex: mykb saved as XML (the default) saves into file mykb.snepsml)
   Supported formats:
   xml: Save to SnepsML, an XML dialect"
  (ecase format
    (xml (save-snepsml kbname)))
  t)

(defun loadkb (filename)
  "Clears the current SNePS KB and loads a new KB from the file.
   Supported formats:
   xml: Read from a SnepsML file"
   (importkb filename))

(defun importkb (filename)
  "Loads a KB into a current KB from the file.
   Supported formats:
   xml: Read from a SnepsML file"
  (sax-parse-file filename :class 'snepsml-sax-parser)
  t)

(defgeneric print-xml-object (xmlobj stream)
  (:documentation "Print the object's XML representation to stream."))

(defun unpack-xml-attribs (attribs)
  "Expects an association list of attrib-value pairs.  Returns in XML format.
   Ex: ((\"name\" . \"mike\") (\"age\" . \"28\")) --> name=\"mike\" age=\"28\"
   Always returns a string, even if empty."
  (if (consp attribs)
        (format nil " ~A=\"~A\"~A"
                (car (first attribs))
                (cdr (first attribs))
                (unpack-xml-attribs (rest attribs)))
        ""))

(defun build-xml-open-tag (tagname attribs)
  "Builds an XML tag from tagname (e.g. tagname attrib='value')"
  (format nil "~A~A" tagname (unpack-xml-attribs attribs)))

(defmacro write-with-xml-tag (stream tagname attribs &body forms)
  (let ((opentag (gensym)))
    (if forms
        `(let ((,opentag (build-xml-open-tag ,tagname ,attribs)))
          (format ,stream "<~A>" ,opentag)
          ,@forms
          (format ,stream "</~A>~%" ,tagname))
        `(let ((,opentag (build-xml-open-tag ,tagname ,attribs)))
          (format ,stream "<~A />~%" ,opentag)))))

;;; here we save everything:
;;; caseframes
;;; relations/slots
;;; terms
;;; semantic types
;;; contexts
;;; what to do about defaults?
(defun save-snepsml (kbname)
  "Saves KB to a SnepsML file."
  (with-open-file (stream (concatenate 'string kbname ".snepsml")
                          :direction :output
                          :if-exists :supersede)
    (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" stream)
    (write-with-xml-tag stream "sneps-kb"
        `(("xmlns" . "http://www.cse.buffalo.edu/sneps")
          ("name" . ,kbname))
      (save-semantic-types stream)
      (save-slots stream)
      (save-caseframes stream))))

;;; Put these in the packages where they belong?

;;; WARNING: uses ACL-specific features (excl::name)
(defun save-semantic-types (stream)
  (snepsml:write-with-xml-tag stream "types" nil
    (loop for type in (remove-if
                       #'(lambda (x)
                           (member (slot-value x 'excl::name)
                                   '(sneps:Entity sneps:Thing sneps:Action
                                     sneps:Category sneps:Policy sneps:Act
                                     sneps:Proposition)))
                       (sneps:subtypes sneps::*TopSemanticType*))
          do (snepsml:print-xml-object type stream))))

;;; WARNING: uses ACL-specific features (excl::name)
(defmethod snepsml:print-xml-object ((type standard-class) stream)
  "Prints the XML representation of a semantic type"
  (let ((name (slot-value type 'excl::name)))
    (snepsml:write-with-xml-tag stream "type" `(("id" . ,name))
      (loop for stype in (sneps:supertypes type)
            do (snepsml:write-with-xml-tag stream "supertype"
                   `(("idref" . ,(slot-value stype 'excl::name))))))))

(defun save-slots (stream)
  (snepsml:write-with-xml-tag stream "slots" nil
    (loop for r being each hash-value of slot:*SLOTS*
          do (snepsml:print-xml-object r stream))))

(defmethod snepsml:print-xml-object ((r slot:slot) stream)
  "Prints the XML representation of a slot"
  (let ((name (symbol-name (slot:slot-name r))))
    (snepsml:write-with-xml-tag stream "slot" `(("id" . ,name))
      (snepsml:write-with-xml-tag stream "type" nil
        (write-string (symbol-name (class-name (slot:slot-type r))) stream))
      (snepsml:write-with-xml-tag stream "docstring" nil
        (write-string (slot:slot-docstring r) stream))
      (snepsml:write-with-xml-tag stream "posadjust" nil
        (write-string (symbol-name (slot:slot-posadjust r)) stream))
      (snepsml:write-with-xml-tag stream "negadjust" nil
        (write-string (symbol-name (slot:slot-negadjust r)) stream))
      (snepsml:write-with-xml-tag stream "min" `(("val" . ,(slot:slot-min r))))
      (snepsml:write-with-xml-tag stream "max" `(("val" . ,(slot:slot-max r))))
      (snepsml:write-with-xml-tag stream "path" nil
        (print-object (slot:slot-path r) stream)))))
      
(defun save-caseframes (stream)
  (snepsml:write-with-xml-tag stream "caseframes" nil
    (set:loopset for cf in cf:*CASEFRAMES*
      do
      (snepsml:print-xml-object cf stream))))

(defmethod snepsml:print-xml-object ((cf cf:caseframe) stream)
  "Prints the XML representation of a caseframe"
  (let ((id (cf:caseframe-name cf)))
    (snepsml:write-with-xml-tag stream "caseframe" `(("id" . ,id))
      (snepsml:write-with-xml-tag stream "type" nil
        (write-string (symbol-name (class-name (cf:caseframe-type cf)))
                      stream))
      (snepsml:write-with-xml-tag stream "docstring" nil
        (write-string (cf:caseframe-docstring cf) stream))
      (snepsml:write-with-xml-tag stream "slots" nil
        (loop for i from 0 below (length (cf:caseframe-slots cf))
              do (snepsml:write-with-xml-tag stream "slot"
                     `(("idref" . 
                        ,(symbol-name (slot:slot-name
                                       (elt (cf:caseframe-slots cf) i))))))))
      (snepsml:write-with-xml-tag stream "print-pattern" nil
        (print-object (cf:caseframe-print-pattern cf) stream))
      (when (cf:caseframe-adj-to cf)
        (set:loopset for tocf in (cf:caseframe-adj-to cf)
          do (let ((idref (cf:caseframe-name tocf)))
               (snepsml:write-with-xml-tag stream "adj-to"
                   `(("idref" . ,idref))))))
      (when (cf:caseframe-adj-from cf)
        (set:loopset for fromcf in (cf:caseframe-adj-from cf)
          do (let ((idref (cf:caseframe-name fromcf)))
               (snepsml:write-with-xml-tag stream "adj-from"
                   `(("idref" . ,idref))))))
      (snepsml:write-with-xml-tag stream "terms" nil
        (print-object (cf:get-caseframe-terms cf) stream)))))

;;; Read in a SnepsML file using the SAX processor

;;; Saves state information during the parse
(defclass snepsml-sax-parser (sax-parser)
  ((kbname :accessor kbname)
   (object :accessor current-object)
   (type :accessor current-type)))

(defmacro aval (key lst)
  "Convenience macro for getting the value from the key in an assoc list.  ~
   Test using #'equal."
  `(cdr (assoc ,key ,lst :test #'equal)))

(defun handleType (parser attrs)
  (let ((newobj `((name . ,(aval "id" attrs))
                  (supertypes . ,(make-list 0)))))
    (setf (current-type parser) 'type
          (current-object parser) newobj)))

(defun handleSupertype (parser attrs)
  (let ((superlist (assoc 'supertypes (current-object parser)))
        (idref (aval "idref" attrs)))
    (push idref (cdr superlist))))

(defmethod start-document ((parser snepsml-sax-parser))
  (setf (current-type parser) 'snepskb
        (current-object parser) nil
        (kbname parser) nil))

;;; Implemented with state info, based on knowledge of layout of XML file.
;;; Could be more cleanly implemented with DTD?
(defmethod start-element ((parser snepsml-sax-parser)
                          iri localname qname attrs)
  (declare (ignore iri localname))
  (case (current-type parser)
    (snepskb (cond ((equal qname "sneps-kb")
                    (setf (kbname parser) (aval "name" attrs))
                    (format t "Importing KB: ~A~%" (kbname parser)))
                   ((equal qname "types")
                    (setf (current-type parser) 'types))))
    (types (cond ((equal qname "type") (handleType parser attrs))))
    (type (cond ((equal qname "supertype") (handleSupertype parser attrs))))))

(defmethod end-element ((parser snepsml-sax-parser) iri localname qname)
  (declare (ignore iri localname))
  (case (current-type parser)
    (types (cond ((equal qname "types") (setf (current-type parser)
                                              'snepskb))))
    (type (cond ((equal qname "type")
                 (let ((obj (current-object parser)))
                   (format t "Creating semantic type: ~A~%" obj)
                   ;; defineType not yet working!
                   (sneps:defineType (aval 'name obj) (aval 'supertypes obj)))
                 (setf (current-type parser) 'types))))))

;; (defmethod content ((parser snepsml-sax-parser) content start end ignorable))

;; (defmethod end-document ((parser snepsml-sax-parser)))
