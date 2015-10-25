;;; SNePS 3: Arithmetic
;;; ===================
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

(defun box (n)
  "Returns a term whose name looks like n."
  (build n 'Entity))

(defun unbox (term)
  "If term is a number, return it;
   if term's name looks like a number, return the number;
   else throw an error."
  (if (numberp term)
      term
    (let ((n (ignore-errors (read-from-string (symbol-name (name term))))))
      (if (numberp n)
	  n
	(error "~S does not look like a number." term)))))

(defun .+. (&rest numbs)
  "Returns a term whose name looks like the sum of the numbs,
      which can be boxed or unboxed numbers."
  (box (apply #'+ (mapcar #'unbox numbs))))

(defun .-. (&rest numbs)
  "Returns a term whose name looks like the difference of the numbs,
      which can be boxed or unboxed numbers."
  (box (apply #'- (mapcar #'unbox numbs))))

(defun .*. (&rest numbs)
  "Returns a term whose name looks like the product of the numbs,
      which can be boxed or unboxed numbers."
  (box (apply #'* (mapcar #'unbox numbs))))

(defun ./. (&rest numbs)
  "Returns a term whose name looks like the quotient of the numbs,
      which can be boxed or unboxed numbers."
  (box (apply #'/ (mapcar #'unbox numbs))))

(defun .<. (&rest numbs)
  "Returns t if each num is less than the next,
     nil otherwise."
  (apply #'< (mapcar #'unbox numbs)))

(defun .<=. (&rest numbs)
  "Returns t if each num is less than or equal to the next,
     nil otherwise."
  (apply #'<= (mapcar #'unbox numbs)))

(defun .>. (&rest numbs)
  "Returns t if each num is greater than the next,
     nil otherwise."
  (apply #'> (mapcar #'unbox numbs)))

(defun .>=. (&rest numbs)
  "Returns t if each num is greater than or equal to the next,
     nil otherwise."
  (apply #'>= (mapcar #'unbox numbs)))

(defun .=. (&rest numbs)
  "Returns t if each num is equal to the next,
     nil otherwise."
  (apply #'= (mapcar #'unbox numbs)))

(defun ./=. (&rest numbs)
  "Returns t if none of the numbs are equal,
     nil otherwise."
  (apply #'/= (mapcar #'unbox numbs)))
