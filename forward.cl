;;; SNePS 3: Natural Deduction Forward Inference
;;; ============================================
;;; Zhaomo Yang
;;; Department of Computer Science and Engineering
;;; State University of New York at Buffalo
;;; zhaomoya@buffalo.edu

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

(in-package :snip)

(defun inferByForward (term context resultstack)
   (push term resultstack)
    
   (setf resultstack (inferByForwardUsingNegation term context resultstack))
   
   (setf resultstack (inferByForwardUsingConjunction term context resultstack))
   (setf resultstack (inferByForwardUsingConjunct term context resultstack)) 

   (setf resultstack (inferByForwardUsingAndorarg term context resultstack))
   (setf resultstack (inferByForwardUsingAndor term context resultstack))
   
   (setf resultstack (inferByForwardUsingThresharg term context resultstack))
   (setf resultstack (inferByForwardUsingThresh term context resultstack))

   (setf resultstack (inferByForwardUsingAntecedent term context resultstack))
   (setf resultstack (inferByForwardUsingImplication term context resultstack))

   resultstack)


(defun inferByForwardUsingNegation (term context resultstack)
   "This function is used to do inference via slot labelled 'nor'" 
   (let ((downnegs (sneps:findto term 'nor)))
      (if (not (or (eq downnegs nil) (set:emptyp downnegs))) 
         (set:loopset for downneg in downnegs 
            do (setf resultstack (inferByForwardUsingNegationDown downneg term context resultstack))
            do (setf resultstack (inferByForwardUsingNegationUp downneg context resultstack)))))

   (let ((upnegs (sneps:findfrom term 'nor)))
      (if (or (eq upnegs nil) (set:emptyp upnegs)) (return-from inferByForwardUsingNegation resultstack)
         (set:loopset for upneg in upnegs 
            do (setf resultstack (inferByForwardUsingNegationUp upneg context resultstack)))))     
   
   resultstack)


(defun inferByForwardUsingNegationDown (term assertedterm context resultstack)
   "This function is used to infer downwardly via slots labeled 'nor'.
    'assertedterm' is just asserted, thus term cannot be asserted because there is a slot labelled 'nor' connecting 'term' and 'assertedterm'.
    So if A is a filler of slot 'nor' which is from 'term' and all the other fillers cannot be asserted, then A should be inferred."

   (let ((nextnegs (sneps:findto term 'nor)))
      (if (or (eq nextnegs nil) (set:emptyp nextnegs)) (return-from inferByForwardUsingNegationDown resultstack)
         (let ((threshold (- (set:cardinality nextnegs) 1))
               (num 0)
               (reasonlist nil)
               (reason nil)
               (newasserted nil))
               (set:loopset for nextneg in nextnegs
                  do (setf reason (assertedOrNot nextneg context))
                  always (not (eq reason nextneg))
                           
                  if (not (eq nil reason))
                    do (incf num)
                    and do (push reason reasonlist)
                  else
                    do (setf newasserted nextneg))
               (if (and (eq threshold num) (not (eq newasserted nil)))
                  (progn
                     (push assertedterm reasonlist) 
                     (assertTrace nil reasonlist newasserted "Forward chaining" context)        
                     (setf resultstack (inferByForward newasserted context resultstack)))))))
   resultstack)

(defun inferByForwardUsingNegationUp (term context resultstack)
   "This function is used to infer upwardly via slots labeled 'nor'.
    If 'term' is just asserted, and 'term' is a filler of slot 'nor' which is from A, obviously A cannot be asserted.
    So if A is a filler of slot 'nor' which is from B and all the other fillers of B cannot be asserted, then B should be inferred."

   (let ((nextnegs (sneps:findfrom term 'nor)))
      (if (or (eq nextnegs nil) (set:emptyp nextnegs)) (return-from inferByForwardUsingNegationUp resultstack)
         (let ((status nil))
            (set:loopset for nextneg in nextnegs
               do (setf status (assertedOrNot nextneg context))
               if (not (eq status nextneg))   ; If 'nextneg' is not  asserted
                 if (not (eq status nil))  ; If 'nextneg' cannot be asserted
                   do (setf resultstack (inferByForwardUsingNegationDown nextneg status context resultstack))
                 else  ; If 'nextneg' can be asserted 
                   do (let* ((fillers (sneps:findto nextneg 'nor))
                             (threshold (set:cardinality fillers))
                             (num 0)
                             (reasonlist nil)
                             (reason nil)) 
                            (set:loopset for filler in fillers
                               do (setf reason (assertedOrNot filler context)) 
                               always (and (not (eq reason filler)) (not (eq reason nil))) ; If filler is asserted or filler can be asserted, break                         
                               do (push reason reasonlist)
                               do (incf num)) 
                            (if (eq threshold num)
                               (progn 
                                  (assertTrace nil reasonlist nextneg "Forward chaining" context)        
                                  (setf resultstack (inferByForward nextneg context resultstack)))))
                 end
               end))))
   resultstack)


(defun assertedOrNot (term context)
   "This function is used to check the status of 'term'. 
    If 'term' is asserted, return itself;
    If the negation or nor of 'term' is asserted, return the negation or nor;
    Else return nil which means 'term' can be asserted but now it is not asserted."

   ;; If 'term' is already asserted, return 'term' itself.
   (if (ct:assertedp term context) (return-from assertedOrNot term))   

   ;; If the negation or nor of 'term' is asserted(that is, we are sure 'term' cannot be asserted), return the negation or nor.
   (let ((downnegs (sneps:findto term 'nor)))
      (if (not (or (eq downnegs nil) (set:emptyp downnegs))) 
         (set:loopset for downneg in downnegs
            if (ct:assertedp downneg context)
              do (return-from assertedOrNot downneg)))
      (let ((upnegs (sneps:findfrom term 'nor)))
         (if (not (or (eq upnegs nil) (set:emptyp upnegs))) 
            (set:loopset for upneg in upnegs
               if (ct:assertedp upneg context)
                 do (return-from assertedOrNot upneg)))))
 
   ;; Else return nil. 
   nil)


(defun inferByForwardUsingConjunction (term context resultstack)
   "This function is used to do inference triggered by conjuction."

   (let ((conjuncts (sneps:findto term 'and)))
      (if (or (eq conjuncts nil) (set:emptyp conjuncts)) (return-from inferByForwardUsingConjunction resultstack))
      (set:loopset for conjunct in conjuncts
         if (not (ct:assertedp conjunct context))
           do (assertTrace nil (list term) conjunct "Forward chaining" context)
           and do (setf resultstack (inferByForward conjunct context resultstack))))

   resultstack)


(defun inferByForwardUsingConjunct (term context resultstack)
   "This function is used to do inference triggered by conjuct."

   (let ((conjunctions (sneps:findfrom term 'and)))
      (if (or (eq conjunctions nil) (set:emptyp conjunctions)) (return-from inferByForwardUsingConjunct resultstack))
      (set:loopset for conjunction in conjunctions
         if (not (ct:assertedp conjunction context))
           do (let ((conjuncts (sneps:findto conjunction 'and))
                    (reasonlist nil))
                 (set:loopset for conjunct in conjuncts
                    always(ct:assertedp conjunct context)                    
                    do (push conjunct reasonlist)
                 )
                 (if (eq (list-length reasonlist) (set:cardinality conjuncts))
                    (progn 
                       (assertTrace nil reasonlist conjunction "Forward chaining" context)
                       (setf resultstack (inferByForward conjunction context resultstack)))))))
   resultstack)



(defun inferByForwardUsingAndorarg (term context resultstack)
    "This function is used to do inference triggered by andorarg."
    
    (let ((andors (sneps:findfrom term 'andorargs)))
        (if (not (or (eq  andors nil) (set:emptyp andors)))
           (set:loopset for andor in andors   
              if (ct:assertedp andor context)
                do (setf resultstack (inferByForwardUsingAndor andor context resultstack))
              end)))

    (let ((downnegs (sneps:findto term 'nor)))
       (if (not (or (eq downnegs nil) (set:emptyp downnegs)))
          (set:loopset for downneg in downnegs 
             do (let ((downnegandors (sneps:findfrom downneg 'andorargs)))
                   (if (not (or (eq downnegandors nil) (set:emptyp downnegandors)))
                      (set:loopset for downnegandor in downnegandors   
                         if (ct:assertedp downnegandor context)
                           do (setf resultstack (inferByForwardUsingAndor downnegandor context resultstack))
                         end))))))  

    (let ((upnegs (sneps:findfrom term 'nor)))
       (if (not (or (eq upnegs nil) (set:emptyp upnegs)))
          (set:loopset for upneg in upnegs 
             do (let ((upnegandors (sneps:findfrom upneg 'andorargs)))
                   (if (not (or (eq upnegandors nil) (set:emptyp upnegandors)))
                      (set:loopset for upnegandor in upnegandors   
                         if (ct:assertedp upnegandor context)
                           do (setf resultstack (inferByForwardUsingAndor upnegandor context resultstack))
                         end)))))) 

    ;; In the end, return resultstack which contains all the asserted terms by forward chaining 
    resultstack)



(defun inferByForwardUsingAndor (term context resultstack)
   "This function is used to do inference triggered by andor"

   (let ((andorargs (sneps:findto term 'andorargs)))
      (if (or (eq andorargs nil) (set:emptyp andorargs)) (return-from inferByForwardUsingAndor resultstack)
          (let ((max (sneps:maxparam term))
                (tot-min (- (sneps:totparam term) (sneps:minparam term)))
                (status nil)
                (posassertedlist nil)  ; this list containing all the andor arguments which are asserted
                (negassertedlist nil)  ; this list containing all the andor arguments whose negations are asserted
                (unassertedlist nil))  ; this list containing all the andor arguments s.t. either itself or its negation is not asserted
               (set:loopset for andorarg in andorargs
                  do (setf status (assertedOrNot andorarg context))
 
                  if (eq status andorarg)
                    do (push andorarg posassertedlist)
                  else
                    if (eq status nil)
                      do (push andorarg unassertedlist)
                    else
                      do (push status negassertedlist)    
                    end     
                  end)                

               (if (or (eq (list-length unassertedlist) 0) (and (< (list-length posassertedlist) max) (< (list-length negassertedlist) tot-min)))
                  (return-from inferByForwardUsingAndor resultstack)
                  (if (>= (list-length posassertedlist) max)  
                     (progn
                        (push term posassertedlist) 
                        (loop for unasserted in unassertedlist  
                           do (assertTrace nil posassertedlist (notprop unasserted) "Forward chaining" context)) 
                        (loop for unasserted in unassertedlist  
                           do (setf resultstack (inferByForward (notprop unasserted) context resultstack))))

                     (progn 
                        (push term negassertedlist) 
                        (loop for unasserted in unassertedlist  
                           do (assertTrace nil negassertedlist unasserted "Forward chaining" context)) 
                        (loop for unasserted in unassertedlist  
                           do (setf resultstack (inferByForward unasserted context resultstack)))))))))

    resultstack)



(defun inferByForwardUsingThresharg (term context resultstack)
    "This function is used to do inference triggered by thresharg."   

    (let ((threshs (sneps:findfrom term 'threshargs)))
        (if (not (or (eq  threshs nil) (set:emptyp threshs)))
           (set:loopset for thresh in threshs   
              if (ct:assertedp thresh context)
                do (setf resultstack (inferByForwardUsingThresh thresh context resultstack))
              end)))

    (let ((downnegs (sneps:findto term 'nor)))
       (if (not (or (eq downnegs nil) (set:emptyp downnegs)))
          (set:loopset for downneg in downnegs 
             do (let ((downnegthreshs (sneps:findfrom downneg 'threshargs)))
                   (if (not (or (eq downnegthreshs nil) (set:emptyp downnegthreshs)))
                      (set:loopset for downnegthresh in downnegthreshs   
                         if (ct:assertedp downnegthresh context)
                           do (setf resultstack (inferByForwardUsingThresh downnegthresh context resultstack))
                         end))))))  

    (let ((upnegs (sneps:findfrom term 'nor)))
       (if (not (or (eq upnegs nil) (set:emptyp upnegs)))
          (set:loopset for upneg in upnegs 
             do (let ((upnegthreshs (sneps:findfrom upneg 'threshargs)))
                   (if (not (or (eq upnegthreshs nil) (set:emptyp upnegthreshs)))
                      (set:loopset for upnegthresh in upnegthreshs   
                         if (ct:assertedp upnegthresh context)
                           do (setf resultstack (inferByForwardUsingThresh upnegthresh context resultstack))
                         end)))))) 

   resultstack)



(defun inferByForwardUsingThresh (term context resultstack)
   "This function is used to do inference triggered by thresh"

   (let ((threshargs (sneps:findto term 'threshargs)))
      (if (or (eq threshargs nil) (set:emptyp threshargs)) (return-from inferByForwardUsingThresh resultstack)
          (let* ((min (sneps:minparam term))
                 (min-1 (- min 1))
                 (tot-max (- (sneps:totparam term) (sneps:maxparam term)))
                 (tot-max-1 (- tot-max 1))
                 (status nil)
                 (posassertedlist nil)  ; this list containing all the thresh arguments which are asserted
                 (negassertedlist nil)  ; this list containing all the thresh arguments whose negations are asserted
                 (unassertedlist nil)   ; this list containing all the thresh arguments s.t. either itself or its negation is not asserted
                 (reasonlist nil))      
               (set:loopset for thresharg in threshargs
                  do (setf status (assertedOrNot thresharg context))
 
                  if (eq status thresharg)
                    do (push thresharg posassertedlist)
                  else
                    if (eq status nil)
                      do (push thresharg unassertedlist)
                    else
                      do (push status negassertedlist)    
                    end     
                  end)                

               (if (eq (list-length unassertedlist) 0) 
                  (return-from inferByForwardUsingThresh resultstack)
                  (if (and (>= (list-length posassertedlist) min) (eq (list-length negassertedlist) tot-max-1))
                     (progn
                        (setf reasonlist (append posassertedlist negassertedlist))
                        (push term reasonlist) 
                        (loop for unasserted in unassertedlist  
                           do (assertTrace nil reasonlist unasserted "Forward chaining" context)) 
                        (loop for unasserted in unassertedlist  
                           do (setf resultstack (inferByForward unasserted context resultstack))))
                     (if (and (>= (list-length negassertedlist) tot-max) (eq (list-length posassertedlist) min-1))
                        (progn 
                           (setf reasonlist (append posassertedlist negassertedlist))
                           (push term reasonlist) 
                           (loop for unasserted in unassertedlist  
                              do (assertTrace nil reasonlist (notprop unasserted) "Forward chaining" context)) 
                           (loop for unasserted in unassertedlist  
                              do (setf resultstack (inferByForward (notprop unasserted) context resultstack))))))))))

    resultstack)



(defun inferByForwardUsingAntecedent (term context resultstack)
    "This function is used to do inference triggered by antecedent."
    
    (let 
        ;; Find all the entailment whose antecedent(s) contains term 
        ((entailments (sneps:findfrom term 'ant)))

        ;; If there is no entailment whose antecedent(s) contains term, return
        (if (or (eq  entailments nil) (set:emptyp entailments)) (return-from inferByForwardUsingAntecedent resultstack)
           (set:loopset for entailment in entailments
              if (ct:assertedp entailment context)
                do (setf resultstack (inferByForwardUsingImplication entailment context resultstack))
              end)))

    ;; In the end, return resultstack which contains all the asserted terms by forward chaining
    resultstack)



(defun inferByForwardUsingImplication (term context resultstack)
    "This function is used to do inference tirggered by implication."
    
    (let 
        ;; Find all the antecedent(s) of 'term' 
        ((antecedents (sneps:findto term 'ant)))

        ;; If 'term' contains no antecedents, return
        (if (or (eq antecedents nil) (set:emptyp antecedents)) (return-from inferByForwardUsingImplication resultstack)
            (let* ((minnum (sneps:minparam term))
                   (currentnum 0) 
                   ;(temp (set:new-set))
                   (assertedlist (list term)))
  
                   (set:loopset for antecedent in antecedents
                      if (ct:assertedp antecedent context)
                        do (incf currentnum)
                        and do (push antecedent assertedlist)
                      end
                      
                      always (< currentnum minnum))

                   (if (< currentnum minnum) (return-from inferByForwardUsingImplication resultstack)                               
                      (let ((consequents (sneps:findto term 'cq)))
                           (if (or (eq consequents nil) (set:emptyp consequents)) (return-from inferByForwardUsingImplication resultstack))  
                           (set:loopset for consequent in consequents
                              unless(ct:assertedp consequent context)
                              do (assertTrace nil assertedlist consequent "Forward chaining" context)                            
                              and do (setf resultstack (inferByForward consequent context resultstack))))))))

    ;; In the end, return resultstack which contains all the asserted terms by forward chaining
    resultstack)
 
