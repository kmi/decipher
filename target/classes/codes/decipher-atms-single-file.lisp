(in-package "COMMON-LISP-USER")


;;; ****************************************************************
;;; main ATMS database
(defparameter *dbase* (make-array 100 :adjustable t :fill-pointer 0))
(defstruct node 
   (datum nil)
   (label nil)
   (justifs nil)
   (conseq nil))

;;; *******************************************
;;; assumptions 
(defstruct (assumption (:include node) )
   (mask nil))

;;; *******************************************
;;; nogoods and diagnosis databases
(defparameter *nogoods* nil)
(defparameter *diag* nil)

;;; *******************************************
;;; environment database
(defparameter *environment* (make-hash-table)) 
(defstruct environment
   (nodes nil)
   (contra-bit nil))
 
;;; ****************************************************************
(defconstant ffalse (make-symbol "ffalse"))
(defconstant undef (make-symbol "undefined"))
(defparameter *mask* 1)    ; current bit-mask

;;; ****************************************************************
(defmacro get-entry (key) 
  `(or (aref *dbase* ,key) 
       (error "Key ~S undefined" ,key)))
     
;;; ****************************************************************
(defmacro get-entry-d (datum) 
  `(position ,datum *dbase* :key #'node-datum :test #'equal))
     
;;; ****************************************************************
(defmacro unknown (datum) 
   `(null (position ,datum *dbase* :key #'node-datum :test #'equal)))

;;; ****************************************************************
;;; ************** Getting values of individual slots **************
(defmacro get-label (key) 
  `(node-label (get-entry ,key)))

;;; ****************************************************************
(defmacro get-label-of-datum (datum) 
  `(let ((key (get-entry-d ,datum))) 
     (if key (get-label key)
        undef)))

;;; ****************************************************************
(defmacro get-justifs (key) 
  `(node-justifs (get-entry ,key)))

;;; ****************************************************************
(defmacro get-conseq (key) 
  `(node-conseq (get-entry ,key)))

;;; ****************************************************************$
(defmacro get-mask (key) ; key = assumption
   `(let ((x (get-entry ,key)))
     (if (typep x 'assumption) (assumption-mask x)
        (error "Node with key ~S is not assumption" ,key))))

;;; ****************************************************************
(defmacro get-nodes (env) 
  `(environment-nodes (gethash ,env *environment*)))

;;; ****************************************************************
(defmacro get-env-contra-bit (env) 
  `(environment-contra-bit  (gethash ,env *environment*)))

;;; ************** Setting individual items ************************
(defmacro set-label (datum new-value)
   `(setf (node-label (aref *dbase* ,datum)) ,new-value))
  
;;; ****************************************************************
(defmacro set-justifs (datum new-value)
   `(setf (node-justifs (aref *dbase* ,datum)) ,new-value))

;;; ****************************************************************
(defmacro set-conseq (datum new-value)
   `(setf (node-conseq (aref *dbase* ,datum)) ,new-value))
  
;;; ****************************************************************
(defmacro set-nodes (datum new-value)
   `(setf (environment-nodes (gethash ,datum *environment*)) ,new-value))
  
;;;*****************************************************************
(defmacro set-env-contra-bit (datum new-value)
   `(setf (environment-contra-bit 
           (gethash ,datum *environment*)) ,new-value))

;;; ****************************************************************
(defmacro update-conseq (datum justif)
  `(let ((y))
     (dolist (x ,justif)
	(setf y (get-conseq x))
	(unless (member ,datum y) (set-conseq x (append y (list ,datum)))))))

;;; ****************************************************************
;;; Elements of *nogoods* which are subsumed by those of nogood-lst
;;; are removed, then new *nogood* is restored.
;;; Elements of nogood-lst cannot be subsumed by *nogoods*, they have
;;; been removed by remove-nogoods function. 
(defmacro remove-subsumed-nogoods (nogood-lst)
  `(append (remove-subsumed *nogoods* ,nogood-lst) ,nogood-lst))

;;; ****************************************************************
(defun ini()
   (setf (fill-pointer *dbase*) 1)
   (set-justifs 0 ())
   (setf *nogoods* nil)
   (setf *mask* 1)
   (setf *diag* nil)
   (setf *environment* (make-hash-table)))

;;; ****************************************************************
(defun new-complete-label (just-lst)
  (if just-lst (list-subsume (remove-duplicates 
                  (apply #'append (mapcar #'new-label just-lst))))
            '(0)))

;;; ****************************************************************
(defun encode (just)
  (let ((x) (res))
    (dolist (y just res)
      (setf x (position y *dbase* :key #'node-datum :test #'equal))
      (if x (setf res (adjoin x res))
        (error "Undefined node ~S" y)))))

;;; ****************************************************************
(defun atms-accept (input)
  (let ((datum (first input)) (just (second input))
        (first-index) (lbl) (just1) (datum1))
    (if (null datum) (if just (process-nogoods (encode just))
                        (error "Contradiction ~S with empty justification" 
                           input))
      (progn
        (setf first-index (fill-pointer *dbase*))
        (setf datum1 (position datum *dbase* 
                        :key #'node-datum 
                        :test #'equal))
        (if datum1 ; node exists
           (process-new-just datum1 (encode just) (aref *dbase* datum1))
          (progn
             (if (equal (list datum) just) 
                (progn  ; new assumption
                   (setf lbl (list *mask*))
                   (vector-push-extend 
                          (make-assumption 
                               :datum datum
                               :mask *mask*
                               :label lbl
                               :justifs (list (list first-index))
                               :conseq nil) *dbase*)
                   (setf *mask* (ash *mask* 1)))
                (progn  ; premise or general node
                   (setf just1 (encode just))
                   (vector-push-extend 
                          (make-node
                               :datum datum
                               :justifs (list just1)
                               :conseq nil
                               :label 
                                  (setf lbl 
                                        (if (null just1) '(0) ; premise
                                          (progn ; general node
                                            (update-conseq first-index just1)
                                            (new-label just1))))) *dbase*)))
             (update-environment-dbase first-index () lbl)))))))            

;;; ****************************************************************
(eval-when (load eval) 
  (vector-push-extend (make-node :datum ffalse
                                 :label nil
                                 :justifs nil
                                 :conseq nil) *dbase*))

;;; ****************************************************************
;;; ****************************************************************
(defun process-nogoods (just)
  (let ((nogood-lst (new-label just))(old-nogoods *nogoods*) 
        (false-key 0)) ; 0-th item of *dbase* = ffalse
    (set-justifs false-key (append (get-justifs false-key) (list just)))
    (unless (null nogood-lst)
      (min-candidates-msk nogood-lst) ;;$Diagnosis
      (setf *nogoods* (remove-subsumed-nogoods nogood-lst))
      (remove-nogood-environment (set-difference *nogoods* old-nogoods)))))

;;; ****************************************************************$
(defun remove-nogood-environment (nogood-lst)
  (dolist (x nogood-lst)
     (maphash #'(lambda (key val)
                 (when (eq (logand key x) x)
                    (dolist (w (environment-nodes val))
                       (set-label w (delete key (get-label w))))
                    (remhash key *environment*))) *environment*)))  

;;; ****************************************************************
;;; ****************************************************************
(defun remove-subsumed (subsumed-lst subsuming-lst)
  (let ((result subsumed-lst))
    (dolist (y subsuming-lst result)
      (dolist (z subsumed-lst)
        (when (eq (logand z y) y) (setf result (delete z result)))))))

;;; ****************************************************************
(defun new-label (justif)
  (let ((w) (rst))
    (if justif (progn
                 (setf w (get-label (first justif)))
                 (setf rst (rest justif))
                 (dolist (z rst w )
                   (setf w 
                        (remove-nogoods (list-subsume (label2node-logior w 
                                 z)))))) 
          '(0))))
   
;;; ****************************************************************
;;; list-1 ... numbers - bit-masks, returned value = list-1 without
;;; numbers that are bit superset of some other number
(defun list-subsume (list-1)
  (let ((z list-1))
    (dolist (x list-1 z)
      (when  (member x z) 
        (setf z (remove-duplicates 
                 (mapcar #'(lambda (y)(if (eql x (logand x y)) x y)) z)))))))

;;; ****************************************************************
;;; the same as list2list-logior but the 2nd list is defined in terms of
;;; the node key
(defun label2node-logior (label-1 node-2)
   (remove-duplicates (apply #'append 
      (mapcar  #'(lambda (z)(mapcar #'(lambda (y)(logior z y)) 
               (get-label node-2))) 
         label-1))))

;;; ****************************************************************
;;; compares list and *nogoods* and deletes from list elements that are
;;; bit supersets of some element of *nogood*
(defun remove-nogoods (list)
  (let ((z list))
    (dolist (x *nogoods* z)
     (setf z (remove nil 
              (mapcar #'(lambda (y)(if (eq x (logand x y)) nil y)) z))))))

;;; ****************************************************************
(defun propagate (to-be-updated)
  (let ((datum-1) (label-1) (label-2))
    (loop
     (when (null to-be-updated) (return))
     (setf datum-1 (first to-be-updated))
     (setf to-be-updated (rest to-be-updated))
     (setf label-1 (get-label datum-1))
     (setf label-2 (new-complete-label (get-justifs datum-1)))
     (unless (equal label-1 label-2)
       (set-label datum-1 label-2)
       (update-environment-dbase datum-1 label-1 label-2)
       (setf to-be-updated (append to-be-updated (get-conseq datum-1)))))))
      
;;; ****************************************************************
(defun update-environment-dbase (datum old-label new-label)
  (let ((remove-lst (set-difference old-label new-label))
        (add-lst (set-difference new-label old-label))
        (y))
      (dolist (x remove-lst)
      (setf y (delete datum (get-nodes x)))
      (if (null y) (remhash x *environment*)
         (set-nodes x y)))
     (dolist (x add-lst)
      (setf y (gethash x *environment*))
      (if (null y) (setf (gethash x *environment*) (make-environment 
                                                      :nodes (list datum)))
      (set-nodes x (adjoin datum (environment-nodes y)))))))


;;; ****************************************************************
(defun process-new-just (datum just old-node) 
 (let ((label-1 (node-label old-node)) (label-2))
  (if (equal (list datum) just) 
        (error "Datum ~S already assumed" (assumption-datum old-node)) ;ass
    (if (null just) (progn  ; premise
                      (set-label datum '(0))
                      (set-justifs datum '(nil))
                      (update-environment-dbase datum label-1 '(0))
                      (propagate (node-conseq old-node)))
      (progn
        (setf label-2 (new-label just))
        (update-conseq datum just) 
        (unless (null label-2)
;$          (setf label-2 (append (remove-subsumed label-1 label-2) label-2))
          (setf label-2 (list-subsume (append label-1 label-2)))
          (set-label datum label-2)
          (set-justifs datum (cons just (get-justifs datum)))
          (update-environment-dbase datum label-1 label-2)
          (propagate (get-conseq datum))))))))
        
;;; ****************************************************************
;;; ************** Diagnosis ***************************************
;;; ****************************************************************
(defun mask2bit-list (bit-mask)
   (let ((x (integer-length bit-mask)) (msk 1) (result ()))
      (dotimes (y x result)
         (unless (eq (logand msk bit-mask) 0) 
            (setf result (append result (list msk))))
         (setf msk (ash msk 1)))))

;;; ****************************************************************
(defun mask2node-list (bit-mask)
   (let ((x (integer-length bit-mask)) (msk 1) (result ()))
      (dotimes (y x result)
         (unless (eq (logand msk bit-mask) 0) 
            (setf result (append result (list (mask2datum msk)))))
         (setf msk (ash msk 1)))))

;;; ****************************************************************
(defun mask2datum (msk)
  (let ((x (find-if #'(lambda (z) 
                       (and (typep z 'assumption) 
                       (equal (assumption-mask z) msk))) *dbase*)))
           (if x (assumption-datum x)
             (error "Assumption with mask ~S undefined" msk))))

;;; ****************************************************************
(defun mask-list2data (msk-lst) 
   (apply #'append (mapcar #'mask2node-list msk-lst)))

;;; ****************************************************************
(defun min-candidates-msk (nogood-lst)
 (dolist (nogood nogood-lst (setf *diag* (sort *diag* #'<)))
   (if (null *diag*) (setf *diag* (mask2bit-list nogood))
     (dolist (x *diag*)
      (unless (eq (logand x nogood) x)
        (setf *diag* 
           (list-subsume
                (append 
                   (remove x *diag*) 
                   (mapcar #'(lambda (y) (logior x y)) 
                      (mask2bit-list nogood))))))))))

;;; ****************************************************************
(defun min-candidates-data ()
   (mapcar #'mask2node-list *diag*))

;;; ****************************************************************
(defun key-lst2data (key-list)
   (mapcar #'(lambda (key) (node-datum (aref *dbase* key))) key-list))

;;; ****************************************************************
(defun list-key-lst2data (list-of-key-lists)
   (mapcar #'(lambda (key-list) (key-lst2data key-list)) 
         list-of-key-lists))
   
;;; ****************************************************************
;;; ****************************************************************
;;; ****************************************************************
;;; ****************************************************************
;; a-tst
;;; ****************************************************************
(defun z ()
 (let ((xx))
  (format t "~%~2A ~12A ~12A ~15A ~A" "#" 'DATUM 'TYPE 'LABEL 'JUSTIFICATIONS)
  (terpri)
  (format t "~%~2A ~12A ~12A ~15A ~A" 0 'ffalse 'nogood (get-label 0) 
            (list-key-lst2data (get-justifs 0)))
  (dotimes (x (fill-pointer *dbase*)) 
     (unless (eq x 0)
       (setf xx (aref *dbase* x))
       (format t "~%~2A ~12A ~12A ~15A ~A" 
               x
               (node-datum xx)
               (if (member nil (node-justifs xx)) 'premise
                (if (member (list x) (node-justifs xx) :test #'equal) 'assumption
                   'general))
               (node-label xx)
               (list-key-lst2data (node-justifs xx)))))
  (terpri) (values) ))
 
;;; **************************************************************** 
(defmacro a (input) `(atms-accept ,input))

;;; ****************************************************************
(defun e ()
  (format t "~% ~15a ~a" "Environment" "Nodes")
  (maphash #'(lambda (k v) 
               (format t "~% ~15a ~a" k 
                  (mapcar #'(lambda (z) (node-datum (aref *dbase* z)))
                     (environment-nodes v))))
               *environment*)
  (terpri)
  (values))

;;; ****************************************************************
(defun m ()
 (let ((xx))
  (format t "~% ~15a ~a" "Mask" "Datum")
  (dotimes (x (fill-pointer *dbase*)) 
    (setf xx (aref *dbase* x))
     (when (typep xx 'assumption) 
        (format t "~% ~15a ~a" (assumption-mask xx) (assumption-datum xx))))
  (values)))

;;; ****************************************************************
(defun n () (mapcar #'mask2node-list *nogoods*))

;;; ****************************************************************
(defparameter *txt* nil)
(defun tst ()
  ;(let ((x) (y) (list (parse-rules))); "votice-input.lisp")))
  (let ((x) (y) (list (parse-rules))); "votice_statements_v1.lisp")))
   (setq *txt* list)
   (terpri)
  (princ "**********************************************************")
  (terpri)
  (princ "q=quit, i=ini, d=dbase, e=environs, n=nogoods, m=masks, x=diag")
  (terpri)
  (princ "p=premise, a=assumption, g=general node, c=contradiction")
  (terpri)
  (princ "b=interactive enetring: ")
    (do ((c (read-line) (read-line)))
        ((string-equal c "q") 
         (terpri) (terpri)
         (princ "                        ~o~")
         (terpri)
         (princ "                     That's it")
         (values))
      (terpri)
      (princ "_________________________________________________________")
      (terpri)
      (case (elt c 0)
        (#\d (z))
        (#\e (e))
        (#\m (m))
        (#\i (ini))
        (#\b (princ "Interactive enetering of inputs:")
             (terpri)
             (interactive-enter list)
             (terpri))
        (#\n (print (n)))
        (#\p (terpri) (princ "Node: ") 
             (setf x (list (read) ()))
             (atms-accept x)
             (print x))
        (#\a (terpri) 
             (princ "Node: ") 
             (setf x (read))
             (setf x (cons x (list (list x))))
             (atms-accept x)
             (print x))
        (#\g (terpri) 
             (princ "Node: ") 
             (setf x (read))
             (terpri)
             (princ "Justif: ")
             (setf y (read))
             (setf x (cons x (list y)))
             (atms-accept x)
             (print x))
        (#\x (terpri) 
             (process-diag (min-candidates-data) list))
        (#\c (terpri)
             (princ "Justif: ")
             (setf y (read))
             (setf x (cons () (list y)))
             (atms-accept x)
             (print x))
        (otherwise (print "Error")))
      (terpri)
      (princ "**********************************************************")
      (terpri)(terpri) (terpri)
      (princ "**********************************************************")
      (terpri)
      (princ "q=quit, i=ini, d=dbase, e=environs, n=nogoods, m=masks, x=diag")
      (terpri)
      (princ "p=premise, a=assumption, g=general node, c=contradiction")
      (terpri)
      (princ "b=interactive enetring: "))))

(defmacro assum (x)
  `(atms-accept (cons ,x (list (list ,x)))))
(defmacro prem (x)
  `(atms-accept (list ,x)))
(defmacro gen-n (x y)
  `(atms-accept (cons ,x (list ,y))))
(defmacro contra (y)
  `(atms-accept (cons nil (list ,y))))


;(defparameter *votice-list* )

;(defparameter *rules-directory* (list *mypath* ))
(defparameter *rules-directory* nil)
;(defparameter *orig-file* "votice-input-part1.lisp")
(defparameter *orig-file* "votice_statements_v1.lisp")
;(defparameter *orig-file* "votice-input.lisp")

;;; check which of directories from *rules-directory* is present on computer
(defun choose-dir (x)
  (cond  ((null x) x)
         ((null (directory (car x))) (choose-dir (cdr x)))
         (t (car x))
         ))

;;; read the input file, there are parameter listed above as defaults
;;; paths and default file
(defun parse-rules (&optional (file-name *orig-file*)) 
  (let* (
         (dir (choose-dir *rules-directory*))
         (file (concatenate 'string dir file-name))
         (eof (make-symbol "eof"))
         (result nil))
    (setf *read-suppress* nil)
     (with-open-file (stream file :direction :input)
              (do ((x (read stream nil eof) (read stream nil eof)))
                  ((eq x eof))
                (setf result (append result (list x)))))
(values result)))

;;; unused funciton changing directly the array *dbase*, the change works
;;; but there are no more available the diagnosis and basicaly everything
;;; what's interesting on the ATMS
(defun change-node (x)
  (let* (
        (n (get-entry x))
        (premise (equal (node-justifs n) '(NIL)))
        (assumption (eq (type-of n) 'ASSUMPTION))
        (fp (fill-pointer *dbase*))
        )
    (cond (premise (setf (fill-pointer *dbase*) x) (assum (node-datum n)) (setf (fill-pointer *dbase*) fp))
          (assumption (setf (fill-pointer *dbase*) x) (prem (assumption-datum n)) (setf (fill-pointer *dbase*) fp))
          (t nil))
    ))

;;; read the whole list of input, change it as desired and output the file, which can be readable
;;; by next run of the program
(defun change-db (lst x)
  (let* (
         (file-name *orig-file*)
         (dir (choose-dir *rules-directory*))
         (number (parse-integer file-name :junk-allowed t))
         (number-txt (if (null number) (write-to-string 1) (write-to-string (+ number 1))))
         (length (if (null number) 0 (length number-txt)))
         ;#'(lambda (x) (concatenate 'string (third x) ":" (second x) ":" (car x) "---" (nth 3 x) "/" (nth 4 x) "/" (nth 5 x)))
         (new-file (concatenate 'string number-txt (subseq file-name length)))
         (file (concatenate 'string dir new-file))
         (node-to-change (nth x lst))
         (identifier (caar node-to-change))
         (changed-node (append (list (append (list (cond ((eq identifier 'ASSUM) 'PREM)((eq identifier 'PREM) 'ASSUM)(t identifier))) (cdar node-to-change)))
                               (cdr node-to-change)))
         (changed-list (append (subseq lst 0 x) (list changed-node) (subseq lst (+ x 1)))))
    (with-open-file (stream  file :direction :output :if-exists :supersede)
      (mapcar #'(lambda (x) (format stream "(~A ~S ~S)~%" (car x) (second x) (third x))) changed-list))
    (setq *orig-file* new-file)
  (values changed-list)))

;;; evaluate the selected part of the list, use batch proces, the rest of list is
;;; stored into r-list
(defun process-to-n (lst n)
(let (
      (1-part (subseq lst 0 n))
      (2-part (subseq lst n))) 
  (batch-proces 1-part) 2-part))       

;;; function for convenient output of list with numbers for easy pointer, can start from optional variable x
(defun output-list-of-param (lst &optional count x)
  (let (
        (index (if (null x) 1 x))
        )
(if (endp lst) '()
          (progn (if count (format t "~D = ~A~%" index (car lst)) (format t "~A~%" (car lst)))
    (output-list-of-param (cdr lst) count (+ index 1)))
    )))

;;; just take name from input ((PREM 'DB1) "...." "....") ---> DB1
(defun get-input-name (x)
  (cadr (cadar x)))

;;; evaluate all inputs in r-list
(defun batch-proces (lst) 
  (mapcar #'(lambda (x) (eval (car x))) lst))

;;; evaluating of inputs one-by-one, each must be fired manualy to process
(defun one-by-one-premise (lst)
  (let* (
        (next-one (car lst))
        (input (car next-one))
        (decs (cadr next-one))
        (story-part (third next-one))
        )
   (format t "raw input:  ~A~%math like description:  ~A~%part of story:  ~A~%Would you like to apply it? (y/n):" input decs story-part)
   (when (eq (elt (read-line) 0) #\y) (eval input)) 
   (values (cdr lst))
))

;;; clean a little bit the input and return it in more readable way 
;;; ((PREM 'DB1) "$1" "$2") ---> "DB1 --- $2"
(defun nice-output (lst)
  (mapcar #'(lambda (x) (list (get-input-name x) "---" (caar x) "---" (caddr x))) lst))

;;; output the difference between two databases, output what have been changed - the new version
(defun compare-versions (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) (list "original input file"))
        (t (append (set-difference (car lst) (cadr lst)) (compare-versions (cdr lst))))))

;;; make a nicer output of diagnosis
(defun process-diag (cand lst)
   (mapcar #'(lambda (c) 
               (mapcar #'(lambda (y) 
                           (mapcan #'(lambda (x) 
                                       (when (eq (get-input-name x) y)
                                         (nice-output-diag x))) lst)) c)) cand))

;;; write the diagnosis in a very convenient way for human beeings
(defun nice-output-diag (x)
  (let (
        (rule (get-input-name x))
        (desc (second x))
        (story-like (third x)))
    (format t "rule: ~A~%math-like: ~A~%story:~A~%" rule desc story-like)))

;;; command line user interface alowing to enter all premises and assumptions
(defun interactive-enter (lst)
 (let ((x) (y) (a-list lst) (r-list lst) (list-of-db (list lst)))
  (format t "Interactive input for Voticky fall~%")
  (format t "---------------------------------------------------------~%")
  (format t "n=new one, q=quit, d=database, c=contradiction~%b=batch proces rest of list, v=view of rest input")
  (format t "~%p=proces until number, z=change node, l=list versions~%s=set bversion:")
  (do ((ch (read-line) (read-line)))
      ((string-equal ch "q") 
         (format t "~%Leaving interactive output.~%")
         (values lst))
    (case (elt ch 0)
      (#\l (format t "~%List all the version of story~%") 
           ;(mapcar #'(lambda (x) (output-list-of-param (nice-output x) t)(format t "~%~%")) list-of-db))
           (output-list-of-param (compare-versions list-of-db) t))
    ;  (#\s (format t "~%Set new version of story:")
    ;       (setq x (read))
    ;       (
      (#\p (format t "~%Process till number:")
           (setq x (read))
           (setq r-list (process-to-n a-list x)))
      (#\b (batch-proces r-list))
      (#\z (format t "~%Change database~%")
           (output-list-of-param a-list t)
           (format t "~%Choose the node you would like to change: ")
           (setq x (read))
           (push (change-db a-list (- x 1)) list-of-db)
           (setq a-list (car list-of-db) r-list (car list-of-db)))
      (#\n (format t "~%Add new input")
           (setq lst (one-by-one-premise a-list)))
      (#\d (format t "~%Inspecting databse:")
           (z))
      (#\c (terpri)
           (princ "Justif: ")
           (setq y (read))
           (setq x (cons () (list y)))
           (atms-accept x)
           (print x))
      (#\w (format t "~%Rest of input verbose~%")
           (output-list-of-param a-list t))
      (#\v (format t "~%Rest of input~%")
           (output-list-of-param (nice-output a-list) t))
      (otherwise (print "Error"))
  )
  (format t "~%---------------------------------------------------------~%")
  (format t "n=new one, q=quit, d=database, c=contradiction~%b=batch proces rest of list, v=view of rest input")
  (format t "~%p=proces until number, z=change node, l=list versions~%s=set versions:")
)))


;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: CL-USER;   -*-

(in-package "COMMON-LISP-USER")

(defun run-atms-from-plot-data2 (&optional (plot-element-list *test-plot-list*))
  (run-atms (add-quotes-to-atms-list (generate-plot-atms-input plot-element-list)))
  ;(accepted-assumption-ids)
  (values plot-element-list)
)

(defun run-atms-from-plot-data (&optional (plot-element-list *test-plot-list*))
  (run-atms (add-quotes-to-atms-list (generate-atms-input plot-element-list)))
  (accepted-assumption-ids) 
  ;(values plot-element-list)
)

(defun run-atms-from-plot-data-string (plot-element-list-string)
  (setq res (run-atms-from-plot-data (read-from-string (jobject-lisp-value plot-element-list-string))))
  (format nil "~A" res)
  ;(values plot-element-list)
)

(defun run-atms (input-lis)
  (ini)
  (setq *txt* input-lis)
  (batch-proces *txt*)
  (values)
)

;new input paramaters for generting atms files from more storyscope like structure
(defparameter *test-plot-list* 
 '((PE1 (source (E1 E2)) (consequence (E3 E4)))
   (PE3 (source (E1 E4)) (consequence (E3 E2)))
   (PE2 (related (E1 E2 E3 E4)))
   (PE4 (related (E1 E2 E3 E4)))))

(defparameter *test-plot-list2* 
 '((PE1 (source (E1 E2)) (consequence (E3 E4)))
   (PE3 (source (E1 E10)) (consequence (E3 E11)))
   (PE2 (related (E1 E5)))
   (PE4 (related (E12 E5)))))


(defun add-quotes-to-atms-list (atms-items)
  (mapcar #'(lambda (q) 
                         (cond ((= 3 (length (car q)))
                                (cons (read-from-string (format nil "(~A '~A '~A)" (caar q) (cadar q) (caddar q))) (cdr q)))
                               ((= 2 (length (car q)))
                                (cons (read-from-string (format nil "(~A '~A)" (caar q) (cadar q))) (cdr q)))))
          atms-items)
)


(defun generate-plot-atms-input (plot-element-list &aux pe-event-list atms-list event-mapping-list res pairwise-list flat-event-list event-matching-pairs event-plot-element-map plot-plot-event-matches plot-pair-list plot-pairs pes plot-pair-lists plot-pair-lists-formatted plot-triples plot-triple-symbols plot-triple-symbols-formatted plot-triple-contras plot-triple-contras-formatted output)
  ;(setq plot-element-list *test-plot-list*)
  
  (setq pe-event-list nil)
  (setq atms-list nil)
  (setq event-mapping-list nil)

  ;;take each PE in turn
  ;makes (1,2) link between plot elements and events - atms list
  
  (mapcar #'(lambda (plot-element) (setq res (process-plot-element plot-element event-mapping-list))
              (setq pe-event-list (append pe-event-list (list (third res))))
              (setq atms-list (append atms-list (car res)))
              (setq event-mapping-list (cadr res))) plot-element-list)

  ;(E4 ((RELATED (E4PE4RELATED E4PE2RELATED)) (SOURCE (E4PE3SOURCE)) (CONSEQUENCE (E4PE1CONSEQUENCE))))

  (setq flat-event-list (mapcar #'(lambda (event-list) (cons (car event-list) (mapcan #'cadr (cadr event-list)))) event-mapping-list))
  
  (setq event-matching-pairs (mapcar #'(lambda (x) (list (car x) (expand-contradictions (cdr x)))) flat-event-list))
 
  (setq event-plot-element-map (mapcan #'(lambda (pe) (mapcar #'(lambda (x) (cons x (car pe))) (cdr pe))) pe-event-list))
 
  ;mapping-list of new to original event names
  (setq pairwise-list (generate-pairwise-mapping-list event-mapping-list))

  ;;make the plot-plot-event matches
  (setq res (make-plot-plot-event-matches event-matching-pairs event-plot-element-map pairwise-list))
  
  ;used to make (3) - plot-plot-event-matches
  (setq plot-plot-event-matches (car res))

  (setq plot-pair-list (cadr res))
  ;used to make (4) - plot-pairs
  (setq plot-pairs (mapcar #'(lambda (x) 
                               (setq pes (sort (list (caadr x) 
                                                     (cadadr x)) #'string-lessp))
                               (list (car x)
                                     (intern (format nil "~A~A" (car pes) (cadr pes)))
                                     ;(make-symbol (format nil "~A~A" (car pes) (cadr pes)))

                                     )) plot-pair-list))
  (setq plot-pair-lists (build-plot-pairs-with-events plot-pairs))
  (setq plot-pair-lists-formatted (mapcar #'(lambda (plot-pair-list) (list (list 'gen-n (car plot-pair-list) (cadr plot-pair-list)) (format nil "~A" (car plot-pair-list)))) plot-pair-lists))
  ;use the plot pair list to make plot triples
  ;used to make (5) - plot-triple-symbols
  (setq plot-triples (make-plot-triples plot-pair-list))

  (setq plot-triple-symbols (make-plot-triple-symbols plot-triples))
  (setq plot-triple-symbols-formatted (mapcar #'(lambda (plot-triple) (list (list 'gen-n (second plot-triple) (cddr plot-triple)) (format nil "~A" (second plot-triple)))) plot-triple-symbols))

  ;make (6) - plot-triple-contras
  ;make pairs with same middle
  (setq plot-triple-contras (plot-triple-contra-pairs plot-triple-symbols))
  (setq plot-triple-contras-formatted (mapcar #'(lambda (contra) (list (list 
                                                                        'contra 
                                                                        ;(make-symbol (format nil "~A~A" 'contra (setq c (+ 1 c))))
                                                                        contra) (format nil "contra"))) plot-triple-contras))
  

  ;(values (list atms-list plot-plot-event-matches plot-pair-lists-formatted plot-triple-symbols-formatted plot-triple-contras-formatted))
  
  (setq output (append atms-list plot-plot-event-matches plot-pair-lists-formatted plot-triple-symbols-formatted plot-triple-contras-formatted))
  (values output)
)

(defun build-plot-pairs-with-events (plot-list &aux res-list res-entry new-entry)
  (setq res-list nil)
  (mapcar #'(lambda (plot-pair)
              (setq res-entry (car (member (cadr plot-pair)  res-list :key #'car)))
              (cond (res-entry 
                     (setq res-list (remove (cadr plot-pair) res-list :key #'car)) 
                               (setq new-entry (list (car res-entry) (cons (car plot-pair) (cadr res-entry))))
                               (setq res-list (append (list new-entry) res-list))

)
                               
                    (t (setq res-list (cons (list (cadr plot-pair) (list (car plot-pair))) res-list))))) plot-list)
  (values res-list)
)


(defun make-plot-triple-symbols (plot-triples &aux edge1 edge2 pair1 pair2 edges middle)
  (mapcar #'(lambda (plot-triple) 
              (setq middle (caaddr plot-triple)) 
              (setq edge1 (car (set-difference (car plot-triple) (caddr plot-triple))))
              (setq edge2 (car (set-difference (cadr plot-triple) (caddr plot-triple))))
              (setq pair1 (sort (car plot-triple) #'string-lessp))
              (setq pair2 (sort (cadr plot-triple) #'string-lessp))
              ;sort the edges
              (setq edges (sort (list edge1 edge2) #'string-lessp))
              (list middle 
                    (intern (format nil "~A~A~A"  middle (car edges) (cadr edges))) 
                    ;(make-symbol (format nil "~A~A~A"  middle (car edges) (cadr edges)))
                    (intern (format nil "~A~A" (car pair1) (cadr pair1)))
                    ;(make-symbol (format nil "~A~A" (car pair1) (cadr pair1)))
                    (intern (format nil "~A~A" (car pair2) (cadr pair2)))
                    ;(make-symbol (format nil "~A~A" (car pair2) (cadr pair2)))
                    )) plot-triples)
  
)

(defun plot-triple-contra-pairs (plot-triples &aux contra-pairs remaining-list)
  (setq contra-pairs nil)
  (setq remaining-list plot-triples)

  (mapcar #'(lambda (plot-triple) (setq remaining-list (remove plot-triple remaining-list :test #'equal))
              (mapcar #'(lambda (candidate) (if (eq (car candidate) (car plot-triple))
                                                (setq contra-pairs (append contra-pairs (list (list (cadr candidate) (cadr plot-triple))))))
                                                ) remaining-list)) plot-triples)
  (values contra-pairs)
)

(defun make-plot-triples (plot-pair-list &aux pair-list remaining-list plot-triples inter)
  (setq pair-list (mapcar #'cadr plot-pair-list))
  (setq pair-list (remove-duplicates pair-list :test #'equal))


  (setq remaining-list pair-list)
  (setq plot-triples nil)
  (mapcar #'(lambda (plot-pair) (setq remaining-list (remove plot-pair remaining-list :test #'equal))
              (mapcar #'(lambda (potential-match) (setq inter (intersection plot-pair potential-match))
                          (if inter (setq plot-triples (append plot-triples (list (list plot-pair potential-match inter)))))) remaining-list)) pair-list)
  (values plot-triples)
)

(defun make-plot-plot-event-matches (event-matching-pairs event-plot-element-map pairwise-list &aux emp res plotmap plot-elements node-label)
  (setq emp (car event-matching-pairs))
  (setq emp (second emp))
  (setq res nil)
  (setq plotmap nil)
  (mapcar #'(lambda (emp)
                     (mapcar #'(lambda (event-pair)
                                 (setq plot-elements (sort (list (cdr (assoc (car event-pair) event-plot-element-map)) 
                                                                 (cdr (assoc (cadr event-pair) event-plot-element-map))) #'string-lessp))
                                 (setq plotmap (append plotmap (list (list 
                                                                      (intern (format nil "~A~A~A" (car plot-elements) (cadr plot-elements) (cdr (assoc (cadr event-pair) pairwise-list))))
                                                                      ;(make-symbol (format nil "~A~A~A" (car plot-elements) (cadr plot-elements) (cdr (assoc (cadr event-pair) pairwise-list))))
                                                                      (list (cdr (assoc (car event-pair) event-plot-element-map)) (cdr (assoc (cadr event-pair) event-plot-element-map)))))))
                                 (setq node-label (format nil "~A~A~A" (car plot-elements) (cadr plot-elements)
                                         (cdr (assoc (cadr event-pair) pairwise-list))))
                                 (setq res (append res (list (list (list 'gen-n 
                                                                         (intern node-label)
                                                                         ;(make-symbol node-label)
                                                                         event-pair) node-label))))) (second emp))) event-matching-pairs)

  (values (list res plotmap))
)


;((PE1 E1PE1SOURCE E2PE1SOURCE E3PE1CONSEQUENCE E4PE1CONSEQUENCE) (PE3 E1PE3SOURCE E4PE3SOURCE E3PE3CONSEQUENCE E2PE3CONSEQUENCE) (PE2 E1PE2RELATED E2PE2RELATED E3PE2RELATED E4PE2RELATED) (PE4 E1PE4RELATED E2PE4RELATED E3PE4RELATED E4PE4RELATED))
;(PE1 E1PE1SOURCE E2PE1SOURCE E3PE1CONSEQUENCE E4PE1CONSEQUENCE)
(defun make-event-pe-mapping (pe-list)
  (mapcan #'(lambda (pe) (mapcar #'(lambda (x) (cons x (car pe))) (cdr pe))) pe-list)
)


(defun make-plot-element-event-list (input)
  (mapcar #'(lambda (plot-element) (list (car plot-element) (mapcan #'(lambda (plot-set) (cadr plot-set)) (cdr plot-element)))) input)

)


;;******************functions for pulling different parts of the data from the atms model of interpretation
;;pull the assumptions out of the databse text
(defun extract-assumption-text (&aux txt-lst)
  (setq txt-lst (mapcan #'(lambda (x) 
                            (and (eq (caar x) 'assum) (list x))) 
                        *txt*))
  (setq txt-lst (mapcar #'(lambda (x)
                            (list (second (second (car x))) (second x))) txt-lst))
  (values txt-lst))





;;sets of assumptions rejected in each of the worlds
(defun show-rejected-assumptions (&aux assump-lst txt-lst item outputlist celltext)
  (setq outputlist nil)
  (setq txt-lst (extract-assumption-text))
  (setq assump-lst (min-candidates-data))

  (setq outputlist (cons (format nil "<td valign=\"top\">Rejected hypotheses:</td>") outputlist))

  (mapcar #'(lambda (x)
              (setq celltext "<td valign=\"top\">")
              (mapcar #'(lambda (y)
                          (setq item (car (member y txt-lst :key #'car)))
                          (setq celltext (concatenate 'string celltext (format nil "~A ~A</br>" (car item) (second item)))))
                      x)
              (setq celltext (concatenate 'string celltext "</td>"))
              (setq outputlist (cons celltext outputlist)))
          assump-lst)
  (values (reverse outputlist)))

;;sets of assumptions accepted in each of the worlds
(defun show-accepted-assumptions (&aux assump-lst txt-lst all-assump item outputlist celltext)
  (setq outputlist nil)
  (setq txt-lst (extract-assumption-text))
  (setq all-assump (mapcar #'car txt-lst))
  (setq assump-lst (min-candidates-data))
  
  (setq outputlist (cons (format nil "<td valign=\"top\">Accepted hypotheses:</td>") outputlist))

  (mapcar #'(lambda (x)
              (setq celltext "<td valign=\"top\">")
              (mapcar #'(lambda (y)
                          (setq item (car (member y txt-lst :key #'car)))

                          (setq celltext (concatenate 'string celltext (format nil "~A ~A<br/>" (car item) (second item)))))
                          ;;(setq celltext (concatenate 'string celltext (format nil "~A ~A~%" (car item) (second item)))))

                      (reverse (set-difference all-assump x)))
              (setq celltext (concatenate 'string celltext "</td>"))
              (setq outputlist (cons celltext outputlist)))
          assump-lst)
  (values (reverse outputlist)))

;;assumptions that hold/do not hold in each of the interpretations
(defun show-result-assumptions (&aux assump-lst txt-lst all-assump (count 0))
  (setq txt-lst (extract-assumption-text))
  (setq all-assump (mapcar #'car txt-lst))
  (setq assump-lst (min-candidates-data))
  (mapcar #'(lambda (x)
              (format t "~%Interpretation ~D~%" (incf count))
              (format t "~%The following assertions hold:")
              (mapcar #'(lambda (y)
                          (format t "~%      - ~A" (second (car (member y txt-lst :key #'car))))) 
                      (set-difference all-assump x))
              (format t "~%~%and the following assertions do not hold in this interpretation:")
              (mapcar #'(lambda (y)
                          (format t "~%      - ~A" (second (car (member y txt-lst :key #'car))))) 
                      x)
              (format t "~%___________________________________________________________________________~%")
             (read-char)) 
          assump-lst)

  (values))


(defun res ()
  (show-result-assumptions))

;;pull the gen-n statements out of the database
(defun extract-gen-nodes (&aux txt-lst)
  (setq txt-lst (mapcan #'(lambda (x) 
                            (and (eq (caar x) 'gen-n) (list x))) 
                        *txt*))
  (setq txt-lst (mapcar #'(lambda (x)
                            (list (second (second (car x))) (second (third (car x))) (second x))) txt-lst))
  (values txt-lst))


(defun all-rejected-gen-nodes (&aux rejected-assumption-lists)
 (setq rejected-assumption-lists (min-candidates-data))
 (mapcar #'(lambda(x) (rejected-gen-nodes x)) rejected-assumption-lists)
)

;;; (rejected-gen-nodes (car (min-candidates-data)))
;;gives a list of rejected gen nodes
;;each item in the list is the id of the rejected gen, the assum(s) on which it fails, its preconditions, the accumulated list of rejected nodes
(defun rejected-gen-nodes (rejected-assump &aux result rejected-ante gen-list rejected current-gen-list rejected-list completed)
  (setq rejected-list rejected-assump
        gen-list (extract-gen-nodes))
  (loop
  (setq completed T)
  (setq current-gen-list gen-list)
  (dolist (gen current-gen-list)
    (setq rejected-ante (intersection rejected-list (second gen)))
;     (break "~S ~S ~S" rejected-ante rejected-list gen)
     (when rejected-ante
       (setq rejected (car gen))
       (push (list rejected rejected-ante (second gen) rejected-list) result)
       (setq rejected-list (cons rejected rejected-list))
       (setq completed NIL))
       (setq gen-list (remove rejected gen-list :key #'car)))
  (when completed (return)))
  (values (reverse result)))


;;show the databse contents
(defun extract-text (&aux result)
  (setq result (mapcan #'(lambda (x) 
                           (and (not (eq (caar x) 'contra)) ;(list x))) *txt*))
                                (list (list (second (second (car x))) (second x))))) 
                       *txt*))
  (values result))

;;get text for a statement
;;(find-text 'X100 (extract-text))
(defun find-text (x node-text)
  (second (car (member x node-text :key #'car))))

(defun explain-all-rejected-gen (&aux rejected-assumption-lists) 
 (setq rejected-assumption-lists (min-candidates-data))
 (mapcar #'(lambda(x) 
             (format t "~3%****interpretation***")
             (explain-rejected-gen x)) rejected-assumption-lists)
)

(defun explain-all-rejected-gen2 (&aux rejected-assumption-lists outputlist)
  (setq outputlist nil)
  (setq outputlist (cons "<td valign=\"top\">Rejected inferences</td>" outputlist))
  (setq rejected-assumption-lists (min-candidates-data))
  (mapcar #'(lambda(x)
             (setq outputlist (cons (explain-rejected-gen2 x) outputlist))) rejected-assumption-lists)
  (values (reverse outputlist))
)








;;****************
;;(explain-rejected-gen  (car (min-candidates-data)))
(defun explain-rejected-gen (rejected-assump &aux rejected-just node-text)
  (setq rejected-just (rejected-gen-nodes rejected-assump))
  (setq node-text (extract-text))
  (dolist (x rejected-just)
    (format t "~%________________________________________________________________________")
    (format t "~%~%Statement:~%       <~A>~%has been rejected and the following statements have been rejected:" (find-text (car x) node-text))
    (dolist (y (second x))
      (format t "~%       <~A>" (find-text y node-text)))
    (format t "~%~%-------~%and the follwing rule applies:~%")
    (format t "~%       <~A>~%holds if the following statements:" (find-text (car x) node-text))
    (dolist (z (third x))
      (format t "~%       <~A>" (find-text z node-text)))
    (format t "~%hold.")))

(defun explain-rejected-gen2 (rejected-assump &aux rejected-just node-text accepted-statements celltext)
  (setq celltext "<td valign=\"top\">")
  (setq rejected-just (rejected-gen-nodes rejected-assump))
  (setq node-text (extract-text))
  (dolist (x rejected-just)
    (setq celltext (concatenate 'string celltext (format nil "~A ~A</br><em>has been rejected and the following statements have been rejected:</em>" (car x) (find-text (car x) node-text))))
    (setq celltext (format nil "~A<ul style=\"margin-top: 0; margin-bottom: 0\">" celltext))
    (dolist (y (second x))&&&&&&&
      (setq celltext (concatenate 'string celltext (format nil "<li>~A ~A</li>" y (find-text y node-text)))))
    (setq celltext (format nil "~A</ul style=\"margin-top: 0;\">" celltext))
    (setq accepted-statements (set-difference (third x) (second x)))
    (cond ((> (length accepted-statements) 0)
           (setq celltext (concatenate 'string celltext (format nil "<em style=\"margin-top: 0;\">although the following were accepted:</em>")))
           (setq celltext (format nil "~A<ul style=\"margin-top: 0;\">" celltext))
           (dolist (z accepted-statements)
             (setq celltext (concatenate 'string celltext (format nil "<li>~A ~A</li>" z (find-text z node-text)))))
           (setq celltext (format nil "~A</ul>" celltext))))
    )
  (setq celltext (format nil "~A</td>" celltext))
  (values celltext))

;;prints rejected gens for each interpretation
(defun explain-rejected-assumptions-for-all-interpretations (&aux (count 1))
  (dolist (x (min-candidates-data))
    (format t "~%~%********************* Interpretation ~D" count)    
    (explain-rejected-gen x)
    (format t "~%~%******* End of interpretation ~D ... type q to quit,  any other char to continue   " count)
    (incf count)
    (if (char-equal (read-char) #\q) (return)))
  (values))


;list of all assumptions from the database
(defun extract-all-assumptions ()
  (mapcar #'car (extract-assumption-text)))

;;returns a list of possible worlds
;;each world contains 2 lists
;;the first list is a list of accepted assumptions. the second is a list of rejected assumptions
(defun extract-valid (&aux all res)
  (setq all (extract-all-assumptions))
  (setq res (mapcar #'(lambda (x) 
                        (list (set-difference all x) x))
                    (min-candidates-data)))
  (values res))

(defun extract-valid-reversed (&aux res)
  (setq res (extract-valid))
  (values (cons (reverse (car res)) (cdr res)))
)

(defun accepted-assumption-ids ()
  (mapcar #'(lambda (x) (car x)) (extract-valid))
)

(defun explain-all-accepted-gen (&aux assum-and-prem outputlist)
  (setq outputlist nil)
  (setq outputlist (cons (format nil "<td valign=\"top\">Accepted inferences:</td>") outputlist))
  (setq assum-and-prem (merge-valid-assump-prem))
  (mapcar #'(lambda (x)
              (setq outputlist (cons (explain-accepted-gen (car x)) outputlist))) assum-and-prem)
  (values (reverse outputlist)))

;;*************************
;;(explain-accepted-gen (caar (merge-valid-assump-prem)))
(defun explain-accepted-gen (accepted-assump &aux accepted-gen node-text outputstring)
  (setq outputstring "<td valign=\"top\">")
  (setq accepted-gen (accepted-gen-nodes accepted-assump)) ;(break "~S" accepted-assump)
  (setq node-text (extract-text))
  (dolist (x accepted-gen)

    (setq outputstring (concatenate 'string outputstring (format nil "~A ~A</br><em>has been accepted and the following statements have been accepted</em>" (car x) (find-text (car x) node-text))))
    (setq outputstring (format nil "~A<ul style=\"margin-top: 0;\">" outputstring))
    (dolist (y (second x))
      (setq outputstring (concatenate 'string outputstring (format nil "<li>~A ~A</li>" y (find-text y node-text)))))
    (setq outputstring (format nil "~A</ul>" outputstring))
    )
  (setq outputstring (concatenate 'string outputstring "</td>"))
  (values outputstring))


(defun all-accepted-gen-nodes (&aux assum-and-prem)
  (setq assum-and-prem (merge-valid-assump-prem))
  (mapcar #'(lambda (x) (accepted-gen-nodes (car x))) assum-and-prem)
                                 

)

;;;
;;(accepted-gen-nodes (caar (merge-valid-assump-prem)))
(defun accepted-gen-nodes (accepted-assump &aux result accepted-ante gen-list accepted current-gen-list accepted-list completed)
  (setq accepted-list accepted-assump
        gen-list (extract-gen-nodes))
  (loop
  (setq completed T)
  (setq current-gen-list gen-list)
  (dolist (gen current-gen-list)
    (setq accepted-ante (intersection accepted-list (second gen) ))
;     (break "~S ~S ~SA~%~D" accepted-ante accepted-list gen (length accepted-list))
     (when (null (set-exclusive-or accepted-ante (second gen)))
       (setq accepted (car gen))

 ;     (push (list accepted accepted-ante ) result)
       (push (list accepted accepted-ante accepted-list) result)
 ;     (push (list accepted accepted-ante (second gen) accepted-list) result)
       (setq accepted-list (cons accepted accepted-list))

       (setq completed NIL))
       (setq gen-list (remove accepted gen-list :key #'car)))
  
  (when completed (return)))
  (values (reverse result)))


(defun extract-prem-nodes (&aux txt-lst)
  (setq txt-lst (mapcan #'(lambda (x) 
                            (and (eq (caar x) 'prem) (list x))) 
                        *txt*))
  (setq txt-lst (mapcar #'(lambda (x)
                            (list (second (second (car x)))  (second x))) txt-lst))
  (values txt-lst))

(defun extract-or-nodes (&aux txt-lst)
  (setq txt-lst (mapcan #'(lambda (x) 
                            (and (eq (caar x) 'or) (list x))) 
                        *txt*))
  (setq txt-lst (mapcar #'(lambda (x)
                            (list (second (second (car x)))  (second x))) txt-lst))
  (values txt-lst))


(defun print-prem-nodes (&aux string)
  (setq string "<p>The facts:</p><ul>")
  (mapcar #'(lambda (x) (setq string (concatenate 'string string "<li>" (format nil "~A" (car x)) " " (cadr x) "</li>"))) (extract-prem-nodes))
  (setq string (concatenate 'string string "</ul>"))
)


(defun compare-merged-statements-against-ors (&aux merged-statments or-nodes)
  (setq merged-statments (merge-accepted-statements (extract-prem-ids) (accepted-assumption-ids) (accepted-gen-ids)))
  (setq or-nodes (extract-or-nodes))

  (mapcar #'(lambda (x) (compare-statement-list-against-all-ors x or-nodes)) merged-statments)
)
  
;  (mapcar #'(lambda (ornode) (cond (member )) or-nodes)

;  (mapcar #'(lambda (x) ((cond (not (member (car or-nodes) '(b c d)))
;                                (equal (length (cdr x)) (length (intersection x merged-statments

;merged-statments)
  
;)

(defun compare-statement-list-against-all-ors (list ors &aux res compare)
  (setq compare (mapcar #'(lambda (or) (compare-statement-list-against-or list or)) ors))
  (if (member nil compare)
      (setq res nil)
    (setq res t))
  (values res)
)


(defun compare-statement-list-against-or (list or &aux res)
  (if (member (caar or) list)
      (if (intersection (cdar or) list)
          (setq res t)
        (setq res nil))
    (setq res t))
  (values res))


(defun extract-prem-ids (&aux nodes)
  (setq nodes (extract-prem-nodes))
  (mapcar #'(lambda (x) (car x)) nodes)
)


(defun explain-accepted-rejected (lst)
  (explain-accepted-gen (car lst))
  (format t "~%~%===================================================================================") (read-char)
  (explain-rejected-gen (second lst))
  (values))

;like extract-valid but appends the list of prems to the list of accepted assumptions. For use in finding which gen-ns match the prems and assums of the world
(defun merge-valid-assump-prem (&aux res prem)
  (setq res (extract-valid))
  (setq prem (mapcar #'car (extract-prem-nodes)))
  (setq res (mapcar #'(lambda (x) 
                        (list (append (car x) prem) (second x)))
                    res))                 
  (values res))

(defun accepted-gen-ids ()
  (mapcar #'(lambda (x)  (mapcar #'(lambda (x) (car x)) (accepted-gen-nodes (car x)))) (merge-valid-assump-prem))
)

(defun merge-accepted-statements (prems assum-lists gen-lists)
  (cond ((or (null assum-lists) (null gen-lists)) nil)
        (t (cons (append prems (car assum-lists) (car gen-lists)) (merge-accepted-statements prems (cdr assum-lists) (cdr gen-lists)))))
)


(defun show-diagnosis (&optional (choice 1) &aux string) 
  (bbqq choice)
  (setq string (diagnosis-table choice))
  (values string)
)


(defun concatenate-list-of-strings-with-tds (strings &aux output)
  (setq output "")
  (mapcar #'(lambda (x) (setq output (format nil "~A<td>~A</td>" output x))) strings)
  (values output)
)

(defun concatenate-list-of-strings (strings &aux output)
  (setq output "")
  (mapcar #'(lambda (x) (setq output (format nil "~A~A" output x))) strings)
  (values output)
)

(defun concatenate-list-of-quotes-with-spaces (strings &aux output)
  (setq output "")
  (mapcar #'(lambda (x) (setq output (format nil "~A~A " output x))) strings)
  (values (string-trim " " output))
)

(defun output-database (db &aux output)
  (setq output "<table><tr><th>Id</th><th>Description</th><th>Type</th>")
  (mapcar #'(lambda (x)
              (cond ((eq (caar x) 'CONTRA) (setq output (concatenate 'string output "<tr><td>" (rename-statement-status (caar x)) "</td><td>" (cadr x) "</td><td>"  "All of these cannot be accepted:<br/>" (concatenate-list-of-quotes-with-spaces (cadr (cadar x))) "</td></tr>")))
                    ((eq (caar x) 'OR) (setq output (concatenate 'string output "<tr><td>" (rename-statement-status (caar x)) "</td><td>" (cadr x) "</td><td>If " (format nil "~A" (caadr (cadar x))) " then at least one of these must be accepted:<br/>" (concatenate-list-of-quotes-with-spaces (cdadr (cadar x))) "</td></tr>")))
                    (t (setq output (concatenate 'string output "<tr><td>" (format nil "~A" (cadr (cadar x))) "</td><td>" (cadr x) "</td><td>" (rename-statement-status (caar x))))
                       (if (null (third (car x)))
                           (setq output (concatenate 'string output "</td>"))
                         (setq output (concatenate 'string output "<br/>" (concatenate-list-of-quotes-with-spaces (cadr (third (car x)))) "</td></tr>"))))))
          db)
  (setq output (format nil "~A</table>" output))
  (values output)
)

(defun rename-statement-status (status &aux new)
  (cond ((eq status 'PREM) (setq new "Fact"))
        ((eq status 'GEN-N) (setq new "Inferred from "))
        ((eq status 'ASSUM) (setq new "Hypothesis"))
        ((eq status 'CONTRA) (setq new "Contradiction"))
        ((eq status 'OR) (setq new "Required")))
  (values new)
)

(defun open-para ()
  (values "")           
;;  (values "<p>")  
)

(defun close-para ()
  (values "~%")           
;;  (values "</p>")  
)

(defun open-em ()
  (values "")           
;;  (values "<em>")  
)

(defun close-em ()
  (values "")           
;;  (values "</em>")  
)


(defun open-strong ()
  (values "")           
;;  (values "<strong>")  
)


(defun close-strong ()
  (values "")           
;;  (values "</strong>") 
)

(defun dec-story-graph (&aux output)
  (setq output (concatenate 'string "<html><head>" (style) "</head><body><div id=\"wrapper\">" (menu)))
  (setq output (format nil "~A<h2>Story graph</h2><a href=\"http://people.kmi.open.ac.uk/paulm/thefall-graph-final.pdf\">Dowload story graph</a>" output  ))
  (setq output (format nil "~A</div></body></html>" output))
  (values output)
)


(defparameter *test-label-list* '((PE1 . "pe 1")(PE2 . "pe2")(PE3 . "pe 3")(PE4 . "pe4")(E1 . "e1")(E2 . "e2")(E3 . "e3")(E4 . "e4")(E5 . "e5")(E10 . "e10")(E11 . "e11")(E12 . "e12")))

;(defparameter *plot-element-list* '((PE1 . inspired)(PE2 . related) (PE3 . motivated)(PE4 . related)))



;;code for plot ATMS

;(defparameter *event-list* '(X3 X4 X5 X12 X13 X14 X15 X16 X19 X20 X21 X22 X32 X33 X34 X35 X37 X42 X44 X45 X46 X47 X52 X53 X54 X56 X62 X63 X64 X65 X67 X72 X73 X82 X83 X84 X85))
;(defparameter *event-list* '(X3 X4 X5  X12 X13 X14 X15 X16 X19 X20 X21 X22 X32 X33 X34 X35 X37 X42 X44 X45 X46 X47 X52 X53 X54 X56 X62 X63 X64 X65 X72 X73 X74 X75 X76 X82 X83 X84 X85 X92 X93 X94 X95 X96 X97 X102 X103 X104 X105 X112 X113 X114 X115))

(defparameter *plot-element-list* '((X1 . related) (X10 . influenced) (X30 . motivated) (X40 . in-reaction-to) (X50 . inspired) (X60 . influenced) (X70 . related) (X80 . related)))
;(defparameter *plot-element-list* '((X1 . related)(X10 . influenced) (X30 . motivated)(X40 . in-reaction-to)(X50 . inspired)(X60 . related)(X70 . related)(X80 . related)(X90 . related)(X100 . related)(X110 . related)))

(defparameter *plot-drama-rating* '((related . 1) (influenced . 2) (motivated . 3) (in-reaction-to . 4) (inspired . 5)))

;(defparameter *event-mapping-list* '((X3 . X3) (X4 . X4) (X5 . X5) (X12 . X12) (X13 . X13) (X14 . X14) (X15 . X15) (X16 . X16) (X19 . X19) (X20 . X20) (X21 . X21) (X22 . X22) (X32 . X32) (X33 . X33) (X34 . X34) (X35 . X35) (X37 . X37) (X42 . X32) (X44 . X44) (X45 . X45) (X46 . X46) (X47 . X47) (X52 . X52) (X53 . X53) (X54 . X54) (X56 . X56) (X62 . X62) (X63 . X63) (X64 . X64) (X65 . X65) (X67 . X67) (X72 . X72) (X73 . X73) (X82 . X4) (X83 . X3) (X84 . X84) (X85 . X47)))
;;(defparameter *event-mapping-list* '((X3 . X3) (X4 . X4) (X5 . X5) (X12 . X12) (X13 . X13) (X14 . X14) (X15 . X15) (X16 . X16) (X19 . X190) (X20 . X20) (X21 . X21) (X22. X22) (X32 . X32) (X33 . X33) (X34 . X34) (X35 . X35) (X37 . X37) (X42 . X32) (X44 . X44) (X45 . X45) (X46 . X46) (X47 . X47) (X52 . X52) (X53 . X53) (X54 . X54) (X56 . X56) (X62. X4) (X63 . X3) (X64 . X64) (X65 . X47) (X72 . X35) (X73 . X34) (X74 . X33) (X75 . X37) (X76 . X76) (X82 . X47) (X83 . X44) (X84 . X3) (X85 . X64) (X92 . X21) (X93 . X20)  (X94 . X19) (X95 . X12) (X96 . X96) (X97 . X97) (X102 . X102) (X103 . X103) (X104 . X104) (X105 . X105) (X112 . X4) (X113 . X3) (X114 . X64) (X115 . X115)))
;(defparameter *event-mapping-list* '((E2PE1SOURCE . E2) (E4PE1CONSEQUENCE . E4) (E10PE3SOURCE . E10) (E3PE1CONSEQUENCE . E3) (E3PE3CONSEQUENCE . E3) (E11PE3CONSEQUENCE . E11) (E1PE1SOURCE . E1) (E1PE3SOURCE . E1) (E1PE2RELATED . E1) (E12PE4RELATED . E12) (E5PE2RELATED . E5) (E5PE4RELATED . E5)))
(defparameter *event-mapping-list* '((X3 . X3) (X4 . X4) (X5 . X5) (X12 . X12) (X13 .X13) (X14 . X14) (X15 . X15) (X16 . X16) (X19 . X190) (X20 . X20) (X21 . X21)(X22. X22) (X32 . X32) (X33 . X33) (X34 . X34) (X35 . X35) (X37 . X37) (X42 . X32) (X44 . X44) (X45 . X45) (X46 . X46) (X47 . X47) (X52 . X52) (X53 . X53) (X54 . X54) (X56 . X56) (X62. X4) (X63 . X3) (X64 . X64) (X65 . X47) (X72 . X35) (X73 . X34) (X74 . X33) (X75 . X37) (X76 . X76) (X82 . X47) (X83 . X44) (X84 . X3) (X85 . X64) (X92 . X21) (X93 .X20)  (X94 . X19) (X95 . X12) (X96 . X96) (X97 . X97) (X102 . X102) (X103 . X103) (X104 . X104) (X105 . X105) (X112 . X4) (X113 . X3) (X114 . X64) (X115 . X115)))

(defparameter *plot-element-object-stories* '((X1 . (os1 os2)) (X10 . (os3 os4)) (X30 . (os5 os6)) (X40 . (os1 os7)) (X50 . (os4 os6)) (X60 . (os5 os10)) (X70 . (os0 os10 os11)) (X80 . (os8 os9 os10))))

;(defun remove-event-duplicates (node-list)
;  (mapcar #'(lambda (x) (

(defun plot-atms (&aux accepted-node-lists event-lists number-of-events plot-lists plot-element-number plot-element-ratio plot-os-lists plot-flat-os-list)
  (setq accepted-node-lists (merge-accepted-statements (extract-prem-ids) (accepted-assumption-ids) (accepted-gen-ids)))

  ;map nodes to events
  (setq event-lists (mapcar #'(lambda (accepted-node-list) (mapcar #'(lambda (accepted-node) (cdr (assoc accepted-node *event-mapping-list*))) accepted-node-list)) accepted-node-lists))

  ;remove nils (as these are not events) and duplicates (as these are multiple nodes pointing to the same event)
  (setq event-lists (mapcar #'(lambda (event-list) (remove-duplicates (remove NIL event-list))) event-lists))

  ;;number of events
  (setq number-of-events (mapcar #'length event-lists))
  
  ;;map to plot elements
  (setq plot-lists (mapcar #'(lambda (accepted-node-list) (mapcar #'(lambda (accepted-node) (cdr (assoc accepted-node *plot-element-list*))) accepted-node-list)) accepted-node-lists))
  
  (setq plot-lists (mapcar #'(lambda (plot-list) (remove NIL plot-list)) plot-lists))
  
  ;;number of plot elements
  (setq plot-element-number (mapcar #'length plot-lists))

  ;;plot ratio
  (setq plot-element-ratio (mapcar #'calculate-plot-ratio plot-lists))
  
  ;;plot-object-stories
  (setq plot-os-lists (mapcar #'(lambda (accepted-node-list) (mapcar #'(lambda (accepted-node) (cdr (assoc accepted-node *plot-element-object-stories*)
                                                                                                )) accepted-node-list)) accepted-node-lists))
  (setq plot-os-lists (mapcar #'(lambda (plot-list) (remove NIL plot-list)) plot-os-lists))
  
  (setq plot-flat-os-list (mapcar #'(lambda (plot-os-list) (length (remove-duplicates (flatten-sublists plot-os-list)))) plot-os-lists))



  (values (list event-lists plot-lists number-of-events plot-element-number plot-element-ratio plot-flat-os-list))

)

(defun flatten-sublists (lists &aux list)
  (setq list nil)
  (mapcar #'(lambda (x) (setq list (append list x))) lists)
  (values list)
)

(defun filter (fn lst)
  (let ((acc nil))
  (dolist (x lst)
    (let ((val (funcall fn x)))
      (if val (push val acc))))
  (nreverse acc)))

(defun calculate-plot-ratio (lis &aux length score-lis score-sum)
  (setq length (length lis))
  (setq score-lis (mapcar #'(lambda (x) (cdr (assoc x *plot-drama-rating*))) lis))
  (setq score-sum (reduce #'+ score-lis))

  (float (/ score-sum length))
  
)

(defun expand-contradictions (lst &aux res) 
  (loop
   (when (null (cdr lst)) (return))
   (setq res (append (mapcar #'(lambda (x)  (list x (car lst))) (cdr lst)) res))
   (setq lst (cdr lst)))
  (values res))


(defun process-plot-element (plot-element event-mapping-list &aux atms-list plot-element-event-list)
  ;(setq event-mapping-list nil)
  ;(setq atms-list (list (list (list 'assum (car plot-element)) (cdr (assoc (car plot-element) *test-label-list*)))))
  (setq atms-list (list (list (list 'assum (car plot-element)) (format nil "~A" (car plot-element) ))))

  (setq plot-element-event-list (list (car plot-element)))

  (mapcar #'(lambda (plot-set)  
              (setq atms-list (append atms-list (list (list (list 'gen-n 
                                                                  (intern (format nil "~A~A" (car plot-element) (car plot-set))) 
                                                                  ;(make-symbol (format nil "~A~A" (car plot-element) (car plot-set)))
                                                                  (list (car plot-element))) (format nil "~A - ~A" (car plot-element) (car plot-set))))))
              (mapcar #'(lambda (event) 
                          (setq atms-list (append atms-list (list (list (list 'gen-n 
                                                                              (intern (format nil "~A~A~A" event (car plot-element) (car plot-set))) 
                                                                              ;(make-symbol (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                              (list 
                                                                               (intern (format nil "~A~A" (car plot-element) (car plot-set)))
                                                                               ;(make-symbol (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                               )) (format nil "~A" event)))))
                          (setq event-mapping-list (add-to-event-mapping-list event-mapping-list 
                                                                              (intern (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                              ;(make-symbol (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                              event (car plot-set)))
                          (setq plot-element-event-list (append plot-element-event-list (list 
                                                                                         (intern (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                                         ;(make-symbol (format nil "~A~A~A" event (car plot-element) (car plot-set)))
                                                                                         )))

                          ) (cadr plot-set))

) (cdr plot-element))

  (values (list atms-list event-mapping-list plot-element-event-list))
)

(defun make-atms-statment-outptut-string (atms-list &aux string)
  (setq string "")
  (mapcar #'(lambda (statement) (cond ((eq (caar statement) 'assum) (setq string (format nil "~A ((assum '~A) ~S ~S)~%" string (cadar statement) (cadr statement) (cadr statement) )))
                                      ((eq (caar statement) 'gen-n) (setq string (format nil "~A ((gen-n '~A '~A) ~S ~S)~%" string (cadar statement) (third (car statement)) (cadr statement) (cadr statement))))
                                      ((eq (caar statement) 'prem) (setq string (format nil "~A ((prem '~A) ~S ~S)~%" string (cadar statement) (cadr statement) (cadr statement) )))
                                      )
              )
                                      atms-list)
  (values string)
)

(defun make-contra-statement-output-string (contra-pairs &aux string)
  (setq string "")
  (mapcar #'(lambda (statement)  (setq string (format nil "~A ((contra '~A) \"~A\" \"~A\")~%" string statement statement statement))) contra-pairs)
  (values string)
)

(defun generate-atms-input (&optional (plot-element-list *test-plot-list*) &aux atms-list event-mapping-list res contras contra-pairs pairwise-list atms-list-string contra-pairs-string atms-file output contras-formatted)
  ;(setq plot-element-list *test-plot-list*)

  (setq atms-list nil)
  (setq event-mapping-list nil)

  ;;take each PE in turn
  (mapcar #'(lambda (plot-element) (setq res (process-plot-element plot-element event-mapping-list))
              (setq atms-list (append atms-list (car res)))
              (setq event-mapping-list (cadr res))) plot-element-list)

  ;;contra list
  (setq contras (generate-contradiction-list event-mapping-list))
  
  (setq contra-pairs nil)
  (mapcar #'(lambda (event-list) (setq contra-pairs (append (expand-contradictions event-list) contra-pairs))) contras)

  (setq contras-formatted (mapcar #'(lambda (contra) (list (list 
                                                                        'contra 
                                                                        ;(make-symbol (format nil "~A~A" 'contra (setq c (+ 1 c))))
                                                                        contra) (format nil "contra"))) contra-pairs))

  ;mapping-list
  (setq pairwise-list (generate-pairwise-mapping-list event-mapping-list))
  
  (setq atms-list-string (make-atms-statment-outptut-string atms-list))

  (setq contra-pairs-string (make-contra-statement-output-string contra-pairs))

  (setq atms-file (concatenate 'string atms-list-string contra-pairs-string))
  
  ;(values (list atms-list contras-formatted))
  (setq output (append  atms-list contras-formatted))
  
  (values output)
  ;(values (list pairwise-list atms-file atms-list event-mapping-list contra-pairs contras-formatted))

  ;(values (list contras event-mapping-list atms-list))


)

;(process-event-type-list '((RELATED (E1PE2RELATED)) (SOURCE (E1PE3SOURCE E1PE1SOURCE))))
(defun process-event-type-list (event-type-list &aux event-list)
  (setq event-list nil)
  (mapcar #'(lambda (event-type) (setq event-list (append event-list (cadr event-type)))) event-type-list)
  (values event-list)        
)

;(generate-flat-mapping-list '((E5 ((RELATED (E5PE4RELATED E5PE2RELATED)))) (E12 ((RELATED (E12PE4RELATED)))) (E1 ((RELATED (E1PE2RELATED)) (SOURCE (E1PE3SOURCE E1PE1SOURCE)))) (E11 ((CONSEQUENCE (E11PE3CONSEQUENCE)))) (E3 ((CONSEQUENCE (E3PE3CONSEQUENCE E3PE1CONSEQUENCE)))) (E10 ((SOURCE (E10PE3SOURCE)))) (E4 ((CONSEQUENCE (E4PE1CONSEQUENCE)))) (E2 ((SOURCE (E2PE1SOURCE))))))
(defun generate-pairwise-mapping-list (mapping-list &aux flattened-mapping pairwise-mappings)
  (setq flattened-mapping (mapcar #'(lambda (event) (list (car event) (process-event-type-list (cadr event)))) mapping-list))
  (setq pairwise-mappings nil)
  (mapcar #'(lambda (event) (mapcar #'(lambda (classified-event) (setq pairwise-mappings (cons (cons classified-event (car event)) pairwise-mappings))) (cadr event))) flattened-mapping)
  (values  pairwise-mappings)
)

;((E5 ((RELATED (E5PE4RELATED E5PE2RELATED)))) (E12 ((RELATED (E12PE4RELATED)))) (E1 ((RELATED (E1PE2RELATED)) (SOURCE (E1PE3SOURCE E1PE1SOURCE)))) (E11 ((CONSEQUENCE (E11PE3CONSEQUENCE)))) (E3 ((CONSEQUENCE (E3PE3CONSEQUENCE E3PE1CONSEQUENCE)))) (E10 ((SOURCE (E10PE3SOURCE)))) (E4 ((CONSEQUENCE (E4PE1CONSEQUENCE)))) (E2 ((SOURCE (E2PE1SOURCE)))))
(defun generate-contradiction-list (mapping-list &aux contra-list)
  (setq contra-list nil)
  (mapcar #'(lambda (event) (mapcar #'(lambda (event-set) (if (> (length (cadr event-set)) 1) (setq contra-list (cons (cadr event-set) contra-list))     )) (cadr event))) mapping-list)
  (values contra-list)
)

;(add-to-event-mapping-list '((e1  ((source (e1_1)) (consequence (el_2)))) (e2  ((related e2_1)))) 'e2_2 'e2 related)
;(add-to-event-mapping-list '((e1  ((source (e1_1))) (consequence (el_2))) (e2  ((related (e2_1)) (source (e2_2))))) 'e2_3 'e2 'related)
;(add-to-event-mapping-list '((e1  ((source (e1_1))) (consequence (el_2))) (e2  ((source (e2_2))))) 'e2_3 'e2 'related)
(defun add-to-event-mapping-list (event-mapping-list atms-event event event-classification &aux current current-event-type new-current-rest new-current new-event-type)
  (setq current (assoc event event-mapping-list))
  (setq current-event-type (assoc event-classification (cadr current)))
  (setq new-event-type (list event-classification (cons atms-event (cadr current-event-type))))
  (setq new-current-rest (remove current-event-type (cadr current) :test #'equal))
  (setq new-current (list event (cons new-event-type new-current-rest)))
  
  (setq event-mapping-list (remove current event-mapping-list :test #'equal))

  (setq event-mapping-list (cons new-current event-mapping-list))
;  (values current current-event-type new-event-type new-current-rest new-current event-mapping-list)

)



;;my text data
(setq *txt* '(((assum 'X1) "Event 1")
((assum 'X2) "Event 2a")
((assum 'X3) "Event 2b")
((prem 'X4) "Event 3a")
((assum 'X5) "Event 3b")
((assum 'X6) "Event 4")
((gen-n	'X7 '(X1)) "Consequence 1")
((gen-n	'X8 '(X2)) "Source 1")
((gen-n	'X9 '(X3)) "Source 2")
((gen-n	'X10 '(X4)) "Consequence 2")
((gen-n	'X11 '(X5)) "Consequence 3")
((gen-n	'X12 '(X6)) "Source 3")
((gen-n	'X13 '(X7 X8)) "Plot 1")
((gen-n	'X14 '(X9 X10)) "Plot 2")
((gen-n	'X15 '(X11 X12)) "Plot 3")
((contra '(X2 X3)) "Source 1 and Source 2 share an event")
((contra '(X4 X5)) "Consequence 2 and Consequence 3 share an event")))


