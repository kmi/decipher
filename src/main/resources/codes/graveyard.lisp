;;;; THESE FUCNTIONS MAY NOT BE USED NOW

(defun get-stories (param &aux section-list stories story-list) ; param is 'all or 'last
 (setq section-list (mapcar #'car *sections*))
 (setq story-list nil)
 (dolist (section section-list)
  (setq stories nil)

  (setq stories (create-from-section param section))

  (format t "~%~%~a" stories)
  (push stories story-list)
)
(print-stories story-list)

)

(defun print-stories (story-list)
 (dolist (story story-list)
   (print-story story)))

(defun print-story (story)
  (setq linear (car story))
  (setq layered (cadr story))
  (setq section (caar story))
  (setq section-title (cadr (find section *lookup* :key #'car)))
  (format t "~%~%SECTION TITLE: ~a" section-title)
  (dotimes (i (- (length linear) 1)) ; print linear story
    (if (< i 5)
        (progn (setq title (cadr (find (nth i (cdr linear)) *lookup* :key #'car)))
               (format t "~%~a" title))))
;;; (format t "~%~%LAYERED STORIES")
;;;(dolist (l layered)
;;;   (setq top-level (cadr (find (car l) *lookup* :key #'car)))
;;;   (format t "~%topnode: ~a" top-level)
;;;   (dotimes (j (length (cdr l)))
;;;   (if (< j 4)
;;;      (progn
;;;         (setq next-level (cadr (find (nth j (cdr l)) *lookup* :key #'car)))
;;;         (format t "~%next: ~a" next-level)))
;;;  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: create-from-section
;;; INPUT: sections - a list of story sections (generated from hill-climbing output)
;;; OUTPUT: a list of (linear output, layered output, multiroute output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(create-narrative-structures 'nid *sections*)

(defun create-from-section (param section &aux start-node best-sections layered-paths unused-sections current-eval next-node current-node eval next-cosine next-coverage next-object-coverage merged-node linear layered)
  (setq start-node (find (list section) *nodes* :key #'node-sections :test #'equal))
   (cond 
    ((equal param 'all) (setq best-sections (hill-climbing start-node)))
    ((equal param 'last) (setq best-sections (hill-climbing-last start-node))))

   (setq linear (node-sections best-sections))
 
   (setq layered-paths (mapcar (lambda (x) (find (list x) *nodes* :key #'node-sections :test #'equal)) (node-sections best-sections))) ; make a list of node structures (search in *nodes*) for each section in the backbone
   (setq unused-sections (copy-list *nodes*)) ; then make a list of nodes for currently unused sections

   (mapcar (lambda (x) (setq unused-sections (remove (list x) unused-sections :key #'node-sections :test #'equal))) (node-sections best-sections))
   ;;; version to use all unused-sections
   (loop while unused-sections ; while there are unused-sections
        do (setq eval 0)
     do  (loop for i from 0 to (- (length unused-sections) 1) ; for every unused section
           do (loop for j from i to (- (length layered-paths) 1) ; against each layered-path
           do (setq current-eval (evaluate-next-node (nth i unused-sections) (nth j layered-paths)))  ; get the weighted score for merging the two sections
           do (if (> (fourth current-eval) eval) ; if the eval score is the highest, then set up the parameters for merging.
                  (progn 
                    (setq next-node (nth i unused-sections))
                    (setq current-node (nth j layered-paths))
                    (setq eval (fourth current-eval))
                    (setq next-cosine (first current-eval))
                    (setq next-coverage (second current-eval))
                    (setq next-object-coverage (third current-eval))))))
     ;;; then merge the best nodes
        (setq merged-node (new-current-node next-node current-node eval next-cosine next-coverage next-object-coverage)) ; this is a new merged-node
   
  ;;; UNCOMMENT THE IF STATEMENT TO ONLY EXPAND PATH WHERE IT INCREASES WEIGHTED SCORE
      ;  (if (>= eval (node-weighted-score current-node))
        ;;; replace it with the previous item in the layered-paths
       (setq layered-paths (substitute merged-node (node-sections current-node) layered-paths :key #'node-sections :test #'equal))
      ;   )
       ;;; and remove the node from unused
       (setq unused-sections (remove (node-sections next-node) unused-sections :key #'node-sections :test #'equal))
      )
   (setq layered (mapcar #'node-sections layered-paths))
  
   (list linear layered (textual-summary (list best-sections)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE NARRATIVE STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: create-narrative-structure
;;; INPUT: sections - a list of story sections (generated from hill-climbing output); type - one of 'linear 'layered or 'multi
;;; OUTPUT: an ordered list of story sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(create-narrative-structure 'layered)

(defun create-narrative-structure (type &aux best-sections layered-paths unused-sections next-node current-node eval next-cosine next-coverage next-object-coverage current-eval merged-node )
(cond
  ((equal type 'linear)
   (setq best-sections (car (sort (copy-list (random-hill-climbing 3)) #'> :key #'node-weighted-score))) ; use random hill-climbing with n=3 to find the best path and take the first result
   (node-sections best-sections)
   )
 ((equal type 'layered)
   (setq best-sections (car (random-hill-climbing 3))) ; best-sections is a node-structure
   (setq layered-paths (mapcar (lambda (x) (find (list x) *nodes* :key #'node-sections :test #'equal)) (node-sections best-sections))) ; make a list of node structures (search in *nodes*) for each section in the backbone
   (setq unused-sections (copy-list *nodes*)) ; then make a list of nodes for currently unused sections
   (mapcar (lambda (x) (setq unused-sections (remove (list x) unused-sections :key #'node-sections :test #'equal))) (node-sections best-sections))
   ;;; version to use all unused-sections
   (loop while unused-sections ; while there are unused-sections
        do (setq eval 0)
     do  (loop for i from 0 to (- (length unused-sections) 1) ; for every unused section
           do (loop for j from i to (- (length layered-paths) 1) ; against each layered-path
           do (setq current-eval (evaluate-next-node (nth i unused-sections) (nth j layered-paths)))  ; get the weighted score for merging the two sections
           do (if (> (fourth current-eval) eval) ; if the eval score is the highest, then set up the parameters for merging.
                  (progn 
                    (setq next-node (nth i unused-sections))
                    (setq current-node (nth j layered-paths))
                    (setq eval (fourth current-eval))
                    (setq next-cosine (first current-eval))
                    (setq next-coverage (second current-eval))
                    (setq next-object-coverage (third current-eval))))))
     ;;; then merge the best nodes
        (setq merged-node (new-current-node next-node current-node eval next-cosine next-coverage next-object-coverage)) ; this is a new merged-node
   
  ;;; UNCOMMENT THE IF STATEMENT TO ONLY EXPAND PATH WHERE IT INCREASES WEIGHTED SCORE
      ;  (if (>= eval (node-weighted-score current-node))
        ;;; replace it with the previous item in the layered-paths
       (setq layered-paths (substitute merged-node (node-sections current-node) layered-paths :key #'node-sections :test #'equal))
      ;   )
       ;;; and remove the node from unused
       (setq unused-sections (remove (node-sections next-node) unused-sections :key #'node-sections :test #'equal))
      )
   (mapcar #'node-sections layered-paths)
  )
  ((equal type 'multi)
   (setq best-sections (remove-duplicates (random-hill-climbing 6) :test #'equal)) ;;; create a maximmum of 6 coherent sets of sections using random hill climbing
   (mapcar #'node-sections best-sections)
  )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: create-narrative-structures
;;; INPUT: sections - a list of story sections (generated from hill-climbing output)
;;; OUTPUT: a list of (linear output, layered output, multiroute output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(create-narrative-structures *sections*)

(defun create-narrative-structures (&aux best-sections linear layered-paths unused-sections next-node current-node eval current-eval next-cosine next-coverage next-object-coverage merged-node layered multi )
   (setq best-sections (random-hill-climbing 6)) ; use random hill-climbing with n=3 to find the best path and take the first result
   (setq linear (node-sections (car (sort (copy-list best-sections) #'> :key #'node-weighted-score))))
   (setq layered-paths (mapcar (lambda (x) (find (list x) *nodes* :key #'node-sections :test #'equal)) linear)) ; make a list of node structures (search in *nodes*) for each section in the backbone
   (setq unused-sections (copy-list *nodes*)) ; then make a list of nodes for currently unused sections
   (mapcar (lambda (x) (setq unused-sections (remove (list x) unused-sections :key #'node-sections :test #'equal))) (node-sections best-sections))
   ;;; version to use all unused-sections
   (loop while unused-sections ; while there are unused-sections
        do (setq eval 0)
     do  (loop for i from 0 to (- (length unused-sections) 1) ; for every unused section
           do (loop for j from i to (- (length layered-paths) 1) ; against each layered-path
           do (setq current-eval (evaluate-next-node (nth i unused-sections) (nth j layered-paths)))  ; get the weighted score for merging the two sections
           do (if (> (fourth current-eval) eval) ; if the eval score is the highest, then set up the parameters for merging.
                  (progn 
                    (setq next-node (nth i unused-sections))
                    (setq current-node (nth j layered-paths))
                    (setq eval (fourth current-eval))
                    (setq next-cosine (first current-eval))
                    (setq next-coverage (second current-eval))
                    (setq next-object-coverage (third current-eval))))))
     ;;; then merge the best nodes
        (setq merged-node (new-current-node next-node current-node eval next-cosine next-coverage next-object-coverage)) ; this is a new merged-node
   
  ;;; UNCOMMENT THE IF STATEMENT TO ONLY EXPAND PATH WHERE IT INCREASES WEIGHTED SCORE
      ;  (if (>= eval (node-weighted-score current-node))
        ;;; replace it with the previous item in the layered-paths
       (setq layered-paths (substitute merged-node (node-sections current-node) layered-paths :key #'node-sections :test #'equal))
      ;   )
       ;;; and remove the node from unused
       (setq unused-sections (remove (node-sections next-node) unused-sections :key #'node-sections :test #'equal))
      )
     (setq layered (mapcar #'node-sections layered-paths))
  
     (setq multi (mapcar #'node-sections best-sections))

   (list linear layered multi)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graveyard for functions that aren't called anywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMOVE-NTH : THIS FUNCTION DOESN'T SEEM TO BE CALLED ANYWHERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: `remove-nth'
;;; INPUT: 
;;; OUTPUT: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEEP DUPLICATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: keep-duplicates
;;; INPUT: a list
;;; OUTPUT: another list that only includes duplicated items in the input
;;; i.e. this is the opposite behavior to the function `delete-duplicates'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keep-duplicates (list)
  (let (result)
    (loop while list do
         (let ((elt (car list)))
           (when (and (member elt (cdr list))
                      (not (member elt result)))
             (setq result (nconc result (list elt)))))
         (setq list (cdr list)))
    result))

(defun textual-summary-old (paths)
  (let* ((node-events-list (mapcar #'node-events paths))
         (events-basis *basis*)
         (events-summary
          ;; for each node...
          (mapcar 
           ;; examine the events...
           (lambda (event-sublist)
             ;; and for each event, merge the results...
             (keep-duplicates-count
              (mapcan (lambda (event)
                        ;; return a list of the matching BASIS items
                        (mapcan (lambda (item base)
                                  (when (eq item 1)
                                    (list base))) (second event) events-basis))
                      event-sublist)))
           node-events-list)))
    ;; Combine the `events-summary' with some of the things that were
    ;; calculated about the nodes
    (mapcar #'list            
            events-summary
            (mapcar (lambda (x) 
                      (let ((coverage (node-coverage x)))
                        (if (eq coverage 1)
                            (setq coverage "FULL"))
                        (format nil "Coverage: ~A" coverage)))
                      paths)
            (mapcar (lambda (x) 
                      (let ((object-coverage (node-object-coverage x)))
                        (if (eq object-coverage 1)
                            (setq object-coverage "FULL"))
                        (format nil "Object-Coverage: ~A" object-coverage)))
                    paths))))
