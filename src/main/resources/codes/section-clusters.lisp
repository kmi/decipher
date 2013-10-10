;;; A node is used to maintain a search path and the attributes
;;; associated with that path. A path is a list of story
;;; sections. Events and objects is a count of the number of events or
;;; objects associated with the whole path. Cosine similarity is a
;;; measure of coherence across the path, the two coverage values are
;;; a percentage of events and objects compared to the number across
;;; all available story sections. The weighted score is a weighted
;;; total for the path.

(defstruct node sections events objects cosine-similarity coverage object-coverage weighted-score)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSLATE TO NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: translate-to-node
;;; INPUT: section - a list constructed as (section-id (list-of-event-vectors) (list-of-object-nids))
;;; OUTPUT: a node structure where the initial path contains just the input story section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-to-node (section &aux section-id events objects cohesion coverage object-coverage weighted-score)
;;; set up initial parameters
  (setq weighted-score 0)
  (setq section-id (car section))
  (setq events (cadr section))
  (setq objects (caddr section))
;;; if there are events, calculate the cohesion across all events and then the coverage
  (if (not (null events))
  (setq cohesion (mean-similarities-of-cluster events))
  (setq cohesion 0))
  (if (not (null events))
  (setq coverage (/ (length events) *event-total*))
  (setq coverage 0))
;;; if there are objects, calculate the object-coverage
  (if (not (null objects))
  (setq object-coverage (/ (length objects) *object-total*))
  (setq object-coverage 0))
;;; calculate the weighted score according to the *weights* variable
  (setq weighted-score  (+
                         (* cohesion (first *weights*))
                         (* coverage (second *weights*))
                         (* object-coverage (third *weights*))))
;;; and make a node structure to store the data for this story section, with the story section as the only item in the sections path
  (make-node :sections (list (car section)):events (cadr section) :objects (caddr section)
             :cosine-similarity cohesion :coverage coverage :object-coverage object-coverage :weighted-score weighted-score
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MEAN SIMILARITIES OF CLUSTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: mean-similarities-of-cluster
;;; INPUT: events - a list of event vectors in the form '((E1 (1 0 1 0 1 1)) (E2 (1 0 0 0 1 0)) (E4 (1 0 1 1 1 1)))
;;; OUTPUT: a numerical value - the calculated cosine similarity across the input events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mean-similarities-of-cluster (events &aux similarities total-similarity len sim)
  (cond ((= (length events) 1) (setq sim 1)) ;if there is only one event then the similarity is 1
        (t
         (setq similarities (similarities-of-cluster events)) ; otherwise call the function similarities on the list of events, this returns a list of numbers
         (setq total-similarity (reduce #'+ similarities))
         (setq len (length similarities))
         (setq sim (/ total-similarity len)))) ; divide the sum of the similarity scores by the number of similarity scores.
  (values sim) ; return this value
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIMILARITIES OF CLUSTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: similarities-of-cluster
;;; INPUT: events - a list of event vectors in the form '((E1 (1 0 1 0 1 1)) (E2 (1 0 0 0 1 0)) (E4 (1 0 1 1 1 1)))
;;; OUTPUT: a list of similarity scores comparing each event with all others.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun similarities-of-cluster (events &aux head rest)
  (cond ((null events) (values events)) ; if there are no events return nil
  ((setq head (car events))
  (setq rest (cdr events))
  (append (mapcar #'(lambda (rest-event)
                      (cosine-similarity (cadr head) (cadr rest-event))) rest) (similarities-of-cluster (cdr events))))) ; otherwise calculate cosine similarity between all event vectors
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPARE CLUSTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: compare-clusters
;;; INPUT: c1 and c2 - both lists of event vectors, in the form '((1 2 3 4)(3 1 2 4)) '((4 3 2 1)(4 1 3 2)))
;;; OUTPUT: a numerical value - the mean of the distance between the clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-clusters (c1 c2 &aux distance-list sum len mean distance)
  (setq distance-list nil)
  (dolist (c1-item c1) ; for each event in cluster 1
           (dolist (c2-item c2)
             (setq distance (cosine-similarity c1-item c2-item)) ; calculate the cosine similarity between this event and each event in cluster 2, in turn
             (setq distance-list (cons distance distance-list)))) ; and add this distance to a list of all distances
  (setq sum (eval (cons '+ distance-list))) ; when this is done, add up all the distance values
  (setq len (length distance-list))
  (setq mean (/ sum len)) ; then divide the distance values by the number of distances to get the mean
  (values mean)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPARE EVENT LISTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: compare-events-lists
;;; INPUT: c1 and c2 - both lists of event vectors, in the form '((e1 (1 0 1 0 1 1)) (e2 (1 0 0 0 1 0))) ((e4 (1 0 1 1 1 1)) (e5 (1 1 1 0 1 0))))
;;; OUTPUT: a numerical value - the mean distance between two set of events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-event-lists (c1 c2 &aux distance distance-list sum len mean)
  (setq distance-list nil)
  (dolist (c1-item c1) ; for each event in cluster 1
           (dolist (c2-item c2)
             (setq distance (cosine-similarity (cadr c1-item) (cadr c2-item))) ; calculate the cosine similarity between this event and each event in cluster 2, in turn
             (setq distance-list (cons distance distance-list)))) ; and add this distance to a list of all distances
  (setq sum (eval (cons '+ distance-list)))  ; when this is done, add up all the distance values
  (setq len (length distance-list))
  (setq mean (/ sum len)) ; then divide the distance values by the number of distances to get the mean
  (values mean)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPARE ALL CLUSTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: compare-all-clusters
;;; INPUT: cluster-list - a list of clusters in the form '(((c1) ((1 2 3 4)(3 1 2 4))) ((c2) ((4 3 2 1)(4 1 3 2))) ((c3) ((1 4 3 2)(2 3 4 1)))))
;;; OUTPUT: a list of comparison values between all clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-all-clusters (cluster-list &aux head rest comparisons)
  (setq comparisons nil)
  (cond (cluster-list
         (setq head (car cluster-list))
         (setq rest (cdr cluster-list))
         (mapc #'(lambda (cluster) (setq comparisons (cons (list (compare-clusters (cadr cluster) (cadr head)) cluster head) comparisons))) rest)
         (setq comparisons (append comparisons (compare-all-clusters rest))))) ; run compare-clusters on every possible pair of clusters
  (values comparisons) ; return a list of the values
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOSEST CLUSTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: closest-clusters
;;; INPUT: cluster-list - a list of clusters in the form '(((c1) ((1 2 3 4)(3 1 2 4))) ((c2) ((4 3 2 1)(4 1 3 2))) ((c3) ((1 4 3 2)(2 3 4 1)))))
;;; OUTPUT: the highest value obtained by running compare-all-clusters on the cluster list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun closest-clusters (cluster-list &aux res new-res)
  (setq res (compare-all-clusters cluster-list)) ; the output of the function compare-all-clusters
  (setq new-res (sort res #'(lambda (x y) (> (car x) (car y))))) ; sorted in ascending order
  (values (car new-res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RANDOM HILL CLIMBING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: random-hill-climbing
;;; INPUT: iterations - the number of iterations of hill climbing
;;; OUTPUT: a list of paths produced from each iteration of calling the hill climbing on a random start point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;carries out hill climbing from a random start point for the number of times specified
;iterations must be smaller than the number of sections from which you can start
(defun random-hill-climbing (iterations &aux untested-nodes produced-paths node path)
  (setq untested-nodes *nodes*) ;; make a copy of all the nodes
  (setq produced-paths nil)
  (if (> iterations (length *nodes*))
      (setq iterations (length *nodes*))) ; if an iteration value is given that is longer than the number of sections, reduce the number of iterations to match the section number
  (loop
   (if (< iterations 1)
       (return)) ; end when all iterations are complete
   (setq node (car (subseq untested-nodes (random iterations)))) ; take the next untested node from the list of untested nodes and ake it a start point for hill climbing
   (setq untested-nodes (remove node untested-nodes)) ; remove it from the untested node list
   (setq iterations (- iterations 1)) ; adjust the iteration value
   (setq path (hill-climbing node)) ; perform hill-climbing from the current start-point
   (setq produced-paths (cons path produced-paths))) ; store the path

  (values produced-paths) ; return the paths
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HILL CLIMBING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: hill-climbing
;;; INPUT: start-node - a node structure
;;; OUTPUT: the best path resulting from the hill-climbing algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hill-climbing (start-node &aux current-node neighbours next-eval next-node current-eval neighbour-eval)
  (setq current-node start-node)
  (loop while t do
   (setq neighbours (get-neighbours current-node)) ; find the sections not used in the current node-path
;;; reset parameters
   (setq next-eval 0)
   (setq next-node nil)
   (setq next-coverage 0)
   (setq next-object-coverage 0)
   (setq next-cosine 0)
  ;; (setq current-eval (node-cosine-similarity current-node))
   (setq current-eval (node-weighted-score current-node)) ; find the weighted-score for the current node

   (dolist (item neighbours)
         (setq neighbour-eval (evaluate-next-node item current-node)) ; evaluate each section in turn against the current path
         (cond ((> (fourth neighbour-eval) next-eval) ;; if the returned weighted score is greater than the next-eval
       ;;; set up the values for the variables
                (setq next-node item)
                (setq next-eval (fourth neighbour-eval))
                (setq next-cosine (first neighbour-eval))
                (setq next-coverage (second neighbour-eval))
                (setq next-object-coverage (third neighbour-eval))))
   )
   (if (<= next-eval current-eval)  ; when the overall weighted score is not better than the current score then simply return the current node
       (return current-node))
;;; merge the new story section including events and objects into the current node and store the new values
   (setq current-node (new-current-node next-node current-node next-eval next-cosine next-coverage next-object-coverage))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW CURRENT NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: new-current-node
;;; INPUT: new-node - a node structure from *sections*, current-node - a node structure built through hill-climbing, new-* - a set of scores for merging into the current-node
;;; OUTPUT: adds together the sections and events of two node-structures and returns a node-structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-current-node (new-node current-node new-eval new-cosine new-coverage new-object-coverage &aux new-events current-events new-sections current-sections new-objects current-objects)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
  (setq new-sections (node-sections new-node))
  (setq current-sections (node-sections current-node))
  (setq new-objects (node-objects new-node))
  (setq current-objects (node-objects current-node))
;;; make the new node structure
  (make-node :cosine-similarity new-cosine :coverage new-coverage :object-coverage new-object-coverage
             :weighted-score new-eval :sections (append new-sections current-sections) :events (append new-events current-events)
             :objects (append new-objects current-objects))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET NEIGHBOURS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: get-neighbours
;;; INPUT: current-node - a node structure
;;; OUTPUT: a list of nodes (each reresenting a story section) that are currently unused in the current-node path (node-section-list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-neighbours (current-node &aux all-nodes sections)
  (setq all-nodes *nodes*)
  (setq sections (node-sections current-node))
  (dolist (section sections)
    (setq all-nodes (remove-if #'(lambda (x) (equal section (car (node-sections x)))) all-nodes)) ; remove nodes that are already in the path
    )
  (values all-nodes)  ; return what is left
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATE NEXT NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: evaluate-next-node
;;; INPUT: new-node - a node structure, current-node - a node structure built through hill-climbing
;;; OUTPUT: the scores obtained comparing one node to the other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-next-node (new-node current-node &aux coherence coverage objects weighted-score)
 (setq coherence (evaluate-next-node-coherence new-node current-node)) ; find the coherence value if both input nodes are merged
 (if (> *event-total* 0) (setq coverage (evaluate-next-node-coverage new-node current-node)) (setq coverage 0)) ; if there are events in story sections, calculate the event-coverage obtained by merging the input nodes
 (if (> *object-total* 0) (setq objects (evaluate-next-node-objects new-node current-node)) (setq *object-total* 0)) ; and the same for merging objects.
 (setq weighted-score (+
       (* coherence (first *weights*))
       (* coverage (second *weights*))
       (* objects (third *weights*)))) ; calculate the weighted score
 (values (list coherence coverage objects weighted-score))
)

;this is the function that combines the two clusters plus the "marriage"
; i.e. A1*Q1 + A2*Q2 + A3(R1*R2) / Q1 + Q2 + (R1*R2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATE NEXT NODE COVERAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: evaluate-next-node-coverage
;;; INPUT: new-node - a node structure, current-node - a node structure built through hill-climbing
;;; OUTPUT: the coverage score obtained comparing one node to the other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-next-node-coverage (new-node current-node &aux new-events current-events)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
 ;;; put code to remove duplicate events here, if wanted
  (/ (length (append new-events current-events)) ; divide the number of events across the two nodes by the total number of events in all sections
   *event-total*)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATE NEXT NODE OBJECTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: evaluate-next-node-objects
;;; INPUT: new-node - a node structure, current-node - a node structure built through hill-climbing
;;; OUTPUT: the object-coverage score obtained comparing one node to the other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-next-node-objects (new-node current-node &aux new-objects current-objects)
  (setq new-objects (node-objects new-node))
  (setq current-objects (node-objects current-node))
 ;;; put code to remove duplicate objects here, if wanted
  (/ (length (append new-objects current-objects)) ; divide the number of objects across the two nodes by the total number of objects in all sections
   *object-total*)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATE NEXT NODE COHERENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: evaluate-next-node-coherence
;;; INPUT: new-node - a node structure, current-node - a node structure built through hill-climbing
;;; OUTPUT: the coherence score obtained comparing one node to the other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-next-node-coherence (new-node current-node &aux new-events current-events new-coherence current-coherence comparison-coherence)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
  (setq new-coherence (node-cosine-similarity new-node)) ; find the cosine similarity of the new node
  (setq current-coherence (node-cosine-similarity current-node)) ; and of the current node
  (setq comparison-coherence (compare-event-lists new-events current-events)) ; then create a comparison coherence from both sets of events
;;; from this calculate the coherence of the merged nodes
  (/ (+ (* new-coherence (number-of-pairs (length new-events))) (* current-coherence (number-of-pairs (length current-events))) (* comparison-coherence (* (length new-events) (length current-events))))
     (+ (number-of-pairs (length new-events)) (number-of-pairs (length current-events)) (* (length new-events) (length current-events)))))  ;this does R(R-1)/2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NUMBER OF PAIRS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: number-of-pairs
;;; INPUT: num
;;; OUTPUT: the number of pairs in the event set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-of-pairs (num)
  (/ (* num (- num 1)) 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COSINE SIMILARITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: cosine-similarity
;;; INPUT: vec-one and vec-two - event vectors in the form '(1 1 1 0 1) '(1 1 1 1 1)
;;; OUTPUT: the cosine similarity between the two vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cosine-similarity (vec-one vec-two &aux vec-one-mag vec-two-mag)
  (setq vec-one-mag (mag vec-one))
  (setq vec-two-mag (mag vec-two))
;;; calculateand return the cosine similarity between the two vectors
  (if (zerop vec-one-mag)
      (setq vec-one-mag 0.00001))
  (if (zerop vec-two-mag)
      (setq vec-two-mag 0.00001))
  (/ (dot vec-one vec-two)
     (* vec-one-mag vec-two-mag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: dot
;;; INPUT: vec-one and vec-two - event vectors in the form '(1 1 1 0 1) '(1 1 1 1 1)
;;; OUTPUT: the dot product of the two vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dot (vec-one vec-two &aux result)
  (setq result 0)
  (dotimes (i (length vec-one))
    (if (numberp (nth i vec-one))
	  (setq result (+ result
			  (* (nth i vec-one)
			     (nth i vec-two))))))
   (values result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: mag
;;; INPUT: vector - event vector in the form '(1 1 1 0 1)
;;; OUTPUT: the magnitude of the vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mag (vector &aux mag)
  (setq mag 0)
  (dolist (val vector)
    (if (numberp val)
        (setq mag (+ mag (* val val)))))
  (sqrt mag)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE NARRATIVE STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: create-narrative-structure
;;; INPUT: sections - a list of story sections (generated from hill-climbing output); type - one of 'linear 'layered or 'multi
;;; OUTPUT: an ordered list of story sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(create-narrative-structure *sections* 'linear)

(defun create-narrative-structure (sections type &aux best-sections ordered-sections result)
(cond
  ((equal type 'linear)
   (setq best-sections (car (random-hill-climbing 3))) ; use random hill-climbing with n=3 to find the best path and take the first result
   (if (> (length (node-sections best-sections)) 1) ; if there is more than one section in the returned path
   (setq ordered-sections (organise-sections (node-sections best-sections))) ; then organise the sections
   (setq ordered-sections (node-sections best-sections)) ; otherwise return the one section
   ))
 ((equal type 'layered)
   (setq best-sections (car (random-hill-climbing 3)))
  ;;; functions to create layered structure
     )
  ((equal type 'multi)
   (setq result nil)
   (setq best-sections (random-hill-climbing 6)) ;;; create a maximmum of 6 coherent sets of sections using random hill climbing
   (setq n (length best-sections)) ; find out how many sets were returned
   (dotimes (i n (remove-duplicates result :test #'equal)) ;loop through the section sets (having removed any duplicates)
    (if (> (length (node-sections (nth i best-sections))) 1) ; if there is more than one section in the set
    (push (organise-sections (node-sections (nth i best-sections)))  result) ; organise the sections and push the section path into the result
    (push (node-sections (nth i best-sections)) result)) ; otherwise just push the section into the result
   )
  )
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORGANISE STORY SECTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: organise-sections
;;; INPUT: section-list - a list of story sections (generated from hill-climbing output)
;;; OUTPUT: an ordered list of story sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun organise-sections (section-list &aux start-list unused-sections similarity-scores ordered-sections)
 (setq unused-sections section-list) ; the full list of sections created by hill-climbing
 (setq similarity-scores (get-pairwise-similarity section-list)) ; the pairwise similarity for all items in the section list
  (setq ordered-sections (car (stable-sort (copy-list similarity-scores) #'> :key #'third))) ; order the sections with best first
  (setq unused-sections (remove (car ordered-sections) unused-sections :test #'equal)) ; remove the first item in ordered-sections from the unused-sections, as this becomes the starting point
  (setq unused-sections (remove (cadr ordered-sections) unused-sections :test #'equal)) ; remove the second item in ordered-sections from the unused-sections, as this becomes 2nd in the path

    (do
    ((start-list (list (car ordered-sections) (cadr ordered-sections))) ; create the start-list from the ordered-sections
     (unused unused-sections))
    ((null unused) start-list) ;; if all sections are used, return result
     ;;; otherwise
    (progn
         (setq next-best (cadr (find-best-score (cadr start-list) unused similarity-scores))) ; find the next-best section according to the similarity score
           ;;; then add the section to the ordered-sections and remove it from unused-sections
        (setq unused (remove next-best unused))
        (setq start-list (append start-list (list next-best)))
       ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND BEST SCORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: find-best-score
;;; INPUT: section-id - a nid; unused - a list of story-sections generated from hill-climbing; scores - the result of finding pairwise-similariry on a section list
;;; OUTPUT: the section-id which has the best similarity to the end of the story-path and which is not already a member of the path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-best-score (section-id unused scores &aux result best)
(setq result nil)
(dolist (x scores) ; loop through the scores
 (if (and (equal section-id (car x)) (member (cadr x) unused)) (push x result))) ; find all the scores relating to the section
  (setq best (car (stable-sort (copy-list result) #'> :key #'third)))) ; and return the best one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET PAIRWISE SIMILARITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: get-pairwise-similarity
;;; INPUT: section-list - a list of secion nids
;;; OUTPUT: a list of pairwise similarity scores comparing each section against all others, in the form (nid nid score)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-pairwise-similarity (section-list &aux result)
(setq result nil)
(loop for i from 0 to (- (length section-list) 1) ; for every section
 do (loop for j from i to (- (length section-list) 1) ; against every other sections
    when (not (equal i j)) ; unless they are the same
      do (progn
;;; calculate the similarity and store in result
        (push (list (nth i section-list) (nth j section-list) (compare-event-lists (cadr (find (nth i section-list) *sections* :key #'car))
                                                                                                  (cadr (find (nth j section-list) *sections* :key #'car)))) result)
        (push (list (nth j section-list) (nth i section-list) (compare-event-lists (cadr (find (nth i section-list) *sections* :key #'car))
                                                                                                  (cadr (find (nth j section-list) *sections* :key #'car)))) result))))
 result)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC SUMMARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: generic-summary
;;; INPUT: the output of `random-hill-climbing'
;;; OUTPUT: a more readable summary of the data that appears, showing only the duplicated
;;; items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generic-summary (paths)
  (let* ((node-events-list (mapcar #'node-events paths))
         (events-basis (mapcar (let ((n 0)) (lambda (x) (setq n (1+ n)) (format nil "A~A" n)))
                               (second (first (first node-events-list))))))
    ;; for each node...
    (mapcar 
     ;; examine the events...
     (lambda (event-sublist)
       ;; and for each event, merge the results...
       ;; sorted based on numerical identifier
       (sort
        (keep-duplicates-count
         (mapcan (lambda (event)
                   ;; return a list of the matching basis items
                   (mapcan (lambda (item base)
                             (when (eq item 1)
                               (list base))) (second event) events-basis))
                 event-sublist)
         ;; set a lower bound for how many duplicate items are needed to be interesting
         5)
        (lambda (x y) (< (parse-integer (subseq x 1)) (parse-integer (subseq y 1))))))
     node-events-list)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEEP DUPLICATES, COUNTING NUMBER OF DUPLICATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: keep-duplicates-count
;;; INPUT: a list
;;; OUTPUT: an alist that only includes duplicated items together with their frequency
;;; Alternate treatment to the simpler function above, use `multiple-value-bind'
;;; to get the frequency data.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keep-duplicates-count (list &optional (min-accept 1))
  (let ((rv (make-hash-table :test #'equal)))
    (mapcar (lambda (x) (let ((place (gethash x rv)))
                          (if place
                              (setf (gethash x rv) (1+ place))
                              (setf (gethash x rv) 1))))
            list)
    (let ((counts (loop for key being the hash-keys of rv
                       when (> (gethash key rv) min-accept)
                     collect (cons key (gethash key rv)))))
         (sort counts
               (lambda (a b) (> (cdr a) (cdr b))))
         ;; Maybe there would be a way to get away with less looping...
         (loop for (a . b) in counts
               collect a into keys
               collect b into vals
               finally (return (values keys vals))))))

;; try - (convert-input-to-sections-format *sample-three*)
(defun convert-input-to-sections-format (input)
  (mapcar #'(lambda(x) (list (car x) (cdr (caadr x)) (caddr x))) (cddr input)))