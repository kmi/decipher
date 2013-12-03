(in-package "COMMON-LISP-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A basic internal object for keeping track of computations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node 
  "A node is used to maintain a search path and the attributes associated with that path.
A path is a list of story sections.  Events and objects is a count of
the number of events or objects associated with the whole path.
Cosine similarity is a measure of coherence across the path, the two
coverage values are a percentage of events and objects compared to the
number across all available story sections.  The weighted score is a
weighted total for the path."  
sections events objects cosine-similarity coverage object-coverage weighted-score)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code for pre-processing the input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERT INPUT TO SECTIONS FORMAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: convert-input-to-sections-format
;;; INPUT: format as with *sample-three*
;;; OUTPUT: strips off the "basis vector" and coding of its components
;;;         to create something simpler
;;; EXAMPLE: (convert-input-to-sections-format *sample-three*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-input-to-sections-format (input)
  (mapcar #'(lambda (x) 
              (list (car x) (cdr (caadr x)) (caddr x)))
          (cdddr input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REDUCE VECTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: reduce-vector
;;; INPUT: the output of `convert-input-to-sections-format'
;;; OUTPUT: a more succinct version of the input data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reduce-vector (sections &aux merged-vec vec-num vec-ln count index-list reduced section-nid event-list objects)
  (setq reduced nil)
  (dolist (section sections)
    (dolist (event (cadr section))
      ;; get the events out of each section into one merged list
      (push (cadr event) merged-vec))) 
  ;; find the length of a vector
  (setq vec-ln (length (car merged-vec))) 
  ;; find the number of vectors across all sections
  (setq vec-num (length merged-vec)) 
  ;; for each position in the vector
  (loop for i from 0 to (- vec-ln 1) 
     do (setq count 0)
     ;; look at each vector in turn
     do (loop for j from 0 to (- vec-num 1) 
           ;; and if there is a 1 in that position of the vector, add 1 to the count
           do (if (equal (nth i (nth j merged-vec)) 1) (setq count (+ 1 count)))) 
     ;; if the count is less than 2, then add this index to list of columns to remove
     do (if (< count 2) (push i index-list))) 

  ;; then for each section...
  (dolist (section sections) 
    (setq section-nid (car section)) 
    (setq objects (third section))
    (setq event-list nil)
    ;; loop over the events in this section, and remove the unwanted elements
    (dolist (event (cadr section)) 
      (push (list (car event) (remove-indexes index-list (cadr event))) event-list))
    (push (list section-nid event-list objects) reduced))
  (values 
   reduced
   index-list)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMOVE INDEXES : HELPER FUNCTION FOR `reduce-vector'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: remove-indexes
;;; INPUT: `ind' is a list of index places to remove; `vec' is a list to remove from
;;; OUTPUT: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-indexes (ind vec &aux result)
  (setq result nil)
  ;; remove all the specified items from the list
  (loop for i from 0 to (- (length vec) 1)
     do (if (not (member i ind)) 
            (push (nth i vec) result)))
  (reverse result)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSLATE TO NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: translate-to-node
;;; INPUT: section - a list constructed as 
;;;        (section-id (list-of-event-vectors) (list-of-object-nids))
;;; OUTPUT: a node structure where the initial path contains just the input story section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-to-node (section &aux events objects cohesion coverage object-coverage weighted-score)
;;; set up initial parameters
  (setq weighted-score 0)
 ; (setq section-id (car section))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq sum (sum distance-list)) ; when this is done, add up all the distance values
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
  (setq sum (sum distance-list))  ; when this is done, add up all the distance values
  (setq len (length distance-list))
  (setq mean (/ sum len)) ; then divide the distance values by the number of distances to get the mean
  (values mean)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: sum
;;; NOTE: This is the fast version from "Successful Lisp"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum (list)
    (let ((sum 0))
      (do ((list list (rest list)))
          ((endp list) sum)
        (incf sum (first list)))))

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

(defun hill-climbing (start-node &aux current-node neighbours next-eval next-node current-eval neighbour-eval next-coverage next-object-coverage next-cosine)
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
  (setq new-events (remove-duplicates (node-events new-node)))
  (setq current-events (remove-duplicates (node-events current-node)))
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
  (setq new-objects (remove-duplicates (node-objects new-node)))
  (setq current-objects (remove-duplicates (node-objects current-node)))
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
;;; FIND BEST SCORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: find-best-score
;;; INPUT: section-id - a nid; unused - a list of story-sections generated from hill-climbing; scores - the result of finding pairwise-similariry on a section list
;;; OUTPUT: the section-id which has the best similarity to the end of the story-path and which is not already a member of the path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-best-score (section-id unused scores &aux result)
(setq result nil)
(dolist (x scores) ; loop through the scores
 (if (and (equal section-id (car x)) (member (cadr x) unused)) (push x result))) ; find all the scores relating to the section
  (car (stable-sort (copy-list result) #'> :key #'third))) ; and return the best one

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
       (sort (keep-duplicates-count
              (mapcan (lambda (event)
                        ;; return a list of the matching basis items
                        (mapcan (lambda (item base)
                                  (when (eq item 1)
                                    (list base))) (second event) events-basis))
                      event-sublist)
              ;; set a lower bound for how many duplicate items are needed to be interesting
              1)
             (lambda (x y) (< (parse-integer (subseq x 1)) (parse-integer (subseq y 1))))))
     node-events-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEXTUAL SUMMARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: textual-summary
;;; INPUT: the output of `random-hill-climbing'
;;;        currently relies on a global variable `*basis*', but that should change
;;; OUTPUT: a readable summary of the data that appears, showing only the duplicated items
;;;         Ready for further processing (or adaptation into a string)
;;; NOTE: `generic-summary' is a variant that uses an artificially computed basis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Once we have the keys and their values, we're going to have to look
;; into the datastructure defined by the CAR of our input sample,
;; which is a global variable at the moment.  The plan is to look for
;; the three most popular items of different types (if we have three
;; different things of different types, otherwise I guess we could
;; just stop with the most popular things)
(defun textual-summary (paths)
  (let* ((node-events-list (mapcar #'node-events paths))
         (events-basis *basis*))
         ;; for each NODE, we will examine the events...
         (mapcar 
          (lambda (event-sublist)
            ;; and for each set of events, merge the results into one giant list...
            (let* ((merged-list (mapcan (lambda (event)
                                          ;; specifically comprised of the matching BASIS items
                                          ;; found via comparing each vector of events with the
                                          ;; basis
                                          (mapcan (lambda (item base)
                                                    (when (eq item 1)
                                                      (list base)))
                                                  (second event)
                                                  events-basis))
                                        event-sublist))
                   ;; and count the occurrences of each item of interest
                   (keys (keep-duplicates-count merged-list)))
              ;; Loop over this data...
              (let (list-of-top-types
                    list-of-top-symbols)
                (dolist (k keys)
                  (dolist (type (cadr *sample-three*))
                    ;; and build a list of the most popular items belonging to distinct types
                    (when (member k (cadr type) :test #'string=)
                      (unless (or
                               (member (car type) list-of-top-types)
                               (member k list-of-top-symbols :test #'string=))
                        (setq list-of-top-types (nconc list-of-top-types (list (car type)))
                              list-of-top-symbols (nconc list-of-top-symbols (list k)))
                        ;; break out of inner loop when we add something
                        (return))))
                  ;; break out of outer loop once we've collected three different types
                  (if (= 3 (length list-of-top-types))
                      (return)))
                ;; Finally, print these elements with a short natural language summary
                (let ((to-print (mapcar #'cons list-of-top-types list-of-top-symbols)))
                  (unless (null to-print)
                   (text-summary-engine to-print))))))
           node-events-list)))


(defun textual-summary (paths)
  (let* ((node-events-list (mapcar #'node-events paths))
         (events-basis *basis*))
         ;; for each NODE, we will examine the events...
(format t "~% node-events-list: ~a" node-events-list)
(format t "~% evenst-basis: ~a" events-basis)
         (mapcar 
          (lambda (event-sublist)
            ;; and for each set of events, merge the results into one giant list...
            (let* ((merged-list (mapcan (lambda (event)
                                          ;; specifically comprised of the matching BASIS items
                                          ;; found via comparing each vector of events with the
                                          ;; basis
                                          (mapcan (lambda (item base)
                                                    (when (eq item 1)
                                                      (list base)))
                                                  (second event)
                                                  events-basis))
                                        event-sublist))
                   ;; and count the occurrences of each item of interest
                   (keys (keep-duplicates-count merged-list)))
              ;; Loop over this data...
              (let (list-of-top-types
                    list-of-top-symbols)
                (dolist (k keys)
                  (dolist (type (cadr *sample-three*))
                    ;; and build a list of the most popular items belonging to distinct types
                    (when (member k (cadr type) :test #'string=)
                      (unless (or
                               (member (car type) list-of-top-types)
                               (member k list-of-top-symbols :test #'string=))
                        (setq list-of-top-types (nconc list-of-top-types (list (car type)))
                              list-of-top-symbols (nconc list-of-top-symbols (list k)))
                        ;; break out of inner loop when we add something
                        (return))))
                  ;; break out of outer loop once we've collected three different types
                  (if (= 3 (length list-of-top-types))
                      (return)))
                ;; Finally, print these elements with a short natural language summary
                (let ((to-print (mapcar #'cons list-of-top-types list-of-top-symbols)))
                  (unless (null to-print)
                   (text-summary-engine to-print))))))
           node-events-list)))

; (HASAGENT HASSTARTTIME HASGENRE HASMATERIALS HASLOCATION HASCLASSIFICATION SEEALSO)
(defun text-summary-engine (types-symbol-alist)
  (let ((beg "This is a story")
        med
        (end "."))
    (dolist (elt types-symbol-alist)
      (cond ((eq (car elt) 'HASAGENT)
             (setq med
                   (nconc med (list (concatenate 'string " featuring " (cdr elt))))))
            ((eq (car elt) 'HASSTARTTIME)
             (setq med
                   (nconc med (list (concatenate 'string " beginning in " (cdr elt))))))
            ((eq (car elt) 'HASGENRE)
             (setq med
                   (nconc med (list (concatenate 'string " in the genre " (cdr elt))))))
            ((eq (car elt) 'HASMATERIALS)
             (setq med
                   (nconc med (list (concatenate 'string " using " (cdr elt))))))
            ((eq (car elt) 'HASLOCATION)
             (setq med
                   (nconc med (list (concatenate 'string " set in " (cdr elt))))))
            ((eq (car elt) 'HASCLASSIFICATION)
             (setq med
                   (nconc med (list (concatenate 'string " dealing with " (cdr elt))))))
            ((eq (car elt) 'SEEALSO)
             (setq med
                   (nconc med (list (concatenate 'string " also featuring " (cdr elt))))))
            (t nil)))
    (concatenate 'string beg (apply #'concatenate 'string med) end)
    ))

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
             :weighted-score new-eval :sections (append current-sections new-sections) :events (append new-events current-events)
             :objects (append new-objects current-objects))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE LAYERED NARRATIVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: make-layered-narrative
;;; INPUT: linear-node - a node structure 
;;; OUTPUT: a list of (layered) paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-layered-narrative (linear-node &aux layered-paths unused-sections current-eval next-node current-node eval next-cosine next-coverage next-object-coverage merged-node)
;;; *** modify to take *nodes* as a parameter
   (setq unused-sections (copy-list *nodes*)) ; then make a list of nodes for currently unused sections
   (setq layered-paths (mapcar (lambda (x) (find (list x) *nodes* :key #'node-sections :test #'equal)) (node-sections linear-node))) ; make a list of node structures (search in *nodes*) for each section in the backbone
   (setq unused-sections (copy-list *nodes*)) ; then make a list of nodes for currently unused sections

   (mapcar (lambda (x) (setq unused-sections (remove (list x) unused-sections :key #'node-sections :test #'equal))) (node-sections linear-node))
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
       (setq unused-sections (remove (node-sections next-node) unused-sections :key #'node-sections :test #'equal))
      )
layered-paths
)




(setq *weights* nil)
(setq *event-total* nil)
(setq *object-total* nil)
(setq *nodes* nil)
(setq *lookup* nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS INPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: process-input
;;; INPUT: input vector from storyscope (via Java)
;;; OUTPUT: a string of XML code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-input (data-str)
; (setq *sample-three* data-str)
(setq *sample-three* (read-from-string (jobject-lisp-value data-str)))

(multiple-value-bind (interesting-sections 
                      to-remove) 
    (reduce-vector (convert-input-to-sections-format *sample-three*))
  (defparameter *sections* interesting-sections)
  (defparameter *basis* (remove-indexes to-remove (third *sample-three*)))
)

;;; a parameter for setting the weight of different measures: first item is coherence, second is coverage and third for object-coverage

(setq *weights* '(0.8 0.5 0.5))
(setq *event-total* (apply #'+ (mapcar #'(lambda (x) (length (cadr x))) *sections*)))
(setq *object-total* (apply #'+ (mapcar #'(lambda (x) (length (caddr x))) *sections*)))
(setq *nodes* (mapcar #'translate-to-node *sections*))
(setq *lookup* (car *sample-three*))

;;; **** MODIFY THIS TO TAKE THE STRING AS INPUT AND THEN PROCESS IT

;;; an input comes in from a single narrative, it consists of a set of sections

;;; the input is processed to create the list of story sections, from which the set of nodes is made

;;; random hill climbing produces a list of 6 trails (a multi-route narrative)
     (setq best-sections (random-hill-climbing 6)) 
;;; the multi-output consists of a list of trails and a list of explanations for each trail
     (setq multi-output (list 'multi (mapcar #'node-sections best-sections) (textual-summary best-sections)))
;;; the linear node/output is the best multi-route trail (same format)
     (setq linear-node (car (sort (copy-list best-sections) #'> :key #'node-weighted-score)))
     (setq linear-output (list 'linear (list (node-sections linear-node)) (textual-summary (list linear-node))))
;;; the layered outptu resemebles the multi-route, but each first node is a top level node in a trail
     (setq layered-nodes (make-layered-narrative linear-node))
     (setq layered-output (list 'layered (mapcar #'node-sections layered-nodes) (textual-summary layered-nodes)))

     (setq outputs (list multi-output linear-output layered-output))

     (setq xml-string  (xml-trails outputs ))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPLACE ALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: replace-all
;;; INPUT: a string, a sub-pat of a string and a new string to replace the subart with
;;; OUTPUT: The modified string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XML TRAILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: xml-trails
;;; INPUT: outputs - a list of narrative-trails from either linear, layered or multiroute output, in the form '((type ((trail1) (trail2)) (exp1 exp2)) (type......))
;;; OUTPUT: A string of XML code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-trails (outputs &aux narrative-start trail-string description nid type   section-str output-str)
 (setq output-str "")
;;; START XML-string
 (setq output-str (concatenate 'string output-str "<?xml version=\"1.0\" encoding=\"UTF-8\"?><narratives>"))
;;; for each type of narrative (linear, layered and multiroute)
        (dotimes (i (length outputs)) 
        (setq summary-description  (make-description (nth i outputs))) ;make a function to create a summary across trails (this can add up what is already there for coverage, plus print some stats about the structure)
        (setq nid (generate-unique-identifier)) ; call a function to create a unique id
        (setq type (car (nth i outputs)))
    ;;; open up the narrative xml, including trails summary
          (setq narrative-start (format nil 
 "<narrative>
    <description>~a</description>
    <narrativeId>~a</narrativeId>
    <narrativeType>~a</narrativeType>"
      summary-description
      nid
      type
        ))
    ;;; then insert the narratives trails here
      (setq output-str (concatenate 'string output-str narrative-start "<trails>"))
 ;;; for each trail in the narrative
        (dotimes (j (length (second (nth i outputs)))) 
          (setq section-length (length (nth j (second (nth i outputs)))))
          (setq description (format nil "There ~[are~;is~:;are~] ~a ~[sections~;section~:;sections~] in this story" section-length section-length section-length)) ; the output of textual summary
          (setq tid (generate-unique-identifier)) ; call a function to create a unique id
          (setq title (nth j (third (nth i outputs))))
         (setq trail-string (format nil 
         "<trail>
            <trailId>~a</trailId>
            <title>~a</title>
            <description>~a</description><sections>" tid title description))
          (setq output-str (concatenate 'string output-str trail-string))
 ;;; for each section
        (dotimes (k (length (nth j (second (nth i outputs))))) 
         (setq section_uri (nth k (nth j (second (nth i outputs))))) ; but concatenate this onto a base uri
         (setq title (replace-all (cadr (find (nth k (nth j (second (nth i outputs)))) *lookup* :key #'car)) "&" "&amp;")) ;;look up title from section nid, which is (nth k (nth j (second (nth i outputs))))
         (setq section-str (format nil "
        <section>
          <title>~a</title>
         <section_uri>~a</section_uri>
      </section>" title section_uri))
     (setq output-str (concatenate 'string output-str section-str))   
       )
        (setq output-str (concatenate 'string output-str "</sections></trail>"))
    )
 (setq output-str (concatenate 'string output-str "</trails></narrative>"))
 )
 (setq output-str (concatenate 'string output-str "</narratives>"))
)

(defun make-description (trails)
(cond 
 ((equal (car trails) 'MULTI)
    (format nil "This is a multiroute narrative with ~a trails" (length (second trails))))
 ((equal (car trails) 'LAYERED)
    (format nil "This is a layered narrative with ~a top level narrative sections"(length (second trails)) ))
 ((equal (car trails) 'LINEAR)
    (format nil "This is a linear narrative with ~a ordered narrative sections" (length (car (second trails))) )))
)

(defun generate-unique-identifier ()
  (concatenate 'string (symbol-name (gensym)) (write-to-string (get-universal-time))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; processing the imput and setting up the parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parameters for the narrative recommender                   -*- mode: Lisp; -*-

;; The parameters are derived from input coming from the recommender
;; For now, see sample_three.txt or sample_three_short.txt
;; The derivation of these parameters depend on the functions
;; `convert-input-to-sections-format', `reduce-vector',
;; and `translate-to-node' that are defined in lisp-process-input.lisp

;; Note, this file and the sample data includes Unicode characters,
;; you may need to run `set-buffer-file-coding-system' inside Emacs to
;; ensure it is saved properly.


;(multiple-value-bind (interesting-sections 
;                      to-remove) 
;    (reduce-vector (convert-input-to-sections-format *sample-three*))
;  (defparameter *sections* interesting-sections)
;  (defparameter *basis* (remove-indexes to-remove (third *sample-three*)))
;)

;;; a parameter for setting the weight of different measures: first item is coherence, second is coverage and third for object-coverage

;(defparameter *weights* '(0.8 0.5 0.5))
;(defparameter *event-total* (apply #'+ (mapcar #'(lambda (x) (length (cadr x))) *sections*)))
;(defparameter *object-total* (apply #'+ (mapcar #'(lambda (x) (length (caddr x))) *sections*)))
;(defparameter *nodes* (mapcar #'translate-to-node *sections*))
;(defparameter *lookup* (car *sample-three*))
