
;;; event-coverage is a list in which the first value is the total number of events and the second is the calculated value, object-coverage is also a list (the same for objects)

(defstruct node :sections :events :objects :cosine-similarity :coverage :object-coverage :weighted-score)



;turns a story section into a search node with pre-calculated similarity
(defun translate-to-node (section &aux section-id events objects cohesion coverage object-coverage weighted-score)

  (setq weighted-score 0)
  (setq section-id (car section))
  (setq events (cadr section))
  (setq objects (caddr section))
  (if (not (null events))
  (setq cohesion (mean-similarities-of-cluster events))
  (setq cohesion 0))
  (if (not (null events))
  (setq coverage (/ (length events) *event-total*))
  (setq coverage 0))
  (if (not (null objects))
  (setq object-coverage (/ (length objects) *object-total*))
  (setq object-coverage 0))

  (setq weighted-score  (+
                         (* cohesion (first *weights*))
                         (* coverage (second *weights*))
                         (* object-coverage (third *weights*))))
  (make-node :sections (list (car section)):events (cadr section) :objects (caddr section) 
             :cosine-similarity cohesion :coverage coverage :object-coverage object-coverage :weighted-score weighted-score
     ))





;;; first item in *weights* is the weight for coherence, second is for coverage and third for objects
(defparameter *weights* '(1 0 0))


;(mean-similarities-of-cluster '((E1 (1 0 1 0 1 1)) (E2 (1 0 0 0 1 0)) (E4 (1 0 1 1 1 1))))
(defun mean-similarities-of-cluster (events &aux similarities total-similarity len sim)
  (cond ((= (length events) 1) (setq sim 1))
        (t 
         (setq similarities (similarities-of-cluster events))
         (setq total-similarity (reduce #'+ similarities))
         (setq len (length similarities))
         (setq sim (/ total-similarity len))))
  (values sim)
)
  
;(similarities-of-cluster '((E1 (1 0 1 0 1 1)) (E2 (1 0 0 0 1 0)) (E4 (1 0 1 1 1 1))))
(defun similarities-of-cluster(events &aux head rest)
  (cond ((null events) (values events))
  ((setq head (car events))
  (setq rest (cdr events))
  ;(append (mapcar #'(lambda (rest-event) (list head rest-event)) rest) (mean-distance-of-cluster rest))))
  (append (mapcar #'(lambda (rest-event) 
                      (cosine-similarity (cadr head) (cadr rest-event))) rest) (similarities-of-cluster (cdr events)))))
)

;(pearson-distance '(1 2 3 4) '(4 3 2 1))
;was using this. now using cosine similarity instead
(defun pearson-distance (v1 v2)
  (let* ((n (length v1))
         (sum1 (reduce #'+ v1))
         (sum2 (reduce #'+ v2))
         (sum1-sq (reduce #'+ (map 'vector #'(lambda (x) (* x x)) v1)))
         (sum2-sq (reduce #'+ (map 'vector #'(lambda (x) (* x x)) v2)))
         (psum (reduce #'+ (map 'vector #'* v1 v2)))
         (num (- psum (/ (* sum1 sum2) n)))
         (den (sqrt (* (- sum1-sq (/ (* sum1 sum1) n)) (- sum2-sq (/ (* sum2 sum2) n))))))
    (if (zerop den) 0 (- 1.0 (/ num den)))))

;(compare-clusters '((1 2 3 4)(3 1 2 4)) '((4 3 2 1)(4 1 3 2)))
(defun compare-clusters (c1 c2 &aux distance-list sum len mean distance) 
  (setq distance-list nil)
  (dolist (c1-item c1)
           (dolist (c2-item c2)
                    ;(setq distance (list (pearson-distance c1-item c2-item) c1 c2))
             (setq distance (cosine-similarity c1-item c2-item))
             (setq distance-list (cons distance distance-list))))
  (setq sum (eval (cons '+ distance-list)))
  (setq len (length distance-list))
  (setq mean (/ sum len))
  (values mean)
)

;(compare-event-lists ((e1 (1 0 1 0 1 1)) (e2 (1 0 0 0 1 0))) ((e4 (1 0 1 1 1 1)) (e5 (1 1 1 0 1 0))))
;mean distance between two sets of events
(defun compare-event-lists (c1 c2 &aux distance distance-list sum len mean) 
  (setq distance-list nil)
  (dolist (c1-item c1)
           (dolist (c2-item c2)
             (setq distance (cosine-similarity (cadr c1-item) (cadr c2-item)))
             (setq distance-list (cons distance distance-list))))
  (setq sum (eval (cons '+ distance-list)))
  (setq len (length distance-list))
  (setq mean (/ sum len))
  (values mean)
)

;(compare-all-clusters '(((c1) ((1 2 3 4)(3 1 2 4))) ((c2) ((4 3 2 1)(4 1 3 2))) ((c3) ((1 4 3 2)(2 3 4 1)))))
(defun compare-all-clusters (cluster-list &aux head rest comparisons)
  (setq comparisons nil)
  (cond (cluster-list
         (setq head (car cluster-list))
         (setq rest (cdr cluster-list))
  
         (mapc #'(lambda (cluster) (setq comparisons (cons (list (compare-clusters (cadr cluster) (cadr head)) cluster head) comparisons))) rest)
         (setq comparisons (append comparisons (compare-all-clusters rest)))))
  
  
  (values comparisons)
)

;(closest-clusters '(((c1) ((1 2 3 4)(3 1 2 4))) ((c2) ((4 3 2 1)(4 1 3 2))) ((c3) ((1 4 3 2)(2 3 4 1)))))
(defun closest-clusters (cluster-list &aux res new-res)
  (setq res (compare-all-clusters cluster-list))
  (setq new-res (sort res #'(lambda (x y) (> (car x) (car y)))))
  (values (car new-res)))

;carries out hill climbing from a random start point for the number of times specified
;iterations must be smaller than the number of sections from which you can start
(defun random-hill-climbing (iterations &aux untested-nodes produced-paths node path)
  (setq untested-nodes *nodes*)
  (setq produced-paths nil)
  (if (> iterations (length *nodes*))
      (setq iterations (length *nodes*)))

  (loop
   (if (< iterations 1)
       (return))
   (setq node (car (subseq untested-nodes (random iterations))))
   (setq untested-nodes (remove node untested-nodes))
   (setq iterations (- iterations 1))
   (setq path (hill-climbing node))
   (setq produced-paths (cons path produced-paths)))

  (values produced-paths)

)        

;(hill-climbing '(0.6972136 (S8) ((E4 (1 0 1 1 1 1)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 1)))))
(defun hill-climbing (start-node &aux current-node neighbours next-eval next-node current-eval neighbour-eval)
  (setq current-node start-node)
  (loop 
   (setq neighbours (get-neighbours current-node))
   (setq next-eval 0)
   (setq next-node nil)
  ;; (setq current-eval (node-cosine-similarity current-node))
   (setq current-eval (node-weighted-score current-node))

   (dolist (item neighbours)
         (setq neighbour-eval (evaluate-next-node item current-node)) ;; now looking at weighted score (second in list) not cosine similarity
          (cond ((> (second neighbour-eval) next-eval)
            (setq next-node item)
            (setq next-eval (second neighbour-eval))
            (setq next-cosine (first neighbour-eval)))))
   (if (<= next-eval current-eval)
       (return current-node))
   (setq current-node (new-current-node next-node current-node next-eval next-cosine))
   )
)

(defun hill-climbing (start-node &aux current-node neighbours next-eval next-node current-eval neighbour-eval)
  (setq current-node start-node)
  (loop 
   (setq neighbours (get-neighbours current-node))
   (setq next-eval 0)
   (setq next-node nil)
  ;; (setq current-eval (node-cosine-similarity current-node))
   (setq current-eval (node-weighted-score current-node))

   (dolist (item neighbours)
         (setq neighbour-eval (evaluate-next-node item current-node)) ;; now looking at weighted score (second in list) not cosine similarity
          (cond ((> (fourth neighbour-eval) next-eval) ;; currently weighted score is calculated within this function and returned as fourth item in list. But could be done as separate function, called from here.
            (setq next-node item)
            (setq next-eval (fourth neighbour-eval))
            (setq next-cosine (first neighbour-eval))))
            (setq next-coverage (second neighbour-eval))
            (setq next-object-coverage (third neighbour-eval))
)
   (if (<= next-eval current-eval)
       (return current-node))
   (setq current-node (new-current-node next-node current-node next-eval next-cosine next-coverage next-object-coverage))
   )
)


;add togther the sections and events of two nodes
(defun new-current-node (new-node current-node new-eval new-cosine new-coverage new-object-coverage &aux new-events current-events new-sections current-sections new-objects current-objects)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
  (setq new-sections (node-sections new-node))
  (setq current-sections (node-sections current-node))
  (setq new-objects (node-objects new-node))
  (setq current-objects (node-objects current-node))
  (make-node :cosine-similarity new-cosine :coverage new-coverage :object-coverage new-object-coverage 
             :weighted-score new-eval :sections (append new-sections current-sections) :events (append new-events current-events) 
             :objects (append new-objects current-objects)) 
)


;;need to use the section list in the new node structure
(defun get-neighbours (current-node &aux all-nodes sections)
  (setq all-nodes *nodes*)
  (setq sections (node-sections current-node))
  (dolist (section sections)
    (setq all-nodes (remove-if #'(lambda (x) (equal section (car (node-sections x)))) all-nodes))
    )
  (values all-nodes)        
  
)


;;(defun evaluate-next-node (new-node current-node &aux new-events current-events new-coherence current-coherence comparison-coherence)
;;(setq val1 (evaluate-next-node-coherence (new-node current-node))) )

;this is the function that combines the two clusters plus the "marriage"
; i.e. A1*Q1 + A2*Q2 + A3(R1*R2) / Q1 + Q2 + (R1*R2)


(defun evaluate-next-node (new-node current-node &aux coherence coverage objects weighted-score)
 (setq coherence (evaluate-next-node-coherence new-node current-node))
 (if (> *event-total* 0) (setq coverage (evaluate-next-node-coverage new-node current-node)) (setq coverage 0))
 (if (> *object-total* 0) (setq objects (evaluate-next-node-objects new-node current-node)) (setq *object-total* 0))
 (setq weighted-score (+
       (* coherence (first *weights*))
       (* coverage (second *weights*))
       (* objects (third *weights*))))
 (values (list coherence coverage objects weighted-score))

)

(defun evaluate-next-node-coverage (new-node current-node &aux new-events current-events)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
 ;;; put code to remove duplicate events here, if wanted
  (/ (length (append new-events current-events))
   *event-total*)
)


(defun evaluate-next-node-objects (new-node current-node &aux new-objects current-objects)
  (setq new-objects (node-objects new-node))
  (setq current-objects (node-objects current-node))
 ;;; put code to remove duplicate objects here, if wanted
  (/ (length (append new-objects current-objects))
   *object-total*)
)



(defun evaluate-next-node-coherence (new-node current-node &aux new-events current-events new-coherence current-coherence comparison-coherence)
  (setq new-events (node-events new-node))
  (setq current-events (node-events current-node))
  (setq new-coherence (node-cosine-similarity new-node))
  (setq current-coherence (node-cosine-similarity current-node))
  (setq comparison-coherence (compare-event-lists new-events current-events))

  (/ (+ (* new-coherence (number-of-pairs (length new-events))) (* current-coherence (number-of-pairs (length current-events))) (* comparison-coherence (* (length new-events) (length current-events))))
     (+ (number-of-pairs (length new-events)) (number-of-pairs (length current-events)) (* (length new-events) (length current-events)))))
;this does R(R-1)/2

(defun number-of-pairs (num)
  (/ (* num (- num 1)) 2))





;cosine similarity
;(cosine-similarity '(1 1 1 0 1) '(1 1 1 1 1))
(defun cosine-similarity (vec-one vec-two &aux vec-one-mag vec-two-mag)
  (setq vec-one-mag (mag vec-one))
  (setq vec-two-mag (mag vec-two))
  (if (zerop vec-one-mag)
      (setq vec-one-mag 0.00001))
  (if (zerop vec-two-mag)
      (setq vec-two-mag 0.00001))
  (/ (dot vec-one vec-two)
     (* vec-one-mag vec-two-mag)))


(defun dot (vec-one vec-two &aux result)
  (setq result 0)
  (dotimes (i (length vec-one))
    (if (numberp (nth i vec-one))
	  (setq result (+ result
			  (* (nth i vec-one)
			     (nth i vec-two))))))
   (values result))


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
;;; INPUT: a list of story sections (generated from hill-climbing output) and a type of narrative
;;; OUTPUT: an ordered list of story sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;(create-narrative-structure *sections* 'linear)

(defun create-narrative-structure (sections type &aux best-sections ordered-sections result)
(cond
  ((equal type 'linear)
   (setq best-sections (car (random-hill-climbing 3))) ;;; this could be modified to find a better starting point, or to generate a specified number of 'section sets' based on selected themes. 
   (if (> (length (node-sections best-sections)) 1)
   (setq ordered-sections (organise-sections (node-sections best-sections)))
   (setq ordered-sections (node-sections best-sections))
   ))
  ((equal type 'multi)
   (setq result nil)
   (setq best-sections (random-hill-climbing 6)) ;;; 6 is chosen for maximum 6 paths
   (setq n (length best-sections))
   (dotimes (i n (remove-duplicates result :test #'equal))
    (if (> (length (node-sections (nth i best-sections))) 1)
    (push (organise-sections (node-sections (nth i best-sections)))  result)
    (push (node-sections (nth i best-sections)) result))
   )
  )
   ;;; make n length ordered-sections
    ;;; dotimes n make organised sections

;;; then repeat on the rest of the 'best-sections to create multiple paths

))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORGANISE STORY SECTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME: organise-sections
;;; INPUT: a list of story sections (generated from hill-climbing output)
;;; OUTPUT: an ordered list of story sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun organise-sections (section-list &aux start-pair)
 ;;; take first section as starting point
 (setq unused-sections section-list)

 (setq similarity-scores (get-pairwise-similarity section-list))
  (setq ordered-sections (car (stable-sort (copy-list similarity-scores) #'> :key #'third))) ;;; for now keeping the order of sections as it happens to appear
  (setq unused-sections (remove (car ordered-sections) unused-sections :test #'equal))
  (setq unused-sections (remove (cadr ordered-sections) unused-sections :test #'equal))

    (do
    ((start-list (list (car ordered-sections) (cadr ordered-sections)))
     (unused unused-sections))
    ((null unused) start-list) ;; if all sections are used, return result
     ;;; otherwise
    (progn
         (setq next-best (cadr (find-best-score (cadr start-list) unused similarity-scores)))    ;;; find the next-best score
           ;;; then add it to the ordered-sections and remove from unused-sections
        (setq unused (remove next-best unused))
        (setq start-list (append start-list (list next-best)))
       ))
)




(defun find-best-score (section-id unused scores &aux result best)
 ;;; get a list of all sections matching
(setq result nil)
(dolist (x scores)
 (if (and (equal section-id (car x)) (member (cadr x) unused)) (push x result)))
  (setq best (car (stable-sort (copy-list result) #'> :key #'third))))



(defun get-pairwise-similarity (section-list &aux result)
(setq result nil)
(loop for i from 0 to (- (length section-list) 1)
 do (loop for j from i to (- (length section-list) 1)
 ;; do (format t "~%~a ~a" (nth i section-list) (nth j section-list)j)
    do if (not (equal i j))
      do (progn
        (push (list (nth i section-list) (nth j section-list) (compare-event-lists (cadr (find (nth i section-list) *sections* :key #'car)) 
                                                                                                  (cadr (find (nth j section-list) *sections* :key #'car)))) result)
        (push (list (nth j section-list) (nth i section-list) (compare-event-lists (cadr (find (nth i section-list) *sections* :key #'car)) 
                                                                                                  (cadr (find (nth j section-list) *sections* :key #'car)))) result))))
 result)







(defun get-events (section)
(cadr (find section *sections* :key #'car))
)






;;; Method for generating Linear, multiroute and layered narratives
;;; 1) Find the strongest theme or setting of the dossier (take as input from previous step?)
;;; 2) Find the story section that most reflects this theme as starting point
;;; 3) Find next strongest theme, use as next starting point (for multiroute)
;;; 4) Organise sections by their pairwise similarity
;;; 5) To create the layered narrative, place section under the one it is most similar to. To organise layers with repsect to each other, use proximity again
;;; 6) Also look at whether to modify plot recommender step so that either keep a theme constant and vary setting and theme across sections, or vice versa, but ensure that both don't change. 



;;; new measure of 'uniqueness' so score loewr if same events are present.




;how to specify sections
;(defparameter *sections* '((s9 ((E4 (1 0 1 1 1 0))(E5 (1 1 1 0 1 0))(E6 (1 1 1 0 0 0)))) (s8 ((E4 (1 0 1 1 1 1)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 1)))) (s7 ((E4 (1 0 1 1 1 0))(E5 (1 1 1 0 1 0))(E6 (1 1 1 0 0 0))(E4 (1 0 1 1 1 1)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 1)))) (s1 ((e1 (1 0 1 0 1 1)) (e2 (1 0 0 0 1 0)))) (s2 ((e3 (1 1 1 0 0 1)) (e4 (1 0 1 1 1 1)))) (s3 ((e4 (1 0 1 1 1 1)))) (s4 ((e5 (1 1 1 0 1 0))))))


(defparameter *sections* 
'((S9 ((E4 (1 0 1 1 1 0)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 0))) (O1 O2))
(S8 ((E4 (1 0 1 1 1 1)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 1))) nil) 
(S7 ((E4 (1 0 1 1 1 0)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 0)) (E4 (1 0 1 1 1 1)) (E5 (1 1 1 0 1 0)) (E6 (1 1 1 0 0 1))) (O1 O3 O4)) 
(S1 ((E1 (1 0 1 0 1 1)) (E2 (1 0 0 0 1 0))) (O5 O6)) 
(S2 ((E3 (1 1 1 0 0 1)) (E4 (1 0 1 1 1 1))) (O1)) 
(S3 ((E4 (1 0 1 1 1 1))) (O7)) 
(S4 ((E5 (1 1 1 0 1 0))) (O9))))


(defparameter *sections*
'((2683 ((2647 (1 0 1 0 0 0 1 0 0)) (2656 (0 0 0 0 0 0 0 0 0)) (2668 (0 0 0 0 0 0 0 0 0)) (2644 (0 0 0 0 0 0 0 0 0)) (2662 (1 0 1 0 0 0 1 0 0)) (2653 (0 0 0 0 0 0 0 0 0)) (2665 (0 0 0 0 0 0 0 0 0)) (2650 (1 0 1 0 0 0 0 0 0)) (2659 (1 0 1 0 0 0 1 0 0))) (2642))

))


(defparameter *sections*
'(
 (2935 ((2225 (0 0 0 0 0 0 0 0 0)) (2223 (1 0 1 0 0 0 1 0 0))) (1910 1887 1861))
 (2951 ((2550 (1 0 0 0 0 0 1 0 0)) (2538 (0 0 0 0 0 0 0 0 0)) (2547 (1 0 1 0 0 0 1 0 0)) (2541 (1 0 1 0 0 0 1 0 0)) (2544 (0 0 0 0 0 0 0 0 0))) (2537))
 (2950 ((2656 (0 0 0 0 0 0 0 0 0)) (2647 (1 0 1 0 0 0 1 0 0)) (2668 (0 0 0 0 0 0 0 0 0)) (2644 (0 0 0 0 0 0 0 0 0)) (2662 (1 0 1 0 0 0 1 0 0)) (2665 (0 0 0 0 0 0 0 0 0)) (2653 (0 0 0 0 0 0 0 0 0)) (2650 (1 0 1 0 0 0 0 0 0)) (2659 (1 0 1 0 0 0 1 0 0))) (2642))
 (2952 ((2562 (0 0 0 0 0 0 0 0 0)) (2570 (1 1 1 0 0 0 0 0 0)) (2845 (0 0 0 0 0 0 0 0 0)) (2565 (0 0 0 0 0 0 0 0 0)) (2843 (0 0 0 0 0 0 0 0 0)) (2574 (1 1 1 0 0 1 1 0 0)) (2567 (0 0 0 0 0 0 0 0 0)) (2841 (1 1 1 0 0 0 0 0 0))) (2558 2840)) 
(2955 () (2295 2289))
(2954 ((2619 (1 0 0 0 0 0 1 0 0)) (2722 (0 0 0 0 0 0 0 0 0)) (2631 (1 0 1 0 0 0 1 0 0)) (2625 (1 0 1 0 0 0 0 0 0)) (2634 (1 0 1 0 0 0 1 0 0)) (2710 (0 0 0 0 0 0 0 0 0)) (2616 (1 0 0 0 0 0 1 0 0)) (2622 (0 0 0 0 0 0 0 0 0)) (2628 (1 0 1 0 0 0 1 0 0)) (2637 (1 0 1 0 0 0 1 0 0)) (2715 (0 0 0 0 0 0 0 0 0)) (2719 (1 1 1 0 0 0 1 0 0)) (2707 (0 0 0 0 0 0 0 0 0))) (2717 2847 2823))
(2957 () (2368))
(2956 ((2599 (1 0 1 0 0 0 1 0 0)) (2605 (1 1 0 0 0 0 1 0 0)) (2611 (0 0 0 0 0 0 0 0 0)) (2593 (1 0 1 0 0 0 0 0 0)) (2590 (1 0 1 0 0 0 1 0 0))) (2577))
(2959 ((2902 (0 0 0 0 0 0 0 0 0)) (2905 (1 0 1 0 0 0 1 0 0)) (2908 (1 0 1 0 0 0 1 0 0)) (2890 (1 0 0 0 0 0 1 0 0)) (2911 (0 0 0 0 0 0 0 0 0)) (2881 (0 0 0 0 0 0 0 0 0)) (2899 (1 0 1 0 0 1 0 0 0)) (2887 (0 0 0 0 0 0 0 0 0)) (2875 (0 0 0 0 0 0 0 0 0)) (2884 (1 1 1 0 0 0 1 0 0)) (2893 (0 0 0 0 0 0 0 0 0)) (2878 (0 0 0 0 0 0 0 0 0)) (2896 (1 0 1 0 0 0 1 0 0))) (2852))
 (2966 ((2283 (1 1 1 0 0 0 1 0 0)) (2280 (0 0 0 0 0 0 0 0 0)) (2274 (1 0 1 0 0 0 1 0 0))) (2272 2264)) 
(2965 ((2053 (1 1 1 0 0 1 1 0 0)) (2018 (1 0 1 0 0 0 1 0 0)) (2055 (1 0 1 0 0 0 1 0 0)) (2065 (1 1 1 0 0 0 1 0 0)) (2051 (1 1 1 0 0 1 1 0 0)) (2073 (1 1 1 0 0 0 0 0 0)) (2093 (1 0 1 0 0 0 1 0 0)) (2096 (0 0 0 0 0 0 0 0 0)) (2086 (1 0 1 0 0 0 1 0 0)) (2068 (1 0 1 0 0 0 0 0 0)) (1994 (1 0 1 0 0 0 0 0 0)) (2104 (1 1 1 0 0 0 1 0 0)) (2020 (0 0 0 0 0 0 0 0 0)) (2024 (0 0 0 0 0 0 0 0 0)) (2101 (1 1 1 0 0 0 0 0 0)) (2026 (1 0 1 0 0 0 0 0 0)) (2028 (1 1 1 0 0 1 1 0 0))) (2032 1956))
 (2964 ((2780 (1 0 1 0 0 0 1 0 0)) (2792 (1 0 1 0 0 0 0 0 0)) (2789 (0 0 0 0 0 0 0 0 0)) (2783 (0 0 0 0 0 0 0 0 0)) (2786 (0 0 0 0 0 0 0 0 0)) (2795 (1 0 1 0 0 0 1 0 0))) (2778))
 (2963 () (2370))
 (2944 ((2259 (1 0 0 0 0 0 0 0 0)) (2256 (1 0 0 0 0 0 1 0 0)) (2252 (1 0 0 0 0 0 1 0 0))) (2246 2238)) 
(2962 ((2349 (1 0 0 0 0 0 1 0 0)) (2358 (0 0 0 0 0 0 0 0 0)) (2343 (0 0 0 0 0 0 0 0 0)) (2346 (0 0 0 0 0 0 0 0 0)) (2355 (0 0 0 0 0 0 0 0 0)) (2334 (1 0 0 0 0 0 1 0 0)) (2728 (0 0 0 0 0 0 0 0 0)) (2725 (1 0 0 0 0 0 1 0 0)) (2337 (1 0 1 0 0 0 1 0 0)) (2331 (0 0 0 0 0 0 0 0 0)) (2340 (0 0 0 0 0 0 0 0 0)) (2364 (0 0 0 0 0 0 0 0 0)) (2352 (0 0 0 0 0 0 0 0 0)) (2361 (0 0 0 0 0 0 0 0 0))) (2329))
 (2943 () (2219 1975 1977))
 (2961 ((2221 (1 0 1 0 0 0 1 0 0)) (2167 (1 0 1 0 0 0 1 0 0)) (2178 (1 0 0 0 0 0 1 0 0)) (2769 (1 0 0 0 0 0 1 0 0)) (2240 (1 0 1 0 0 0 1 0 0)) (2248 (1 0 1 0 0 0 1 0 0)) (2236 (1 0 1 0 0 0 1 0 0)) (2229 (1 0 1 0 0 0 1 0 0)) (2244 (1 0 1 0 0 0 1 0 0)) (2217 (1 0 1 0 0 0 1 0 0)) (2201 (0 0 0 0 0 0 0 0 0)) (2214 (1 0 0 0 0 0 0 0 0))) (2159))
 (2942 () (1947 1968 1943))
 (2960 ((2266 (1 0 1 0 0 0 1 0 0)) (2305 (1 0 1 0 0 0 1 0 0)) (2277 (1 0 1 0 0 0 0 0 0)) (2736 (1 0 1 0 0 0 1 0 0)) (2755 (0 0 0 0 0 0 0 0 0)) (2299 (1 0 1 0 0 0 1 0 0)) (2302 (1 1 1 0 0 0 0 0 0)) (2270 (1 0 1 0 0 0 1 0 0)) (2296 (1 0 1 0 0 0 1 0 0)) (2287 (1 1 1 0 0 0 1 0 0)) (2293 (1 0 1 0 0 0 0 0 0))) (2254 2263))
 (2941 () (1910 1937)) 
(2948 ((2867 (1 0 1 0 0 0 1 0 0))) (2071))
 (2947 () (1998 2002))
 (2946 () (2099 2108)) 
 (2945 ((2036 (1 1 0 0 0 0 0 0 0))) (2040 2034))
 (2949 () (2161)))
)


(defparameter *sections*
'(
(	2966	((	2283	(	1749	1755	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2280	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2274	(	1722	0	0	0	0	0	0	0	0	0	0	0	0	0	))) (2272 3093 2264))
(	2965	((	2053	(	1957	1958	1	0	0	0	0	0	0	0	0	0	0	0	))
		(	2018	(	2009	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2055	(	2009	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2065	(	1964	1966	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2051	(	1941	0	1	0	0	0	0	0	0	0	0	0	0	0	))
		(	2073	(	1982	1982	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2093	(	1990	0	0	0	0	0	0	1	0	0	0	0	0	0	))
		(	2096	(	2000	2000	1	0	0	0	0	1	0	0	1	0	0	0	))
		(	2086	(	1991	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2068	(	1969	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	1994	(	1979	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2020	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2024	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2104	(	2006	2006	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2101	(	2000	2000	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2026	(	1996	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2028	(	1941	0	1	0	0	0	0	0	0	0	0	0	0	0	)))  (2032 1956))
(	2964	((	2780	(	1921	0	0	0	0	1	0	0	0	0	0	0	0	0	))
		(	2792	(	1991	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2789	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2783	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2795	(	1975	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2786	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))) (2778))
(	2950	((	2647	(	1941	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2656	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2644	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2653	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2659	(	1970	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2668	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2662	(	1971	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2665	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2845	(	1941	2011	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2650	(	1971	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2843	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2841	(	2011	2011	0	0	0	0	0	0	0	0	0	0	0	0	))) (2642 2840)) 
(	2944	((	2259	(	1990	0	0	1	0	0	0	0	0	0	0	0	0	0	))
		(	2256	(	1990	0	0	1	0	0	0	0	0	0	0	0	0	0	))
		(	2252	(	1952	0	0	1	0	0	0	0	0	0	0	0	0	0	))) (2246 2238))
(	2960	((	2167	(	1812	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2769	(	1846	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2266	(	606	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2270	(	900	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2296	(	606	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2248	(	1819	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2229	(	1803	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2244	(	1764	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2293	(	1014	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2997	(	1846	942	0	0	1	0	0	0	0	1	1	1	0	0	))
		(	2201	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2221	(	1829	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2178	(	1847	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2305	(	1832	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2277	(	1014	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2736	(	1014	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2755	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2240	(	1813	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2299	(	1803	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2302	(	1832	1832	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2236	(	1807	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	3000	(	1948	0	0	0	1	0	0	0	0	0	0	0	0	0	))
		(	2287	(	923	942	0	0	0	0	0	0	0	1	0	0	0	0	))
		(	2217	(	1760	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2214	(	1846	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	3045	(	1901	0	0	0	0	0	0	0	0	0	0	1	0	0	))) (2159 2263 2254 2996))
(	2954	((	2619	(	1899	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2722	(	1871	1872	0	0	0	0	0	0	1	0	1	0	0	1	))
		(	2631	(	1914	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2710	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2634	(	1877	0	0	0	0	0	0	0	1	0	0	0	0	0	))
		(	2625	(	1878	0	0	0	0	0	0	0	0	0	0	0	0	1	))
		(	2616	(	1872	0	0	0	0	0	0	0	1	0	0	0	0	0	))
		(	2622	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2715	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2637	(	1895	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2628	(	1832	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2719	(	1871	1872	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2707	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))) (2847 2717 2823))
(	2956	((	2599	(	1999	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2605	(	2005	2005	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2611	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2593	(	2008	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2590	(	1950	0	0	0	0	0	0	0	0	0	0	0	0	0	))) ())
(	2959	((	2358	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2355	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2334	(	1933	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2911	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2728	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2881	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2725	(	600	0	0	0	0	0	0	0	0	0	0	0	1	0	))
		(	2337	(	2000	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2899	(	2002	0	0	0	0	0	1	0	0	0	0	0	0	0	))
		(	2893	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2875	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2340	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2896	(	2002	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2878	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2364	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2361	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2902	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2349	(	750	0	0	0	0	0	0	0	0	0	0	0	1	0	))
		(	2905	(	2002	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2343	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2346	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2908	(	2002	0	0	0	0	0	0	0	0	0	1	0	0	0	))
		(	2890	(	2002	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2887	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2884	(	2002	2002	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2331	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))
		(	2352	(	0	0	0	0	0	0	0	0	0	0	0	0	0	0	))) (2329))
(	2945	((	2036	(	1671	1671	0	0	0	0	0	0	0	0	0	0	0	0	))) (2040 2034))))




;nodes are generated from sections
;;; first item in node is cosine similarity, second is section id, third is list of events, fourth is list of objects. Items 1, 3 and 4 are used for calculating coherence, coverage and objects

(defparameter *event-total* (apply #'+ (mapcar #'(lambda (x) (length (cadr x))) *sections*)))
(defparameter *object-total* (apply #'+ (mapcar #'(lambda (x) (length (caddr x))) *sections*)))
(defparameter *nodes* (mapcar #'translate-to-node *sections*))







