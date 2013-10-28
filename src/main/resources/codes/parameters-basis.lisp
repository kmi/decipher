;; parameters for the narrative recommender                   -*- mode: Lisp; -*-

;; The parameters are derived from input coming from the recommender
;; For now, see sample_three.txt or sample_three_short.txt
;; The derivation of these parameters depend on the functions
;; `convert-input-to-sections-format', `reduce-vector',
;; and `translate-to-node' that are defined in lisp-process-input.lisp

;; Note, this file and the sample data includes Unicode characters,
;; you may need to run `set-buffer-file-coding-system' inside Emacs to
;; ensure it is saved properly.

;; Old version:
;(defparameter *sections* (convert-input-to-sections-format *sample-three*))

;; We need to reduce the size of the vector in order to make the computations manageable.
(multiple-value-bind (interesting-sections 
                      to-remove) 
    (reduce-vector (convert-input-to-sections-format *sample-three*))
  (defparameter *sections* interesting-sections)
  (defparameter *basis* (remove-indexes to-remove (second *sample-three*)))
)

;;; a parameter for setting the weight of different measures: first item is coherence, second is coverage and third for object-coverage

;(defparameter *weights* '(1 0 0))
;(defparameter *weights* '(0 1 0))
;(defparameter *weights* '(0 1 1))
;(defparameter *weights* '(0.5 0.5 0.5))
;(defparameter *weights* '(0.8 0.2 0.2))
;(defparameter *weights* '(0.2 0.8 0.8))
;(defparameter *weights* '(0.2 0.5 0.5))
;(defparameter *weights* '(0.9 0.9 0.5))
(defparameter *weights* '(0.8 0.5 0.5))

(defparameter *event-total* (apply #'+ (mapcar #'(lambda (x) (length (cadr x))) *sections*)))
(defparameter *object-total* (apply #'+ (mapcar #'(lambda (x) (length (caddr x))) *sections*)))
(defparameter *nodes* (mapcar #'translate-to-node *sections*))

(defparameter *lookup*
'(
(3742 "Section 15 - Patrick Ireland 1972 - 2008")
(4673 "Section 58 - 8b4f0367-90cb-4012-b48f-945d0f10992d")
(4677 "Section 61 - 942e85f2-967f-4485-b867-d29771f028fc")
(3774 "Section 42 - Escaped Animals")
(3759 "Section 29 - Ferdia for nÁth / Ferdia at the Ford")
(4676 "Section 60 - bf0c2c42-0a42-4b58-adde-543419270ae6")
(3718 "Section 6 - Barry Flanagan")
(3770 "Section 39 - Frederick Sleigh Roberts, 1st Earl Roberts")
(3788 "Section 55 - Untitled")
(3772 "Section 41 - Frederick Sleigh Roberts, 1st Earl Roberts and Ireland")
(3768 "Section 37 - North South East West")
(3787 "Section 54 - James McKenna")
(3785 "Section 52 - The Richmond Tower")
(3752 "Section 24 - High Cross Shaft, Bully's Acre")
(3771 "Section 40 - The Architect of the Richmond Tower")
(3782 "Section 49 - The Royal Visit of 1900")
(3786 "Section 53 - Arthur Wellesley, 1st Duke of Wellington")
(3744 "Section 16 - Brian O'Doherty")
(3753 "Section 22 - Burgoyne's Bell from the Royal Hospital Bell Tower")
(3783 "Section 50 - Charles Lennox, 4th Duke of Richmond and Lennox")
(3746 "Section 18 - Iran do Espírito Santo")
(3740 "Section 13 - Edward Delaney")
(3748 "Section 20 - Michael Kliën")
(3715 "Section 4 - Michael Warren")
(3756 "Section 26 - Lieutenant-General John Burgoyne")
(3762 "Section 32 - Carmen")
(3734 "Section 9 - Liam Gillick")
(3749 "Section 21 - Byzantine")
(3763 "Section 33 - Susana Solano")
(3781 "Section 48 - The Royal Visit of 1849")
(3784 "Section 51 - The Richmond Tower")
(3769 "Section 38 - An Oak Tree for Joseph Beuys")
(3739 "Section 12 - Eve With Apple")
(3732 "Section 7 - Militaria from the Gates of the Royal Hospital Kilmainham")
(3750 "Section 22 - Janet Mullarney")
(3780 "Section 47 - The Callanish Stones")
(4675 "Section 59 - 7f62a669-e854-4dc1-a613-fe1e77457e1b")
(3677 "Lawrence Weiner")
(3713 "WATER & SAND + STICKS & STONES ")
(3714 "Beneath the 'bow")
(3716 "The Drummer")
(3735 "8 Limestones cut to a specific size ...")
(3738 "Ulrich Rückriem")
(3741 "Three Putti (cherubs)")
(3745 "Untitled / Corrections D")
(3747 "Slattery's Lamp")
(3750 "Janet Mullarney")
(3751 "Bully's Acre")
(3757 "Back of Snowman")
(3758 "Gary Hume")
(3760 "217.5° Arc x 1")
(3761 "Judith and Holofernes")
(3764 "SENTINEL VIII")
(3766 "Catherine Lee")
(3767 "Gravestone of Vonolel")
(3769 "An Oak Tree for Joseph Beuys")
(3775 "Prince Albert in Dublin")
(3777 "The Dying Fusilier")
(3778 "Queen Victoria in the Courtyard of the Royal Hospital Kilmainham")
(3779 "Statue fragments in Bully's Acre")
(3780 "The Callanish Stones")
(3784 "The Richmond Tower")
(3789 "Julian Opie")
(3844 "Recurring Line: North/South")
))

