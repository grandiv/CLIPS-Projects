;;;===================================================================================
;;;     Developed by:
;;;     Muhammad Grandiv Lava Putra
;;;     22/493242/TK/54023
;;;     Teknologi Informasi
;;;
;;;     Forward Chaining Expert System to Recommend Fabrics Based on User Preferences.
;;;
;;;     Developed on CLIPS Version 6.31 
;;;
;;;     To execute, just load, reset and run.
;;;===================================================================================

;;****************
;;  DEFFUNCTIONS *
;;****************

;; This function iteratively asks the questions
(deffunction next-questionnaire (?questionnaire ?allowed-values)
   (printout t ?questionnaire crlf)
   (printout t "reply: ")
   (bind ?reply (read))
   (printout t crlf)
   (if (lexemep ?reply) then (bind ?reply (lowcase ?reply)))
   (while (not (member ?reply ?allowed-values)) do
      (printout t ?questionnaire crlf)
      (printout t "reply: ")
      (bind ?reply (read))
      (printout t crlf)
      (if (lexemep ?reply) then (bind ?reply (lowcase ?reply))))
   ?reply)

;;*****************
;;  INIT 
;;*****************

;; This rule prints the initial information about the program
(defrule start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (printout t "" crlf)
  (printout t "Welcome to Grandiv's Fabric Recommendation Expert System" crlf)
  (printout t "" crlf)
  (printout t "Please reply ONLY with the following values:" crlf)
  (printout t "-------------------------------------" crlf)
  (printout t " Value           Meaning " crlf)
  (printout t "-------------------------------------" crlf)
  (printout t " -1              Certainly Not "	crlf)
  (printout t " -0.8            No thanks "	crlf)
  (printout t " -0.6            Not quite sure "	crlf)
  (printout t " -0.4            Don't think so "	crlf)
  (printout t "  0              Maybe "	crlf)
  (printout t "  0.4            Uhmm.. Yeah "	crlf)
  (printout t "  0.6            Sure "	crlf)
  (printout t "  0.8            Of course "	crlf)
  (printout t "  1              Certainly! "	crlf)
  (printout t "-------------------------------------" crlf)
  (printout t "" crlf))

;; Form of the facts
(deftemplate a-fact
   (slot name)
   (slot cf (default 0)))

;;*********************
;;  COMBINE CERTAINTIES 
;;*********************

;; Certainty factor combination rules
(defrule combine-certainties-1
  (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0))
  (test (> ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* ?cf2 (- 1 ?cf1))))))

(defrule combine-certainties-2
  (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (< ?cf1 0))
  (test (< ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* ?cf2 (+ 1 ?cf1))))))

(defrule combine-certainties-3
  (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0))
  (test (< ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2)))))))

;;*******************
;;  QUESTIONNAIRE 
;;*******************

;; Form of the questions
(deftemplate questionnaire
   (slot a-fact (default ?NONE))
   (slot the-questionnaire (default ?NONE))
   (slot already-asked (default FALSE)))

;; Rule for asking a question
(defrule ask-a-questionnaire
   ?f <- (questionnaire (already-asked FALSE)
                        (the-questionnaire ?the-questionnaire)
                        (a-fact ?the-fact))
   =>
   (modify ?f (already-asked TRUE))
   (bind ?accepted (create$ -1 -0.8 -0.6 -0.4 0 0.4 0.6 0.8 1))
   (assert (a-fact (name ?the-fact) (cf (next-questionnaire ?the-questionnaire ?accepted)))))

;; List of questions
(deffacts questionnaire-facts
  (questionnaire (a-fact q1)
                 (the-questionnaire "Do you prefer fabrics made from natural fibers? "))
  (questionnaire (a-fact q2)
                 (the-questionnaire "Do you prefer fabrics that are soft and comfortable to wear? "))
  (questionnaire (a-fact q3)
                 (the-questionnaire "Do you like fabrics that are strong and long-lasting? "))
  (questionnaire (a-fact q4)
                 (the-questionnaire "Are you fond of textured fabrics? "))
  (questionnaire (a-fact q5)
                 (the-questionnaire "Would you like to work on fabrics that require high-level skills? "))
  (questionnaire (a-fact q6)
                 (the-questionnaire "Are your fabrics likely to be worn in colder season? "))
  (questionnaire (a-fact q7)
                 (the-questionnaire "Would you like fabrics that are easy to maintain? ")))

;;******************
;;  THE RULES 
;;******************

;; Based on the answers to the questions these rules infer facts
(defrule rule1
  (or
    (a-fact (name q1) (cf ?cf1))
    (a-fact (name prefers-soft-comfortable) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-silk) (cf (* 0.9 ?cf1))))
      (assert (a-fact (name prefers-linen) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-wool) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-cotton) (cf (* 0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-synthetic-fibers) (cf (* 0.6 ?cf1)))))))

(defrule rule2
  (and
    (a-fact (name q2) (cf ?cf1))
    (a-fact (name q3) (cf ?cf2)))
  =>
  (if (and (>= ?cf1 0.4) (>= ?cf2 0.4)) 
       then 
      (assert (a-fact (name prefers-silk) (cf (* 0.8 (min ?cf1 ?cf2)))))
      (assert (a-fact (name prefers-rayon) (cf (* 0.7 (min ?cf1 ?cf2)))))
      (assert (a-fact (name prefers-linen) (cf (* 0.7 (min ?cf1 ?cf2)))))
      (assert (a-fact (name prefers-jersey) (cf (* 0.6 (min ?cf1 ?cf2)))))
       else (if (and (<= ?cf1 -0.4) (<= ?cf2 -0.4))
                then (assert (a-fact (name prefers-polyester) (cf (* 0.6 (min ?cf1 ?cf2)))))
                     (assert (a-fact (name prefers-brocade) (cf (* 0.7 (min ?cf1 ?cf2))))))))

(defrule rule3
  (or 
    (a-fact (name q3) (cf ?cf1))
    (a-fact (name prefers-synthetic-fibers) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-denim) (cf (* 0.9 ?cf1))))
      (assert (a-fact (name prefers-canvas) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-fleece) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-jersey) (cf (* -0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-brocade) (cf (* 0.6 ?cf1)))))))

(defrule rule4
  (or 
    (a-fact (name q4) (cf ?cf1))
    (a-fact (name prefers-textured-fabrics) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-brocade) (cf (* 0.9 ?cf1))))
      (assert (a-fact (name prefers-lace) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-leather) (cf (* 0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-soft-comfortable) (cf (* 0.6 ?cf1)))))))

(defrule rule5
  (or 
    (a-fact (name q5) (cf ?cf1))
    (a-fact (name requires-high-skill-fabrics) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-silk) (cf (* 0.8 ?cf1))))
      (assert (a-fact (name prefers-brocade) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-leather) (cf (* 0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-cotton) (cf (* 0.6 ?cf1)))))
                     (assert (a-fact (name prefers-linen) (cf (* 0.6 ?cf1))))))

(defrule rule6
  (or 
    (a-fact (name q6) (cf ?cf1))
    (a-fact (name requires-cold-season-fabrics) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-wool) (cf (* 0.8 ?cf1))))
      (assert (a-fact (name prefers-leather) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-cotton) (cf (* 0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-jersey) (cf (* 0.6 ?cf1)))))))

(defrule rule7
  (or 
    (a-fact (name q7) (cf ?cf1))
    (a-fact (name prefers-easy-maintenance-fabrics) (cf ?cf1)))
  =>
  (if (>= ?cf1 0.4) 
       then 
      (assert (a-fact (name prefers-polyester) (cf (* 0.9 ?cf1))))
      (assert (a-fact (name prefers-cotton) (cf (* 0.7 ?cf1))))
      (assert (a-fact (name prefers-linen) (cf (* 0.7 ?cf1))))
       else (if (<= ?cf1 -0.4)
                then (assert (a-fact (name prefers-silk) (cf (* 0.6 ?cf1)))))))

;;****************
;;* FINAL RESULTS *
;;****************

;; Based on the inferred facts, these rules suggest the best clothing fabrics
(defrule print-results
  (a-fact (name q1) (cf ?cf1))
  =>
  (printout  t "Based on your preferences, the recommended clothing fabrics for you are:" crlf)
  (printout  t crlf)
  (assert (finished)))

(defrule fabric-selection-rule-1
  (finished)
    (a-fact (name prefers-silk) (cf ?cf1))
    (a-fact (name prefers-linen) (cf ?cf2))
  =>
  (if (>= (* 0.7 (min ?cf1 ?cf2)) 0.4) 
      then
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Silk Linen Blend with cf " (* 0.7 (min ?cf1 ?cf2)) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-2
  (finished)
    (a-fact (name prefers-cotton) (cf ?cf1))
    (a-fact (name prefers-fleece) (cf ?cf2))
  =>
  (if (>= (* 0.8 (min ?cf1 ?cf2)) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Cotton Fleece with cf " (* 0.8 (min ?cf1 ?cf2)) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-3
  (finished)
    (a-fact (name prefers-leather) (cf ?cf1))
    (a-fact (name prefers-denim) (cf ?cf2))
    (a-fact (name prefers-cotton) (cf ?cf3))
  =>
  (if (>= (* 0.9 (min ?cf1 ?cf2 ?cf3)) 0.4)
      then  
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Leather Denim Cotton Blend with cf " (* 0.9 (min ?cf1 ?cf2 ?cf3)) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-4
  (finished)
    (a-fact (name prefers-brocade) (cf ?cf1))
    (a-fact (name prefers-lace) (cf ?cf2))
    (a-fact (name prefers-leather) (cf ?cf3))
  =>
  (if (>= (* 0.8 (min ?cf1 ?cf2 ?cf3)) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Brocade Lace Leather Blend with cf " (* 0.8 (min ?cf1 ?cf2 ?cf3)) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-5
  (finished)
    (a-fact (name prefers-wool) (cf ?cf1))
    (a-fact (name prefers-rayon) (cf ?cf2))
  =>
  (if (>= (* 0.7 (min ?cf1 ?cf2)) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Wool Rayon Blend with cf " (* 0.7 (min ?cf1 ?cf2)) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-6
  (finished)
    (a-fact (name prefers-canvas) (cf ?cf1))
  =>
  (if (>= (* 0.6 ?cf1) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Canvas with cf " (* 0.6 ?cf1) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-7
  (finished)
    (a-fact (name prefers-jersey) (cf ?cf1))
  =>
  (if (>= (* 0.8 ?cf1) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Jersey with cf " (* 0.8 ?cf1) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))

(defrule fabric-selection-rule-8
  (finished)
    (a-fact (name prefers-polyester) (cf ?cf1))
  =>
  (if (>= (* 0.6 ?cf1) 0.4)
      then 
      (printout  t "------------------------------------------------------" crlf)
      (printout  t "Polyester with cf " (* 0.6 ?cf1) crlf)
      (printout  t "------------------------------------------------------" crlf)
      (printout  t crlf)))