;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :om)
;; (in-package :tot/tests)

#|

;; Run different random tests each time
(let ((*random-state* (make-random-state T)))
  (5am:run! 'articulations))

(progn
  (asdf:load-system :tot/tests)
  (5am:run! 'articulations))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:def-suite articulations :in tot)
(5am:in-suite articulations)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process articulations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test articulate-bars
	  (5am:is (equal '((H D4 STACC+MARC Q TIE) (H. D4 MARC) (Q D4 TIE+MARC D4 D4 STACC) (-Q D4))
			 (articulate-bars '((h d4 stacc q tie) (h. d4) (q d4 tie q q stacc) (-q q d4))
					  :accent 'marc)))
	  (5am:is (equal '((S C4 F P C4) (S C4 F P C4) (S C4 F P C4))
			 (articulate-bars (gen-repeat 3 '((s s s)))
					  :accent 'f :default 'p :parameter :velocity))))
