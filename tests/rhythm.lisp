;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#|

;; Run different random tests each time
(let ((*random-state* (make-random-state T)))
  (run! 'rhythm))

(progn
  (asdf:load-system :tot)
  (run! 'rhythm))
|#


(in-package :om)
;; (in-package :tot/tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:def-suite rhythm :in tot)
(5am:in-suite rhythm)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm transformation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



