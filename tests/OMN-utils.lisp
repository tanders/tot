;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#|

;; Run different random tests each time
(let ((*random-state* (make-random-state T)))
  (run! 'OMN-utils))

(progn
  (asdf:load-system :tot)
  (run! 'OMN-utils))
|#


(in-package :om)
;; (in-package :tot/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:def-suite OMN-utils :in tot)
(5am:in-suite OMN-utils)



#| 
;; BUG: 
(TOTAL-DURATION '(1/4 1/8 1/16)) ; => 3/8. but should be 7/16, or not? 
;; Comparison
(apply #'+ '(1/4 1/8 1/16)) 
|#

