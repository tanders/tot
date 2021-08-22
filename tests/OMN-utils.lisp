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


;; TODO: map-events


;; ? TODO: process-element

;; ? TODO: map-omn

;; !! TODO: copy-time-signature (called often)


;; TODO: map-section


;; !! TODO: edit-omn


;; ? TODO: process-omn2


;; ? TODO: map-position-in-bar


;; !! TODO: total-duration


;; TODO: flattened-length-adjust



;; !! TODO: count-notes


;; TODO: phrase-lengths



;; TODO: length-subtract



;; TODO: length-add


;; ? TODO: rnd-section


;; ? TODO: mk-seed


;; TODO: ensure-double-list



;; TODO: fn-unfold


;; TODO: swap-args
