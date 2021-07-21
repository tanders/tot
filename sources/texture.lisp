;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
(in-package :om)


;; Implements the texture idea of Stephane Boussuge's 2nd Zoom into Opusmodus from 23 May 2020,
;; see https://opusmodus.com/forums/topic/1758-session-2-230520/?tab=comments#comment-6351
(defun quasi-heterophony (materials bar-no instruments)
  "Return multiple voices (i.e. a polyphonic result) that form a polyphonic texture, where all voices are based on the same collection of input material. Their arrangement and variation is controlled by ..."
  ;; First implement orig texture, then by and by abstract that
  (let* (;; number of voices
	 (n (length instruments))
	 ;; TODO: Should tendency-vectors perhaps be input args?
	 ;; NOTE: These are actually lists (not vectors) of floats
	 (tendency-mask '(0.1 0.5 0.3 1 0.4 0.6 0.1))
	 (tendency-vectors (loop repeat n
			      collect (gen-tendency bar-no tendency-mask :variance 0.6)))
	 ;; NOTE: Bars of input material do not need to share same length, and therefore resulting
	 ;; voices can differ in length
	 (voices-init (loop for vec in tendency-vectors
			 collect (vector-map materials vec)))
	 ;; The bar durations of all voices should use the same bar durations as the first voice
	 (shared-span (get-span (first voices)))
	 (voices-sp (cons (first voices-init) ;; no need for span correction for first voice
			  (loop for voice in (rest voices-init)
			     collect (length-span shared-span voice))))
	 )
    
    ))


#|
;;; Patterns definitions
;; Hand made:
(setf patterns '(
                 (q d4 p ord  e4 < fs4 < h gs4 mf)
                 (e gs5 mf pizz e5 d5 gs4 q d4)
                 (q fs4 p snap e4 snap)
                 (h b4 pp trem+ponte d5 trem+ponte)
                 (h e5 pp trem+ponte gs4 trem+ponte)
                 (6q a4 p leg b4 < leg c5 < leg d5 < leg e5 < leg fs5 < q gs5 f stacc)
                 (3q gs4 f stacc a4 stacc e4 leg fs4 stacc gs4 stacc c5 stacc e4 leg fs4 stacc c5 stacc)
                 (s gs5 p leg e5 d4 stacc a4 stacc q d4 stacc)
                 
                 ))

;; Algorithmically made
(setf patterns2 (gen-eval
                 8
                 '(make-omn
                   :pitch (rnd-sample 8 '(d4 e4 fs4 gs4 a4 b4 c5 d5 e5 fs5))
                   :length (setf len (euclidean-rhythm
                                      (setf lvl (rnd-round 8 16))
                                      4 lvl 's :type 2))
                   :velocity (rnd-pick '((pp)(p)(mp)(mf)(f)))
                   :articulation (length-map '((e (stacc))((q (marc ord)))) 
                                             len :otherwise '(ord))
                   )))

;;; Assemble the 2 list of patterns into 1 reservoir
(setf reservoir (assemble-seq patterns patterns2))

(setf bar-no 24)
(quasi-heterophony reservoir bar-no)
|#
