;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process pitches
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTE: This encodes pitches into fenv by position. Later do some that also takes rhythmic positions of pitches into account
;; Idea for another improvement in future: do this process phrase-wise
(defun pitches->fenv (pitches)
  "Translates a sequence of pitches into a fenv that linearily inerpolates between the pitches. Fenv values encode pitches by their corresponding MIDI note."
  (linear-fenv-fn 
   (loop 
     for i from 0 to 1 by (/ 1 (1- (length pitches)))
     for pitch in (pitch-to-midi (flatten pitches))
     collect (list i pitch))))
(defun fenv->pitches (fenv n)
  "Translates a fenv into a sequence of pitches. Fenv values encode pitches by their corresponding MIDI note."
  (midi-to-pitch (mapcar #'round (fenv->list fenv n))))

#|
;; varying the number of pitches in a sequence by an intermediate translation into a fenv

(fenv->pitches (pitches->fenv '(c4 d4 e4 g4 f4 d4)) 6)
=> (c4 d4 e4 g4 f4 d4)
(fenv->pitches (pitches->fenv '(c4 d4 e4 g4 f4 d4)) 4)
=> (c4 eb4 fs4 d4)
(fenv->pitches (pitches->fenv '(c4 d4 e4 g4 f4 d4)) 8)
=> (c4 cs4 eb4 e4 g4 fs4 e4 d4)
|#


(defun fenv-transpose-pitch (sequence fenv &rest args)
  "Transposes pitches in sequence by their corresponding fenv value. 
  
  Args:
  sequence: (possibly nested) list of pitches or OMN expression
  fenv: a fenv that ranges over the full sequence; mapping of notes to fenv value by position in sequence (not temporal position); intervals are specified in semitones; intervals are rounded to their closes integer

  All arguments of pitch-transpose-n are supported as well.

  Examples:
  (fenv-transpose-pitch '(c4 c4 g4 g4) (linear-fenv (0 0) (1 2)))
  => (c4 cs4 gs4 a4)
  (fenv-transpose-pitch '(c4 c4 g4 g4) (linear-fenv (0 0) (1 2)) :ambitus '(d4 a4))
  => (d4 eb4 gs4 a4)
  (fenv-transpose-pitch '((q c4 e c4 g4) (h g4)) (linear-fenv (0 0) (1 2)))
  => ((q c4 e cs4 gs4) (h a4))"
  (let* ((omn? (omn-formp sequence))
         (pitches (if omn?
                    (omn :pitch sequence)
                    sequence))
        (n (count-notes sequence))
        (result (apply #'pitch-transpose-n (mapcar #'round (vector-to-list (v fenv n)))
                       pitches args)))
    (if omn?
      (omn-replace :pitch result sequence)
      result)))


