;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pc->pitch (pc octave)
  "Converts the pitch class `pc' into an OMN pitch in the given `octave'.

  Example:
  (pc->pitch 1 4)
  => cs4"
  (midi-to-pitch (+ pc (* 12 (1+ octave)))))

(defun pcs->chord (pcs &optional (octave 4))
  "Expects a list of pitch class integers and returns an Opusmodus chord symbol.

  Example:
  (pcs->chord '(0 4 7))
  => c4e4g4"
  (first (chordize
          (loop for pc in pcs
	     collect (pc->pitch pc octave)))))

(defun pitch->pc (pitch)
  "Converts the OMN pitch `pitch' into a pitch class integer"
  (mod (pitch-to-midi pitch) 12))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Chord generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ?? TODO: revise for actual Opusmodus chords, and dynamic use for (possibly nested) lists of chords and pitches
;; Inspired by PWGL library FDSDB_XXth_CT
(defun chord-multiplication (chord1 chord2)
	 "Boulez's multiplication of chords. The intervals of `chord1' are built over every pitch of `chord2'.

Example:

Over every pitch of the C-major triad (chord2) the fifths of chord1 is created. 

;;; (chord-multiplication '(d4 a4) '(c4 e4 g4))

  References:

Boulez, Pierre (1963) Musikdenken heute. Schott's Söhne, Mainz.
"
	 (mapcar #'MIDI-to-pitch 
		 (remove-duplicates
		  (loop for p in (mapcar #'pitch-to-MIDI chord2)
		     append (tu:dx->x (tu:x->dx (mapcar #'pitch-to-MIDI chord1)) p)))))


;;; ?? TODO: revise for actual Opusmodus chords, and dynamic use for (possibly nested) lists of chords and pitches
;; Inspired by PWGL library FDSDB_XXth_CT, box Melody-expansion
(defun stretch-pitches (pitches factor &key (round T))
  "Proportional streching/shrinking of intervals.

  Args:
  - pitches: list of Opusmodus pitches
  - factor (integer, float or fraction): Factor by which all but the first `pitches' are stretched or shrunk. 
  - round: whether or not to round the result to semitones. Must be T for now, but in future when Opusmodus supports microtonal music this might be refined.

  Examples:

;;; (stretch-pitches '(c4 e4 g4) 2)
;;; 
;;; (stretch-pitches '(c4 e4 g4) 1.5)
;;; 
;;; (stretch-pitches '(c4 e4 g4) 2/3)  

  References:

Boulez, Pierre (1963) Musikdenken heute. Schott's Söhne, Mainz.
"
  (let ((MIDI-pitches (mapcar #'pitch-to-MIDI chord)))
    (mapcar #'MIDI-to-pitch 
	    (if round
		(mapcar #'round
			(tu:dx->x (mapcar #'(lambda (p) (* p factor))
					  (tu:x->dx MIDI-pitches))
				  (first MIDI-pitches)))
		;; depends on Opusmodus microtonal music support
		(warn "feature not yet implemented")))))


;;; TODO: consider revising to support Opusmodus chords instead of pitch lists
;; Inspired by PWGL library FDSDB_XXth_CT, box Chord-Expansion
(defun stretch-pitches2 (number chord factor &key (round T))
  "Creates `number' derivates from chord following a procedure suggested by Giacomo Manzoni where the chord intervals are systematically stretched or shrunk. 

  Args:
  - number (int): number of chords to generate.
  - chord: list of Opusmodus pitches
  - factor (integer, float or fraction): controls the interval between resulting chord pitches. If 1, the first chord interval is increased by 1 semitone, the second by 2 and so on. If 2, the first interval is increased by 2 semitones, the second by 4 etc. 

  Examples:

;;; (stretch-pitches2 5 '(c4 e4 g4) 1)
;;; 
;;; (stretch-pitches2 5 '(c4 e4 g4) 1.5)


  References:

Series of conferences by Giacomo Manzoni at Fiesole (Florence, Italy) School of Music from 26th of June to 1st of July, 1988.
"
  (let ((MIDI-pitches (mapcar #'pitch-to-MIDI chord)))
    (mapcar #'MIDI-to-pitch 
	    (if round
		(mapcar #'(lambda (ps) (mapcar #'round ps))
			(loop for i from 0 to number
			   collect (tu:dx->x (mapcar #'(lambda (x) (* x factor))
						     (mapcar #'(lambda (x) (+ x i))
							     (tu:x->dx MIDI-pitches)))
					     (first MIDI-pitches))))
		;; depends on Opusmodus microtonal music support
		(warn "feature not yet implemented")))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Selecting chords
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spectra-transpositions-fitting-in-scale (spectra scale)
  "Function for generating a chord/spectral domain. Returns all transpositions of the given spectral that fit in the given scale (i.e., all their pitch classes are contained in the scale).

  Args:
  spectra (OMN chords): a list of untransposed spectra  
  scale (OMN pitch list)  
  "
  (let ((all-spectra (loop for transposition in (gen-integer 0 11)
                       append (pitch-transpose transposition spectra)))
        (scale-pcs (mapcar #'pitch->pc scale)))
    (loop for spectrum in all-spectra 
      if (every #'(lambda (pc) (member pc scale-pcs))
                (mapcar #'pitch->pc (melodize spectrum)))
      collect spectrum)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process pitches
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTE: This encodes pitches into fenv by position. Later do some that also takes rhythmic positions of pitches into account
;; Idea for another improvement in future: do this process phrase-wise
(defun pitches->fenv (pitches)
  "Translates a sequence of pitches into a fenv that linearily inerpolates between the pitches. Fenv values encode pitches by their corresponding MIDI note."
  (fenv:linear-fenv-fn 
   (loop 
     for i from 0 to 1 by (/ 1 (1- (length pitches)))
     for pitch in (pitch-to-midi (flatten pitches))
     collect (list i pitch))))
(defun fenv->pitches (fenv n)
  "Translates a fenv into a sequence of pitches. Fenv values encode pitches by their corresponding MIDI note."
  (midi-to-pitch (mapcar #'round (fenv:fenv->list fenv n))))

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
  - sequence: (possibly nested) list of pitches or OMN expression
  - fenv: a fenv that ranges over the full sequence; mapping of notes to fenv value by position in sequence (not temporal position); intervals are specified in semitones; intervals are rounded to their closes integer
  All arguments of pitch-transpose-n are supported as well.


  Examples:
  ;;; (fenv-transpose-pitch '(c4 c4 g4 g4) (fenv:linear-fenv (0 0) (1 2)))
  ;;; => (c4 cs4 gs4 a4)

  ;;; (fenv-transpose-pitch '(c4 c4 g4 g4) (fenv:linear-fenv (0 0) (1 2)) :ambitus '(d4 a4))
  ;;; => (d4 eb4 gs4 a4)

  ;;; (fenv-transpose-pitch '((q c4 e c4 g4) (h g4)) (fenv:linear-fenv (0 0) (1 2)))
  ;;; => ((q c4 e cs4 gs4) (h a4))
"
  (let* ((omn? (omn-formp sequence))
         (pitches (if omn?
                    (omn :pitch sequence)
                    sequence))
        (n (count-notes sequence))
        (result (apply #'pitch-transpose-n (mapcar #'round (vector-to-list (fenv:v fenv n)))
                       pitches args)))
    (if omn?
      (omn-replace :pitch result sequence)
      result)))


(defun pitch-retrograde-omn (sequence &key (flat T))
  "My version of pitch-retrograde that is working until the original is fixed for OMN expressions with flattened input."
  (edit-omn :pitch sequence 
            #'(lambda (ps) (pitch-retrograde ps))
	    :flat flat))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accent model
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun thomassen-accents (sequence &optional (format :omn))
  "Expects an OMN sequence (or a different format, see below), and returns a *flat* list of floats representing the associated melodic accent value of each pitch as defined by the Thomassen model.

  Args:
  - format (keyword): sets the format of `sequence'. 
    - :omn - full OMN sequence, possibly nested (though the result will be flat)
    - :pitch - list of OMN pitches, possibly nested
    - :midi - list of MIDI note numbers  

NOTE: no accent values are available for the first two and the last pitch, therefor nil is return for those pitches.

Hack: Quick evaluation: strong melodic accents have an accent value greater than 0.4.

References:
Thomassen, J. M. (1982) Melodic accent: Experiments and a tentative model. The Journal of the Acoustical Society of America. 71 (6), 1596–1605.
Huron, D. & Royal, M. (1996) What is melodic accent? Converging evidence from musical practice. Music Perception. 489–516.
"
  (cr:thomassen-accents
   (flatten
    (case format
      (:omn (pitch-to-midi (omn :pitch sequence)))
      (:pitch (pitch-to-midi sequence))
      (:midi sequence)))))
