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

* Examples:
  ;;; (pc->pitch 1 4)
  ;;; => cs4
"
  (midi-to-pitch (+ pc (* 12 (1+ octave)))))

(defun pcs->chord (pcs &optional (octave 4))
  "Expects a list of pitch class integers and returns an Opusmodus chord symbol.

* Examples:
  ;;; (pcs->chord '(0 4 7))
  ;;; => c4e4g4
"
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

* Examples:

Over every pitch of the C-major triad (chord2) the fifths of chord1 is created. 

;;; (chord-multiplication '(d4 a4) '(c4 e4 g4))

* Notes:

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

* Arguments:
  - pitches: list of Opusmodus pitches
  - factor (integer, float or fraction): Factor by which all but the first `pitches' are stretched or shrunk. 
  - round: whether or not to round the result to semitones. Must be T for now, but in future when Opusmodus supports microtonal music this might be refined.

* Examples:

;;; (stretch-pitches '(c4 e4 g4) 2)
;;; 
;;; (stretch-pitches '(c4 e4 g4) 1.5)
;;; 
;;; (stretch-pitches '(c4 e4 g4) 2/3)  

* Notes:

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

* Arguments:
  - number (int): number of chords to generate.
  - chord: list of Opusmodus pitches
  - factor (integer, float or fraction): controls the interval between resulting chord pitches. If 1, the first chord interval is increased by 1 semitone, the second by 2 and so on. If 2, the first interval is increased by 2 semitones, the second by 4 etc. 

* Examples:

;;; (stretch-pitches2 5 '(c4 e4 g4) 1)
;;; 
;;; (stretch-pitches2 5 '(c4 e4 g4) 1.5)


* Notes:

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

* Arguments:
  - spectra (OMN chords): a list of untransposed spectra  
  - scale (OMN pitch list)  
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
  
* Arguments:
  - sequence: (possibly nested) list of pitches or OMN expression
  - fenv: a fenv that ranges over the full sequence; mapping of notes to fenv value by position in sequence (not temporal position); intervals are specified in semitones; intervals are rounded to their closes integer
  
  All arguments of pitch-transpose-n are supported as well.


* Examples:
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



;;; TODO:
;; - consider rewriting using map-selected-events -- much shorter then
;; - add args for number of trill notes (e.g., trill in triplets)
(defun trill-selected-notes (sequence test &key (interval 2) (ignore-articulations '(marc)))
  "Increases rhythmic interest by subdividing all notes that meet the function test and turning these into a trill.

* Arguments:
  - sequence: nested OMN sequence
  - test: function expecting four arguments of a given note, its length, pitch, velocity and articulation.
  - interval: integer specifying size and direction of trill interval (positive is up, negative is down).
  - ignore-articulations: list of articulations not to repeat at inserted notes.

* Examples:
  ;;; (trill-selected-notes '((q c4 e e) (q d4 e e)) (make-is-trill-length? 'e))
  ;;; => ((q c4 mf 1/16 c4 mf 1/16 d4 mf 1/16 c4 mf 1/16 d4 mf) (q d4 mf 1/16 d4 mf 1/16 e4 mf 1/16 d4 mf 1/16 e4 mf))
  "
  (let ((remove-articulations (flatten (mapcar #'disassemble-articulations (tu:ensure-list ignore-articulations)))))
    (loop for bar in (single-events sequence)
       collect (loop for (l p v a) in bar
		  append (remove nil 
				 (cond 
				   ((length-restp l) (list l))
				   ((funcall test l p v a) 
				    (let ((half-l (/ (omn-encode l) 2))) 
				      (list half-l p v a
					    half-l (first (pitch-transpose interval (list p))) v
					    (merge-articulations
					     (reduce #'(lambda (list art) (remove art list))
						     '(marc ten) :initial-value (disassemble-articulations a)))
					    )))
				   (T (list l p v a))))))))


(defun make-is-trill-length? (length)
  "[Helper function for trill-selected-notes] Returns a function to be used as `test' argument for function `trill-selected-notes'. The returned function returns true for all notes of the given `length'."
  #'(lambda (l p v a) (= (omn-encode length) (omn-encode l))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process chords
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chord->line (sequence position)
  "Transforms chords in `sequence' into single notes, extracting the chord pitch at `position' (or the closest pitch, if there is no pitch at `position').

* Examples:

  (chord->line '((h c4e4g4 q) (h.)) 0)"
  (map-selected-events
   #'(lambda (l p v a)
       (list l
	     (let* ((pitches (melodize p))
		    (length-1 (1- (length pitches)))
		    (pos (if (<= position length-1)
			     position
			     length-1)))
	       (nth pos pitches))
	     v a))
   sequence))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ambitus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-ambitus-sequence (lows highs)
  "Returns a sequence of ambitus pairs, where the first element follows the given low and the second element the given high values. If one of the lists is shorter then it is circled.

* Arguments:
  lows (list of integers or pitches)
  highs (list of integers or pitches)

* Examples:

  (gen-ambitus-sequence '(0 1 2 3) '(5))
  => ((0 5) (1 5) (2 5) (3 5))
"
  (let ((l (max (length lows) (length highs))))
    (matrix-transpose (list (circle-repeat lows l) (circle-repeat highs l)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accent model
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun thomassen-accents (sequence &optional (format :omn))
  "Expects an OMN sequence (or a different format, see below), and returns a *flat* list of floats representing the associated melodic accent value of each pitch as defined by the Thomassen model.

* Arguments:
  - format (keyword): sets the format of `sequence'. 
    -- :omn - full OMN sequence, possibly nested (though the result will be flat)
    -- :pitch - list of OMN pitches, possibly nested
    -- :midi - list of MIDI note numbers  

Note that no accent values are available for the first two and the last pitch, therefor nil is return for those pitches.

* Notes:

Hack: Quick evaluation: strong melodic accents have an accent value greater than 0.4.

- Thomassen, J. M. (1982) Melodic accent: Experiments and a tentative model. The Journal of the Acoustical Society of America. 71 (6), 1596–1605.
- Huron, D. & Royal, M. (1996) What is melodic accent? Converging evidence from musical practice. Music Perception. 489–516.
"
  (cr:thomassen-accents
   (flatten
    (case format
      (:omn (pitch-to-midi (omn :pitch sequence)))
      (:pitch (pitch-to-midi sequence))
      (:midi sequence)))))
