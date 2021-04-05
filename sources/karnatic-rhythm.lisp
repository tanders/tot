;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Karnatic rhythmical techniques
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anga (anga &optional (laghu-size 3) (beat 1/4))
  "Return an anga represented by a rhythmic value of its duration wrapped in a list.

* Arguments:
 - anga (keyword): either :anudrutam, :a (1 beat), :drutam, :d (2 beats), :laghu or :l (laghu-size beats)
 - laghu-size (int): number of beats of laghu. Possible values: 3, 4, 5, 7, and 9
 - beat (ratio): length of a beat
"
  (assert (member laghu-size '(3 4 5 7 9)))
  (let ((options (list :anudrutam 1 :a 1
		       :drutam 2 :d 2
		       :laghu laghu-size :l laghu-size)))
    (list (* (getf options anga) beat))))

;; (anga :a)
;; (anga :l 5)

(defun tala (angas &optional (laghu-size 3) (beat 1/4))
  "Return a tala represented by a list of angas (see function anga), i.e. a list of rhythmic values, each wrapped in a sublist.

* Arguments:
 - angas (list of keyword): consisting of either :anudrutam, :a (1 beat), :drutam, :d (2 beats), :laghu or :l (laghu-size beats)
 - laghu-size (int): number of beats of laghu
 - beat (ratio): length of a beat

The seven suladi tala categories collected by Purandaradasa (each with all possible laghu sizes): 
 - L D L L
 - L D L
 - D L
 - L A D
 - L D D
 - L L D D
 - L
"
  (loop for anga in angas
     collect (anga anga laghu-size beat)))

;; (tala '(:l :d :d) 5)
;; (tala '(:l :d :l :l) 3)
;; (tala '(:l))
;; (total-duration (tala '(:l :d :d) 5))


(defun tala-beat-number (tala &optional (beat 1/4))
  "Number of beats in tala"
  (/ (total-duration tala) beat))
;; (tala-beat-number (tala '(:l :d :d) 5))


(defparameter *tala* (tala '(:d :l) 5)
  "Globally set default tala.")

;; (defconstant +tala+ '((wq) (h) (h))) ; whole tala one bar -- is that appropriate?
;; (defconstant +harmonic-tala-rhythm+ +tala+) ;;; ??
;; (defconstant +tala-dur+ (total-duration +tala+))
;; (defconstant +tala-beat-number+ (* +tala-dur+ 4))
;; (defconstant +tala-meter+ (length->time-signature +tala+)) ; tala split into multiple bars
;; (defconstant +tala-beats+ (even-length-rhythm +tala-dur+ 'q :time-sig +tala-meter+))


(defun tala-time-signatures (sequence &optional (tala *tala*))
  "Return list of OMN time signatures (every anga represented as a separate bar) whose overall duration matches the length of `sequence'."
  (length->time-signature (length-span (total-duration sequence) (flatten tala))))

					; (tala-time-signatures '(9/2))
					; (tala-time-signatures '(10/4))


(defun complete-phrase-in-tala (sequence &key (tala *tala*) (append-sam? NIL))
  "For preview within the time signature sequence corresponding to tala. Optionally, append a next tala sam. `sequence' should be a full tala or multiple talas for this to work.

* Arguments:
 - sequence (OMN sequence)
 - tala (nested list of durations): a tala as returned by function tala
 - append-sam? (Boolean): whether or not to append a last beat at the very end

Note: ties in sequence are preserved, if it contains pitches (a pitch for just the first note is fine), but not for purely rhythmic sequences.

;;; TODO: Generalise to better work if sequence is not full tala
"
  (let* ((last-tala-sam (first tala))
	 (seq (if append-sam?
		  (append sequence last-tala-sam)
		  sequence)))
    (omn-to-time-signature seq (tala-time-signatures seq tala))))

					; (complete-phrase-in-tala '((9/2 c4)))

;;; BUG: ties missing in result, but with added pitches things are working fine 
;; (omn-to-time-signature '(3) '(5 4))
;; (omn-to-time-signature '(3 c4) '(5 4))

(defun preview-score-in-tala (score &optional (tala *tala*) (append-sam? NIL))
  "Preview score in correct time signatures for tala and with next tala same appended.

* Arguments:
 - score (preview-score format score)
 - tala (nested list of durations): a tala as returned by function tala
 - append-sam? (Boolean): whether or not to append a last beat at the very end
"
  (preview-score 
   (map-parts-equally 
    score
    #'(lambda (seq) (complete-phrase-in-tala seq :tala tala :append-sam? append-sam?))
    '(_))))

#||
(preview-score-in-tala '(:vln ((q. c5 e d5 q e5) (h. f5) (h. e5))
			 :vlc ((q c4 b3 a3) (h. g3) (h. c3)))
		       (tala '(:l) 3)
		       NIL)
||#


;; List of all possible combinations of note subdivisions in a group of n notes.
;; These are sorted into potentially accented (starting with a durational accent and/or leading to an accent if the next following note is longer, even though that is not really a karnatic concept) and non-accented
(let* ((accented-1 ; non-standard 
	(apply #'vector '((1))))
       (unaccented-1
	(apply #'vector '()))
       (both-1 (vector accented-1 unaccented-1))
       (accented-2 ; non-standard 
	(apply #'vector '((2) (1 1))))
       (unaccented-2
	(apply #'vector '()))
       (both-2 (vector accented-2 unaccented-2))
       (accented-3 ; tisra 
	(apply #'vector '((3) (2 1) (1 1 1))))
       (unaccented-3 ; tisra 
	(apply #'vector '((1 2))))
       (both-3 (vector accented-3 unaccented-3))
       (accented-4 ; chatusra 
	(apply #'vector '((4) (3 1) (2 2) (2 1 1) (1 1 1 1))))
       (unaccented-4 ; chatusra 
	(apply #'vector '((1 3) (1 2 1) (1 1 2))))
       (both-4 (vector accented-4 unaccented-4))
       (accented-5; khanda 
	(apply #'vector '((5) (4 1) (3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1))))
       (unaccented-5 ; kanda 
	(apply #'vector
	       '((2 3) (1 4)
		 (2 1 2) (1 3 1) (1 2 2) (1 1 3)
		 (1 2 1 1) (1 1 2 1) (1 1 1 2))))
       (both-5 (vector accented-5 unaccented-5))
       (accented-6 ; tisra 
	(apply #'vector
	       '((6) (5 1) (4 2) (3 3) (4 1 1) (3 2 1) (2 2 2) (3 1 1 1) (2 2 1 1) (2 1 1 1 1) (1 1 1 1 1 1))))
       (unaccented-6 ; tisra
	(apply #'vector
	       '((2 4) (1 5)
		 (3 1 2) ; this one is borderline
		 (2 3 1) (2 1 3) (1 4 1) (1 3 2) (1 2 3) (1 1 4) 
		 (2 1 2 1) (2 1 1 2)
		 (1 3 1 1)
		 (1 2 2 1) (1 2 1 2) 
		 (1 1 3 1) (1 1 2 2) (1 1 1 3)
		 (1 2 1 1 1) (1 1 2 1 1) (1 1 1 2 1) (1 1 1 1 2)
		 )))
       (both-6 (vector accented-6 unaccented-6))
       #|
;;; TODO: see (Reina, 2016, p. 24)
       (defconstant accented-7 ; misra
       )
       
       (defconstant unaccented-7 ; misra
       )
       (both-7 (vector accented-7 unaccented-7))

       |#
       )
  ;; TODO:
  ;; - ? Args prefix and suffix
  ;; - ?? More generic filter function called filter expecting a Boolean function -- add it when you really need it.
  ;; - I can in principle also have very many possible transformations, e.g., turning some notes into rests (e.g., the first n of a cell), or adding some tie (e.g., tie to the very beginning of the cell), though these might be less effecting
;;;   => Do that with separate functions
  (defun gen-karnatic-cell-nested (gati jathi position
				   &key (accented? T) (max-number nil) (min-number nil) (first-length nil)
				     (include-length nil) (exclude-length nil) (seed nil))
    "[Aux function called by gen-karnatic-cell, which only calls this function] Two functions are necessary here simply because (i) I do not want to define all the variables like accented-3 and friends (see above) globally and (ii) the automatic documentation generation I currently use (cldoc) skips nested function definitions."
    (rnd-seed seed)
    (if (listp position)
	;; generate sequence of cells (list of lists)
	(let ((n (length position)))
	  (mapcar #'(lambda (gati jathi position accented? max-number min-number first-length include-length exclude-length)
		      (gen-karnatic-cell gati jathi position
					 :accented? accented? :max-number max-number :min-number min-number
					 :first-length first-length :include-length include-length :exclude-length exclude-length
					 :seed (seed)))
		  (circle-repeat gati n)
		  (circle-repeat jathi n)
		  position
		  (circle-repeat accented? n)
		  (circle-repeat max-number n)
		  (circle-repeat min-number n)
		  (circle-repeat first-length n)
		  (circle-repeat include-length n)
		  (circle-repeat exclude-length n)))
	;; generate a single cell (list)
	(let* ((all-accented-and-unaccented (case jathi
					      (1 both-1)
					      (2 both-2)
					      (3 both-3)
					      (4 both-4)
					      (5 both-5)
					      (6 both-6)
					      ;; (7 both-7)      
					      ))
	       (all-accented/unaccented-selected
		(cond ((or (eql accented? T)
			   (eql accented? 1))
		       (aref all-accented-and-unaccented 0))
		      ((or (eql accented? nil)
			   (eql accented? 0))
		       (aref all-accented-and-unaccented 1))))
	       (all-in-gati
		(map 'vector #'(lambda (seq)
				 (mapcar #'(lambda (x) (/ x gati 4)) seq))
		     all-accented/unaccented-selected))
	       (all-filtered
		(tu:filter
		 (let ((first-l (when first-length
				  (omn-encode first-length)))
		       (include-l (when include-length
				    (omn-encode (tu:ensure-list include-length))))
		       (exclude-l (when exclude-length
				    (omn-encode (tu:ensure-list exclude-length)))))
		   #'(lambda (seq)
		       (and (if min-number
				(<= min-number (length seq))
				T)
			    (if max-number
				(>= max-number (length seq))
				T)
			    (if first-length
				(= (first seq) first-l)
				T)
			    (if include-length
				(every #'(lambda (x) (member x seq)) include-l)
				T)
			    (if exclude-length
				(notany #'(lambda (x) (member x seq)) exclude-l)
				T)
			    )))
		 all-in-gati))
	       (last-possible-position (1- (length all-filtered)))
	       )
	  (if (= last-possible-position -1)
	      ;; filtering removed all options
	      nil
	      ;;
	      (let ((pos (if (eql position '?)
			     (rnd1 :low 0 :high last-possible-position :seed (seed))
			     (if (<= position last-possible-position)
				 position
				 last-possible-position))))
		(aref all-filtered pos)))
	  ))))

;; !! TODO: allow for ties across rhythmic cells
;; !! TODO: allow for turning cells or parts of them into rests (perhaps in a separate function?)
(defun gen-karnatic-cell (gati jathi position
			  &key (accented? T) (max-number nil) (min-number nil) (first-length nil)
			    (include-length nil) (exclude-length nil) (seed nil))
  "Returns a Karnatic rhythmic cell (list of rhythmic values) or a list of such cells (if `position' is a list). The shape of the resulting cell(s) can be controlled in many ways, allowing to generate, e.g., rhythms that are similar even if they are in different gatis or jathis.

See Reina (2016) for details on terms like gati, jathi and matra.

* Arguments:
- gati (integer -- commonly in {3, 4, 5, 7} -- or list of them): gati for the cell(s) to generate.
- jathi (integer in range 1-7, or list of them): jathi for the cell(s) to generate.
  1 and 2 no standard Karnatic jathi, but added for more flexibility preserving consistency.
  BUG: 7 not yet defined/supported
- position (integer, '?, or list of either): specifies which cell of the available options to generate. If no filtering is enabled (see further arguments below) then the list of available options are all the possible standard subdivisions of a 'beat' depending on the current jathi as listed by (Reina, 2016, p. 23f). These options are sorted  by the number of notes in rhythmic cells (fewest first), and in case of equal note numbers the length of notes starting with the first note (longer first note first). So, the position 0 is always a single note per beat/cell (length depends on gati and jathi) and so on. If `position' exceeds the number of available options, the last option is return, which is always an even subdivision of the cell in jathi matras. 
If `position' is '? then the position is randomised.
- accented? (Boolean, or binary integer, i.e. 0 or 1): whether the returned cells potentially carry durational accents on the start of the cells (or on an immediately following longer note). If accented? is nil (or 0) then the cell(s) carry a durational accent that is not on the start of the cell.
Binary integers are supported so that values for this argument can be generated with Opusmodus' binary number generators.
- max-number/min-number (integer or list of them): max/min number of notes in the cell(s).
- first-length (ratio or OMN length, or list of either): length of the first note in cell(s).
- include-length/exclude-length (ratio or OMN length, or list of either): length values that must be included in or excluded from the cell(s).
- seed (integer): the seed to use for the randomised position, is position is '?.

NOTE: For resulting in a full number of beats, the number of elements in position should be a multiple of gati. Remember that (with constant gati and jathi for one period), gati = number of accents and yati = number of beats.

* Examples:

The first position is always a single note per cell (if no other filtering is selected)
  ;;; (gen-karnatic-cell 4 4 0)

Generating multiple cells, and showing the order of cells in this gati and jathi without filtering (only cells carrying potentially a durational accent on the first note of the cell).
  ;;; (gen-karnatic-cell 4 4 '(0 1 2 3 4))

Setting a different jathi.
  ;;; (gen-karnatic-cell 4 5 '(0 1 2 3 4))

If position exceeds the range of possible cell, the last cell is chosen (which is always an even subdivision of the full cell length into `yati' matras.
  ;;; (gen-karnatic-cell 4 4 100)

If position receives a list, also all other arguments can be lists (of the same length), e.g., different jathis. Note that equal positions in different jathis tend to result in similar cells.
  ;;; (gen-karnatic-cell 4 '(3 4 5) '(1 1 1))

Filtering arguments can further shape the result. The meaning of the position argument changes accordingly, always depending on the remaining number of rhythmic options for cells. E.g., the minimum number of notes per cell can be set...
  ;;; (gen-karnatic-cell 4 4 0 :min-number 2)
									       
... or the maximum number of notes. Note that the position is randomised here.
  ;;; (gen-karnatic-cell 4 4 '? :max-number 3)

... or the first note value in the cell can be set.
  ;;; (gen-karnatic-cell 4 4 '(1 1 1) :first-length 1/8)

All these filter arguments also support lists for setting different values for each sublist.
  ;;; (gen-karnatic-cell 4 4 '(0 0) :exclude-length '(e q))

Again, an example with randomised positions, but here the seed is fixed.
  ;;; (gen-karnatic-cell 4 4 '(? ?) :min-number 2 :seed 1)

Filtering can remove all options for cells, in which case nil returned.
  ;;; (gen-karnatic-cell 4 4 0 :min-number 5)

You can of course overwrite the resulting time signature with `omn-to-time-signature'. If you want to keep track of where the accents are located, you could mark them before this transformation. You could then manually later revise the notation to instead you the beam-breaking that Reina recommends. 
 
;;; (omn-to-time-signature 
;;;  (articulate-bars (gen-karnatic-cell 4 5 '(0 6 3 2 1 0))
;;;                    :accent 'marc)
;;; '(4 4))

For more examples see also https://opusmodus.com/forums/topic/1097-updated-library-of-many-custom-opusmodus-functions/?tab=comments#comment-3497


You may want to consider further transforming results with rhythm transformations functions like, e.g., `tie-whole-notes'. 

* BUGS:

The argument min-number is seemingly not fully working yet:

;;; (gen-karnatic-cell 4 5 '(? ? ? ?) :min-number '(3 3 3 3) :seed 1)
;;; => ((1/8 1/8 1/16) (3/16 1/16 1/16) (1/16 1/16 1/16 1/16 1/16) (1/8 1/8 1/16)) 


* Notes:

- Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
"
  (gen-karnatic-cell-nested
   gati jathi position
   :accented? accented? :max-number max-number :min-number min-number :first-length first-length
   :include-length include-length :exclude-length exclude-length :seed seed))


#||
(defun gen-karnatic-cell* (&key gati jathi position
			     (accented? T) (max-number nil) (min-number nil) (first-length nil)
			     (include-length nil) (exclude-length nil) (seed nil))

  )
||#


;; TODO: variant that allows for specifying the number of matras to add/remove at the front or end of the generated full cycle.
;; Perhaps simply removing some values from the generated result?
(defun make-karnatic-cycle-fn (positions
                               &key (gati-as-length T) (accented? T) (max-number nil) (min-number nil) (first-length nil)
				 (include-length nil) (exclude-length nil) (seed nil))
  "Return function similar to gen-karnatic-cell, but with a predefined list of positions.

* Arguments:
- gati-as-length (Boolean): if true, uses gati number elements of the positions (results in a full number of beats). gati then must be an integer. Otherwise use the full positions always.
Other arguments inherited from gen-karnatic-cell.
"
  (lambda (gati jathi &key (accented? accented?) (max-number max-number) (min-number min-number)
			(first-length first-length)
			(include-length include-length) (exclude-length exclude-length) (seed seed))
    (gen-karnatic-cell gati jathi
		       (if gati-as-length
			   (subseq positions 0 gati)
			   positions)
                       :accented? accented? :max-number max-number :min-number min-number 
                       :first-length first-length
                       :include-length include-length :exclude-length exclude-length :seed seed)))

#||
(setf (fdefinition 'karnatic-cycle-a)
      ;; long list of positions, for quick testing...
      (make-karnatic-cycle-fn (gen-repeat 20 '(1))))

(setf cells 
      (make-omn :length (append (karnatic-cycle-a 4 3)
                                (karnatic-cycle-a 4 4)
                                (karnatic-cycle-a 4 5))
                :pitch '(d4)))

(setf cells 
      ;; NOTE: tuplets.
      (make-omn :length (append (karnatic-cycle-a 5 4)
                                (karnatic-cycle-a 4 5))
                :pitch '(d4)))
||#


(let ((constant-cells (gen-repeat 9 '(0)))) ;; supports up to gati 9
  ;; Function constant-cycle 
  (setf (fdefinition 'constant-cycle)
	(make-karnatic-cycle-fn constant-cells)))
(setf (documentation 'constant-cycle 'function)
      "A function similar to `gen-karnatic-cell`, but with a predefined list of positions that always result in a single note per jathi. See the doc of `gen-karnatic-cell` for further details.")


;; ? TODO: allow that duration can be larger than `gati` by adding further notes, not rests.
(defmethod gb-plan ((jathi integer) &key (beats NIL) (gati 4) (extend 's) (tie-first NIL) (beat-duration 1/4))
  "Outlines (plans) the rhythmic form of a gati bhedam (gb) sequence. The result is an OMN sequence where each jathi is represented by a single note (i.e. without rhythmic phrasing).

* Arguments:
  - jathi (integer): jathi of the result.
  - beats (number): duration of the result measured in beats.
  - gati (integer): gati of the result.
  - extend (s or e): if duration deviates from the duration of a full gati/jathi cycle (i.e., beats is set), whether to cut material (or add rests) at the beginning (s) or end (e).
  - tie-first (Boolean): if true, the first note of the result is tied to the second note. This is useful, e.g., if the result forms the beginning of a tala/anga and a short note (caused by cutting off notes at the beginning) should be avoided.
  - beat-duration (rational): duration of the beats used by `beats`.

* Examples:
Full cycle of default gati 4, jathi 3
;;; (gb-plan 3)
;;; => ((3/16) (3/16) (3/16) (3/16))

Full cycle of gati 3, jathi 5
;;; (gb-plan 5 :gati 3)
;;; => ((5/12) (5/12) (5/12))

jathi 3 cut to last 2 beats.
;;; (gb-plan 3 :beats 2)
;;; => ((1/8) (3/16) (3/16))

Tie first note (remainder for cut) to following note.
;;; (gb-plan 3 :beats 2 :tie-first T)
;;; => ((1/8 tie) (3/16) (3/16))

If the duration is larger than *gati*, rests are inserted. 
;;; (gb-plan 3 :beats 6)
;;; => ((-3/4) (3/16) (3/16) (3/16) (3/16))
"
  (let ((result (if beats
		    (fit-to-span (* beats beat-duration) (constant-cycle gati jathi) :extend extend)
		    (constant-cycle gati jathi))))
    (when tie-first
      (push 'tie (cdr (first result))))
    result))


;; (defmethod gb-plan ((jathis list) &optional duration &key (extend 's))
;;   "Specifies the rhythmic form of a gati bhedam sequence."
;;   (loop for jathi in jathis
;;        collect (gb-plan jathi duration :extend extend)))


(defparameter *gati* 4
  "Current globally set gati, used by `tala-plan`.")

;; (defun group (&rest args)
;;   args)

;; ?? TODO: Allow setting the `gen-karnatic-cell` arg position. But I would need to set that for every tala element, so I might rather define that otherwise, e.g., with gen-karnatic-cell directly.
(defun tala-plan (talas &key (tala *tala*) (gati *gati*) (append-sam? NIL) (accent '-) (tala-sam-accent '-) (beat-duration 1/4))
  "Some mini language for outlining the rhythmic form of a tala sequence, in particular gati bhedam sequences. The result is a rhythmic OMN sequence (expressed in the time signature corresponding to `tala`), where each jathi is represented by a single note (i.e. without rhythmic phrasing).

* Arguments:
  - talas (nested list): a sequence of talas (or tala groups) expressed in a mini language described below.
  - tala (nested list of durations): a tala as returned by function `tala`.
  - gati (integer): the default gati of the result.
  - append-sam? (Boolean): whether or not to append a last beat at the very end.
  - accent (symbol): articulation to use on every first note of a jathis. 
  - tala-sam-accent (symbol): articulation to use on every first note of a tala (group). Intended for debugging/proofreading results.
  - beat-duration (rational): duration of a beat (for the symbol `beat`).

Each element of `talas` is a sublist in the following format. Within such sublist, an integer represents a full jathi cycle (i.e. a gati bhedam sequence with constant jathi starting and ending on a beat). The integer specifies the jathi and also the duration of that cycle measured in beats. The symbol `beat` represents a full beat in the result, while the symbol tied-beat is a beat tied to whatever follows. Additionally, any other rhythmic OMN sequence can occur within a tala specification (inserted into the tala time signatures in the result). For example, a partial jathi cycle can be expressed with the function `gb-plan` as demonstrated in an example below.

IMPORTANT: It is the responsibility of the user to ensure that the elements within a tala spec actually 'fit' into the given tala. This freedom allows to compose gati bhedam sequences that last multiple talas. However, with the argument `tala-sam-accent` it is easy to ensure that the beginnings of all tala (groups) fall on the intended metric position.

* Examples:

A specification of two L D5 talas in gati 5. The first tala is rather simple and accents tala an anga sam. The second tala is a gati bhedam sequence of two full cycles that nevertheless ends on tala sam again. 

;;; (setf *tala* (tala '(:d :l) 5))

;;; (tala-plan '(;; beat expands to '((1/4))
;;; 	        (beat beat 5)
;;; 	        ;; gati bhedam
;;; 	        (3 4))
;;; 	       ;; global gati of the result
;;; 	       :gati 5)

An example demonstrating further tala plan features. 

;;; (tala-plan `((beat beat 5)
;;; 	         ;; partial gati bhedam cycle
;;; 	         ;; NOTE: the global gati is not inherited by independent 
;;;              ;; functions like gb-plan and therefore set again 
;;; 	         (beat beat beat beat ,(gb-plan 4 :beats 3 :gati 5))
;;; 	         ;; tala group lasting over two talas with a change of gati
;;; 	         (4 4 6 :gati 3)
;;; 	         ;; simple reprise
;;; 	         (beat beat 5))
;;; 	       :gati 5
;;; 	       ;; Append a tala sam at the end
;;; 	       :append-sam? T)

Polyphonic example: monophonic results of tala-plan combined in polyphonic score To have multiple
voices with longer tala sequences more easily synchornised, consider somehow splitting the tala
sequences into multiple calls of `tala-plan` per part.

;;; `(:treble ,(tala-plan '((beat beat 5)))
;;;   :bass ,(tala-plan '((3 4))))
 "
  (complete-phrase-in-tala
   (articulate-bars  
    (loop for tala in talas
       append (let* ((actual-tala (tu:before-keyword tala))
		     (key-words (tu:after-keyword tala))
		     (actual-gati (getf key-words :gati gati))
		     (result (loop for x in actual-tala
				append (cond ((integerp x) (gb-plan x :gati actual-gati))
					     ((eql x 'beat) `((,beat-duration)))
					     ((eql x 'tied-beat) `((,beat-duration tie)))
					     (T x)))))
		(articulate-bars result :accent tala-sam-accent :section '(0))
		))
    :accent accent)
   :tala tala :append-sam? append-sam?))

#||
(setf *tala* (tala '(:d :l) 5))

(tala-plan '((beat beat 5)
	     (3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc 
	   :tala-sam-accent 'mart)

(tala-plan `((beat beat 5)
	     ;; partial gati bhedam cycle
	     ;; NOTE: the global gati is not inherited by separate functions like gb-plan
	     (beat beat beat beat ,(gb-plan 4 :beats 3 :gati 5))
	     ;; tala group lasting over two talas with a change of gati
	     (4 4 6 :gati 3)
	     ;; simple reprise
	     (beat beat 5))
	   :gati 5
	   ;; Append a tala sam at the end
	   :append-sam? T)
'((Q C4 C4) (Q C4 C4 C4 C4 C4) (Q C4 C4) (Q C4 C4 5H. 5W 5W 5W) (3W C4 3H TIE) (3H C4 3W 3W 3W 3Q TIE) (Q C4 C4 TIE) (Q C4 H H) (Q C4 C4) (Q C4 C4 C4 C4 C4) (H C4))

;; Polyphonic example
`(:treble ,(tala-plan '((beat beat 5)))
  :bass ,(tala-plan '((3 4))))
||#


(defun gen-matras (gati jathi jathi-number &key prefix suffix)
  "Generates a sequence of matras (equal note durations) where `gati' defines the beat subdivision, `jathi' the number of matras per 'bar' (sublist) and `jathi-number' the resulting number of sublists.

* Arguments:
  - gati (int)
  - jathi (int)
  - jathi-number (int)
  - prefix (length value or length sequence, possibly nested): preceeding phrase
  - suffix (length value or length sequence, possibly nested): succeeding phrase

* Examples:
  gati 5 (quintuplets), jathi 4
  ;;; (gen-matras 5 4 3)

  gati 5 (quintuplets), jathi 4, but preceeded by a quarter note rest.
  ;;; (gen-matras 5 4 3 :prefix '-q)

* Notes:

  See Reina (2016) for details on the terms matras, gati and jathi.
  
  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.

"
  (append 
   (when prefix (ensure-double-list prefix))
   (gen-repeat jathi-number (list (gen-repeat jathi (/ 1/4 gati))))
   (when suffix (ensure-double-list suffix))   
   ))



;;; BUG: Not working anymore?
;;; TODO: consider revising (or an alternative function) definition by using gen-karnatic-cell: stored are then not actual cells, but args for gen-karnatic-cell. E.g., in such a revision I would not be limited to have a durational accent on every cell/pala
#||
;Compiler warnings :
;   In ACCENTED-YATI-PHRASE: In the call to EVEN-LENGTH-RHYTHM with arguments (GATI :TOTAL-DURATION (CCL::*-2 GATI-RATIO LONGEST-PALA-LENGTHS) :TIME-SIG LONGEST-PALA-TIME-SIGS),
;     the variable portion of the argument list ((CCL::*-2 GATI-RATIO LONGEST-PALA-LENGTHS) :TIME-SIG LONGEST-PALA-TIME-SIGS) contains an odd number
;     of arguments and so can't be used to initialize keyword parameters
;     for the current global definition of EVEN-LENGTH-RHYTHM
||#
(defun accented-yati-phrase (gati pala-lengths gap &rest args &key (type '(:srotovahayati :at-end)) &allow-other-keys)
  "Generates the matras sequence for a yati phrase, see Reina (2016, p. 205ff) for details. Resulting sublists are jathis for potential post-processing (e.g., adding an accent on their first notes) before redefining the metric structure (usually to follow the tala, e.g., with `omn-to-time-signature'). 

  NOTE: In contrast to Karnatic music, resulting accents are expressed by durational accents by this function. Hence the word `accented' in the  function name.
  
* Arguments:
  - gati (OMN length): beat subdivision 
  - pala-lengths (list of ints): number of matras per pala 
  - gap (OMN length, typically a rest): length of gaps between palas (always constant). For mridangamyati phrases, when you need different gap lengths, simply append two calls to the present function.
  - type (list of two keywords): specifies first the type of the yati phrase (:srotovahayati, where matras are added or :gopuchayati, where matras are removed over time). Secondly, it sets at which side the pala matras are added or removed  (:at-end :at-front). There are four combinations in total. You can compose mridangamyati and damaruyati yati phrases by combining the results of two calls of this function. Note that inserting matras in the middle is currently not supported.

  Additionally, all `durational-accent' key args are supported.

* Examples:
  ;;; (accented-yati-phrase 's '(4 7 10 13) '-e :type '(:srotovahayati :at-end) :divide-prob 0.3 :merge-prob 0.7 :seed 1)

  ;;; (accented-yati-phrase '3q '(4 7 10 13) '-3e :type '(:gopuchayati :at-front) :divide-prob 0.7 :merge-prob 0.5 :seed 3)

* Notes:

  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
  "
  (let* ((gati-ratio (omn-encode gati))
         (longest-pala-jathis (cons (first pala-lengths) (tu:x->dx pala-lengths)))
         (longest-pala-time-sigs (length->time-signature
                                  (loop for jathi in longest-pala-jathis
				     collect (* jathi gati-ratio))))
         (longest-pala-lengths (apply #'max pala-lengths))
         (longest-pala-matras
	  ;; BUG: even-length-rhythm given args not correct
          (omn :length (even-length-rhythm gati 
                                           :total-duration (* gati-ratio longest-pala-lengths) 
                                           :time-sig longest-pala-time-sigs)))
         (longest-pala (apply #'durational-accent longest-pala-matras (tu:remove-property :type args))))
    (cond 
      ((equal type '(:srotovahayati :at-end))          ; grows at end
       (reverse 
        (loop for palas on (reverse longest-pala)
	   if (rest palas)
	   append (append palas `((,gap)))
	   else append  palas)))
      ((equal type '(:srotovahayati :at-front))        ; grows at front
       (tu:one-level-flat 
        (reverse 
         (loop for palas on longest-pala
	    unless (equal palas longest-pala)
	    collect (append palas `((,gap)))
	    else collect palas))))
      ((equal type '(:gopuchayati :at-end))            ; reduced at end
       (loop for palas on (reverse longest-pala)
	  if (rest palas)
	  append (append (reverse palas) `((,gap)))
	  else append (reverse palas)))
      ((equal type '(:gopuchayati :at-front))          ; reduced at front
       (loop for palas on longest-pala
	  if (rest palas)
	  append (append palas `((,gap)))
	  else append  palas))
      (T (error "Yati phrase type note supported: ~A. Note that the order of keywords must not be swapped. The type is a pair like the following example: (:srotovahayati :at-end)." type)))))

;; (accented-yati-phrase 's '(4 7 10 13) '-e :type '(:srotovahayati :at-front))
;; (accented-yati-phrase 's '(4 7 10 13) '-e :type '(:srotovahayati :at-end))


#||
;;; TODO:
(defun change-gati (sequence in-gati out-gati)
  "Changes the duration of notes in `sequence' to fit into the given gati without changing their jathi."
  )

;;; TODO:
(defun change-jathi (sequence jathi)
  "Changes the grouping of notes in `sequence' to follow the given jathi but adding removing notes.

The sequence is supposed to be arranged such that each sublist is one jathi."
  )
||#



