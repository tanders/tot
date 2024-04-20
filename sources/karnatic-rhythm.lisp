;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
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
  "Return a tala given a list of angas (see function anga), i.e. a list of rhythmic values, each wrapped in a sublist.

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
;; (tala '(:l) 4)  ; = 4/4
;; (tala '(:l) 3)  ; = 3/4


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
  "Return list of OMN time signatures (every anga represented as a separate bar) whose overall duration matches the length of `sequence' (or is longer)."
  (length->time-signature (length-span (total-duration sequence) (flatten tala))))

;; (tala-time-signatures (tala '(:l :d :l :l) 3))

;; (tala-time-signatures '(9/2))
;; (tala-time-signatures '(10/4))

;; (tala-time-signatures (tala '(:d :l) 3))


(defun complete-phrase-in-tala (sequence &key (tala *tala*) (append-sam? NIL))
  "For preview within the time signature sequence corresponding to tala. Optionally, append a next tala sam. `sequence' should be a full tala or multiple talas for this to work.

* Arguments:
 - sequence (OMN sequence)
 - tala (nested list of durations): a tala as returned by function tala
 - append-sam? (Boolean): whether or not to append a last beat at the very end

Note: ties in sequence are preserved, if it contains pitches (a pitch for just the first note is fine), but not for purely rhythmic sequences.

;;; TODO: Generalise to better work if sequence is not full tala
"
  (when (/= 0 (mod (total-duration sequence) (total-duration tala)))
   (warn "Sequence does not exactly fit into tala"))
  (let* ((last-tala-sam (first tala))
	 (seq (if append-sam?
		  (append sequence last-tala-sam)
		  sequence)))
    (omn-to-time-signature seq (tala-time-signatures seq tala))))

#|
(complete-phrase-in-tala '((7/2 c4)) :tala (tala '(:d :l) 5))
(complete-phrase-in-tala '((7/2 c4)) :tala (tala '(:d :l) 5) :append-sam? T)
;; Issue warning
(complete-phrase-in-tala '((8/2 c4)) :tala (tala '(:d :l) 5))
|#


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
;; See Reina (2016, p. 23f)
(let* ((accented-1 ; non-standard 
	(apply #'vector '((1))))
       (unaccented-1
	(apply #'vector '()))
       (both-1 (vector accented-1 unaccented-1))
       (accented-2 ; non-standard 
	(apply #'vector '((2)
			  (1 1))))
       (unaccented-2
	(apply #'vector '()))
       (both-2 (vector accented-2 unaccented-2))
       (accented-3 ; tisra 
	(apply #'vector '((3)
			  (2 1)
			  (1 1 1))))
       (unaccented-3 ; tisra 
	(apply #'vector '((1 2))))
       (both-3 (vector accented-3 unaccented-3))
       (accented-4 ; chatusra 
	(apply #'vector '((4)
			  (3 1)
			  (2 2)
			  (2 1 1)
			  (1 1 1 1))))
       (unaccented-4 ; chatusra 
	(apply #'vector '((1 3)
			  (1 2 1)
			  (1 1 2))))
       (both-4 (vector accented-4 unaccented-4))
       (accented-5; khanda 
	(apply #'vector '((5)
			  (4 1)
			  (3 2)
			  (3 1 1)
			  (2 2 1)
			  (2 1 1 1)
			  (1 1 1 1 1))))
       (unaccented-5 ; kanda 
	(apply #'vector
	       '((2 3) 
		 (1 4)
		 (2 1 2)
		 (1 3 1)
		 (1 2 2)
		 (1 1 3)
		 (1 2 1 1)
		 (1 1 2 1)
		 (1 1 1 2))))
       (both-5 (vector accented-5 unaccented-5))
       (accented-6 ; tisra 
	(apply #'vector
	       '((6)
		 (5 1)
		 (4 2)
		 (3 3)
		 (4 1 1)
		 (3 2 1)
		 (2 2 2)
		 (3 1 1 1)
		 (2 2 1 1)
		 (2 1 1 1 1)
		 (1 1 1 1 1 1))))
       (unaccented-6 ; tisra
	(apply #'vector
	       '((2 4)
		 (1 5)
		 (3 1 2) ; borderline whether accented or not
		 (2 3 1) (2 1 3) (1 4 1) (1 3 2) (1 2 3) (1 1 4) 
		 (2 1 2 1) (2 1 1 2) (1 3 1 1) (1 2 2 1) (1 2 1 2) (1 1 3 1) (1 1 2 2) (1 1 1 3)
		 (1 2 1 1 1) (1 1 2 1 1) (1 1 1 2 1) (1 1 1 1 2)
		 )))
       (both-6 (vector accented-6 unaccented-6))
       (accented-7 ; misra 
       	(apply #'vector
       	       '((7)
       		 (6 1) (5 2) (4 3)
       		 (5 1 1) (4 2 1) (4 1 2) (3 3 1) (3 2 2)
       		 (4 1 1 1) (3 2 1 1) (3 1 2 1) (3 1 1 2) (2 2 2 1)
       		 (3 1 1 1 1) (2 2 1 1 1)
		 (2 1 1 1 1 1)
		 (1 1 1 1 1 1 1))))
       (unaccented-7 ; misra
	(apply #'vector
	       '((1 6) (3 4) (2 5)
		 (3 1 3) (2 4 1) (2 3 2) (2 2 3) (2 1 4)
		 (1 5 1) (1 4 2) (1 3 3) (1 2 4) (1 1 5)
		 (2 3 1 1) (2 2 1 2) (2 1 3 1) (2 1 2 2) (2 1 1 3)
		 (1 4 1 1) (1 3 2 1) (1 3 1 2) (1 2 3 1) (1 2 2 2) (1 2 1 3) (1 1 4 1) (1 1 2 3) (1 1 1 4)
		 (2 1 2 1 1) (2 1 1 2 1) (2 1 1 1 2)
		 (1 3 1 1 1) (1 2 2 1 1) (1 2 1 2 1) (1 2 1 1 2)
		 (1 1 3 1 1) (1 1 2 2 1) (1 1 2 1 2) (1 1 1 3 1) (1 1 1 2 2) (1 1 1 1 3)
		 (1 2 1 1 1 1) (1 1 2 1 1 1) (1 1 1 2 1 1) (1 1 1 1 2 1) (1 1 1 1 1 2))))
       (both-7 (vector accented-7 unaccented-7))
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
		      ;; TODO: assert that all gati, jathi and position values are integers 
		      (assert (integerp gati) (gati) "gati must be integer: ~A" gati)
		      (assert (integerp jathi) (jathi) "jathi must be integer: ~A" jathi)
		      (assert (or (integerp position)
				  (eql '? position))
			      (position) "position must be integer: ~A" position)
		      (if (< jathi 0)
			  ;; Negative jathi stands for a rest. Can come from gen-karnatic-cell*
			  ;; Return a single rest of corresponding length
			  (list (* -1 (first (gen-karnatic-cell gati (* -1 jathi) 0))))
			  (gen-karnatic-cell gati jathi position
					     :accented? accented? :max-number max-number :min-number min-number
					     :first-length first-length :include-length include-length :exclude-length exclude-length
					     :seed (seed))))
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
					      (7 both-7)))
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
	       (last-possible-position (1- (length all-filtered))))
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

;; !! TODO: Introduce two additional keywords setting with boolean or binary integer whether last event in current cell is tied to first event of following cell and whether returned cell (or some of its events?) is/are a rest.
;;   - Allow for turning cells or parts of them into rests (perhaps in a separate function?)
;; ?? TODO: Consider to count as accented also all cells that would result in a durational accent on a longer note immediately following the returned cell.
(defun gen-karnatic-cell (gati jathi position
			  &key (accented? T) (max-number nil) (min-number nil) (first-length nil)
			    (include-length nil) (exclude-length nil) (seed nil))
  "Returns a Karnatic rhythmic cell (list of rhythmic values) or a list of such cells (if `position' is a list). The shape of the resulting cell(s) can be controlled in many ways, allowing to generate, e.g., rhythms that are similar even if they are in different gatis or jathis.

See Reina (2016) for details on terms like gati, jathi and matra.

* Arguments:
- gati (integer -- commonly in {3, 4, 5, 7} -- or list of them): gati for the cell(s) to generate.
- jathi (integer in range 1-7, or list of them): jathi for the cell(s) to generate.
  1 and 2 no standard Karnatic jathi, but added for more flexibility preserving consistency. Note that for jathi 1 and 2 no unaccented cells are defined (NIL will be returned).
- position (integer, '?, or list of either): specifies which cell of the available options to generate. If no filtering is enabled (see further arguments below) then the list of available options are all the possible standard subdivisions of a 'beat' depending on the current jathi as listed by (Reina, 2016, p. 23f). These options are sorted  by the number of notes in rhythmic cells (fewest first), and in case of equal note numbers the length of notes starting with the first note (longer first note first). So, the position 0 is always a single note per beat/cell (length depends on gati and jathi) and so on. If `position' exceeds the number of available options, the last option is returned, which is always an even subdivision of the cell in jathi matras. 
If `position' is '? then the position is randomised.
- accented? (Boolean, or binary integer, i.e. 0 or 1): whether the returned cells potentially carry durational accents on the start of the cells. If accented? is nil (or 0) then the cell(s) carry a durational accent that is not on the start of the cell. Binary integers are supported so that values for this argument can be generated with Opusmodus' binary number generators.
- max-number/min-number (integer or list of them): max/min number of notes in the cell(s).
- first-length (ratio or OMN length, or list of either): length of the first note in cell(s).
- include-length/exclude-length (ratio or OMN length, or list of either): length values that must be included in or excluded from the cell(s).
- seed (integer): the seed to use for the randomised position, if position is '?.

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
  ;;; (gen-karnatic-cell 4 6 '? :max-number 3)

... or the first note value in the cell can be set.
  ;;; (gen-karnatic-cell 4 6 '(? ? ?) :first-length 3/16)

  Length values for, e.g., first-length, must be in gati.
  ;;; (gen-karnatic-cell 5 6 '(? ? ?) :first-length '5h.)

All these filter arguments also support lists for setting different values for each sublist.
  ;;; (gen-karnatic-cell 4 4 '(0 0) :exclude-length '(e q))

Again, an example with randomised positions, but here the seed is fixed.
  ;;; (gen-karnatic-cell 4 4 '(? ?) :min-number 2 :seed 1)

Filtering can remove all options for cells, in which case nil returned.
  ;;; (gen-karnatic-cell 4 4 '? :min-number 5)

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


;; TODO:
;;  - Allow tala-plan to contain rests
(defun gen-karnatic-cell* (tala-plan gati position
			   &key (accented? T) (max-number nil) (min-number nil) (first-length nil)
			     (include-length nil) (exclude-length nil) (seed nil))
  "Like gen-karnatic-cell, but a list of jathis is automatically deduced from `tala-plan' (flat OMN rhythmic sequence) based on `gati'. Every single note in `tala-plan' is assumed to stand for a rhythmic cell, which is phrased by gen-karnatic-cell.

In contrast to `gen-karnatic-cell', the length of the result depends on the length of `tala-plan' (not the length of `position').

NOTE:  `tala-plan' can contain durations that for the given gati are beyond the available jathis (e.g., length h corresponds to jathi 8, which is not supported). Workaround: avoid such durations, instead use ties (e.g., q tie q). 

NOTE: Handling gati changes in tala-plan is tricky. Better process subsequences with different gati separately (e.g., using `unfold-subseqs').

* Examples:

;;; (gen-karnatic-cell* '(q q q q) 4 '(0 1 2 3))

;;; (gen-karnatic-cell* '(q e. e s) 4 1)

A plan with a rest.
;;; (gen-karnatic-cell* '(q -q q) 4 1)

;; TODO: examples using tala-plan: see comment below definition


"
  (let* ((n (length tala-plan))
	 (articulations-included? (some #'articulationp (flatten tala-plan)))
	 ;; Jathi negative for rests
	 (jathi (get-tala-plan-jathi (flatten (omn :length tala-plan)) gati))
	 (result (gen-karnatic-cell gati jathi (circle-repeat position n)
				    :accented? accented? :max-number max-number :min-number min-number :first-length first-length
				    :include-length include-length :exclude-length exclude-length :seed seed)))
    (if articulations-included?
	;; Re-insert articulations: ties at end of every sublist and other articulations added to first event in sublist
	(loop
	   for cell in result
	   for (articulation) in (omn :articulation (tu:ensure-nested-list tala-plan))
	   for articulation-list = (disjoin-attributes articulation)
	   for tie = (find 'tie articulation-list)
	   for articulation-without-tie = (merge-articulations (remove 'tie articulation-list))
	   for articulations-without-tie = (cons (or articulation-without-tie '-)
						 (make-list (1- (length (flatten cell)))
							    :initial-element '-))
	   for articulations-to-use = (append (butlast articulations-without-tie)
					      (list (merge-articulations (list (or tie '-)
									       (first (last articulations-without-tie))))))
	   collect (make-omn :length cell
			     :pitch '(c4)
			     :articulation articulations-to-use))
	result)))
#|
;; Several tests, all using always only `position' 1 for clarity of results. Results musically stupid, though.

;; Example with a tie
(setf my-plan '((Q C4 P1+MART+MARC+TIE) (Q C4 MARC) (Q C4 MARC)))
(gen-karnatic-cell* my-plan 5 1)

;; Example with my-plan
(setf my-plan (tala-plan '((q p1 q 5)
			   (3 p2 4 p3))
			 ;; global gati of the result
			 :gati 5
			 :accent 'marc
			 :tala-sam-accent 'mart
			 ;; :tala-sam-accent 'stacc
			 :complete-phrase-in-tala? NIL))
(gen-karnatic-cell* my-plan 5 1)

;; Example with different gatis, using unfold-subseqs for differnet subseqs
(setf my-plan (tala-plan `((q p1 q 5)
			   ;; partial gati bhedam cycle
			   ;; NOTE: the global gati is not inherited by separate functions like gb-plan
			   (q p2 q q q ,(gb-plan 4 :beats 3 :gati 5))
			   ;; tala group lasting over two talas with a change of gati
			   (4 p3 4 6 :gati 3)
			   ;; simple reprise
			   (q p1 q 5))
			 :gati 5
			 :accent 'marc
			 :tala-sam-accent 'mart
			 :complete-phrase-in-tala? NIL))
;; Preview score
`(:phrased: ,(complete-phrase-in-tala 
	      (unfold-subseqs '(p1 ((gen-karnatic-cell* 5 1))
				p2 ((gen-karnatic-cell* 5 2))
				p3 ((gen-karnatic-cell* 3 1)))
			      my-plan)
	      :tala *tala* ::append-sam? T)
  :plan: ,(complete-phrase-in-tala my-plan  :tala *tala* ::append-sam? T))
|#



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
  "Outlines (plans) the rhythmic form of a gati bhedam (gb) sequence. The result is an OMN sequence where each jathi is represented by a single note (i.e. without rhythmic phrasing) wrapped in its own sublist.

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

If the duration is larger than gati, rests are inserted. 
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


(defun tala-plan (tala-specs &key (tala *tala*) (gati *gati*) (accent '-) (tala-sam-accent '-) ; (beat-duration 1/4)
			       (complete-phrase-in-tala? T) (append-sam? NIL))
  "Some mini language for outlining the rhythmic form of a tala sequence, in particular gati bhedam sequences. The result is a rhythmic OMN sequence -- expressed in the time signature(s) corresponding to `tala' -- where each rhythmic cell is represented by a single note (i.e. without rhythmic phrasing).

* Arguments:
  - tala-specs (nested list): a sequence of talas (or tala groups) expressed in a mini language described below.
  - tala (nested list of durations): a tala as returned by function `tala`.
  - gati (integer): the default gati of the result.
  - accent (symbol): OMN articulation to use on every first note of a jathi. 
  - tala-sam-accent (symbol): OMN articulation to use on every first note of a tala (group). Intended for debugging/proofreading results.
  - beat-duration (rational): duration of a beat (for the symbol `beat`).
  - complete-phrase-in-tala? (Boolean): whether or not to call complete-phrase-in-tala on result.
  - append-sam? (Boolean): whether or not to append a last beat at the very end (only applies if complete-phrase-in-tala?).

Each element of `tala-specs' is a sublist in the following format. Within such sublist, an integer represents a full jathi cycle (i.e. a gati bhedam sequence with constant jathi starting and ending on a beat). The integer specifies the jathi and also the duration of that cycle measured in beats. Any standard OMN length represents a rhythmic cell of the corresponding duration. E.g., if the underlying beat is 1/4, the symbol q represents a full beat in the result. Any other rhythmic OMN sequence can occur within a tala specification (inserted into the tala time signatures in the result). For example, a partial jathi cycle can be expressed with the function `gb-plan' as demonstrated in an example below. Additionally, arbitrary OMN articulations including ties can be inserted into a tala spec, and such articulation is then added to the preceeding (first) event.

IMPORTANT: It is the responsibility of the user to ensure that the elements within a tala spec actually 'fit' into the given tala. This freedom allows to compose gati bhedam sequences that last multiple talas. However, with the argument `tala-sam-accent' it is easy to ensure that the beginnings of all tala (groups) fall on the intended metric position.

The result of `tala-plan' can be further processed, e.g., using `gen-karnatic-cell*' and `unfold-subseqs'. See `gen-karnatic-cell*' (possibly after that def in comments) for examples.

BUG: Inserting ties currently broken due to a bug in builtin omn-replace (called by articulate-bars). Wait until that is fixed or define workaround in articulate-bars.

* Examples:

A specification of two L D5 talas in gati 5. The first tala is rather simple and accents tala an anga sam. The second tala is a gati bhedam sequence of two full cycles that nevertheless ends on tala sam again. 

;;; (setf *tala* (tala '(:d :l) 5))

;;; (tala-plan '((q q 5)
;;; 	        ;; gati bhedam
;;; 	        (3 4))
;;; 	       ;; global gati of the result
;;; 	       :gati 5)

An example demonstrating further tala plan features. 

;;; (tala-plan `((h 5)
;;; 	         ;; partial gati bhedam cycle
;;; 	         ;; NOTE: the global gati is not inherited by independent 
;;;              ;; functions like gb-plan and therefore set again 
;;; 	         (q q q q ,(gb-plan 4 :beats 3 :gati 5))
;;; 	         ;; tala group lasting over two talas with a change of gati
;;; 	         (4 4 6 :gati 3)
;;; 	         ;; simple reprise
;;; 	         (q q 5))
;;; 	       :gati 5
;;; 	       ;; Append a tala sam at the end
;;; 	       :append-sam? T)


TODO: Examples with inserted articulations. (See exmaples in comments after this def for now)

Polyphonic example: monophonic results of tala-plan combined in polyphonic score To have multiple
voices with longer tala sequences more easily synchornised, consider somehow splitting the tala
sequences into multiple calls of `tala-plan` per part.

;;; `(:treble ,(tala-plan '((beat beat 5)))
;;;   :bass ,(tala-plan '((3 4))))
 "
  (let* ((full-OMN-required? (some #'articulationp (flatten tala-specs)))
	 (result
	  (loop for tala-spec in tala-specs
	     append (let* (;; Single tala spec brokwn into sublists where any OMN articulation is a
			   ;; second element in a sublist
			   (actual-tala-spec (tu:split-if (lambda (x) (not (articulationp x)))
							  (tu:before-keyword tala-spec)))
			   (key-words (tu:after-keyword tala-spec))
			   (actual-gati (getf key-words :gati gati))
			   ;; Destructured arg artic quasi optional, can be NIL
			   (aux (loop for (x artic) in actual-tala-spec
				   for lengths = (cond ((integerp x) (gb-plan x :gati actual-gati))
						       ((lengthp x) `((,x)))
						       ;; ((eql x 'beat) `((,beat-duration)))
						       ;; ((eql x 'tied-beat) `((,beat-duration tie)))
						       ((every #'lengthp (flatten x)) x)
						       (T (error "~A not supported value" x)))
				   ;; do (break)
				   append (if full-OMN-required?
					      ;; articulate-bars (called below) expects either a plain lengths or a full OMN seq
					      (make-omn :length lengths
							:pitch '(c4)
							:articulation (cons (or artic '-)
									    (make-list (1- (length (flatten lengths)))
										       :initial-element '-)))
					      lengths))))
		      ;; (break)
		      (articulate-bars aux :accent tala-sam-accent :section '(0))
		      )))
	 (marked-result (articulate-bars result :accent accent)))
    ;; (break)
    (if complete-phrase-in-tala?
	(complete-phrase-in-tala marked-result :tala tala :append-sam? append-sam?)
	marked-result)))

#||
(setf *tala* (tala '(:d :l) 5))

(tala-plan '((q q 5)
	     (3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc
	   ;; :tala-sam-accent 'mart
	   :tala-sam-accent 'stacc
	   :append-sam? T)

(tala-plan '((q tie q 5)
	     (3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc
	   ;; :tala-sam-accent 'mart
	   :tala-sam-accent 'stacc
	   :append-sam? T)


(tala-plan '((q p1 q 5 p2)
	     (3 p3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc
	   ;; :tala-sam-accent 'mart
	   :tala-sam-accent 'stacc
	   :append-sam? T)


(tala-plan '((beat beat 5)
	     (3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc
	   ;; :tala-sam-accent 'mart
	   :tala-sam-accent 'stacc
	   :append-sam? T)

(tala-plan '((1 2 4) ;; BUG: Not a full tala
	     (3 4))
	   ;; global gati of the result
	   :gati 5
	   :accent 'marc
	   :tala-sam-accent 'mart
	   :append-sam? T)

(tala-plan `((q q 5)
	     ;; partial gati bhedam cycle
	     ;; NOTE: the global gati is not inherited by separate functions like gb-plan
	     (q q q q ,(gb-plan 4 :beats 3 :gati 5))
	     ;; tala group lasting over two talas with a change of gati
	     (4 4 6 :gati 3)
	     ;; simple reprise
	     (q q 5))
	   :gati 5
	   ;; Append a tala sam at the end
	   :append-sam? T)
'((Q C4 C4) (Q C4 C4 C4 C4 C4) (Q C4 C4) (Q C4 C4 5H. 5W 5W 5W) (3W C4 3H TIE) (3H C4 3W 3W 3W 3Q TIE) (Q C4 C4 TIE) (Q C4 H H) (Q C4 C4) (Q C4 C4 C4 C4 C4) (H C4))

;; Polyphonic example
`(:treble ,(tala-plan '((beat beat 5)))
  :bass ,(tala-plan '((3 4))))
||#



#|
;; Note: no distinction between gatis 3 and 6. Would depend on context (i.e. gati of whole tala)
(let ((gati-mappings (alexandria:alist-hash-table
		      '((1. 4)
			(2 . 4)
			(3 . 6)
			;; (4 . 4)
			(5 . 5)
			;; (6 . 6)
			(7 . 7)))))
  (defmethod get-tala-plan-gati ((length rational))
    "Return int represeting gati of `length', assuming that each jathi is represented by a single note (i.e. without rhythmic phrasing) as returned by `tala-plan'.

BUG: Not reliably working, as correct gati depends not on a single value, but on context. For example, the duration q can be used in gati, and a duration like 3q allows for multiple gatis (3 and 6).
!! Avoid using this function for now.
"
    (let ((max-denom-prime (apply #'max (prime-factors (denominator length)))))
      (gethash max-denom-prime gati-mappings))))
(defmethod get-tala-plan-gati ((length symbol))
  (get-tala-plan-gati (omn-encode length)))
(defmethod get-tala-plan-gati ((length list))
  (mapcar #'get-tala-plan-gati (flatten length))
  #|
  (let* ((length-gati-pairs (loop for l in (flatten length)
			       collect (list l . (get-tala-plan-gati l))))))
  |#
  )
#|
(get-tala-plan-gati 2/3)
(get-tala-plan-gati 2/6)
(get-tala-plan-gati 4/5)
(get-tala-plan-gati 3/7)

;; BUG: Can be any gati, depends on context (i.e. gati of whole tala)
;; TODO: Turn get-tala-plan-gati and get-tala-plan-jathi into aux functions and define function on top that considers a whole tala and assumes a constant gati within a single tala. Remember that jathis can cross talas, but the determination of gatis should be fine per tala anyway.
(get-tala-plan-gati 'q) 
(get-tala-plan-gati '3h)
(get-tala-plan-gati '5h)


(get-tala-plan-gati '(3h 3h 3h))
(get-tala-plan-gati '(5h 5h 5h 5h 5h))
|#
|#

(defmethod get-tala-plan-jathi ((length rational) (gati integer) &key (beat-duration 1/4))
  "Return int represeting jathi of `length', assuming that each jathi is represented by a single note (i.e. without rhythmic phrasing) as returned by `tala-plan'."
  (values (* length (/ gati beat-duration))
	  gati))
(defmethod get-tala-plan-jathi ((length symbol) (gati integer) &key (beat-duration 1/4))
  (get-tala-plan-jathi (omn-encode length) gati :beat-duration beat-duration))
(defmethod get-tala-plan-jathi ((lengths list) (gati integer) &key (beat-duration 1/4))
  (loop for length in lengths
       collect (get-tala-plan-jathi length gati :beat-duration beat-duration)))
#|
(get-tala-plan-jathi 3/16 4)
(get-tala-plan-jathi 4/5 5)

(get-tala-plan-jathi 'e. 4)
(get-tala-plan-jathi 'e. 4 :beat-duration 1/8)
(get-tala-plan-jathi 'e. 4 :beat-duration 1/2)

(get-tala-plan-jathi 'q 4)
(get-tala-plan-jathi 'q 5)

(get-tala-plan-jathi '3q 3)
(get-tala-plan-jathi '3h 3)
(get-tala-plan-jathi '5h 5)

(get-tala-plan-jathi '(q e. e.) 4)
|#



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

  With a negative gati, rests are returned. (Results in preview error for incomplete beats)
  ;;; (gen-matras -4 4 1)
  ;;; (gen-matras -5 5 2)

* Notes:

  See Reina (2016) for details on the terms matras, gati and jathi.
  
  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.

"
  (append 
   (when prefix (ensure-double-list prefix))
   (gen-repeat jathi-number (list (gen-repeat jathi (/ 1/4 gati))))
   (when suffix (ensure-double-list suffix))   
   ))


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
	 (longest-pala-total-duration (* gati-ratio longest-pala-lengths))
	 (longest-pala-aux (even-length-rhythm longest-pala-total-duration gati :time-sig longest-pala-time-sigs))
         (longest-pala-matras (omn :length longest-pala-aux))
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



(defun shared-karnatic-constraints (time-sigs stoptime)
  "
  [Rhythmic rule combination] Return constrains that are shared by multiple CSPs for Karnatic
  rhythmic concepts (a CSP searching for only a single voice).
  - The underlying meter follows time-sigs
  - No note ties over stoptime
  - The search ends with stoptime 
  "
  (ce::rules->cluster
   ;; Fixing the time signatures is no limitation for Karnatic music
   (ce:r-predefine-meter (om-time-sigs->cluster-engine-time-sigs time-sigs))
   
   ;; Some ends exactly at stoptime (e.g., tala sam)
   (ce::R-rhythms-one-voice-at-timepoints
    ;; Some note is starting exactly at stoptime (its offset = 0)
    #'(lambda (offset+dur) (equal (first offset+dur) 0)) 
    0 (list stoptime)
    ;; Ideally, I may want to apply such rule to the end of a rhythmic motif, but the params
    ;; :motif-start and :motif-end are seemingly43 not working.
    :dur-start)

   (ce:stop-rule-time 0 stoptime :and)
   ))


;; ? TODO: derive orig-gati automatically
(defun rhythmical-sangati (old-gati new-gati new-jathi sequence)
  "Return a rhythmical sangati, i.e. a rhythmic variation that quasi imitates the given rhythm, but in a simply different ‘tempo’ (durations multiplied with constant factor to fit into new gati), or different tempo and also different accent pattern. For details, see Reina (2016, chap. 5, p. 61).

Jathis in the result are represented as sub lists (quasi measures), so any post-processing of the result can take them into account. In the end, you can 're-bar' the result with the correct tala/time signature with functions like omn-to-time-signature or complete-phrase-in-tala. 

* Arguments:
 - sequence (list): a sequence of OM lengths values (the given rhythm)

* Examples:

  ;;; (setf orig-gati 4)
  ;;; (setf rhy (gen-karnatic-cell orig-gati 4 '(? ? ? ? ?) :first-length '(3/16 2/16 2/16 1/16 1/4) :seed 1))

  The original gati (4) becomes the jathi in the new gati 5
  ;;; (rhythmical-sangati orig-gati 5 4 rhy)

  Gati bhedam: only the jathi is changed to 5.
  NOTE: New note-onsets may be added (instead of ties) in case the input seq is a purely rhythmical sequence.
  ;;; (rhythmical-sangati orig-gati 4 5 rhy)

  The same gati bhedam seq as above, but with ties.
  NOTE: Ties are preserved if the input seq is a full OMN sequence.
  ;;; (setf omn-seq (make-omn :length rhy :pitch '(c4)))
  ;;; (rhythmical-sangati orig-gati 4 5 omn-seq)

  The new gati 5 is also the jathi (in case the same as the original jathi)
  ;;; (rhythmical-sangati orig-gati 5 5 omn-seq)

  Use a different gati and jathi than in the original.
  ;;; (rhythmical-sangati orig-gati 5 3 omn-seq)

  Use gati 3 instead.
  ;;; (rhythmical-sangati orig-gati 3 4 omn-seq)

* Notes:
  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
"
  ;; Simple case: the original gati becomes the jathi in the new gati
  (let ((seq-w-updated-gati (length-diminution new-gati (length-augmentation old-gati sequence))))
    (omn-to-time-signature seq-w-updated-gati (list new-jathi (* new-gati 4)))
    ))


(defun beat-time-points (tala-time-signatures stoptime)
  "Return list of time points of beats in the given tala up to stoptime. (The time points 0 and stoptime itself are excluded.)"
  ;; BUG: Only works if length of tala-time-signatures is 1
  (let ((beat-dur (/ 1 (second (first tala-time-signatures)))))
    (loop for timepoint from beat-dur below stoptime by beat-dur
       collect timepoint)))
; (beat-time-points '((4 4 1)) 2)
; (beat-time-points '((5 8 1)) 2)
; (beat-time-points '((4 4 1)) 1)

(defun tala-sam-time-points (tala-time-signatures stoptime)
  "Return list of time points of tala sam occurrences in the given tala up to stoptime. (The time points 0 and stoptime itself are excluded.)"
  (let ((tala-dur (time-signature-length tala-time-signatures :sum T)))
    (loop for timepoint from tala-dur below stoptime by tala-dur
       collect timepoint)))
; (tala-sam-time-points '((4 4 1)) 3)
; (tala-sam-time-points (tala-time-signatures (tala '(:l :d :l :l) 3)) 3)


;; TODO: Add phrasing support
(defun jathi-bhedam (gati matras &key (allowed-matra-durs '(1 2 3 4 5 6 7 8 9)) (tala-time-signatures '((4 4 1)))
				   (max-no-of-variables 100) (seed 1)) 
  "Return jathi bhedam rhythmic sequence, i.e. a rhythmic sequence where no jathi is perceivable. See Reina (2016, chap. 6, p. 69ff). The returned sequence is 'raw' = un-phrased in the sense that the longest possible notes are used.

* Arguments:
 - gati (int): the constant gati of the result
 - matras (int): the overall duration of the result measured in matras.
 - allowed-matra-durs (lists of ints): in the result, only note durations with the given number of matras are allowed. Be careful not to over-constraint the internal CSP.
 - tala-time-signatures (list of OM time signatures): time signatures of the result. If shorter than result, further time signatures are randomly chosen!
 - max-no-of-variables (int): the returned result is automatically truncated after the required number of matras. This parameter controls roughly the maximum number of rhythmic values that is possible (controlling the generation of the interal Cluster Engine CSP for computing the result).
 - seed (int): random seed.

* Examples:
  
;;; (jathi-bhedam 4 16 :seed 1)
;;; (jathi-bhedam 4 16 :seed 2)

;;; (jathi-bhedam 4 32 :seed 1)
;;; (jathi-bhedam 4 32 :seed 2)

Gati 5
;;; (jathi-bhedam 5 30 :tala-time-signatures '((3 4 1)) :seed 1)
;;; (jathi-bhedam 5 30 :tala-time-signatures '((3 4 1)) :seed 2)

Gati 3
;;; (jathi-bhedam 3 18 :tala-time-signatures '((3 4 1)) :seed 1)
;;; (jathi-bhedam 3 36 :seed 1)

Restrict what note durations are possible.
;;; (jathi-bhedam 4 32 :allowed-matra-durs '(2 3) :seed 1)
;;; (jathi-bhedam 4 32 :allowed-matra-durs '(2 3) :seed 2)

;;; (jathi-bhedam 4 32 :allowed-matra-durs '(3 5) :seed 1)
;;; (jathi-bhedam 4 32 :allowed-matra-durs '(3 5) :seed 2)

Example where result does not exactly fit into multiple talas (but at least multiple beats in this case).
;;; (jathi-bhedam 3 33 :seed 1)

Example with more complex tala.
;;; (jathi-bhedam 4 40 :tala-time-signatures (gen-repeat 2 (tala-time-signatures (tala '(:d :l) 3))) :seed 2)

* Notes:
  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
  "
  (assert (every #'integerp (list gati matras max-no-of-variables seed)))
  (let* ((matra-dur (/ 1/4 gati))
	 (stoptime (* matra-dur matras)))
    ;; (break)
    ;; (when (/= (mod (* matras matra-dur)
    ;; 		   (time-signature-length tala-time-signatures :sum t))
    ;; 	      0)
    ;;   (warn "Jathi bhedam sequence does not exactly fit into full talas."))
    (fit-to-span
     stoptime
     (cluster-engine-snippet
      (let ((*random-state* (ta-utils:read-random-state seed)))
	(let* (;; Possible jathis in given gati
	       (dur-domain (loop for jathi in (rnd-order allowed-matra-durs :seed seed)
			      collect (list (* matra-dur jathi))))
	       (pitch-domain '((60)))
	       (tala-sams (tala-sam-time-points tala-time-signatures stoptime))
	       (beats (beat-time-points tala-time-signatures stoptime))
	       (beat-dur (first beats)))
	  ;; (break)
	  (cr:cluster-engine
	   max-no-of-variables
	   (ce::rules->cluster

	    ;; Never accent any intermediate tala sam before the final tala sam
	    (ce::R-rhythms-one-voice-at-timepoints
	     #'(lambda (offset+dur) (/= (mod (first offset+dur) beat-dur) 0)) 
	     0 tala-sams :dur-start)
	    
	    ;; No accent should fall on a beat until at least the 4th accent of the sequence has been reached
	    (let ((first-3-beats (if (> (length beats) 3)
	    			     (subseq beats 0 4)
	    			     beats)))
	      (ce::R-rhythms-one-voice-at-timepoints
	       #'(lambda (offset+dur) (/= (mod (first offset+dur) beat-dur) 0)) 
	       0 first-3-beats :dur-start))
	    
	    ;; Never accent two consecutive beats
	    (ce::R-rhythms-one-voice-at-timepoints
	     #'(lambda (offset+dur1 offset+dur2)
	    	 (if (= (mod (first offset+dur1) beat-dur) 0)
	    	     (/= (mod (first offset+dur2) beat-dur) 0)
	    	     T))
	     0 beats :dur-start)

	    ;; Do not place the same number more than 3 times in a row (no jathi feeling allowed)
	    ;; duration seq  constraint
	    (ce:r-rhythms-one-voice
	     #'(lambda (dur1 dur2 dur3 dur4)
	    	 (>= (length (remove-duplicates (list dur1 dur2 dur3 dur4)))
	    	     2))
	     0 :durations)

	    ;; Preferred durs
	    (when preferred-matra-durs
	      (let ((preferred-durs (loop for jathi in preferred-matra-durs
				       collect (list (* matra-dur jathi)))))
		(ce:r-rhythms-one-voice
		 #'(lambda (dur)
		     ;; (break)
		     (member dur preferred-durs))
		 0 :durations :heur-switch)))
	    	    
	    (shared-karnatic-constraints tala-time-signatures stoptime)
	    )

	   (cluster-engine-time-sigs-domain tala-time-signatures)	
	   (list dur-domain pitch-domain)
	   )
	  )
	))
     ;; Cut too long result at end
     :extend 'e)
  ))


(defun short-mukthay (gati matras &key (gap-expansion 0) (type :phrased))
  "Return OMN rhythmic sequence that expresses a 'raw' short mukthay over the given number of matras in the given gati. A 'raw' mukthay here is an un-phrased mukthay, where each matra of each pala is 'played'. Palas are represented in sublists to simplify further processing.

* Arguments:
  - gati (int)
  - matras (int): length of the result measured in matras of the given gati
  - gap-expansion (int): control for lengthening the gaps between palas in the result.
  - type (either :phrased or :raw): if 

* Examples:
  
A short mukthay over 13 matras in gati 4. By default, the shorted possible gaps are returned.
  ;;; (short-mukthay 4 16)

A short mukthay over 17 matras.
  ;;; (short-mukthay 4 17)

Again, a short mukthay over 17 matras in gati 4, but now with a longer gap.
  ;;; (short-mukthay 4 17 :gap-expansion 1)

The gap could in principle be further extended.
  ;;; (short-mukthay 4 17 :gap-expansion 2)

A short mukthay over 18 matras. In the case that the matras are exactly divisible by three, some minimal gap is ensured.
  ;;; (short-mukthay 4 18)

It is possible to remove the gap in such a case by setting gap-expansion to -1.
  ;;; (short-mukthay 4 18 :gap-expansion -1)

An example in a different gati.
  ;;; (short-mukthay 5 20)


Test possible exceptions/
  ;;; (short-mukthay 4 6)
  ;;; (short-mukthay 4 18 :gap-expansion -2)

* Notes:

A short mukthay is a phrase that is repeated three times – every repetition usually separated by a gap. It is typically used for filling in some 'leftover duration' of another rhythmic technique so that the overall development ends on tala sam. See Reina (2016, p. 75) for details.
  
  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
  "
  (assert (every #'integerp (list gati matras gap-expansion)))
  ;; All length measured in matras
  (multiple-value-bind (max-pala-length min-gap-sum)
      (floor matras 3)
    (let* ((gap-expansion (if (> min-gap-sum 0) gap-expansion (1+ gap-expansion)))
	   (gap-is-even? (evenp min-gap-sum))
	   (pala-length (if gap-is-even? max-pala-length (1- max-pala-length)))
	   (pala-length-w-gap-expansion (- pala-length (* 2 gap-expansion)))
	   (gap-length (if gap-is-even? (/ min-gap-sum 2) (/ (+ min-gap-sum 3) 2)))
	   (gap-length-w-gap-expansion (+ gap-length (* 3 gap-expansion)))
	   (pala (case type
		   ;; TODO: deduce suitable jathi sequence: split pala-length-w-gap-expansion into suitable jathi bhedam seq
		   ;; TODO: make position param param of this function
		   ;; TODO: add support for more params
		   (:phrased
		    (gen-karnatic-cell gati pala-length-w-gap-expansion 0))
		   (:raw (gen-matras gati pala-length-w-gap-expansion 1))))
	   (gap (gen-matras (* -1 gati) gap-length-w-gap-expansion 1)))
      (assert (> pala-length-w-gap-expansion 0) () "Resulting pala too short. Increase given matras or decrease gap-expansion.")
      (assert (>= gap-length-w-gap-expansion 0) () "Resulting gap length cannot be negative. Increase gap-expansion.")
      (remove NIL (append pala gap pala gap pala))
    )))



#|
sama-mukthay-raw

sama mukthay without phrasing yet

Params
- 2 gati

Rule
- Start and end on tala sam
- Rules of jathi bhedam

|#
