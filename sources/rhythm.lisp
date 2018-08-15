;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; !! TODO: rhythmic transformation with curve/function/fenv/envelope
;; - multiplication (or addition) of fenv etc. values to given rhythmic sequence (numerically)
;; - Optional: some quantisation of results (e.g., allow for limiting rythmic complexity), e.g., with 
;;   - Opusmodus: vector-to-length (no support for tuplets)
;;   - ksquant (needs poking in the code, or translating input data into simple format (very easy, just call simple2score), and then resulting ENP score into OMN more tricky, but possible)

#|
(defun dynamic-rhythmic-transformation (length fenv fun &key (quantize '(1 2 3 4 5 7 8)))
  "Generic transformation function: transforms the rhythmic sequence `length' in a way that changes over the duration of that sequence and is specified with a fenv and a function.  

* Arguments:
  - 
  "
  
  
  )
|#

(defun tuplet-rhythm (lengths subdivisions &rest args 
                              &key (length-dividend 1/2) (count-offset 0) (position 'e) (type '?)
                              &allow-other-keys)
  "Subdivides given note `lengths'.  

* Arguments:
  - lengths: list of length values, can be nested.
  - subdivisions: (circling list of ints) specifies tuplet sequence.
  - length-dividend (default 1/2): duration ratio divided by subdivisions. Default case are half note subdivisions, ie. a subdivision of 3 results 3h notes (triplets splitting a half note). 
  - count-offset (default -1): `subdivisions' also specifies number of equally spaced notes per tuplet plus amount of `count-offset' . I.e., if `count-offset' = 0 then each note is split into subdivision notes.
  
  All keyword args from `length-divide2' are inherited, but some with different default.

* Examples:
  ;;; (tuplet-rhythm '(1/2 1/2 1/2) '(3 4 5))
  ;;; => (1/6 1/6 1/6 1/8 1/8 1/8 1/8 1/10 1/10 1/10 1/10 1/10)

  ;;; (tuplet-rhythm (gen-repeat 8 '((1/2))) '(3 4 5 6 5 4 3 2))

  With irregular meter
  ;;; (tuplet-rhythm (gen-eval 4 '(gen-repeat (rnd1 :low 2 :high 5) 'h)) '(3 4 5 6 5 4 3 2) :seed 1234)

* BUGS: 

  Seed argument not working as expected -- it is not even used in this function!"
  (apply #'length-divide2 (mapcar #'(lambda (x) (+ x count-offset)) subdivisions) 
         (mapcar #'(lambda (x) (/ length-dividend x)) subdivisions)
         lengths
         :position position
         :type type
         :allow-other-keys t
         args))

#|
(tuplet-rhythm (gen-repeat 8 '(1/2)) '(3 4 5 6 5 4 3 2))

(tuplet-rhythm (gen-repeat 8 '((1/2))) '(3 4 5 6 5 4 3 2) :seed 1234)

(tuplet-rhythm (gen-repeat 8 '((1/2))) '(3 4 5 6 5 4 3 2))

; with irregular meter
(tuplet-rhythm (gen-eval 4 '(gen-repeat (rnd1 :low 2 :high 5) 'h)) '(3 4 5 6 5 4 3 2) :seed 1234)
|#


;; Originally inspired by reading rhythm of some v. Schweinitz scores, but an approach like this is not typical for him only.
;;; TODO: Consider to allow specifying subdivisions-ambitus with two fenvs (or one int and one fenv) for greater control.
;;; TODO: consider allowing bar-length to be a list of values.
(defun tuplet-walk-rhythm (bar-no &key (bar-length 1/2) (subdivisions-ambitus '(2 7)) 
                                  (rest-distances '(7 8 9)) 
                                  (rest-distance-order :rnd)
                                  (last-bar nil)
                                  (seed nil))
  "Some custom algorithm to create rhythmic phrases that randomly walk across tuplet subdivisions and includes some rests.

* Arguments:
  - bar-no (int): number of bars to generate
  - bar-length: regular duration of resulting bars
  - subdivisions-ambitus: range of tuplet subdivisions
  - rest-distances: distances between rests 
  - rest-distance-order (:rnd or :seq): whether rest distances will be in the given order (:seq) or randomised (:rnd)
  - last-bar (flat or nested OMN expression, but typically only a rhythm): one or more bars added at end after bar-no bars. If nil, no bar is added. 

* Examples: 
  ;;; (tuplet-walk-rhythm 7 :seed 569 :rest-distances '(9 1 13) :last-bar '(1/4 -1/4))
  "
  ;; ? Old doc of removed arg? -- kann weg?
  ;;   - lengths: list of lists of length values forming the underlying rhythm that is subdivided by this function
  (do-verbose ("")
    (rnd-seed seed)
    (let* ((underlying-rhythm (gen-repeat bar-no `((,bar-length))))
           (subdivisions (ambitus-integer subdivisions-ambitus 
                                          (gen-integer-step 3 bar-no (gen-walk bar-no :seed (seed)))))
           (my-rhythm (tuplet-rhythm underlying-rhythm subdivisions 
                                     :length-dividend bar-length :count-offset 0 :seed (seed))))
      (append 
       (length-rest-series ; rest-distances
        (case rest-distance-order
          (:rnd (rnd-sample bar-no rest-distances :seed (seed)))
          (:seq rest-distances))
        my-rhythm)
       (when last-bar
	 (ensure-double-list last-bar))
         ))))


(defun even-length-rhythm (total-duration pattern &key prefix suffix (time-sig '(4 4)))
  "Custom algorithm to create rhythmic phrases consisting of even note durations over a certain time. 

* Arguments:
  - total-duration (length value): duration of the generated phrase including the prefix and suffix length.
  - pattern (length value, list of length values, or OMN sequence): rhythmic value(s) to repeat
  - prefix (length value, list of length values, or OMN sequence): preceeding phrase
  - suffix (length value, list of length values, or OMN sequence): succeeding phrase
  - time-sig: time signature

* Examples:
  ;;; (even-length-rhythm 'w_w '5q :prefix '-w_5h :suffix '-5q_5h_q)

  Lists and OMN sequences can be specified for pattern, prefix and suffix
  ;;; (even-length-rhythm 'w_w_w '(3q c4 stacc 3e) :prefix '-3h :suffix '(q b3d4 -q))"  
  (let* ((pre-and-suffix-dur (apply #'length-add 
                                    (length-rest-invert (append (tu:ensure-list prefix)
                                                                (tu:ensure-list suffix)))))
         (repetition-dur (length-subtract total-duration pre-and-suffix-dur)))
    (assert (> (omn-encode repetition-dur) 0)
            (total-duration prefix suffix)
            "The total duration ~A is less than the sum of the prefix ~A and suffix ~A.~%"
            total-duration prefix suffix)
    (omn-to-time-signature
     (append 
      (when prefix (tu:ensure-list prefix))
      (length-span (list repetition-dur) (list pattern))
      (when suffix (tu:ensure-list suffix))
      ) 
     time-sig)))


#|
(defun even-length-rhythm2 (length duration 
                                    &key prefix suffix (time-sig '(4 4)))
  "Some custom algorithm to create rhythmic phrases consisten of even note durations over a certain time. Variant of even-length-rhythm, where the total-duration is the sum of duration, prefix and suffix length.

* Arguments:
  - length (length value): rhythmic value to repeat
  - duration (length value): duration over which to repeat the rhythmic value
  - prefix (length value or length sequence): preceeding phrase
  - suffix (length value or length sequence): succeeding phrase
  - time-sig: time signature

* Examples:
  ;;; (even-length-rhythm2 '5q 'w_w_h :prefix '-w_5h :suffix '-5q_5h_q)"  
  (omn-to-time-signature
   (append 
    (when prefix (tu:ensure-list prefix))
    (length-span (list duration) (list length))
    (when suffix (tu:ensure-list suffix))
    ) 
   time-sig))
|#


#|
;; TODO: unfinished
;; see Boulez (1981) Orientations: Collected Writings. Harvard University Press, p. 132
;;
;; See also PWGL library FDSDB_XXth_CT
(defun demultiplied-rhythms (proportions )

    )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm transformation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: Consider controlling how many consecutive whole notes at most are tied? Hm, I can do this with arg tie-prob and a fixed seed...
;;; Todo: ?? consider supporting full OMN seqs, but then overwriting pitch of note tied over to pitch of preceeding note
(defun tie-whole-notes (lengths tie-prob &key (seed nil))
  "Every bar with only a single note in it is potentially tied over to the next bar, and that way can increase the degree of rhythmic contrast in a rhythmic sequence. 

  NOTE: this function is intended for processing note length values only, as ties are otherwise also affected by pitches. If given a full OMN sequence, then nevertheless only the transformed rhythm is returned.

* Arguments:
  - sequence (nested OMN length sequence): input rhythm to process.
  - tie-prob (number or list of numbers): Probability whether 'whole' notes (notes filling a whole sublist) are tied or note. If 1, all whole notes are tied; if 0, no note is tied; any number in between sets the probability. If a list of numbers, it sets the probability of individual whole notes in order to be tied over. For example, `tie-prob' can be a list of binary numbers generated with Openmodus' binary number functions. 
  - seed (integer): random seed for probability.

* Examples:

  Enforce tie at end of every bar with only a single note by setting the probability to 1 (except the last -- there is never a tied added at the end of the last bar). 
  ;;; (tie-whole-notes '((h) (q q) (h) (h)) 1)

  Whether or not a tie is added is randomised and should be rather evenly distributed (probability is 0.7).
  ;;; (tie-whole-notes '((h) (q q) (h) (h)) 0.7)

  The tie probability is controlled for individual 'whole' notes in the sequence. Note that the probabilities are only given for the actual 'whole' notes, not intermediate bars.
  ;;; (tie-whole-notes '((h) (q q) (h) (h)) '(0 1))
"
  (rnd-seed seed)
  (let* ((ensured-lengths (omn :length lengths))
	 (list-lengths (mapcar #'length ensured-lengths))
	 (tie-probs (circle-repeat tie-prob (count-if #'(lambda (l) (= 1 l)) list-lengths))))
    (append 
     (loop
	for bar in (butlast ensured-lengths)
	for l in list-lengths
	if (and (= 1 l)
		;; only pop element from tie-probs in case of whole note
		(= 1 (rnd1 :low 0 :high 1 :prob (pop tie-probs) :seed (seed))))
	collect (append bar '(tie))
	else collect bar)
     (last ensured-lengths))))


(defun count-shortest (sequence)
  "Count the number of the shortest notes in sequence. Helper function for using `divide-shortest'."
  (let* ((flat-seq (flatten (omn :length sequence)))
	 (shortest (apply #'min flat-seq)))
    (count shortest flat-seq)))
  
; (count-shortest '(1/16 1/2 1/16)) 

(defun divide-shortest (sequence &key (divide 2) (count nil) (section nil) (seed nil))
  "Increase rhythmic contrast by dividing the shortest notes in the given `sequence'.

* Examples:
  ;;; (divide-shortest '(1/8 1/16 1/16))

  ;;; (divide-shortest '(1/8 1/16 1/16) :count 1)

  Note that if sequence is a full OMN expression, then the added notes cause all other parameters to shift forward. Additional parameters are added at the end by circling.
  ;;; (divide-shortest '((e c4 s d4 e4) (e f4 s e4 d4) (q g4)))
  ;;; => ((E C4 T D4 E4 F4 E4) (E D4 T G4 C4 D4 E4) (Q F4))

  ;;; (divide-shortest '((e c4 s d4 e4) (e f4 s e4 d4) (q g4)) :section '(1))
   
  This function is a simplified variant of `length-divide'. For more control use that function instead."  
  (let ((shortest (apply #'min (flatten (omn :length sequence))))
	(l (count-shortest sequence)))
    (length-divide (if count count l) divide sequence :set shortest
		   :section section
		   ;; :flat T
		   ;; :span :pitch -- changes the meter. I don't want that.
		   :seed seed
		   )))

; (divide-shortest (gen-karnatic-cell 4 4 2 :min-number 2))



#|
;;; TODO: unfinished
;;; TODO:
;; - Fix bug: if :merge-prob is 1, merging should always happen
;; - Consider whether/how to merge across multiple bars (e.g., increasing whole-note-prob, and then allow for whole note simply to be "followed" by a tie.
(durational-accent (gen-karnatic-cell 4 4 '(2 2 2) :min-number 2)
 :merge-prob 1 :merge-n 2
 :divide-prob 0 :tie-prob 0 :grace-prob 0 :whole-note-prob 0)
|#



#|
;;; Hm: I want to subdivide all notes of a given duration

;; (length-divide 10 2 (gen-karnatic-cell 4 4 '(2 2 2) :min-number 2) :set 1/16)

;; increasing contrast: subdividing shorter notes
(durational-accent (gen-karnatic-cell 4 4 '(2 2 2) :min-number 2)
		   :tie-prob 0 :grace-prob 0 :merge-prob 0 :whole-note-prob 0)
|#



;;
;; map-tuplet
;;

(defun _partition-args (keywords arglist)
  "[Aux for map-tuplet] Partition the argument list around the first argument that is in KEYWORDS."
  (let ((first-key (position-if #'(lambda (elt) (member elt keywords))
                                arglist)))
    (print first-key)
    (if first-key
      (values (subseq arglist 0 first-key)
              (subseq arglist first-key))
      ;; no keyword given
      (values arglist
              nil))))

(defun _map-tuplet-internal (fn lengths more-args &key (type :extend))
  "[Aux for map-tuplet] Defines actual function."
  (let ((time-sigs (get-time-signature lengths))
        (tuplets (split-lengths lengths :type type)))
    (omn :length 
         (omn-to-time-signature (apply #'mapcar fn tuplets
                                       (mapcar #'(lambda (args)
                                                   (circle-repeat args (length tuplets))) 
                                               more-args)) 
                                time-sigs))))

(defun map-tuplet (fn lengths &rest arglist)
  "A function for varying tuplet groups one by one: map-tuplet applies fn to each tuplet length group in lengths.

* Arguments:
  - fn: a function expecting a length list, and potentially more arguments if further args are given
  - lengths: a (possibly nested) lengths list
  - arglist: further argument
    -- args -- arguments that are part of arg-list before keyword args: more argument lists to map in parallel. 
    -- type -- a keyword arg that is part of arglist. :extend, :denominator or :tuplet. The default is :extend (inherited from split-lengths).


* Examples:

;;; (setf rhy '((-1/6 1/6 1/6 -1/8 1/8 1/8 1/8) (-1/10 1/10 1/10 1/10 1/10 -1/12 1/12 1/12 1/12 1/12 1/12) (-1/10 1/10 1/10 1/10 1/10 -1/8 1/8 1/8 1/8) (-1/6 1/6 1/6 -1/4 1/4)))

In this first example, all tuplet groups in rhy are rotated by one. 

;;; (map-tuplet #'(lambda (ls) (gen-rotate 1 ls)) rhy)


Additional arguments can be given to fn by specifying further argument lists to map-tuplet. If an argument list is shorter than the number of tuplet groups in lengths then it is circled through. In this example, tuplet groups are rotated by 1 or 2 alternating.   

;;; (map-tuplet #'(lambda (ls n) (gen-rotate n ls))
;;;             rhy
;;;             '(1 2))

"
  ;; function uses aux _map-tuplet-internal in order to process both &rest and &key args
  (multiple-value-bind (args options)
      (_partition-args '(:type) arglist)
    (append args options)
    (apply #'_map-tuplet-internal fn lengths args options)))


#|
(setf rhy '((-1/6 1/6 1/6 -1/8 1/8 1/8 1/8) (-1/10 1/10 1/10 1/10 1/10 -1/12 1/12 1/12 1/12 1/12 1/12) (-1/10 1/10 1/10 1/10 1/10 -1/8 1/8 1/8 1/8) (-1/6 1/6 1/6 -1/4 1/4)))

;; all tuplets rotated by 1
(map-tuplet #'(lambda (ls) (gen-rotate 1 ls)) 
            rhy)

;; tuplets rotated by 1 or 2 alternating
(map-tuplet #'(lambda (ls n) (gen-rotate n ls))
            rhy
            '(1 2))
|#




;;;

(defun length-divide-ext (count divide length &rest args &key seed)
  "Same as length-divide, but arg divide is list of ints (elements circled through).

  Note: implemented by calling length-divide internally for each sublist in length, and therefore arguments like section and exclude are not supported.

* Examples:
  ;;; (length-divide-ext 1 '(2 3) '((q q) (q q)) :seed 1)
  ;;; => ((1/8 1/8 1/4) (1/4 1/12 1/12 1/12))

  ;;; (length-divide-ext 1 '(2 3) '(q q) :seed 1)
  ;;; => ((1/8 1/8) (1/12 1/12 1/12))"
  (rnd-seed seed)
  (mapcar #'(lambda (div l)
              (apply #'length-divide count div l (append (list :seed (seed)) args)))
          (circle-repeat divide (length length))
          length))


(defun note-rest-series (positions sequence &key (flat nil) (swallow nil) (section nil))
  "Turn notes at specific positions (actually, distances between positions) into rests. This function is like the Opusmodus built-in length-rest-series, but supports arbitrary OMN expressions as input and additionally the arguments swallow and section.

* Arguments:
  - positions (list of ints): 1-based positions of notes to be turned into rests
  - sequence (list of lengths or OMN expression): music to process
  - flat (Boolean): whether positions count for sublists (nil) or the whole list (T)
  - swallow (Boolean): whether the pitches of notes turned into rests should be shifted to the next note or omitted (swallowed) 
  - section (list of ints): 0-based positions of sublists to process. This argument is ignored if flat is T.

* Examples:

;;; (setf melody '((s eb6 < leg f5 < leg c5 < leg f5 < leg) (e e6 f - -q)))
;;; (note-rest-series '(1 1) melody :swallow T :section '(0))
"
  (edit-omn :length sequence 
            #'(lambda (ls) (length-rest-series positions ls))
            :swallow swallow
	    :section section
	    :flat flat))

;; - OK allow for rests at beginning and end and perhaps in the middle
;; - OK set number of consecutive tones to be turned into rests
;; - OK support args like :section
(defun _position-to-rest (position lengths &key (n 1) (seed nil))
  "[Aux] Processing plain list of lengths.
"
  (rnd-seed seed)
  (let* ((l (length lengths))
	 (pos (case position
		(s (append (gen-repeat n '(1)) (list l)))
		(? (cons (rnd1 :low 1 :high (- l (1- n)) :seed (seed))
			 (append (gen-repeat (1- n) '(1)) (list l))))
		(e (list l))
		(otherwise
		 (if (< position 0)
		     ;;; TODO:
		     (cons (+ l position 1) (append (gen-repeat (1- n) '(1)) (list l)))
		     ;; 1+: turn into 1-based position
		     (cons (1+ position) (append (gen-repeat (1- n) '(1)) (list l)))))
		)))    
    (note-rest-series pos lengths)))

#|
(_position-to-rest 's '(q q q q))
(_position-to-rest 's '(q q q q) :n 2)
(_position-to-rest 'e '(q q q q))
(_position-to-rest '? '(q q q q) :n 2)
(_position-to-rest '? '(q q q q) :n 2 :seed 1)
(_position-to-rest 0 '(q q q q) :n 2)
(_position-to-rest 2 '(q q q q) :n 2)
(_position-to-rest -1 '(q q q q))
(_position-to-rest -2 '(q q q q))
(_position-to-rest -3 '(q q q q) :n 2)
|#

;;; TODO:
;; - args position and n should support lists
;; - allow to control probability of notes turned into rests
(defun position-to-rest (position sequence &key (n 1) (flat nil) (swallow T) (section nil) (seed nil))
  "Turn notes at the given position in the bar into rests.

* Arguments:
  - position (symbol or integer): position of the note to turn into a rest. If a positive integer, it denotes the 0-based position. If a negative integer, it counts backwards from the end (-1 is the last element, -2 the one before the last and so on). Symbols have the following meaning: s - first note (start); e - last note (end); ? - randomly chosen position.  
  - sequence (list of lengths or OMN expression): music to process.
  - n (integer): how many consecutive notes after `position' to affect.
  - flat (Boolean): whether positions count for sublists (nil) or the whole list (T)
  - swallow (Boolean): whether the pitches of notes turned into rests should be shifted to the next note or omitted (swallowed) 
  - section (list of ints): 0-based positions of sublists to process. This argument is ignored if flat is T.
  - seed (integer): random seed for ? position.

* Examples:

  Processing a flat list of durations.
;;; (position-to-rest 's '(q q q q) :n 2)

  A sequence to be processed in following examples. 
;;; (setf seq '((q c4 q d4 q e4 q d4) (h b3 q g4 q f4) (w e4)))

  Create rests at the beginning of every bar.
;;; (position-to-rest 's seq)

  Create rests at the end of every bar using a negative position
;;; (position-to-rest -1 seq)
  
  Create rests at a random position in the first bar (section) with two consecutive rests.
;;; (position-to-rest '? seq :section '(0) :n 2)

  Create a rest at a random position in every bar.
;;; (position-to-rest '? seq :seed 1)

  Process the flattened sequence and create two consecutive rests starting from position 5, which happens to be in the second bar.
;;; (position-to-rest 5 seq :n 2 :flat T)

  If there already is a rest at the given position, then it is simply kept as such.
;;; (position-to-rest 1 '(q -q q q) :n 2)
"
  (rnd-seed seed)
  (edit-omn :length sequence 
            #'(lambda (ls) (_position-to-rest position ls :n n :seed (seed)))
            :swallow swallow
	    :section section
	    :flat flat))



(defun _merge-rest-into-note (lengths)
  "[Aux def]
All rests are merged into the following note. This function can be useful for producing harmonic rhythms. 

* Arguments: 
- lengths: a flat list of length values"
  ;; (declare (optimize (debug 3)))
  (reduce #'(lambda (accum elem)
	      (if (< elem 0)
		  (cons (+ (abs elem) (first accum)) (rest accum))
		  (cons elem accum)))
	  (reverse (length-rest-merge (butlast lengths)))
	  :initial-value (last lengths)))

;; (_merge-rest-into-note '(-1/2 3/2 -1/2 3/2 -1/2 3/2 -1/2 -1/2 -1/2 3/2 -1/2 -1 -1/2 1/2 -1/2 3/2 -1/2 -47/2 -1/2 3/2 -1/2 3/2 -1/2 3/2 -1/2 1/2 -1/2 3/2))

	

(defun merge-rest-into-note (sequence &key (flat nil) (section nil))
  "All rests are merged into the following note. This function can be useful for producing harmonic rhythms.

* Arguments:
- sequence: OMN sequence, can be nested
- flat (Boolean): whether or not to merge rests across bar boundaries
- section (list of positive integers): selection of sublists to process

* Examples:

;;; (merge-rest-into-note '((-h w. e4 pp) (-h w. gs4 pp) (-h w. gs4 pp)))
;;; => ((d e4 pp) (d gs4) (d gs4))

;;; (merge-rest-into-note '((-w) (-h h e4 pp)) :flat T)
;;; => ((w e4 pp tie) (w e4 pp))
"
  (edit-omn :length sequence 
            #'(lambda (ls) (_merge-rest-into-note ls))
	    :section section
	    :flat flat))

#|
;;; TODO:
;;; - denominator could be list of ints
;;; - rebarring as before could cause trouble...
;;; - arg flat could cause trouble if I later want to rebar as before...
;;; 
(defun omn-fit (sequence &key (denominator 8) (type :add) (flat nil) (swallow nil) (section nil))
  "Like built-in length-fit, but with additional support of arguments flat, swallow, and section.

  Examples:

(length-fit '((e a4 q c5 tie e c5 tie 3q c5 b4 3e cs5) (q gs4 tie 3q gs4 3e f4 fs5))
	    :section  
	    :denominator 4 
	    :type :rest
"
  (edit-omn :length sequence 
            #'(lambda (ls) (length-fit ls :denominator denominator :type type))
            :swallow swallow
	    :section section
	    :flat flat))
|#


(defun _cut-holes (lengths binary-list)
  "[Aux] Expects a list of lengths and a matching binary list. Every length at a position of a 1 in the binary list is left untouched, while every length at a 0 is turned into a rest. 

  If binary-list is shorter than lengths it is repeated in a circular fashion.

  Related: length-rest-series"
  (mapcar #'(lambda (l b)
	      (if (= b 0) 
		  (* l -1)
		  l))      
          lengths
          (circle-repeat (flatten binary-list) (length lengths))))

;; (_cut-holes '(1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16) '(1 1 1 0 1 1 1))

(defun cut-holes (sequence binary-list &key (swallow nil))
  "Turns notes in `sequence' into rests if `binary-list' contains a 0 at the matching position. Notes are left untouched if there is a 1 at the matching position.

* Arguments:
  - sequence (list of lengths or OMN expression): music to process
  - binary-list (flat list of ints): 
  - flat (Boolean): whether binary-list count for sublists (nil) or the whole list (T)
  - swallow (Boolean): whether the pitches of notes turned into rests should be shifted to the next note or omitted (swallowed) 

* See Also: 

{defun note-rest-series}
"
  (edit-omn :length sequence
	    #'(lambda (ls) (_cut-holes ls binary-list))
	    :flat T
	    :swallow swallow
	      ;; - section (list of ints): positions of sublists to process. This argument is ignored if flat is T.
	    ;; :section section
	    ))

#|
(setf notes (make-omn :length '((s :repeat 12) (e :repeat 8) (q :repeat 8))
		      :pitch (MIDI-to-pitch (gen-integer 60 84))))

(cut-holes notes '(1 0 1 0 1 1 1 1))

(cut-holes notes '(1 0 1 0 1 1 1 1) :swallow T)
|#



#| ;; old
;; Better use length-rest-series or note-rest-series instead
(defun _cut-holes (lengths binary-list)
  "Expects a neste listed of lengths and a matching nested binary list. Every length at a position of a 1 is left untouched, while every length at a 0 is turned into a rest.
  NOTE: For now, a flattened list is return, and OMN expressions are not supported.

  Related: length-rest-series"
  (mapcar #'(lambda (ls bs)
              (mapcar #'(lambda (l b)
                          (if (= b 0) 
                            (* l -1)
                            l))
                      ls bs))          
          ;; must be list of lists
          lengths
          binary-list))
|#

#| ;; very old
(defun cut-holes (lengths binary-list)
  "Expects a list of lengths and a binary list. Every length at a position of a 1 is left untouched, while every length at a 0 is turned into a rest.
  NOTE: For now, a flat list is returned, and OMN expressions are not supported."
  (mapcar #'(lambda (l b)
              (if (= b 0) 
                (* l -1)
                l))
          (flatten lengths)
          (flatten binary-list)))

|#


;;; TODO:
;; - To consider: if tie-previous-beat? is T, then implicitly tie-n is 1, because otherwise I am creating a durational accent on the preceeding beat (I might already with tie-n set to one, in particular if divide-n is a low value such as 2). 
;; - document tie-related args 
;; For now simpler version: accents only supported leading to strong beat at beginning of bar, but metric structure does not need to be regular.
(defun _durational-accent-divide (lengths &key (divide 2) (n 1) (divide-prob 0.5)
					    (tie-n 0) (tie-prob 0.5) (tie-previous-beat? nil)
					    (grace-n 0) (grace-length 1/8) (grace-prob 0.5)
					    (set nil) (ignore nil) (seed nil))
  "Adds durational accents on first notes of bars by subdividing the last note of the preceding bar. `lengths' must be a list of length lists (multiple bars). 

  If a bar starts with a rest, then it cannot carry a durational accent, and hence its preceding note is never subdivided. 

* Arguments:
  - divide (integer or list of integers, default 2): specifies into how many equal note values notes preceeding a durational accent are subdivided. If list of integer, subdivision is randomly chosen.
  - n (integer): maximum number of notes at end of bars that are potentially subdivided.
  - divide-prob (default 0.5): probability value between 0.0 and 1.0, controlling whether a note that could be subdivided for creating a durational accent actually will be. Higher values make durational accents more likely.
  - tie-n (integer): maximum number of subdivided notes at end of bars (before the next durational accent) that are potentially tied together. Should not be larger than n*divide. 
  - tie-previous-beat? (Bool): whether or not the first subdivided note before the durational accent is tied to the preceeding note (e.g., the preceeding accent). Such tie never occurs across bar lines. 
  - tie-prob: probability value controlling whether subdivided notes are tied. 
  - grace-n (integer): number of grace notes potentially inserted before first notes of bars.
  - grace-length (length value, default 1/8): notated note value of inserted grace notes.
  - grace-prob (default 0.5): probability value controlling whether grace notes are inserted.
  - set (length or list of lengths): only specified lengths are subdivided. 
  - ignore (length or list of lengths): specified lengths are *not* subdivided. 
  - seed (integer): random seed.

* Examples:
  ;;; (_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :divide '(2 3) :n 2 :seed 4321)"
  (do-verbose ("")
    (assert (and (listp lengths) (every #'listp lengths)) 
            (lengths)
            "Given `lengths' ~A is not a sequence of bars (a list of lists).~%" lengths)
    (rnd-seed seed)
    (mapcar #'(lambda (bar) ; adding grace notes before bars
                (let ((whether-grace (= 1 (rnd1 :low 0 :high 1 :prob grace-prob :seed (seed))))
                      (grace-no (rnd1 :low 1 :high grace-n :seed (seed))))
                  (if (and (> grace-n 0)
                           whether-grace
                           (length-notep (first bar)))
		      (cons (cons 'acc (gen-repeat grace-no (list grace-length)))
			    bar)
		      bar)))
	    (append 
	     (tu:map-neighbours
	      ;; subdividing last notes in bar1
	      #'(lambda (bar1 bar2) 
		  (let ((whether-divide
			 (= 1 (rnd1 :low 0 :high 1 :prob divide-prob :seed (seed))))
			;; how many notes to subdivide 
			(divide-no (rnd1 :low 1 :high n :seed (seed)))) 
		    (if (and whether-divide
			     (> n 0)
			     (length-notep (first bar2))
			     (every #'length-notep (last bar1 divide-no)))
			;; subdivide last note of bar
			(append
			 (butlast bar1 divide-no)
			 (let* ((divided-lengths (length-divide
						  ;; random control for how many notes subdivision happens
						  divide-no
						  (if (listp divide)
						      (rnd-pick divide :seed (seed))
						      divide) 
						  (last bar1 divide-no)
						  :set set
						  :ignore ignore
						  :seed (seed)))
				(divided-lengths-no (length divided-lengths))
				(whether-tie
				 (= 1 (rnd1 :low 0 :high 1 :prob tie-prob :seed (seed))))
				(first-beat-in-bar-divided? (= divide-no
							       (length bar1)))
				(tie-to-previous-beat?
				 (and tie-previous-beat?
				      (not first-beat-in-bar-divided?)))
				(tie-no-tmp (rnd1 :low 1
						  :high (min tie-n
							     (1- divided-lengths-no))
						  :seed (seed)))
				(tie-no (if tie-to-previous-beat?
					    (1- tie-no-tmp)
					    tie-no-tmp))
				)
			   (if (and whether-tie)
			       (append (when tie-to-previous-beat? '(tie))
				       (loop for l in (first-n tie-no divided-lengths)
					  append (list l 'tie))
				       (nthcdr tie-no divided-lengths))
			       divided-lengths)))
			;; otherwise leave bar unchanged
			bar1)))
	      lengths)
	     ;; last bar never subdivided
	     (last lengths)))))



#|
(defun _durational-accent-divide (lengths &key (divide 2) (n 1) (divide-prob 0.5) 
                                         (grace-n 0) (grace-length 1/8) (grace-prob 0.5) 
                                         (set nil) (ignore nil) (seed nil))
  "Adds durational accents on first notes of bars by subdividing the last note of the preceding bar. `lengths' must be a list of length lists (multiple bars). 

* Arguments:
  divide (integer or list of integers, default 2): specifies into how many equal note values notes preceeding a durational accent are subdivided. If list of integer, subdivision is randomly chosen.
  n (integer): number of notes at end of bars that are potentially subdivided. If a bar starts with a rest, then it cannot carry a durational accent, and hence its preceding note is never subdivided. 
  divide-prob (default 0.5): probability value between 0.0 and 1.0, controlling whether a note that could be subdivided for creating a durational accent actually will be. Higher values make durational accents more likely.
  grace-n (integer): number of grace notes potentially inserted before first notes of bars.
  grace-length (length value): note value of inserted grace notes.
  grace-prob (default 0.5): probability value controlling whether grace notes are inserted.
  set (length or list of lengths): only specified lengths are subdivided. 
  ignore (length or list of lengths): specified lengths are *not* subdivided. 
  seed (integer): random seed.

* Examples:
  (_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :divide '(2 3) :n 2 :seed 4321)"
  (do-verbose ("")
    (assert (and (listp lengths) (every #'listp lengths)) 
            (lengths)
            "Given `lengths' ~A is not a sequence of bars (a list of lists).~%" lengths)
    (rnd-seed seed)
    (mapcar #'(lambda (bar) ; adding grace notes before bars
                (let ((whether (= 1 (rnd1 :low 0 :high 1 :prob grace-prob :seed (seed))))
                      (no (rnd1 :low 1 :high grace-n :seed (seed))))
                  (if (and (> grace-n 0)
                           whether
                           (length-notep (first bar)))
                    (cons (cons 'acc (gen-repeat no (list grace-length)))
                          bar)
                    bar)))
            (append 
             (map-neighbours ; subdividing last notes in bars
              #'(lambda (bar1 bar2) 
                  (let ((whether (= 1 (rnd1 :low 0 :high 1 :prob divide-prob :seed (seed))))
                        (no (rnd1 :low 1 :high n :seed (seed)))) ; how many notes to subdivide max
                    (if (and whether
                             (> n 0)
                             (length-notep (first bar2))
                             (every #'length-notep (last bar1 no)))
                      ;; subdivide last note of bar
                      (append (butlast bar1 no)
                              (length-divide
                               ; random control for how many notes subdivision happens
                               no
                               (if (listp divide)
                                 (rnd-pick divide :seed (seed))
                                 divide) 
                               (last bar1 no)
                               :set set
                               :ignore ignore
                               :seed (seed)))
                      ;; otherwise leave bar unchanged
                      bar1)))
              lengths)
             (last lengths)))))

|#

#|

(_durational-accent-divide '((1/4 1/4 1/4) (1/4 1/4 -1/4) (1/4 1/4 1/4) (1/2 -1/4)) :n 2)

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :divide '(2 3) :n 2)

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :divide '(2 3) :n 2 :seed 4321)

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :seed 120)

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))))

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :n 0 :grace-n 2)

(_durational-accent-divide (gen-repeat 2 '((h q) (q q q) (h -q))) :n 2 :grace-n 2)

|#


; For now simpler version: accents only supported leading to strong beat at beginning of bar, but metric structure does not need to be regular.
(defun _durational-accent-merge (lengths &key (n 2) (prob 0.5) (seed nil))
  "Adds durational accents on first notes of bars by merging notes at the beginning of a bar. `lengths' must be a list of length lists (multiple bars). 

  If a bar starts with a rest, then it cannot carry a durational accent, and hence notes are not merged. 

* Arguments:
  - n (integer): number of notes at beginning of bars that are potentially subdivided.
  - prob (default 0.5): probability value between 0.0 and 1.0, controlling whether notes that could be merged for creating a durational accent actually will be. Higher values make durational accents more likely.
  - seed (integer): random seed.

* Examples:
  ;;; (_durational-accent-merge (gen-repeat 4 '((q q q))) :n 3 :seed 3333)"
  (do-verbose ("")
    (assert (and (listp lengths) (every #'listp lengths)) 
            (lengths)
            "Given `lengths' ~A is not a sequence of bars (a list of lists).~%" lengths)
    (rnd-seed seed)
    (mapcar 
     #'(lambda (bar) 
         (let* ((whether (= 1 (rnd1 :low 0 :high 1 :prob prob :seed (seed))))
		(no (rnd1 :low 1 :high n :seed (seed))) ; how many notes to merge max
		(first-no (first-n no bar)))
           (if (and whether
		    ;; NOTE: only merges if there is no rest among first no notes.
		    ;; TODO: consider in case the first two or more length values are not a rest to still allow for merging those, up to the rest.
                    (every #'length-notep first-no))
	       ;; merge first notes of bar
	       (append (length-merge first-no)
		       (last bar (if (> no (length bar))
				     0
				     (- (length bar) no))))
	       ;; otherwise leave bar unchanged
	       bar)))
     lengths)))



#|
(_durational-accent-divide 
 (_durational-accent-merge (gen-repeat 4 '((q q q))) :n 2)
 :divide '(2 3) :n 2)

(_durational-accent-merge
 (_durational-accent-divide  (gen-repeat 4 '((q q q))) :divide '(2 3) :n 2 :grace-n 2)
 :n 2)

|#

(defun _durational-accent-whole-note (lengths &key (whole-note-prob 0.1) (seed nil))
  "Adds quasi durational accents on first notes of bars (and simplifies the resulting music) by merging all notes of some bars into a single note taken the whole duration of that bar."
  #|
  (assert (and (listp lengths) (every #'listp lengths)) 
  (lengths)
  "Given `lengths' ~A is not a sequence of bars (a list of lists).~%" lengths)
  |#
  (rnd-seed seed)
  (mapcar 
   #'(lambda (bar) 
       (let* ((whether (= 1 (rnd1 :low 0 :high 1 :prob whole-note-prob :seed (seed))))
	      )
	 ;; (break)
	 (if (and whether		  
		  ;; NOTE: only merges if there is no rest among notes of bar.
		  (every #'length-notep (remove 'tie bar)))
	     (length-merge bar)
	     ;; otherwise leave bar unchanged
	     bar)))
   lengths))


#|
(_durational-accent-whole-note (gen-repeat 4 '((q q q))) :whole-note-prob 0.2)
(_durational-accent-whole-note (gen-repeat 4 '((q q q))) :whole-note-prob 1)
|#

;;; TODO:
;;; - [Postponed] All args should support lists, and additionally support args like section: use edit-omn for getting that.
;;;   Differently put, but same point: generalise: several args could alternatively expect lists to allow controlling a development: divide, all *-n args, all *--prob args
;;;   NOTE: Implementation more difficult than anticipated, see comments above sketch below this function for details.  
;;
;; - ??? extra function to turn notes into rests -- 
;;   - leave untouched: first note of bar if preceded by shorter notes and last note of bar is suceeded by longer note
;;; BUG: ? With merge-prob set to 1, merging is still not guarenteed
(defun durational-accent (lengths 
			  &key (divide 2) (divide-n 1) (divide-prob 0.5)
			    (tie-n 0) (tie-prob 0.5) (tie-previous-beat? nil)
			    (grace-n 0) (grace-length 1/8) (grace-prob 0.5)
			    (merge-n 2) (merge-prob 0.5)
			    (whole-note-prob 0.1)
			    (set nil) (ignore nil)
			    (seed nil) (format :omn))
  "Adds durational accents on first notes of bars by subdividing the last note of the preceding bar (see divide-related args) or merging notes at the beginning of a bar (see merge-related args). Subdivided notes can be partially tied together for rhythmic variety (see tie-related args), and durational accents can also be expressed with grace notes (see grace-related args). 

While `durational-accent' only supports accents of the first beat of each bar, you can easily realise other accent patterns by temporarily rebarring the music (e.g., with `omn-to-time-signature' and perhaps `length->time-signature'), and then afterwards align the result of `durational-accent' again with the original time signatures (e.g., with `copy-time-signature').

* Arguments:
  - lengths: a list of length lists (multiple bars). `length' can also be arbitrary OMN expressions, but only length lists are returned and other parameters are ignored. 
  - divide (integer or list of integers, default 2): specifies into how many equal note values notes preceeding a durational accent are subdivided. If list of integer, subdivision is randomly chosen.
  - divide-n (integer): number of notes at end of bars that are potentially subdivided. If a bar starts with a rest, then it cannot carry a durational accent, and hence its preceding note is never subdivided. 
  - divide-prob (default 0.5): probability value between 0.0 and 1.0, controlling whether a note that could be subdivided for creating a durational accent actually will be. Higher values make durational accents more likely.
  - tie-n (integer): maximum number of subdivided notes at end of bars (before the next durational accent) that are potentially tied together. Should not be larger than divide * divide-n. (The returned notes are not necessarily tied but can instead be notated, e.g., as dotted note values.)
  - tie-previous-beat? (Bool): whether or not the first subdivided note before the durational accent is tied to the preceeding note (e.g., the preceeding accent). Such tie never occurs across bar lines. 
    -- NOTE: If tie-previous-beat? is T, then you may want the argument divide to be 3 or larger, and tie-n to be only 1 (or slightly larger for large values of divide), because otherwise you also turn the preceeding note into a durational accent.
  - tie-prob: probability value controlling whether subdivided notes are tied. 
  - merge-n (integer): number of notes at beginning of bars that are potentially subdivided.
  - merge-prob (default 0.5): probability value controlling whether notes at the beginning of a bar are merged.
  - whole-note-prob: probability value controlling whether all notes of a bar are merged to form quasi a whole note spanning that whole bar.
  - grace-n (integer): number of grace notes potentially inserted before first notes of bars.
  - grace-length (length value, default 1/8): note value of inserted grace notes.
  - grace-prob (default 0.5): probability value controlling whether grace notes are inserted.
  - set (length or list of lengths): only specified lengths are subdivided. 
  - ignore (length or list of lengths): specified lengths are *not* subdivided. 
  - format (either :omn or :length): set the output format. In :length format, grace notes are represented explicitly as acciaccatura and ties are represented explicitly, in length format they have simply the duration 0.
  - seed (integer): random seed.

* Examples:

  Evaluate these examples multiple times to see range of solutions.
  ;;; (durational-accent (gen-repeat 4 '((q q q))) :divide 2 :divide-n 2 :merge-n 3)
  ;;; (durational-accent (gen-repeat 4 '((q q q))) :divide '(2 3) :divide-n 2 :merge-n 3)

  allow for ties for greater rhythmic variety, in particular for higher :divide values
  ;;; (durational-accent (gen-repeat 4 (list (gen-repeat 4 '(1/4))))
  ;;;                  :divide '(3 4) :divide-n 2
  ;;;                  :tie-n 2 :tie-prob 0.8)

  ties with ties to previous beat
  ;;; (durational-accent (gen-repeat 4 (list (gen-repeat 2 '(1/4))))
  ;;;                  :divide '(4) :divide-n 2 :divide-prob 1
  ;;;                  :tie-n 1 :tie-prob 1 :tie-previous-beat? t)
  
  shorter tuplet groups to create accents
  ;;; (durational-accent (gen-repeat 4 '((q q q))) :divide '(5 6 7) :divide-n 1 :divide-prob 0.7 :merge-n 3)

  inserting grace notes, but without subdividing notes at end of bar
  ;;; (durational-accent (gen-repeat 4 '((q q q))) :divide-n 0 :merge-n 3 :grace-n 2)

  metric structure can be irregular, but accents are still created on first beat
  ;;; (durational-accent 
  ;;;  (gen-eval 4 '(gen-repeat (rnd1 :low 2 :high 5) (rnd-pick '(h q))))
  ;;;  :divide '(2 3) :divide-n 3 :merge-n 3)

  Note that you can create potential accents on each bear, on syncopations or other accent patterns beyond emphasising the first note of bars by using this function with sublists (temporary bars) according to where you want to have your accents, and then in a next step re-barring your results, e.g., with the function omn-to-time-signature.

  If you want your final rhythm to contain rests you best add these to your rhythmic material before processing it with this function, because turning notes into rests afterwards can contradict your durational accents."
  (do-verbose ("")
    (rnd-seed seed)
    (let ((result (omn-merge-ties
		   (_durational-accent-whole-note
		    (_durational-accent-merge 
		     (_durational-accent-divide (omn :length lengths) :divide divide :n divide-n :divide-prob divide-prob
						:tie-n tie-n :tie-prob tie-prob :tie-previous-beat? tie-previous-beat?
						:grace-n grace-n :grace-length grace-length :grace-prob grace-prob
						:set set :ignore ignore
						:seed (seed))
		     :n merge-n :prob merge-prob :seed (seed))
		    :whole-note-prob whole-note-prob :seed (seed)))))
      (case format
	(:omn result)
	(:length (omn :length result))))
    #|
    (_durational-accent-merge 
     (apply #'_durational-accent-divide lengths :n divide-n :seed (seed) :allow-other-keys T args)
     :n merge-n :prob merge-prob :seed (seed))
    |#
    ))

#|
;; unfinished sketch for supporting 'dynamic' arguments, i.e., transformations that change over the sublists.
;; Implementation more difficult than anticipated, because _durational-accent-divide always needs to look at pairs of consecutive bars, so I would need to generalise edit-omn to allow for that. Also, while _durational-accent-divide depends on pairs, _durational-accent-merge does not, so the nested  _durational-accent-merge must then always only further process the first list returned by _durational-accent-divide. Further, the last bar is never changed by _durational-accent-divide -- that code needs to move into the def below as well. 
(defun durational-accent (sequence &rest args &key section &allow-other-keys)  
  (edit-omn :length sequence #'(lambda (ls)
				 (apply #'_durational-accent ls args))
	    :section section
	    ; ?? :swallow 
	    ; :additional-args  ;; TODO
  ))
|#

#|
(durational-accent (gen-repeat 4 '((q q q))) :whole-note-prob 1 :seed 132)

(durational-accent (gen-repeat 4 '((q q q))) :divide 2 :divide-n 2 :merge-n 3)

(durational-accent (gen-repeat 4 '((q q q))) :divide '(2 3) :divide-n 2 :merge-n 3)

(durational-accent (gen-repeat 4 '((q q q))) :divide-n 0 :merge-n 3 :grace-n 2)

; irregular meter
(durational-accent 
 (gen-eval 4 '(gen-repeat (rnd1 :low 2 :high 5) '(q)))
 :divide '(2 3) :divide-n 3 :merge-n 3)

; irregular meter with tuplets
(durational-accent 
 (tuplet-rhythm (gen-eval 4 '(gen-repeat (rnd1 :low 1 :high 3) 'h)) '(3 4 5 6 5 4 3 2) :count-offset 0)
 :divide 2 :divide-n 3 :merge-n 3 :grace-n 2)

;;; TODO: adding rests without destroying durational accents
;; adding rests completely randomly -- without regard for accents -- is problematic. So, better added them before calling durational-accent
(length-rest-series
 '(6 7 8) 
 (durational-accent (gen-repeat 4 '((q q q))) :divide '(2 3) :divide-n 2 :merge-n 3))

(durational-accent (length-rest-series '(6 7 8)  (gen-repeat 4 '((q q q)))) :divide '(2 3) :divide-n 2 :merge-n 3))

; (make-omn :length (omn :length '((acc 1/8 1/8) 1/2 1/4)))


|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Karnatic rhythmical techniques
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List of all possible combinations of note subdivisions in a group of n notes.
;; These are sorted into potentially accented (starting with a durational accent and/or leading to an accent if the next following note is longer, even though that is not really a karnatic concept) and non-accented
(let* ((accented-3 ; tisra 
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
    (defun gen-karnatic-cell (gati jathi position
			      &key (accented? T) (max-number nil) (min-number nil) (first-length nil)
				(include-length nil) (exclude-length nil) (seed nil))
      "Returns a karnatic rhythmic cell (list of rhythmic values) or a list of such cells (if `position' is a list). The shape of the resulting cell(s) can be controlled in many ways, allowing to generate, e.g., rhythms that are similar even if they are in different gatis or jathis.

  See Reina (2016) for details on terms like gati, jathi and matra.

* Arguments:
  - gati (integer or list of them): gati for the cell(s) to generate.
  - jathi (integer in range 3-7, or list of them): jathi for the cell(s) to generate.
  - position (integer, '?, or list of either): specifies which cell of the available options to generate. If no filtering is enabled (see further arguments below) then the list of available options are all the possible standard subdivisions of a 'beat' depending on the current jathi as listed by (Reina, 2016, p. 23f). These options are sorted  by the number of notes in rhythmic cells (fewest first), and in case of equal note numbers the length of notes starting with the first note (longer first note first). So, the position 0 is always a single note per beat/cell (length depends on gati and jathi) and so on. If `position' exceeds the number of available options, the last option is return, which is always an even subdivision of the cell in jathi matras. 
    If `position' is '? then the position is randomised.
  - accented? (Boolean, or binary integer, i.e. 0 or 1): whether the returned cells potentially carry durational accents on the start of the cells (or on an immediately following longer note). If accented? is nil (or 0) then the cell(s) carry a durational accent that is not on the start of the cell.
    Binary integers are supported so that values for this argument can be generated with Opusmodus' binary number generators.
  - max-number/min-number (integer or list of them): max/min number of notes in the cell(s).
  - first-length (ratio or OMN length, or list of either): length of the first note in cell(s).
  - include-length/exclude-length (ratio or OMN length, or list of either): length values that must be included in or excluded from the cell(s).
  - seed (integer): the seed to use for the randomised position, is position is '?.

* Examples:

  The first position is always a single note per cell (if no other filtering is selected)
  ;;; (gen-karnatic-cell 4 4 0)

  Generating multiple cells, and showing the order of cells in this gati and jathi without filtering (only cells carrying poteentially a durational accent on the first note of the cell).
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

  For more examples see also https://opusmodus.com/forums/topic/1097-updated-library-of-many-custom-opusmodus-functions/?tab=comments#comment-3497


  You may want to consider further transforming results with rhythm transformations functions like, e.g., `tie-whole-notes'. 

* BUGS:

The argument min-number is seemingly not fully working yet:

(gen-karnatic-cell 4 5 '(? ? ? ?) :min-number '(3 3 3 3) :seed 1)
=>((1/8 1/8 1/16) (3/16 1/16 1/16) (1/16 1/16 1/16 1/16 1/16) (1/8 1/8 1/16)) 


* Notes:

  - Reina, R. (2016) Applying Karnatic Rhythmical Techniques to Western Music. Routledge.
"
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




(defun gen-matras (gati jathi jathi-number &key prefix suffix)
  "Generates a sequence of matras (equal note durations) where `gati' defines the beat subdivision, `jathi' the number of matras per 'bar' (sublist) and `jathi-number' the resulting number of sublists.

* Arguments:
  - gati (int)
  - jathi (int)
  - yat-number (int)
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




;;; TODO: consider revising (or an alternative function) definition by using gen-karnatic-cell: stored are then not actual cells, but args for gen-karnatic-cell. E.g., in such a revision I would not be limited to have a durational accent on every cell/pala
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

#|
;;; TODO:
(defun change-gati (sequence in-gati out-gati)
  "Changes the duration of notes in `sequence' to fit into the given gati without changing their jathi."
  )

;;; TODO:
(defun change-jathi (sequence jathi)
  "Changes the grouping of notes in `sequence' to follow the given jathi but adding removing notes.

The sequence is supposed to be arranged such that each sublist is one jathi."
  )
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rhythm utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun _remove-rest-articulations (sequence)
  "Strips all articulations from rests. 

* Arguments:
  - sequence: OMN expression that _cannot_ be nested.
 
  TMP function -- only necessary until length-rest-merge or omn-merge-rests support merging rests with articulations."
  (tu:mappend #'(lambda (event)
                  (if (length-restp (first event))
                    (list (first event))
                    event))
              (single-events sequence)))

#|
(defun merge-rests-with-preceeding-note (sequence)
  "Remove all rests in sequence without changing the actual rhythm: extends the length of each note followed by that rest value, so that the overall duration remains as before.

* Arguments:
  - sequence: OMN expression, can be nested.

* Examples:
  ;;; (merge-rests-with-preceeding-note '(e g6 f stacc -e e ab5 mp ten e c4 mf ten))
  ;;; => (1/4 g6 f stacc e ab5 mp ten e c4 mf ten)

  BUG:
  Articulations attached to rests (e.g., fermatas) are removed.

  TODO: 
  Function can be used for Beethoven-like motivic [condensation] -- turn less important tones into rest, and then call this function.
  TODO: example for that.

  Pretty much the same as length-legato function.
  "
  (do-verbose ("")
    (let* ((nested? (every #'listp sequence))
           (events (single-events (omn-merge-rests 
                                   ;;; TODO: avoid removing rests -- currently necessary, as omn-merge-rests will not merge rests with articulations
                                   (_remove-rest-articulations (if nested? 
                                                                 (flatten sequence)
                                                                 sequence)))))
           (result (append 
                    (tu:mappend ;; mappend consecutive pairs
                     #'(lambda (n1 n2)
                         (cond ((length-restp (first n1)) 
                                nil)
                               ((length-restp (first n2)) 
                                ;; add dur of n2 to n1
                                ;;; TODO: preserve articulations of rests
                                (cons (+ (omn-encode (first n1)) (abs (omn-encode (first n2))))
                                      (rest n1)))
                               (T n1)))
                     (butlast events)
                     (last events (1- (length events))))
                    (let ((last-event (first (last events))))
                      (if (length-restp (first last-event))
                        nil
                        last-event)))))
      (if nested?
        (copy-time-signature sequence result)
        result))))

; (merge-rests-with-preceeding-note '(e g6 f stacc -e e ab5 mp ten e c4 mf ten))
; (length-legato '(e g6 f stacc -e e ab5 mp ten e c4 mf ten))
|#



(defun lengths-with-merged-ties (sequence)
  "Returns a flat list of lengths that preserves the lengths in `sequence' including their tied notes.
  
* Examples:
  ;;; (lengths-with-merged-ties '((h c4 pizz q arco+tie) (q h tie) (h.)))
  ;;; => (1/2 1/2 5/4)

  Contrast:
  ;;; (omn :length '((h c4 pizz q arco+tie) (q h tie) (h.)))
  ;;; => ((1/2 1/4) (1/4 1/2) (3/4))

* See Also:

  ;;; (omn-merge-ties (flatten-omn '((h c4 pizz q arco+tie) (q h tie) (h.))))
  ;;; => (h c4 pizz c4 arco wq)
  "
  (butlast
   (reduce #'(lambda (&optional accum pair2)
               ; (print (list accum pair2))
               (when (and accum pair2)              
                 (append 
                  (butlast accum 2)
                  (if (equal (first (last accum)) 'tie)
                    (list (+ (first (last (butlast accum))) (first pair2)) (second pair2))
                    (list (first (last (butlast accum))) (first pair2) (second pair2)))
                  )))
           (matrix-transpose  
            (list (omn :length (flatten-omn sequence))
                  (mapcar #'(lambda (arts)
                              (when (member 'tie arts)
                                'tie))
                          (mapcar #'disassemble-articulations 
                                  (omn :articulation (flatten-omn sequence)))))))))

(defun total-duration (sequence &optional float?)
  "Returns the total duration (length) of `sequence', i.e. the sum of the length of all its notes and rests. 

  If `float?' is true the result is a float.

* Examples:
  ;;; (total-duration '((h c4 q) (q h tie) (h.)))
  ;;; => 9/4"
  (let ((result (reduce #'+ (mapcar #'abs (omn :length (flatten-omn sequence))))))
    (if float?
      (* result 1.0)
      result)))

(defun bpm->duration (beats tempo)
  "Converts a duration in number of beats and the corresponding tempo in BPM into a duration in seconds."
  (* (* (/ 1 tempo) beats) 60.0))


(defun isolate-time-signatures (ts-forms)
  "Transforms time signatures `ts-forms' so that each resulting time signature denotes only a single bar.  

* Examples:
  ;;; (isolate-time-signatures '((3 4 2) (2 4 1)))
  ;;; => ((3 4 1) (3 4 1) (2 4 1))"
  (mappend #'(lambda (ts-form) 
               (destructuring-bind (num denom no)
                                   ts-form
                 (gen-repeat no `((,num ,denom 1)))))
           ts-forms))


(defun length->time-signature (sequence)
  "Expects any OMN sequence, extracts its rhythm, and translates each note value in a time signature of the same length.

* Examples: 
;;; (rhythm-to-time-sig-sequence '(3/4 1/4 1/2))
;;; => ((3 4 1) (1 4 1) (2 4 1))

* See Also:

  This function is largely the complement of the Opusmodus buildin function `time-signature-length'."
  (let ((ls (flatten (omn :length sequence)))
        ;; Time signature subsitutions, e.g., 1/1 -> 4/4
        (time-sig-subsitutions '(((1 1 1) (4 4 1))
                                 ((1 2 1) (2 4 1)))))
    (mapcar #'(lambda (dur)
                (reduce #'(lambda (xs old-new)
                            (subst (second old-new) (first old-new) xs :test #'equal))
                        time-sig-subsitutions
                        ;; translation of duration ratios into time signature lists
                        :initial-value (list (numerator dur) (denominator dur) 1))) 
            ;; list of duration ratios
            ls)))


#| ;; meanwhile buildin in Opusmodus
(defun metronome (sequence &key (pitch 'c4) (velocity 0.8))
  "See https://opusmodus.com/forums/topic/1025-easiest-way-to-add-click-track-metronome-in-om/"
  (do-verbose ("metronome")
    (let* ((ts (get-time-signature sequence))
           (len (loop for i in ts
                  collect (gen-repeat (last1 i) (gen-repeat (car i) (list (/ 1 (second i)))))))
           (vel (loop for i in ts
                  collect (cons velocity (gen-repeat (1- (car i)) (list (- velocity 0.15))))))
           (pch (loop for i in ts
                  collect (cons pitch (pitch-transpose -2 (gen-repeat (1- (car i)) pitch))))))
      (make-omn :length len :pitch pch :velocity vel))))
|#


(defun metric-shift (l lengths)
  "Appends `l' (a length or omn) before `lengths' (a list of lengths or omn), but maintains the metric structure, i.e., the function shifts `lengths' metrically 'to the right' by `l'.
  Returns an OMN form if lengths is an OMN form, otherwise a length form.
  
* Examples:

;;; (metric-shift '-h '((q q q q) (q q q q)))

;;; (metric-shift '(h g4 e) '((q c4 q d4 q e4 q f4) (q c4 q d4 q e4 q f4)))

* BUGS: 

If `lengths' is only a list of length values and not a full OMN sequence, then other parameters in `l' are ignored.
  
;;; (metric-shift '(h d4) '((q q q q) (q q q q)))


* See Also: 

assemble-seq (but that does not shift across bars)"
  (let* ((time-sigs (get-time-signature lengths))
         (result (omn-to-time-signature (cons l (flatten lengths)) time-sigs)))
    (if (omn-formp lengths)
        result
      (omn :length result))))

; (metric-shift '-h '((q q q q) (q q q q)))
; (metric-shift '(h g4) '((q c4 q d4 q e4 q f4) (q c4 q d4 q e4 q f4)))
;;; BUG: does ignore pitch of l
; (metric-shift '(h d4) '(q q q q q q q q))



;;; TODO: add support for arbitrary OMN sequences, not just lengths
;;; TODO: when shifting bar lines to the left (positions negative), allow for adding corresponding rest at the beginning with extra arg, so that the time signature of the first bar does not change.
(defun shift-meter-boundaries (lengths positions)
  "Shift meter boundaries (sublist lengths) between bars in `lengths', by given number of positions (number of notes or rests) forwards or backwards. Such meter transformations can be useful, e.g., to prepare music for inserting durational accents expressing syncopations, and then afterwards moving the meter boundaries back to their original positions (e.g., using `copy-time-signature' with the original sequence).  

Note that resulting meter changes may occur even within tuplets, though Opusmodus may not support notating the result.  

* Arguments: 
  - length: list of OMN length values, must be nested
  - positions (lists of ints, length most be one shorter than number of bars/sublists in lengths): amount by how many note values the bar line should be moved to the left (negative number) or to the right (positive number). `positions' is circled through to meet the length of `lengths'.

* Examples:
  ;;; (shift-meter-boundaries '((1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18)
  ;;;                           (1/16 1/16 1/16 1/16 1/16 1/16 1/16 -1/16)
  ;;;                           (-1/14 1/14 1/14 1/14 1/14 1/14 1/14) 
  ;;;                           (q -q)) 
  ;;;                         '(0 -1 1))

"
  (reverse
   ;; Loop over two consecutive elements in list, but with processing each element twice, first as first and then the result as second element. In parallel, iterate over positions
   (reduce #'(lambda (bars aux)
	       (let* ((bar1 (first bars))
		      (bar2 (first aux))
		      (pos (second aux))
		      (bars-combined (append bar1 bar2))
		      (pos-combined (+ pos (length bar1))))
		 ;; accumulate backwards
		 (cons (nthcdr pos-combined bars-combined)
		       (cons (first-n pos-combined bars-combined)
			     (rest bars)))))
	   (tu:mat-trans (list (rest lengths)
			       (circle-repeat positions (1- (length lengths)))))
	   :initial-value (list (first lengths)))))

; (shift-meter-boundaries '((1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18) (1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16) (1/14 1/14 1/14 1/14 1/14 1/14 1/14) (q -q)) '(0 -1 1))

#| ;; edit-omn does not work here, because shift-meter-boundaries needs access to consecutive bars
(defun shift-meter-boundaries (sequence)
  (edit-omn :length sequence
	    #'_shift-meter-boundaries
	    :flat nil))
|#

  
(defun insert-into-bar (positions new lengths &key section seed)
  "Inserts item(s) 

* Arguments:
  - positions (list of position values): either symbol 's (start), 'e (end) or '? (random position), or integer specifying position.
  - new: list of OMN length values to be inserted, can be nested or partially nested. 
  - lengths: list of OMN length values, must be nested.
  `positions' and `new' is circled through to meet the length of `lengths' or `section'.
  - section (list of ints): selected sublists to process.

* Examples:
  ;;; (insert-into-bar '(s e) '(q (e e)) '((h) (h) (h)))
  ;;; (insert-into-bar '(?) '(q (e e)) '((h) (h) (h)) :section '(1 2) :seed 1)

* See Also: 

Opusmodus builtin `position-insert'.
"
  (rnd-seed seed)
  (map-section #'(lambda (seq position item)
		   (insert-at-position position item seq :seed (seed)))	       
	       (omn :length lengths)
	       :section section
	       ;;; TODO: circle-repeat positions and new, so that their length can differ 
	       :section-args (tu:mat-trans (list positions new))))




#|
;; TODO: Insert length values at the end of bars
;;; NOTE: cannot use edit-omn, because I do not want to preserve orig time signatures
(defun insert-into-bar (positions new lengths &key section)
  "Inserts item(s) 

  Args
  - positions: either symbol 's (start), 'e (end) or '? (random position), or integer specifying position.
  - items: list of OMN length values to be inserted, can be nested. `new' is circled through to meet the length of `lengths'.
  - lists: list of OMN length values, must be nested.

See also Opusmodus builtin `position-insert'.
"
  (edit-omn :length
 
  )
|#


#|
(position-insert '(0 2 4) '((g4 f4) c5) 
		 '((c4 c4 c4 c4 c4) (d4 d4 d4 d4 d4))
		 :type 'list)
|#

#|

;; TODO: Remove given number of length values at the end of bars
;; NOTE: may result in incomplete tuplets

|#
