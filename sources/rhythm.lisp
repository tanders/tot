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

  Args:
  - 
  "
  
  
  )
|#

;; Useful project-wide definition, but likely no need to store it more generally.
(defun tuplet-rhythm (lengths subdivisions &rest args 
                              &key (length-dividend 1/2) (count-offset 0) (position 'e) (type '?)
                              &allow-other-keys)
  "Subdivides given note `lengths'.  

  Args:
  - lengths: list of length values, can be nested.
  - subdivisions: (circling list of ints) specifies tuplet sequence.
  - length-dividend (default 1/2): duration ratio divided by subdivisions. Default case are half note subdivisions, ie. a subdivision of 3 results 3h notes (triplets splitting a half note). 
  - count-offset (default -1): `subdivisions' also specifies number of equally spaced notes per tuplet plus amount of `count-offset' . I.e., if `count-offset' = 0 then each note is split into subdivision notes.
  
  All keyword args from `length-divide2' are inherited, but some with different default.

  Examples:
  ;;; (tuplet-rhythm '(1/2 1/2 1/2) '(3 4 5))
  ;;; => (1/6 1/6 1/6 1/8 1/8 1/8 1/8 1/10 1/10 1/10 1/10 1/10)

  ;;; (tuplet-rhythm (gen-repeat 8 '((1/2))) '(3 4 5 6 5 4 3 2))

  With irregular meter
  ;;; (tuplet-rhythm (gen-eval 4 '(gen-repeat (rnd1 :low 2 :high 5) 'h)) '(3 4 5 6 5 4 3 2) :seed 1234)

  BUG: seed argument not working as expected -- it is not even used in this function!"
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


;; Originally inspired by reading rhythm of v. Schweinitz scores :)
;;; TODO: consider allowing bar-length to be a list of values.
(defun tuplet-walk-rhythm (bar-no &key (bar-length 1/2) (subdivisions-ambitus '(2 7)) 
                                  (rest-distances '(7 8 9)) 
                                  (rest-distance-order :rnd)
                                  (last-bar nil)
                                  (seed nil))
  "Some custom algorithm to create rhythmic phrases that randomly walk across tuplet subdivisions and includes some rests.

  Args:
  - bar-no: number of bars to generate
  - bar-length: regular duration of resulting bars
  - subdivisions-ambitus: range of tuplet subdivisions
  - rest-distances: distances between rests 
  - rest-distance-order (:rnd or :seq): whether rest distances will be in the given order (:seq) or randomised (:rnd)
  - last-bar: bar added at end after bar-no bars. If nil, no bar is added. 

  Example: 
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
         `(,last-bar))))))


(defun even-length-rhythm (length &key total-duration prefix suffix (time-sig '(4 4)))
  "Custom algorithm to create rhythmic phrases consisting of even note durations over a certain time. 

  Args:
  - length (length value): rhythmic value to repeat
  - total-duration (length value): duration of the generated phrase including the prefix and suffix length.
  - prefix (length value or length sequence): preceeding phrase
  - suffix (length value or length sequence): succeeding phrase
  - time-sig: time signature

  Example:
  ;;; (even-length-rhythm '5q :total-duration 'w_w :prefix '-w_5h :suffix '-5q_5h_q)"  
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
      (length-span (list repetition-dur) (list length))
      (when suffix (tu:ensure-list suffix))
      ) 
     time-sig)))


#|
(defun even-length-rhythm2 (length duration 
                                    &key prefix suffix (time-sig '(4 4)))
  "Some custom algorithm to create rhythmic phrases consisten of even note durations over a certain time. Variant of even-length-rhythm, where the total-duration is the sum of duration, prefix and suffix length.

  Args:
  - length (length value): rhythmic value to repeat
  - duration (length value): duration over which to repeat the rhythmic value
  - prefix (length value or length sequence): preceeding phrase
  - suffix (length value or length sequence): succeeding phrase
  - time-sig: time signature

  Example:
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


(defun length-divide-ext (count divide length &rest args &key seed)
  "Same as length-divide, but arg divide is list of ints (elements circled through).

  Note: implemented by calling length-divide internally for each sublist in length, and therefore arguments like section and exclude are not supported.

  Examples:
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
  "Turn notes at specific positions into rests. This function is like the Opusmodus built-in length-rest-series, but supports arbitrary OMN expressions as input and additionally the arguments swallow and section.

  Args:
  - positions (list of ints): positions of notes to be turned into rests
  - sequence (list of lengths or OMN expression): music to process
  - flat (Boolean): whether positions count for sublists (nil) or the whole list (T)
  - swallow (Boolean): whether the pitches of notes turned into rests should be shifted to the next note or omitted (swallowed) 
  - section (list of ints): positions of sublists to process. This argument is ignored if flat is T.

  Example:

;;; (setf melody '((s eb6 < leg f5 < leg c5 < leg f5 < leg) (e e6 f - -q)))
;;; (note-rest-series '(1 1) melody :swallow T :section '(0))
"
  (edit-omn :length sequence 
            #'(lambda (ls) (length-rest-series positions ls))
            :swallow swallow
	    :section section
	    :flat flat))


(defun _merge-rest-into-note (lengths)
  "[Aux def]
All rests are merged into the following note. This function can be useful for producing harmonic rhythms. 

Args: 
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

Args:
- sequence: OMN sequence, can be nested
- flat (Boolean): whether or not to merge rests across bar boundaries
- section (list of positive integers): selection of sublists to process

Example:

(merge-rest-into-note '((-h w. e4 pp) (-h w. gs4 pp) (-h w. gs4 pp)))
=> ((d e4 pp) (d gs4) (d gs4))

(merge-rest-into-note '((-w) (-h h e4 pp)) :flat T)
=> ((w e4 pp tie) (w e4 pp))
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


(defun cut-holes-aux (lengths binary-list)
  "Expects a list of lengths and a matching binary list. Every length at a position of a 1 in the binary list is left untouched, while every length at a 0 is turned into a rest. 

  If binary-list is shorter than lengths it is repeated in a circular fashion.

  Related: length-rest-series"
  (mapcar #'(lambda (l b)
	      (if (= b 0) 
		  (* l -1)
		  l))      
          lengths
          (circle-repeat (flatten binary-list) (length lengths))))

;; (cut-holes-aux '(1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16) '(1 1 1 0 1 1 1))

(defun cut-holes (sequence binary-list &key (swallow nil))
  "Turns notes in `sequence' into rests if `binary-list' contains a 0 at the matching position. Notes are left untouched if there is a 1 at the matching position.

  Args:
  - sequence (list of lengths or OMN expression): music to process
  - binary-list (flat list of ints): 
  - flat (Boolean): whether binary-list count for sublists (nil) or the whole list (T)
  - swallow (Boolean): whether the pitches of notes turned into rests should be shifted to the next note or omitted (swallowed) 

  See also: note-rest-series"
  (edit-omn :length sequence
	    #'(lambda (ls) (cut-holes-aux ls binary-list))
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
(defun cut-holes-aux (lengths binary-list)
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

  Args:
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

  Example:
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

  Args:
  divide (integer or list of integers, default 2): specifies into how many equal note values notes preceeding a durational accent are subdivided. If list of integer, subdivision is randomly chosen.
  n (integer): number of notes at end of bars that are potentially subdivided. If a bar starts with a rest, then it cannot carry a durational accent, and hence its preceding note is never subdivided. 
  divide-prob (default 0.5): probability value between 0.0 and 1.0, controlling whether a note that could be subdivided for creating a durational accent actually will be. Higher values make durational accents more likely.
  grace-n (integer): number of grace notes potentially inserted before first notes of bars.
  grace-length (length value): note value of inserted grace notes.
  grace-prob (default 0.5): probability value controlling whether grace notes are inserted.
  set (length or list of lengths): only specified lengths are subdivided. 
  ignore (length or list of lengths): specified lengths are *not* subdivided. 
  seed (integer): random seed.

  Example:
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

  Args:
  - n (integer): number of notes at beginning of bars that are potentially subdivided.
  - prob (default 0.5): probability value between 0.0 and 1.0, controlling whether notes that could be merged for creating a durational accent actually will be. Higher values make durational accents more likely.
  - seed (integer): random seed.

  Example:
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
 (_durational-accent-divide  (gen-repeat 4 '((q q q))) :divide '(2 3) :n 2)
 :n 2)
|#


;;; TODO:
;;; - [Postponed] All args should support lists, and additionally support args like section: use edit-omn for getting that.
;;;   Differently put, but same point: generalise: several args could alternatively expect lists to allow controlling a development: divide, all *-n args, all *--prob args
;;;   NOTE: Implementation more difficult than anticipated, see comments above sketch below this function for details.  
;;
;; - ??? extra function to turn notes into rests -- 
;;   - leave untouched: first note of bar if preceded by shorter notes and last note of bar is suceeded by longer note
(defun durational-accent (lengths 
			  &rest args
			  &key (divide-n 1) (merge-n 2) (merge-prob 0.5) (seed nil) &allow-other-keys)
  "Adds durational accents on first notes of bars by subdividing the last note of the preceding bar (see divide-related args) or merging notes at the beginning of a bar (see merge-related args). Subdivided notes can be partially tied together for rhythmic variety (see tie-related args), and durational accents can also be expressed with grace notes (see grace-related args). 

While `durational-accent' only supports accents of the first beat of each bar, you can easily realise other accent patterns by temporarily rebarring the music (e.g., with `omn-to-time-signature' and perhaps `length->time-signature'), and then afterwards align the result of `durational-accent' again with the original time signatures (e.g., with `copy-time-signature').

  Args:
  - lengths: a list of length lists (multiple bars). `length' can also be arbitrary OMN expressions, but only length lists are returned and other parameters are ignored. 
  - divide (integer or list of integers, default 2): specifies into how many equal note values notes preceeding a durational accent are subdivided. If list of integer, subdivision is randomly chosen.
  - divide-n (integer): number of notes at end of bars that are potentially subdivided. If a bar starts with a rest, then it cannot carry a durational accent, and hence its preceding note is never subdivided. 
  - divide-prob (default 0.5): probability value between 0.0 and 1.0, controlling whether a note that could be subdivided for creating a durational accent actually will be. Higher values make durational accents more likely.
  - tie-n (integer): maximum number of subdivided notes at end of bars (before the next durational accent) that are potentially tied together. Should not be larger than divide * divide-n. (The returned notes are not necessarily tied but can instead be notated, e.g., as dotted note values.)
  - tie-previous-beat? (Bool): whether or not the first subdivided note before the durational accent is tied to the preceeding note (e.g., the preceeding accent). Such tie never occurs across bar lines. 
    NOTE: If tie-previous-beat? is T, then you may want the argument divide to be 3 or larger, and tie-n to be only 1 (or slightly larger for large values of divide), because otherwise you also turn the preceeding note into a durational accent.
  - tie-prob: probability value controlling whether subdivided notes are tied. 
  - merge-n (integer): number of notes at beginning of bars that are potentially subdivided.
  - merge-prob (default 0.5): probability value controlling whether grace notes are inserted.
  - grace-n (integer): number of grace notes potentially inserted before first notes of bars.
  - grace-length (length value, default 1/8): note value of inserted grace notes.
  - grace-prob (default 0.5): probability value controlling whether grace notes are inserted.
  - set (length or list of lengths): only specified lengths are subdivided. 
  - ignore (length or list of lengths): specified lengths are *not* subdivided. 
  - seed (integer): random seed.

  Examples (evaluate multiple times to see range of solutions):
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
    (omn :length
	 (omn-merge-ties
	  (_durational-accent-merge 
	   (apply #'_durational-accent-divide (omn :length lengths) :n divide-n :seed (seed) :allow-other-keys T args)
	   :n merge-n :prob merge-prob :seed (seed))))
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
;;; Rhythm utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun _remove-rest-articulations (sequence)
  "Strips all articulations from rests. 

  Args:
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

  Args:
  - sequence: OMN expression, can be nested.

  Examples:
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
  
  Examples:
  ;;; (lengths-with-merged-ties '((h c4 pizz q arco+tie) (q h tie) (h.)))
  ;;; => (1/2 1/2 5/4)

  Contrast:
  ;;; (omn :length '((h c4 pizz q arco+tie) (q h tie) (h.)))
  ;;; => ((1/2 1/4) (1/4 1/2) (3/4))

  See also:
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

(defun total-duration (sequence)
  "Returns the total duration (length) of `sequence', i.e. the sum of the length of all its notes and rests.

  Example:
  ;;; (total-duration '((h c4 q) (q h tie) (h.)))
  ;;; => 9/4"
  (reduce #'+ (mapcar #'abs (omn :length (flatten-omn sequence)))))



(defun isolate-time-signatures (ts-forms)
  "Transforms time signatures `ts-forms' so that each resulting time signature denotes only a single bar.  

  Example:
  ;;; (isolate-time-signatures '((3 4 2) (2 4 1)))
  ;;; => ((3 4 1) (3 4 1) (2 4 1))"
  (mappend #'(lambda (ts-form) 
               (destructuring-bind (num denom no)
                                   ts-form
                 (gen-repeat no `((,num ,denom 1)))))
           ts-forms))


(defun length->time-signature (sequence)
  "Expects any OMN sequence, extracts its rhythm, and translates each note value in a time signature of the same length.

  Example: 
;;; (rhythm-to-time-sig-sequence '(3/4 1/4 1/2))
;;; => ((3 4 1) (1 4 1) (2 4 1))

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


(defun shift-meter-boundaries (lengths positions)
  "Shift meter boundaries (sublist lengths) between bars in `lengths', by given number of positions (number of notes or rests) forwards or backwards. Such meter transformations can be useful, e.g., to prepare music for inserting durational accents expressing syncopations, and then afterwards moving the meter boundaries back to their original positions (e.g., using `copy-time-signature' with the original sequence).  

Note that resulting meter changes may occur even within tuplets, though Opusmodus may not support notating the result.  

  Args: 
  - length: list of OMN length values, must be nested
  - positions (lists of ints, length most be one shorter than number of bars/sublists in lengths): amount by how many note values the bar line should be moved to the left (negative number) or to the right (positive number). `positions' is circled through to meet the length of `lengths'.

  Example:
  (shift-meter-boundaries '((1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18 1/18) (1/16 1/16 1/16 1/16 1/16 1/16 1/16 -1/16) (-1/14 1/14 1/14 1/14 1/14 1/14 1/14) (q -q)) '(0 -1 1))

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

  Args
  - positions (list of position values): either symbol 's (start), 'e (end) or '? (random position), or integer specifying position.
  - new: list of OMN length values to be inserted, can be nested or partially nested. 
  - lengths: list of OMN length values, must be nested.
  `positions' and `new' is circled through to meet the length of `lengths' or `section'.
  - section (list of ints): selected sublists to process.

  Examples
  (insert-into-bar '(s e) '(q (e e)) '((h) (h) (h)))
  (insert-into-bar '(?) '(q (e e)) '((h) (h) (h)) :section '(1 2) :seed 1)

See also Opusmodus builtin `position-insert'.
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
