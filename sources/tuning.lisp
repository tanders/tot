;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
(in-package :om)

#|

TODO: How to get full scale of a given temperament (in cent and ratios or pitch notation)

 - Trivial for EDOs: start somewhere and add generator in cent (or multiply mapped ratio) the number of subdivisions of the octave
   => This still does not give the ratios in a form that takes tempered out commas into account

 - Most (all??) other temperaments contain in principle an infinite number of pitches. So, I would need for each generator some integer how often to apply it for generating further pitches (e.g., 12 or 19 for the Meantone fifth) and a list of the commas that the temperament tempers out (often obtainable from https://en.xen.wiki). Then generate all the pitches per generator (ratios or JI-supporting notation) and for each resulting pitch use list of given commas to generate quasi enharmonic/equal alternative notations of that pitch. You might then want to sort all the resulting JI pitch groups within the group by the interval complexity (multiple approaches possible, see https://en.xen.wiki/w/Complexity) and sort the groups by increasing pitch measured in cents.


|#

#|

Cluster Engine and harmonic fixing with temperament support

 - Some temperament bound to so global variable that is used by certain rules
 - In a first version, the temperament will always be constant, but later I might implement dynamic temperaments by statefully changing the global temperament with some rule that has access to something like the current time (in bars, or beats or ...)
   - Alternatively, I might store indices into the current temperament (into an existing list of temperaments) by pitches in their own voice in a score (like I represent the underlying chords and scales)
 - All pitches (in particular input pitch domains, underlying chords and scales, but also the input voices to roughly follow for harmonic fixing) are represented by rationals. However, only a subset of the (possibly huge) set of JI pitch lattice for the current prime limit is needed, because all these rationals represent tones in the temperament, with certain commas tempered out. 
  => I can translate my JI notation for OMN for temperaments into these rationals...
 - Certain rules (e.g., melodic rules checking whether to avoid pitch repetitions) translate internally the JI pitches into tempered cent values with the given temperament and can then perform their work on those cent values. I might even find a way to translate the JI pitches into integers representing degrees/steps in the temperament (would just have to store some mapping of the JI pitches to the steps together with the temperament)
 - In the result, the pitches are still represented by rationals, so these can be mapped again to my JI notation for OMN. For my harmonic fixing, these ratios will be members of the current chords/scales (possibly octave transposed)

|#

#|

define macro def-tempered-score

Does exactly the same as the builtin Opusmodus def-score, but additionally expects a temperament set as one of the global parameters. In a first version, this will always be a constant temperament, but later I might define how the temperament can change, e.g., per par bar or beat.

The macro def-tempered-score will define a def-score internally. Initially only monophonic parts will be supported, and for each note in that part (in my new OMN JI notation with custom accidentals) some tuning value will be computed with the current temperament, which will all be stored as a list of tuning values measured in tempered semitones (cent values / 100) under the tuning parameter of the voice/part in question. Some some similar functionality see https://opusmodus.com/forums/topic/1138-write-tuning-cents-into-omn-extract-them/ 

In a later version I will support also chords, but then def-tempered-score would need to translate each polyphonic voice/part into multiple monophonic parts, each in their own MIDI channel and with their independent tuning.

|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General utilities
;; TODO: Save elsewhere
;;


(defun flip-hash-table (table)
  "Turns the keys of the given hash `table' into values and vice versa"
  (alexandria:alist-hash-table
   (mapcar (lambda (pair)
	     (let ((key (first pair))
		   (val (rest pair)))
	       (cons val key)))
	   (alexandria:hash-table-alist table))))
#|
(alexandria:hash-table-alist 
 (flip-hash-table 
  (alexandria:alist-hash-table '((a . 1) (b . 2) (c . 3)))))
=> ((2 . B) (1 . A) (3 . C))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tuning math
;;

;; TODO: consider using alextrandria:define-constant -- https://google.github.io/styleguide/lispguide.xml#Defining_Constants
;; https://en.xen.wiki/w/Harmonic_limit
;; ? TODO: change representation into vector?
(defconstant +primes+
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61)
  "List of prime numbers from 2 to 61.")

(defconstant +edo-prime-octave-offsets+
  (loop for prime in +primes+
     collect (round (log (/ prime (ji-pc-ratio prime))
			 2)))
  "List of octave offsets of prime frequency ratios, i.e., in which octave lies a prime frequency ratio compared to 1/1. For example, the first prime (2) is one octave and the third prime (5) is two octaves above 1.")


(defun primes-in-limit (limit)
  "Return list of primes up to the given prime limit."
  ;; TODO: optimise -- use fact that +primes+ are sorted
  (remove-if-not (lambda (prime) (<= prime limit))
		 +primes+))
; (primes-in-limit 11)

(defparameter +limit-31-primes+
  (primes-in-limit 31)
  "List of prime numbers from 2 to 31.")

;; CONSIDER: Move this into some utilities library?
;; https://exercism.io/tracks/common-lisp/exercises/prime-factors/solutions/8a1fc0fd648b44119c5d63c29ec22d26
(defun prime-factors (n)
  "Return list of prime factors of given integer."
  (flet ((is-factor? (x n)
	   (zerop (mod n x)))) 
    (when (> n 1)
      (loop for x from 2 to (isqrt n)
	 when (is-factor? x n)
	 return (nconc (prime-factors x) (prime-factors (/ n x)))
	 finally (return (list n)) ))))
#|
(prime-factors 24)
=> (2 2 2 3)
|#


;; ? TODO: change monzo representation into vector?
(defun monzo-to-ratio (monzo)
  "Return a ratio that corresponds to a monzo represented by a list of integers.

See also https://en.xen.wiki/w/Monzo
"
  (apply #'*
	 (mapcar (lambda (prime exponent)
		   (expt prime exponent))
		 +primes+
		 monzo)))
#|
(monzo-to-ratio '(-2 0 1))
=> 5/4
|#


(defun ratio-to-monzo (frac)
  "Return a monzo represented by a list of integers that corresponds to the fraction `frac'.

See also https://en.xen.wiki/w/Monzo"
  (let* ((num-factors (prime-factors (numerator frac)))
	 (denom-factors (prime-factors (denominator frac)))
	 (limit-primes (primes-in-limit (max (apply #'max
						    ;; cons 1 for case that factors are NIL (if numerator is 1)
						    (cons 1 num-factors))
					     (apply #'max (cons 1 denom-factors))))))
    (loop for prime in limit-primes
       collect (+ (count prime num-factors)
		  (* -1 (count prime denom-factors))))))

#|
(ratio-to-monzo 5/4)
=> (-2 0 1)

(ratio-to-monzo 4/5)

(ratio-to-monzo 25/24)
(ratio-to-monzo 81/80)
=> (-4 4 -1)

(ratio-to-monzo 2/1)
(ratio-to-monzo 1/2)

(ratio-to-monzo 1)
|#



(defun monzo-limit (monzo)
  "Return the prime limit of the given `monzo'."
  (if monzo  
      (nth (1- (length monzo)) +primes+)
      ;; empty monzo represents 1/1
      1))
#|
(monzo-limit '(0 0 1))
(monzo-limit '(-11 7))
(monzo-limit '(-11))
(monzo-limit ())
|#



(let (;; freq at keynum 0, MIDI keynum 69 = 440 Hz
      (freq0 8.175798915643707))
  (defun freq-to-keynum (freq &key (keys-per-octave 12) (round? T))
    "Transforms the frequency `freq' into the corresponding key number in an equally tempered scale with `keys-per-octave' keys per octave. 
The function is 'tuned' such that (freq-to-keynum 440.0) returns 69. The Boolean arg `round?' controls whether the result is rounded or not."
    (let ((result (* (log (/ freq freq0) 2) keys-per-octave)))
      (if round?
	  (round result)
	  result)))
      
  (defun keynum-to-freq (keynum &key (keys-per-octave 12))
    "Transforms the key number `keynum' into the corresponding frequency in an equally tempered scale with `keys-per-octave' keys per octave. 
The function is 'tuned' such that (keynum-to-freq 69) returns 440.0 Hz."
    (* (expt 2 (/ keynum keys-per-octave)) freq0))

  (defun ratio-to-keynum-interval (frac &key (keys-per-octave 12) (round? T))
    "Transform the frequency ratio `frac' into the corresponding keynumber interval in an equally tempered scale with `keys-per-octave' keys per octave."
    (freq-to-keynum (* frac freq0) :keys-per-octave keys-per-octave :round? round?))

  (defun keynum-interval-to-ratio (interval &key (keys-per-octave 12))
    (/ (keynum-to-freq interval :keys-per-octave keys-per-octave) freq0)
    ))
#|
(freq-to-keynum 440)
=> 69

(freq-to-keynum 440 :keys-per-octave 24)
=> 138

(freq-to-keynum 455.0 :round? NIL)
=> 69.58035

(keynum-to-freq 69)
=> 440.00003

(ratio-to-keynum-interval 3/2)
=> 7

(keynum-interval-to-ratio 7)
=> 1.4983071
|#

(defun ratio-to-cent (frac &key (round? NIL))
  "Transform the frequency ratio `frac' into the corresponding cent value."
  (ratio-to-keynum-interval frac :keys-per-octave 1200 :round? round?))
#|
(ratio-to-cent 3/2 :round? T)
=> 702, -0.045043945

(ratio-to-cent 3/2 :round? NIL)
=> 701.95496
|#

(defun cent-to-ratio (cent)
  (keynum-interval-to-ratio cent :keys-per-octave 1200))
#|
(cent-to-ratio 702)
=> 1.500039
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 
;;

(defun edo-val (edo mappings)
  "Return a val (list of ints) given a subdivision of the octave (an int) and mappings for all prime limits as a list of temperament degrees *within an octave* (list of ints). The first mapping should be 0 (and not `edo')."
    (loop
       for mapping in mappings
       for octave-offset in +edo-prime-octave-offsets+
       collect (+ mapping (* edo octave-offset))))
#|
;; vals for 11-limit-12-EDO
(edo-val 12 '(0 7 4 10 6))
|#


(defun ratio-to-1D-temperament-degree (frac val)
  "Return an interval in a 1D temperament specified by its `val' that corresponds to the JI interval ratio `frac'. The resulting interval is specified as a factor (or exponent) for the single generator of the temperament. For example, if the generator is the smallest interval of an equal temperament, the returned degree specifies how many such intervals are needed to reach a pitch that corresponds to the given fraction. (If the generator is measured in cents, the returned degree is to be used as a factor for the measured, but if the generator is a frequency ratio, then the returned degree is to be used as an exponent.)

* Arguments:
  - frac (rational): a JI interval for which we want to get a tempered interval
  - val (list of ints): the val that specifies how JI pitches of prime limits are mapped to the temperament. See also https://en.xen.wiki/w/Keenan%27s_explanation_of_vals and https://en.xen.wiki/w/Val

* Examples:

We can specify arbitrary equal temperaments (not just EDOs) by using their smallest step as a generator. The val then contains the ordered number of theses steps to the temperament intervals that should serve as the prime frequency ratios 2, 3, 5... up to the desired prime limit. For 12-EDO, the smallest step is 100 cent. 12 such steps correspond to the frequency ratio 2 (the octave), 19 such steps to the frequency ratios 3 (the octave plus a fifth), 28 such steps correspond to the frequency ratio 5 and so on. 

;;; (setf 12-EDO-val '(12 19 28))

With this 5-limit val, we can now obtain the 12-EDO intervals for arbitrary 5-limit JI intervals specified by ratios, for example the minor third (6/5) in 12-EDO is 3 steps.

;;; (ratio-to-1D-temperament-degree 6/5 12-EDO-val)
=> 3

Remember that 12-EDO is a meantone temperament (https://en.wikipedia.org/wiki/Meantone_temperament), so the syntonic comma (81/80) is tempered out: the size of that interval is 0. 

;;; (ratio-to-1D-temperament-degree 81/80 12-EDO-val)
=> 0

As a consequence, intervals that differ only by this comma have the same size in this temperament. For example, the Pythagorean minor third (32/27) has also the size of 3 in 12-EDO.

;;; (ratio-to-1D-temperament-degree (* 6/5 80/81) 12-EDO-val)
=> 3

The 7-limit val for the temperament 22-EDO (https://en.xen.wiki/w/22edo) is as follows. You can collect the data for writing such vals yourself simply by collecting the temperament interval sizes for prime frequency ratios up to the relevant limit (2, 3, 5, and 7 in this case), e.g., from a table for that temperament listing all its intervals the the approximated JI ratios (e.g., see https://en.xen.wiki/w/22edo#Superpyth.2FPorcupine_Notation). For 22-EDO, the ratio 2 corresponds to 22 steps, the ratio 3 to 13 (fifth) plus 22 (1 octave) steps and so forth. 

;;; (setf 22-EDO-val (list 22 (+ 13 22) (+ 7 (* 2 22)) (+ 18 (* 2 22))))

With this 7-limit val for 22-EDO, we can now get the 22-EDO intervals for arbitrary 7-limit JI ratios. Here is the subminor third (7/6). You can confirm the resulting value with the above-mentioned table for this temperament.

;;; (ratio-to-1D-temperament-degree 7/6 22-EDO-val)
=> 5
"
  (let ((frac-monzo (ratio-to-monzo frac)))
    (assert (<= (length frac-monzo) (length val))
	    (frac-monzo val)
	    "Prime limit of given ratio too high for given val. 
The length (number of prime factors) of the monzo ~A 
of the given fraction ~A exceeds the length of the given val 
~A"
	    frac-monzo frac val)
    (apply #'+ (mapcar #'* frac-monzo val))
    ))
#|
;; The fourth in 12-EDO, which has the val '(12 19 28)
(ratio-to-1D-temperament-degree 4/3 '(12 19 28))
=> 5

;; The major third in 12-EDO
(ratio-to-1D-temperament-degree 5/4 '(12 19 28))
=> 4

;; The syntonic comma in that EDO is tempered out
(ratio-to-1D-temperament-degree 81/80 '(12 19 28))
=> 0

;; Septimal intervals are undefined in this val
(ratio-to-1D-temperament-degree 7/4 '(12 19 28))

? TODO: add to doc notes on how to use this function for a single dimension of arbitrary regular temperaments?
So far, we only used this function on equal temperaments. We can also use it for individual 'dimensions' (generators) of arbitrary regular temperaments.  

In that case, we would be interested in how often some 

(e.g., number of steps in an ET -- not just EDO, or the factor for some regular temperament generator)



(RATIO-TO-1D-TEMPERAMENT-DEGREE 1/2 '(0 1 4))
|#


;; TODO: rename (cent is not generic enough here) and update doc -- this works for 
;; TODO: Add support for ups and down accidentals (simply additional param that is added to step)
;; generator and result currently always measured in cents
;; TODO: Generalise for JI generators that are not measured in cents, but are rationals
(defun ratio-to-1D-temperament-cents (frac val step-generator)
  "Return the pitch of an interval (measured in cents) that corresponds to the JI interval ratio `frac' in an equal temperament (not just EDOs) defined by the given `val'  and the generator for a step in that equal temperament (also measured in cents)."
  (* (ratio-to-1D-temperament-degree frac val) step-generator))

#|
;; 12 EDO
;; Float generators are measured in cent
(ratio-to-1D-temperament-cents 3/2 '(12 19 28) 100.0)
=> 700.0

(ratio-to-1D-temperament-cents 6/5 '(12 19 28) 100.0)
=> 300

;; 24-EDO
(ratio-to-1D-temperament-cents 3/2 '(24 38 56) 50.0)
=> 700.0

;; 22-EDO
;; https://en.xen.wiki/w/22edo
;; NOTE: Val mapping computed myself simply by counting sizes of intervals in tuning table that match the freq ratios 2, 3 and 5 
(ratio-to-1D-temperament-cents 3/2 '(22 35 51) 54.545)
=> 709.08496 
At xen.wiki: 709.091; error due to rounding error of generator

;; Without that rounding error by calculating the generator directly -- result here now matches xen.wiki result
(ratio-to-1D-temperament-cents 3/2 '(22 35 51) (/ 1200.0 22))
=> 709.09094 

(ratio-to-1D-temperament-cents 6/5 '(22 35 51) 54.545)
=> 327.27
At xen.wiki: 327.273

;; 31-EDO
;; https://en.xen.wiki/w/31edo
(ratio-to-1D-temperament-cents 3/2 '(31 49 72) 38.71)
=> 696.77997
At xen.wiki: 696.77

(ratio-to-1D-temperament-cents 6/5 '(31 49 72) 38.71)
=> 309.68
At xen.wiki: 309.68

;; Bohlen–Pierce scale
;; https://en.wikipedia.org/wiki/Bohlen–Pierce_scale
;; NOTE: this scale does not really contain an octave, but for the mapping we assign an interval that is somewhat close (but >29 cents off)
(ratio-to-1D-temperament-cents 3/2 '(8 13 19) 146.30)
=> 731.5
Wikipedia: 731.52

(ratio-to-1D-temperament-cents 6/5 '(8 13 19) 146.30)
=> 292.6
Wikipedia: 292.61
|#
  


;; TODO: Finish function doc
;; TODO: Better rename ratio-to-1D-temperament-degree for more clarity: val here not measured as step sizes (as for ET), but as exponents for JI dimensions 2, 3 and 5. Therefore, result is also exponents for the corresponding generators.
(defun ratio-to-regular-temperament-degrees (frac vals)
  "Return an interval in an arbitrary regular temperament specified by a list of its `vals' that corresponds to the JI interval ratio `frac'. The resulting interval is specified as a list of factors for the generators of the temperament measured in cent (or as exponents for generators that are frequency ratios).

* Arguments:
  - frac (rational): a JI interval for which we want to get a tempered interval
  - vals (list of list of ints): the list of vals that specify how the temperament how JI pitches are mapped to the temperament. See also https://en.xen.wiki/w/Keenan%27s_explanation_of_vals and https://en.xen.wiki/w/Val

* Examples:

We can specify arbitrary regular temperaments by their generators. The vals then specify how JI pitches are mapped into this temperament. This is done by stating how often each generator must be repeated to reach a tempered tone that should serve as the prime frequency ratios 2, 3, 5... up to the desired prime limit. 

Meantone temperament is specified by two generators, one specifying the size of the octave and one that of the fifth (the second generator can also be an octave plus a fifth, but then the vals are slightly different). As meantone has two generators, we must specify two vals, one for each of them. The first value in each val states how often its generator must be repeated to reach the frequency ratio 2, i.e., the octave. For reaching the octave we need 1 octave (generator 1) and 0 fifths (generator 2), so the first value in the first val is 1 and the first in the second val is 0. The second value in each val states how often its generator must be repeated to reach the frequency ratio 3, i.e., the octave plus a fifth. Obviously, we need one octave (second value of val 1) and one fifth (second value of val 2). The third (and for 5-limit meantone the last) val values state how to reach the prime frequency ratio 5 (two octaves and a major third). We can reach this interval by stacking 5 fifths and no additional octave is needed. So, the third value for the val 1 is 0 and for val 2 is 4. So, the vals for meantone are as follows. 

;;; (setf 5-limit-meantone-vals '((1 1 0) (0 1 4)))

With this 5-limit val, we can now obtain the meantone intervals measured in repetitions of generators for arbitrary 5-limit JI intervals specified by ratios. For example, to reach a fourth (ratio 4/3), we need to go one octave up and one fifth down. So, the first generator is repeated once upwards, and the second once downwards (specified by a negative count).

(ratio-to-regular-temperament-degrees 4/3 5-limit-meantone-vals)
=> (1 -1)

A minor third (6/5) is reached by 2 octaves up and 3 fifths down.
;;; (ratio-to-regular-temperament-degrees 6/5 5-limit-meantone-vals)
=> (2 -3)
"
  (loop for val in vals
     collect (ratio-to-1D-temperament-degree frac val)))
#|
Find temperaments:
https://en.xen.wiki/w/Tour_of_Regular_Temperaments
http://x31eq.com/temper/net.html

;; 5-limit meantone: https://en.xen.wiki/w/Meantone_family
;; Mapping generators: ~2, ~3
(setf 5-limit-meantone-vals '((1 0 -4) (0 1 4)))


(ratio-to-regular-temperament-degrees 3/2 5-limit-meantone-vals)
=> (-1 1)

(ratio-to-regular-temperament-degrees 5/4 5-limit-meantone-vals)
=> (-6 4) 

(ratio-to-regular-temperament-degrees 9/8 5-limit-meantone-vals)
=> (-3 2)

(ratio-to-regular-temperament-degrees 81/80 5-limit-meantone-vals)
=> (0 0)

(ratio-to-regular-temperament-degrees 1/2 5-limit-meantone-vals)



https://en.xen.wiki/w/Regular_temperament#Normal_val_list
;; 7-limit miracle
(ratio-to-regular-temperament-degrees 16/15 '(1 1 3 3) '(0 6 -7 -2))
=> (0 1) ;; 0 steps for generator and 1 for period
|#


(defun regular-temperament-degrees-to-ratio (degrees &optional (generator-ratios +primes+))
  "Translate given temperament `degrees' into a corresponding JI ratio. `generator-ratios' is the list of primes that correspond to the generators for which the given degrees would be fitting. Does not temper out any commas and gives preference to ratios with a lower limit (e.g., Pythagorean third instead of just third)."
  (apply #'*
	 ;; Simply uses only as many primes as needed
	 (mapcar #'expt generator-ratios degrees)))
#|
(regular-temperament-degrees-to-ratio '(-1 1))
=> 3/2

;; Just maj third here interpreted as Pythagorean maj. third, because smaller ratios are given preference (and the temperament here tempers out syntonic comma?)
(regular-temperament-degrees-to-ratio 
 (ratio-to-regular-temperament-degrees 5/4 5-limit-meantone-vals))
=> 81/64

;; Just min. third interpreted as Pythagorean min. third
(regular-temperament-degrees-to-ratio 
 (ratio-to-regular-temperament-degrees 6/5 5-limit-meantone-vals))
=> 32/27
|#




;; TODO: Do this for JI (with high limits), where JI intervals are then translated into cent.
;; TODO: How to use this function for temperaments where the generators are representing different
;;  intervals (not prime freq ratios 2 and 3) -- the val then simply needs to be specified in terms
;;  of those intervals? Try it out!
;; ? TODO: revise function name
;; generators and result currently always measured in cents
;; (defun ratio-to-regular-temperament-cents (frac vals generators)
;;   (let ((degrees (ratio-to-regular-temperament-degrees frac vals)))
;;     (apply #'+ (mapcar #'* degrees generators))))

;; Does the same as commented def above  
(defun ratio-to-regular-temperament-cents (frac vals generators)
  "[main function in background] Translate a JI interval (ratio) into the corresponding interval measured in cents of a regular temperament. The temperament is specified by its `vals' and `generators'.

* Arguments:
  - frac (rational): a JI interval for which we want to get a tempered interval
  - vals (list of list of ints): the list of vals that specify how the temperament how JI pitches are mapped to the temperament. See also https://en.xen.wiki/w/Keenan%27s_explanation_of_vals and https://en.xen.wiki/w/Val
  - generators (list of numbers): the list of generators that specify the temperament.  

The order of vals and generators must match (i.e. the val of a certain generator must be at the same position).

See the documentation of `deftemperament' and also `ratio-to-regular-temperament-degrees' for more information.
"
  (apply #'+ (mapcar (lambda (val gen)
		       (ratio-to-1D-temperament-cents frac val gen))
		     vals generators)))


(defparameter *temperaments* (make-hash-table)
  "A hashtable filled with function store-temperament and accessed by get-temperament-vals and get-temperament-generators.")

(defun store-temperament (name vals generators)
  "Store a temperament with the given `vals' and `generators' under the key the `name' in `*temperaments*'."
  (setf (gethash name *temperaments*)
	(list :vals vals :generators generators)))

(defun get-all-temperament-names ()
  "Return list of names (symbols) of all temperaments so far defined with `deftemperament'.

You can jump to the definition of a temperament, by jumping to the definition of a function/method with the name of a temperament."
  (alexandria:hash-table-keys *temperaments*))
#|
(get-all-temperament-names)

;; Try jumping to the temperament definition. :)
(11-LIMIT-POTE-PAJARA 3/2)
(5-LIMIT-1/4-COMMA-MEANTONE 3/2)
|#

(defun get-temperament-vals (name)
  "Return vals of a temperament with `name' defined with `deftemperament'."
   (getf (gethash name *temperaments*) :vals))

(defun get-temperament-generators (name)
  "Return generators of a temperament with `name' defined with `deftemperament'."
   (getf (gethash name *temperaments*) :generators))


(defparameter *current-temperament*
  '31-limit-JI
  ;; '31-limit-12-EDO
  ;; '23-limit-31-EDO
  ;; '31-limit-72-EDO
  "Currently set global temperament (e.g., used by my snippet audition from within Emacs).")


;; TODO: Consider customising indent of user-defined macros -- all except first param should be uniformly indented here
(deftemperament 11-limit-12-EDO
    (list (edo-val 12 '(0 7 4 10 6)))
  '(100.0)
  "12-EDO temperament with an 11-limit mapping.")

(deftemperament 31-limit-12-EDO
    ;; Mapping based on table at https://en.wikipedia.org/wiki/Harmonic_series_(music)#Harmonics_and_tuning
    (list (edo-val 12 '(0 7
			;; 5 and 7
			4 10 
			;; 11 and 13
			6 8
			;; 17-31
			1 3 6 10 11
			)))
  '(100.0)
  "12-EDO temperament with a 31-limit mapping. This is not really intended for performing 31-limit music, but is used in `omn-to-tunings' to compute tuning values relative to 12-EDO. It also happens to be the default temperament for `def-tempered-score', so that arbitrary accidentals (up to limit 31) are supported for a default 12-EDO playback.")

;; (documentation '11-limit-12-EDO 'method-combination) ;; not yet 
#|
(11-limit-12-EDO 1/2)

(11-limit-12-EDO 3/2)

(11-limit-12-EDO 5/6)

(11-limit-12-EDO 7/4)

(11-limit-12-EDO 7/6)

(11-limit-12-EDO 11/8)

;; Tempered out commas
(11-limit-12-EDO 81/80)
;; NOTE: Same commas as tempered out by Pajara, so 12-EDO is also (subset of) a Pajara temperament? Very handy for using 12-EDO software like Synfire to compose music that can be retuned, and with all intervals behaving consistently. But that syntonic comma is also tempered out might be a problem...
;; Tempered out 50/49 means 7/5 and 10/7 are identical and the octave is divided in two. See https://en.xen.wiki/w/Jubilismic_clan
(11-limit-12-EDO 50/49)
;; septimal comma, 7K. Equates 9/8 and 8/7 if tempered out. https://en.xen.wiki/w/64/63 
(11-limit-12-EDO 64/63)


;; Exceed limit
(11-limit-12-EDO 13/8)
|#


;; 5-limit meantone: https://en.xen.wiki/w/Meantone_family
;; Mapping generators: ~2, ~3 (see below for an example mapping ~2 and ~3/2 instead)
(deftemperament 5-limit-1/4-comma-meantone
    ;; 5-limit meantone: https://en.xen.wiki/w/Meantone_family
    ;; Mapping generators: ~2, ~3
    '((1 0 -4) (0 1 4))
  ;; quartercomma meantone
  ;; https://en.xen.wiki/w/Quarter-comma_meantone
  ;; Vals and generators correspond to prime freq ratios 2 and 3 (not 2 and 3/2)
  (list 1200.0 (+ 696.578 1200.0))
  "Quarter-comma meantone temperament with a 5-limit mapping. Generators are ratios 2 and 3.")

#|
(get-temperament-vals '5-limit-1/4-comma-meantone)
=> ((1 0 -4) (0 1 4))

(5-limit-1/4-comma-meantone 1/2)
=> -1200.0

(5-limit-1/4-comma-meantone 3/2)
=> 696.578

(5-limit-1/4-comma-meantone 4/3)
=> 503.422

(5-limit-1/4-comma-meantone 5/4)
=> 386.312

(5-limit-1/4-comma-meantone 6/5)
=> 310.2661

;; Syntonic comma tempered out
(5-limit-1/4-comma-meantone 81/80)
=> 0.0

;; Exception: exceeds limit
(5-limit-1/4-comma-meantone 7/4)
|#


;; 5-limit meantone with slightly different vals and generators: this def maps how combination of
;; generators for freq ratios 2 and 3/2 can form ratios ~2 and ~3
(deftemperament 5-limit-1/4-comma-meantone-2
    '((1 1 0) (0 1 4))
  '(1200.0 696.578)
  "Quarter-comma meantone temperament with a 5-limit mapping. Generators are ratios 2 and 3/2. From a user's perspective the same as `5-limit-1/4-comma-meantone', only the definitions differ for documentation purposes to demonstrate how `deftemperament' works.")

#|
(5-limit-1/4-comma-meantone-2 1/2)
=> -1200.0

(5-limit-1/4-comma-meantone-2 4/3)
=> 503.422

(5-limit-1/4-comma-meantone-2 6/5)
=> 310.2661
|#


;; ;; translate primes into factors for edo
;; (loop for prime in +primes+
;;    collect (list (round (log (/ prime (ji-pc-ratio prime)) 2))
;; 		 prime))

(deftemperament 23-limit-31-EDO
    (list (edo-val 31 '(0 18 10 25 14 22 3 8 16)))
  (list (/ 1200.0 31))
  "31-EDO with a 23-limit mapping. 
See https://en.xen.wiki/w/31edo.")

#|
(23-limit-31-EDO 1/2)
=> -1200.0

(23-limit-31-EDO 3/2)

(23-limit-31-EDO 4/3)

(23-limit-31-EDO 5/4)

(23-limit-31-EDO 6/5)

;; Syntonic comma
(23-limit-31-EDO 81/80)

(23-limit-31-EDO 7/4)

(23-limit-31-EDO 7/6)

(23-limit-31-EDO 11/8)

(23-limit-31-EDO 11/8)

(23-limit-31-EDO 11/8)

(23-limit-31-EDO 11/8)

(23-limit-31-EDO 13/8)

(23-limit-31-EDO 17/16)

(23-limit-31-EDO 19/16)

(23-limit-31-EDO 23/16)

;; Exceed limit
(23-limit-31-EDO 29/16)
|#


(deftemperament 31-limit-72-EDO
    (list (edo-val 72
		   '(0 42 23 58 33 50 6 18 38 62 69)))
  (list (/ 1200.0 72))
  "72-EDO with a 31-limit mapping. 
See https://en.xen.wiki/w/72edo.")

#|
(31-limit-72-EDO 1/2)
=> -1200.0

(31-limit-72-EDO 3/2)

(31-limit-72-EDO 4/3)

(31-limit-72-EDO 5/4)

(31-limit-72-EDO 6/5)

(31-limit-72-EDO 7/4)

(31-limit-72-EDO 7/6)

(31-limit-72-EDO 11/8)

(31-limit-72-EDO 13/8)

(31-limit-72-EDO 17/16)

(31-limit-72-EDO 19/16)

(31-limit-72-EDO 23/16)

;; Commas: see https://en.xen.wiki/w/72edo#Commas
;; Pythagorean comma
(31-limit-72-EDO (monzo-to-ratio '(-19 12)))

;; Commas NOT tempered out
;; Syntonic comma
(31-limit-72-EDO 81/80)

|#


(deftemperament 17-limit-22-EDO
    (list (edo-val 22 '(0 13 7 18 10 15 2)))
  (list (/ 1200.0 22))
  "22-EDO with an 17-limit mapping (13-limit has a rather high error). 
See https://en.xen.wiki/w/22edo.")


#|
(17-limit-22-EDO 1/2)
=> -1200.0

(17-limit-22-EDO 3/2)

(17-limit-22-EDO 4/3)

(17-limit-22-EDO 5/4)

(17-limit-22-EDO 6/5)

(17-limit-22-EDO 7/4)

(17-limit-22-EDO 7/6)

(17-limit-22-EDO 11/8)

(17-limit-22-EDO 13/8)

(17-limit-22-EDO 17/16)


;; Commas tempered out
;; For more commas see https://en.xen.wiki/w/22edo#Commas
(17-limit-22-EDO 50/49)
(17-limit-22-EDO 64/63)

;; Commas not tempered out:
;; Syntonic comma
(17-limit-22-EDO 81/80)

;; Exceed limit
(17-limit-22-EDO 19/16)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 31-limit JI 
;;;

;; https://rosettacode.org/wiki/Identity_matrix#Common_Lisp
(defun identity-matrix (n)
  (loop for a from 1 to n
     collect (loop for e from 1 to n 
		if (= a e) collect 1
		else collect 0)))
; (identity-matrix 5)

  
(deftemperament 31-limit-JI
    (identity-matrix (length +limit-31-primes+))
  (mapcar #'ratio-to-cent +limit-31-primes+)
  "Just intonation 'temperament' supported up to 31-limit JI.")
#|
(31-limit-JI 1/2)
=> -1200.0

(31-limit-JI 3/2)

(31-limit-JI 4/3)

(31-limit-JI 5/4)

(31-limit-JI 6/5)

;; Syntonic comma
(31-limit-JI 81/80)

(31-limit-JI 7/4)

(31-limit-JI 7/6)

(31-limit-JI 11/8)

(31-limit-JI 31/16)

;; Exceed limit
(31-limit-JI 37/8)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Schismatic
;;;

;; https://en.xen.wiki/w/Schismatic_family
;; POTE generator: ~3/2 = 701.736
;; Mapping generator: ~3
;; Map: [⟨1 0 15], ⟨0 1 -8]]

;; TODO: Many variants exist in this family that often also support higher limits (11 or 13 limits)

(deftemperament 5-limit-POTE-schismatic
    '((1 0 15)
      (0 1 -8))
  (list 1200.0 (+ 1200.0 701.736))
  "A 5-limit version of schismatic temperament (aka Helmholtz temperament) with POTE generators. See https://en.xen.wiki/w/Schismatic_family#Schismatic_aka_Helmholtz")

#|
(5-limit-POTE-schismatic 1/2)
=> -1200.0

(5-limit-POTE-schismatic 3/2)

(5-limit-POTE-schismatic 4/3)

(5-limit-POTE-schismatic 5/4)

(5-limit-POTE-schismatic 6/5)

;; Syntonic comma: not tempered out
(5-limit-POTE-schismatic 81/80)

;; Tempered out
;; schisma of [-15 8 1⟩ = 32805/32768, which is the amount by which the Pythagorean comma exceeds the syntonic comma
(5-limit-POTE-schismatic 32805/32768)

;; Exceed limit
(5-limit-POTE-schismatic 7/4)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miracle
;;;

;; http://lumma.org/tuning/gws/miracle.htm
;; https://en.xen.wiki/w/Miracle
;; http://www.tonalsoft.com/enc/m/miracle.aspx
;; ?? https://en.xen.wiki/w/Tour_of_Regular_Temperaments#Schismatic_or_Layo_family_.28P8.2C_P5.29

;; I [hope] structure of scale can be made apparent in JI notation if I use "multiplicative" accidental symbols
;; like -5*7L

;; !! TODO: Embed chords of miracle in my chord list
;; https://en.xen.wiki/w/Chords_of_miracle


(deftemperament 11-limit-POTE-miracle
    ;; http://lumma.org/tuning/gws/miracle.htm
    '((1 1 3 3 2)
      (0 6 -7 -2 15))
  ;; https://en.xen.wiki/w/Miracle
  '(1200.0 116.63)
  "An 11-limit version of the miracle temperament with POTE generators. See http://www.tonalsoft.com/enc/m/miracle.aspx, https://en.xen.wiki/w/Miracle and http://lumma.org/tuning/gws/miracle.htm")

#|
(11-limit-POTE-miracle 1/2)
=> -1200.0

(11-limit-POTE-miracle 3/2)

(11-limit-POTE-miracle 4/3)

(11-limit-POTE-miracle 5/4)

(11-limit-POTE-miracle 6/5)

(11-limit-POTE-miracle 7/4)

(11-limit-POTE-miracle 7/6)

(11-limit-POTE-miracle 11/8)

;; Syntonic comma: not tempered out
(11-limit-POTE-miracle 81/80)

;; tempered out
;; ampersand
(11-limit-POTE-miracle 34171875/33554432)
(11-limit-POTE-miracle 225/224)
(11-limit-POTE-miracle 385/384)

;; Exceed limit
(11-limit-POTE-miracle 13/8)
|#


(deftemperament 11-limit-TOP-miracle
    ;; http://lumma.org/tuning/gws/miracle.htm
    '((1 1 3 3 2)
      (0 6 -7 -2 15))
  ;; Octave tempered (TOP)
  ;; http://lumma.org/tuning/gws/miracle.htm
  '(1200.631014 116.720642)
  "An 11-limit version of the miracle temperament with TOP generators. See http://www.tonalsoft.com/enc/m/miracle.aspx, https://en.xen.wiki/w/Miracle and http://lumma.org/tuning/gws/miracle.htm")

#|
(11-limit-TOP-miracle 1/2)
=> -1200.631014

(11-limit-TOP-miracle 3/2)

(11-limit-TOP-miracle 4/3)

(11-limit-TOP-miracle 5/4)

(11-limit-TOP-miracle 6/5)

(11-limit-TOP-miracle 7/4)

(11-limit-TOP-miracle 7/6)

(11-limit-TOP-miracle 11/8)

;; Syntonic comma: not tempered out
(11-limit-TOP-miracle 81/80)

;; tempered out
;; ampersand
(11-limit-TOP-miracle 34171875/33554432)
(11-limit-TOP-miracle 225/224)
(11-limit-TOP-miracle 385/384)

;; Exceed limit
(11-limit-TOP-miracle 13/8)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pajara
;;;

;; https://en.xen.wiki/w/Diaschismic_family
;; https://en.xen.wiki/w/Pajara

(deftemperament 7-limit-POTE-pajara
    ;; https://en.xen.wiki/w/Diaschismic_family#Pajara
    '((2 0 11 12)
      (0 1 -2 -2))
  ;; POTE generator
  (list 600.0 (+ 1200.0 707.048))
  "A 7-limit version of the pajara temperament with POTE generators. See https://en.xen.wiki/w/Diaschismic_family#Pajara and https://en.xen.wiki/w/Pajara")

#|
(7-limit-POTE-pajara 1/2)
=> -1200.0

(7-limit-POTE-pajara 3/2)

(7-limit-POTE-pajara 4/3)

(7-limit-POTE-pajara 5/4)

(7-limit-POTE-pajara 6/5)

(7-limit-POTE-pajara 7/4)

(7-limit-POTE-pajara 7/6)

;; Syntonic comma: not tempered out
(7-limit-POTE-pajara 81/80)

;; tempered out
(7-limit-POTE-pajara 50/49)
(7-limit-POTE-pajara 64/63)

;; Exceed limit
(7-limit-POTE-pajara 11/8)
(7-limit-POTE-pajara 13/8)
|#



(deftemperament 11-limit-POTE-pajara
    ;; https://en.xen.wiki/w/Diaschismic_family#Pajara
    '((2 0 11 12 26)
      (0 1 -2 -2 -6))
  ;; POTE generator
  (list 600.0 (+ 1200.0 706.885))
  "An 11-limit version of the pajara temperament with POTE generators. See https://en.xen.wiki/w/Diaschismic_family#Pajara and https://en.xen.wiki/w/Pajara")

#|
(11-limit-POTE-pajara 1/2)
=> -1200.0

(11-limit-POTE-pajara 3/2)

(11-limit-POTE-pajara 4/3)

(11-limit-POTE-pajara 5/4)

(11-limit-POTE-pajara 6/5)

(11-limit-POTE-pajara 7/4)

(11-limit-POTE-pajara 7/6)

(11-limit-POTE-pajara 11/8)

;; Syntonic comma: not tempered out
(11-limit-POTE-pajara 81/80)

;; tempered out
(11-limit-POTE-pajara 50/49)
(11-limit-POTE-pajara 64/63)
(11-limit-POTE-pajara 99/98)

;; Exceed limit
(11-limit-POTE-pajara 13/8)
|#

#|
;; The uneven step sizes of pajara
(ta-utils:x->dx
 (sort (mapcar (lambda (pitch) 
		 (mod pitch 1200.0))
	       (temperament-degrees (get-temperament-generators '11-limit-POTE-pajara)
		 '(2 11) :mode :up-and-down))
       #'<))
|#

#|
(pprint-linear *standard-output*  ;; each tone's info on a separate line
; (pprint
 (mapcar (lambda (data)
	   ; (subseq data 0 2)
	   data
	   )
	 (temperament-pcs-infos 
	  '11-limit-POTE-pajara
	  '(2 22) 
	  '(3/2 5/4 7/4 11/8)
	  '(12 3 1 3)  ;; 7-limit comma tempered out, so ignore it
	  (lambda (ratio _)
	    (list (ratio-to-OMN ratio) ; (ratio-to-ji-pc-symbols ratio)
		  ratio)))))
;; In pajara, 5K and 11K are both tempered to 1 step, but the step sizes differ
(11-limit-POTE-pajara (ji-pc-symbols-to-ratio 'C '5K))
=> 41.310547
(11-limit-POTE-pajara (ji-pc-symbols-to-ratio 'C '11K))
=> 65.575195

;; To OMN notation:
(ta-utils:inner-flat
 (mapcar #'rest
	 (temperament-pcs-infos 
	  '11-limit-POTE-pajara
	  '(2 11) 
	  '(3/2 5/4 7/4 11/8)
	  '(12 3 1 3)  ;; 7-limit comma tempered out, so ignore it
	  (lambda (ratio _)
	    (ratio-to-OMN ratio)))))

;; Decatonic scale: two chains of fifths, one tritone away from each other
;; https://sites.google.com/site/22tonewiki/example-page
(ta-utils:inner-flat
 (mapcar #'rest
	 (temperament-pcs-infos 
	  '11-limit-POTE-pajara
	  '(2 5) 
	  '(3/2 5/4 7/4 11/8)
	  '(12 3 1 3)  ;; 7-limit comma tempered out, so ignore it
	  (lambda (ratio _)
	    (ratio-to-OMN ratio)))))

;; (Mode of) Symmetric decatonic scale intervals: ssLssssLss
(ta-utils:x->dx
 (append 
  (sort (loop for degree in (temperament-degrees
			     (get-temperament-generators '11-limit-POTE-pajara)
			     '(2 5))
	   collect (mod degree 1200.0))
	#'<)
  '(1200.0)))
=> (106.88501 106.88501 172.45996 106.88501 106.88501 106.88501 106.88501 172.45996 106.88501 106.88501)
|#


(deftemperament 13-limit-POTE-pajara
    ;; https://en.xen.wiki/w/Diaschismic_family#Pajara
    '((2 0 11 12 26 1)
      (0 1 -2 -2 -6 2))
  ;; POTE generator
  (list 600.0 (+ 1200.0 708.919))
  "A 13-limit version of the pajara temperament with POTE generators. See https://en.xen.wiki/w/Diaschismic_family#Pajara and https://en.xen.wiki/w/Pajara")

#|
(13-limit-POTE-pajara 1/2)
=> -1200.0

(13-limit-POTE-pajara 3/2)

(13-limit-POTE-pajara 4/3)

(13-limit-POTE-pajara 5/4)

(13-limit-POTE-pajara 6/5)

(13-limit-POTE-pajara 7/4)

(13-limit-POTE-pajara 7/6)

(13-limit-POTE-pajara 11/8)

(13-limit-POTE-pajara 13/8)

;; Syntonic comma: not tempered out
(13-limit-POTE-pajara 81/80)

;; tempered out
(13-limit-POTE-pajara 50/49)
(13-limit-POTE-pajara 64/63)
(13-limit-POTE-pajara 65/63)
(13-limit-POTE-pajara 99/98)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Collecting tones (ratios and notation) suitable for notating a temperament
;;

(defun temperament-degrees (generators ns &key (mode :up-and-down))
  "Return list of degrees of the temperament specified by `generators'.

* Arguments:
 - generators (list of rationals or list of floats): List of temperament generators. If rationals, generators are assumed to be frequency ratios, otherwise they are assumed to be measured in cent.
 - ns (list of its): Specifies how often the generators should be repeated. Each n specifies the number the corresponding generator (at the same position) is repeated. 
 - mode (either :up-and-down or :up): Whether generators are repeated upwards and downwards or only upwards. 

* Examples

Using just the octave (2) as a generator, repeating that generator up and down until in total 7 frequency ratios are generated.
;;; (temperament-degrees '(2) '(7))
=> (1/8 1/4 1/2 1 2 4 8)

Generating a single octave of 12-EDO by repeating its generator (100.0 cent) to result in 12 tones.
;;; (temperament-degrees '(100.0) '(12))
=> (-600.0 -500.0 -400.0 -300.0 -200.0 -100.0 0.0 100.0 200.0 300.0 400.0 500.0)

Collect the ratios of a 5-limit JI lattice without the octave, so that there is a sequence of 12 tones in the dimension of fifths and 3 tones in the dimension of thirds.
;;; (temperament-degrees '(3/2 5/4) '(12 3))

Repeating generators only upwards.
;;; (temperament-degrees '(100.0) '(12) :mode :up)
=> (0.0 100.0 200.0 300.0 400.0 500.0 600.0 700.0 800.0 900.0 1000.0 1100.0)
"
  (let* ((is-ji? (rationalp (first generators)))
	 (combi-fn (if is-ji?
		       #'*
		       #'+))
	 (accum-fn (if is-ji?
		       #'expt
		       #'*)))
    ;; Turn factor sublists into dimensions by multiplying each with each
    (apply #'tu:cartesian-product
	   combi-fn
	   (mapcar (lambda (frac size)
		     (let* ((size/2 (/ size 2))
			    (size-down (case mode
					 (:up-and-down (floor size/2))
					 (:up 0)))
			    (size-up (case mode
				       (:up-and-down (ceiling size/2))
				       (:up size))))
		       (append ;; TODO: optimise: append inefficient here
			(loop for i from size-down downto 1
			   collect (funcall accum-fn frac (- i)))
			(loop for i from 0 upto (1- size-up)
			   collect (funcall accum-fn frac i))
			)))
		   generators ns))))

(defun has-lower-prime-limit? (ratio1 ratio2)
  "[For temperament-pcs-infos arg sort-ratios] Given two ratios, return T if the first ratio has a lower prime limit."
  (let ((monzo1 (ratio-to-monzo ratio1))
	(monzo2 (ratio-to-monzo ratio2)))
    (<= (length monzo1)
	(length monzo2))))

;; ? TODO: Add tie break (e.g., in case of equal number of accidentals prefer lower prime limit and then prefer raising over lowering accidental?)
(defun has-fewer-accidentals? (ratio1 ratio2)
  "[For temperament-pcs-infos arg sort-ratios] Given two ratios, return T if the first ratio needs fewer accidentals for notating it."
  (let ((pc-symbols1 (ratio-to-ji-pc-symbols ratio1))
	(pc-symbols2 (ratio-to-ji-pc-symbols ratio2)))
    (<= (length pc-symbols1)
	(length pc-symbols2))))


;; BUG: Currently only working for octave-repeating temperaments. So, even TOP temperaments likely do not work either
;; !! TODO: Sort collected ratios by some complexity measure (e.g., their limit or the number of accidentals needed)
(defun temperament-pcs-infos (temperament generator-ns limit-ratios ratio-ns fn
				      &key ; (sorted-degrees? T)
					(ratios-comparison #'has-fewer-accidentals?))
  "Return nested list where each sublist starts with a temperament degree measured in cents (as returned by `temperament-degrees') and the remaining values of each sublist are results of the function `fn' given rationals that are tempered to this degree in the given temperament.

* Arguments:
 - temperament (symbol): A temperament defined by `deftemperament'.
 - generator-ns (list of ints): Specifies how often each generator (implicitly specified by the temperament symbol) should be repeated. Each n specifies the number the corresponding generator (at the same position) is repeated. 
 - limit-ratios (list of rationals): Specifies generators for creating a JI tuning that is then matched to the temperament.
 - ratio-ns (list of ints): Specifies how often the corresponding generator (at the same position) is repeated. 
 - fn (binary function): Function for generating further information on temperament tones. The function expects a JI rational and the corresponding temperament degree in cents.
 ;; - sorted-degrees? (Boolean): if T, the result is sorted in increasing order of degrees.
 - ratios-comparison (binary function): Function comparing two ratios that is used for sorting ratios per degree.
 
* Examples

TODO:

Collect list of (relatively simple) note names for temperament degrees

Compute tuning errors as difference between degrees and ratios turned into cents. 
"
  (let* ((temperament-generators (get-temperament-generators temperament))
	 (temperament-pc-degrees (sort (mapcar (lambda (degree) (mod degree 1200.0))
					       (temperament-degrees temperament-generators generator-ns :mode :up-and-down))
				       #'<))
	   ;; Initialise empty hash table
	 (temperament-table (alexandria:alist-hash-table
			     (loop for degree in temperament-pc-degrees
				collect (cons degree NIL))))
	 (JI-pc-ratios (mapcar #'ji-pc-ratio (temperament-degrees limit-ratios ratio-ns)))
	 )
    ;; (break)
    ;; Add ratio PCs to their matching degrees in temperament-table
    (loop
       for ratio in JI-pc-ratios
       for tempered-ratio = (funcall temperament ratio)
       ;; for 
       do (setf (gethash tempered-ratio temperament-table)
		(cons ratio (gethash tempered-ratio temperament-table))))
    (loop for tempered-ratio in (sort (alexandria:hash-table-keys temperament-table)
				      #'<)
       do (when (not (member tempered-ratio temperament-pc-degrees))
	    (warn "No temperament degree at ~A cent for ratios: ~A" tempered-ratio (gethash tempered-ratio temperament-table))))
    ;; (break)
    (let ((result (loop
		     for degree in temperament-pc-degrees
		     for ratios = (sort (gethash degree temperament-table)
					ratios-comparison)
		     do (when (not ratios)
			  (warn "No ratios for temperament degree: ~A" degree))
		     collect (cons degree (mapcar (lambda (ratio)
						    (funcall fn ratio degree))
						  ratios)))))
      ;; (if sorted-degrees?
      ;; 	  (sort result #'< :key #'first)
      ;; 	  result)
      result)))
#|
(pprint
(temperament-pcs-infos 
 '11-limit-12-EDO
 '(12)
 '(3/2 5/4)
 '(12 3)
 (lambda (ratio _)
   (list (ratio-to-ji-pc-symbols ratio)
	 ratio))))

(pprint
(temperament-pcs-infos 
 '11-limit-POTE-miracle
 '(1 21)
 '(3/2 5/4 7/4 11/8)
 '(12 5 5 5)
 (lambda (ratio _)
   (list (ratio-to-ji-pc-symbols ratio)
	 ratio))))
|#

#|
;; TODO: Kann bald weg, veraltet
(defun collect-regular-temperament-tones-infos (temperament generator-ns limit-ratios ratio-ns fn)
  (let* (;; ? BUG: reduce intervals to intervals in an octave? Otherwise matching intervals are not found. Or the hashtable access (currently with gethash) needs to be made smarter by also checking for octaves.
	 (JI-ratios (temperament-degrees limit-ratios ratio-ns))
	 ;; BUG: there can be multiple ratios fitting a single temperament pitch, so mapping from tempered pitches to single ratios is wrong
	 (temperament-table (alexandria:alist-hash-table
			     (loop for ratio in JI-ratios
				collect (cons (funcall temperament ratio) ratio))))
	 (temperament-generators (get-temperament-generators temperament))
	 (temperament-degrees (sort (temperament-degrees temperament-generators generator-ns)
				    #'<)))
    (break)
    (loop for degree in temperament-degrees
	 collect (cons degree (funcall fn (gethash degree temperament-table)))))
  )
|#



#|
;; TODO: Kann bald weg, veraltet
(defun JI-lattice (ratios sizes &optional (fn #'*))
  ;; Turn factor sublists into dimensions by multiplying each with each
  (apply #'tu:cartesian-product
	 fn
	 (mapcar (lambda (frac size)
		   (let* ((size/2 (/ size 2))
			  (size-down (floor size/2))
			  (size-up (ceiling size/2)))
		     (append ;; TODO: optimise: append inefficient here
		      (loop for i from size-down downto 1
			 collect (expt frac (- i)))
		      (loop for i from 0 upto (1- size-up)
			 collect (expt frac i))
		      )))
		 ratios sizes)))
|#

#|
(JI-lattice '(2) '(7))

(JI-lattice '(3/2) '(12))

(JI-lattice '(3/2 5/4) '(12 3))

(sort (mapcar #'ji-pc-ratio
	      (JI-lattice '(3/2 5/4 7/4 11/8) '(12 5 3 3)))
      #'<)


;; all intervals of 12 EDO with their associated 5-limit fractions
;; TODO: group by equal tempered interval, and within group sort by some harmonic complexity
;; !! TODO: Turn into a separate function for analysing temperaments. Add further information like the error of the tempered interval compared to the JI interval.
(pprint
(mapcar (lambda (frac) 
	  (list 
	   (11-limit-12-EDO frac) 
	   (ratio-to-ji-pc-symbols frac)
	   frac
	   (ratio-to-monzo frac)))
	(sort (mapcar #'ji-pc-ratio
		      (JI-lattice '(3/2 5/4) '(24 5)))
	      #'<)))



(pprint
(mapcar (lambda (frac) 
	  (list 
	   (11-limit-TOP-miracle frac) 
	   (ratio-to-ji-pc-symbols frac)
	   frac
	   (ratio-to-monzo frac)))
	(sort (mapcar #'ji-pc-ratio
		      (JI-lattice '(3/2 5/4 7/4 11/8) '(24 7 7 7)))
	      #'<)))
|#




#|
;; TODO: Better collect monzos? Easy to convert either way...
(defun collect-temperament-ratios (vals generators sizes tempered-out-commas)
  'TODO
  )
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;

#|
;; ?!!! NOTE: make-regular-temperament is obsolete !!!

? TODO: The temperament mapping from JI to temperament pitches is defined separately.
? TODO: ? Better define a temperament class, where all this information is more cleanly wrapped in one place.
? TODO: Allow generators to be given as rationals and monzos (frequency ratios) or floats (measured in cent)
? TODO: ? monzos and vals represented by own classes

? TODO: I made up the param sizes here: find in regular temperament some info on how to obtain a suitable size for a temperament
? TODO: Another approach might be to define the temperament not by some stored array of tuning values, but by the function to compute the respective number(s). Somewhat less efficient, but more suitable for dynamic temperaments.


(defun make-1D-temperament (generator size)
  )


;; generators in cent
(defun make-regular-temperament (generators sizes)
  "Return an array that stores the pitches (pitch classes?) of a regular temperament, with one array dimension per given generator."
  (let ((temperament (make-array sizes :initial-element nil)))
    
    
    (mapcar (lambda (generator size) )
	    generators sizes)))

#|
;; 12 EDO
;; Float generators are measured in cent
(make-regular-temperament '(700.0) '(12))

;; Quarter-comma meantone
;; https://en.xen.wiki/w/Quarter-comma_meantone
(make-regular-temperament '(1200.0 696.578) '(5 12))

;; 24-EDO
(make-regular-temperament 50.0)

;; 22-EDO
;; https://en.xen.wiki/w/22edo
(make-regular-temperament 54.545)

;; 31-EDO
;; https://en.xen.wiki/w/31edo
(make-regular-temperament 696.77)

;; 5-limit JI
(make-regular-temperament 2 3/2 5/4)
|#

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mapping note names to JI
;;


;; TODO: octave 4 corresponds to JI 1/1 for now

(defconstant +nominals+ '(F C G D A E B)
  "Standard pitch nominals in order of fifth chain. These will be tuned Pythagorean in JI.")


(defconstant +nominal->ratio-table+
  (alexandria:alist-hash-table
   (loop
      for nom in +nominals+
      ;; Representing exponent for number of fifths
      ;; C corresponds to 1/1, so F is (expt 3/2 -1)
      for no-of-fifths from -1
      ;; alist
      collect (cons nom
		    (ji-pc-ratio (monzo-to-ratio (list 0 no-of-fifths))))))
  "Hashtable mapping pitch nominals (symbols like A, B, C...) to their corresponding Pythagorean frequency ratios.")


(defconstant +ratio->nominal-table+
  (flip-hash-table +nominal->ratio-table+)
  "Hashtable mapping Pythagorean frequency ratios for pitch nominals to their corresponding symbols (A, B, C...).")


(defparameter *accidental->ratio-table*
  (alexandria:alist-hash-table
   '((1K . 1/1)
     (s . 2187/2048)
     (b . 2048/2187)
     (5K . 81/80)
     (-5K . 80/81)
     (7K . 64/63)
     (-7K . 63/64)
     (11K . 33/32)
     (-11K . 32/33)
     (13K . 27/26)
     (-13K . 26/27)
     (17K . 256/255)
     (-17K . 255/256)
     (19K . 513/512)
     (-19K . 512/513)
     (23K . 736/729)
     (-23K . 729/736)
     (29K . 145/144)
     (-29K . 144/145)
     (31K . 1024/1023)
     (-31K . 1023/1024)
     ))
  "Hashtable mapping symbols representing accidentals to corresponding prime limit ratios and their combinations. Combinations are representing by their own symbol, so that for notating chords we only need a single accidental symbol per tone.

Notation:
 s and b are the Pythagorean sharp and flat accidentals
 K stands for prime limit 'c'omma (K as in the original Greek κόμμα, 'c' is already used for OMN attributes denoting cents)
 5K means prime limit 5 comma up
 -5K means prime limit 5 comma down
 1K is the natural accidental
 xx is used for combinations of the same symbol (two quasi multiplication signs read as power of frequency ratios)
 5Kxx3 means triple prime limit 5 comma up
 x combines accidentals (quasi multiplication sign read as factor of frequency ratios)
 -5Kx-7K means a limit 5 and additionally a limit 7 comma down
 5Kxx2x7K means two limit 5 commas and additionally a limit 7 comma up

Remember that these symbols do not distinguish between upper and lower case, case is just used for a (very little) bit of readability.

NOTE: Currently always only at most 2 prime limits are combined in a single accidental.

Source for the initial prime limit ratios: Nicholson, T. & Sabat, M. (2018) Fundamental Principles of Just Intonation and Microtonal Composition. p. 18")

;; (pprint (alexandria:hash-table-alist *accidental->ratio-table*))
;; (length (alexandria:hash-table-alist *accidental->ratio-table*))

(defun aux-add-power-accidentals (accidental-string orig-ratio i)
  (let ((power-accidental (intern (concatenate 'string
					       accidental-string "XX" (write-to-string i))))
	(power-ratio (expt orig-ratio i)))
    (setf (gethash power-accidental *accidental->ratio-table*)
	  power-ratio)))

(defun add-power-accidentals (accidental n)
  "For the given `accidental' symbol, add `n' accidentals that are powers of the corresponding frequency ratios. If `n' is two, accidental doubles are added, if it is tree, triple are added etc. These accidentals are notated with a double star for expressing the power relation. E.g., sxx3 is a triple sharp."
  (let ((accidental-str (symbol-name accidental))
	(ratio (gethash accidental *accidental->ratio-table*)))
    (loop for i from 2 to n
       do (aux-add-power-accidentals accidental-str ratio i))))

(defun equal-prime-limit? (ratio1 ratio2)
  "Return T if ratio1 or ratio2 share the same prime limit (of if either is 1)."
  (or (= ratio1 1)
      (= ratio2 1)
      (let ((monzo1 (ratio-to-monzo ratio1))
	    (monzo2 (ratio-to-monzo ratio2)))
	(= (monzo-limit monzo1)
	   (monzo-limit monzo2)))))


;; NOTE: Not optimised, please consider using cl-ppcre regular expressions and string processing library which is optimized
;; https://lispcookbook.github.io/cl-cookbook/strings.html
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurrences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
	 while pos)))

;; TODO: Very inefficient, optimise. This is pretty inefficient and called on very many accidentals 
(defun prettify-exported-accidental (accidental-string)
  (let* ((power-acc-mapping '(; ("1K" . "♮") ;; TODO: Find better sign for 1K, natural is misleading...
			      ("XX2". "²")
			      ("XX3". "³")
			      ("XX4". "⁴")
			      ("XX5". "⁵")
			      ("XX6". "⁶")
			      ("XX7". "⁷")
			      ("XX8". "⁸")
			      ("XX9". "⁹")))
	 (acc-with-powers (reduce (lambda (acc mapping)
				    (destructuring-bind (string-to-replace . replacement)
					mapping
				      (replace-all acc string-to-replace replacement)))
				  power-acc-mapping
				  :initial-value accidental-string)))
    (substitute #\× ; unicode multiplication sign
		#\X acc-with-powers)))
#|
(prettify-exported-accidental "5KXX2")
(prettify-exported-accidental "5KX7K")
(prettify-exported-accidental "5KXX2X7K")

(prettify-exported-accidental (symbol-name '5Kxx2x7K))
|#


(defun add-accidentals-as-text-attributes ()
  "Automatically define for all accidentals (keys of `*accidental->ratio-table*') corresponding OMN text attributes."
  (apply #'om:add-text-attributes
	 (loop for acc in (alexandria:hash-table-keys *accidental->ratio-table*)
	    ;; do (break)
	    collect (list acc
			  (prettify-exported-accidental (symbol-name acc))
			  :non-sticky))))

;; Initialisation
(add-accidentals-as-text-attributes)

;; ? TODO: combine not just pairs but additionally accidental triplets
(defun aux-add-accidental-combinations ()
  "Pairwise combine all accidentals into product accidentals (resulting in a huge accidental number overall)."
  (let ((accidentals-table-pairs
	 (loop for (accidental . ratio) in (alexandria:hash-table-alist *accidental->ratio-table*)
	    ;; using vectors, because cartesian-product does not support nested lists
	    Collect (vector accidental ratio))))
    (ta-utils:cartesian-product
     (lambda (acc-spec1 acc-spec2)
       (ta-utils:dbind ((accidental1 ratio1) (accidental2 ratio2)) ; destructure the vectors
	   (list acc-spec1 acc-spec2)
	 (when (not (equal-prime-limit? ratio1 ratio2))
	   (let ((combined-accidental (intern (concatenate 'string (symbol-name accidental1) "X" (symbol-name accidental2))))
		 (combined-ratio (* ratio1 ratio2)))
	     (setf (gethash combined-accidental *accidental->ratio-table*)
		   combined-ratio)))))
     accidentals-table-pairs
     accidentals-table-pairs)))

(defun aux-add-all-accidental-powers (specs)
  "Each spec is a pair (accidentals . n), where accidentals is a list of accidental symbols, and n is the number of power accidentals to add to them."
  (loop for (accidentals . n) in specs
     do (loop for acc in accidentals
	   do (add-power-accidentals acc n))))
  
;; Initialisation of these parameters, in case add-all-accidental-powers-and-accidental-combinations is not called
(defparameter *ratio->accidental-table*
  (flip-hash-table *accidental->ratio-table*)
  "Map prime limit ratios to corresponding symbols representing accidentals. The inverse map of *accidental->ratio-table*")
(defparameter *JI-accidentals*
  (alexandria:hash-table-keys *accidental->ratio-table*))

(defun add-all-accidental-powers-and-accidental-combinations (specs)
  "Each spec is a pair (accidentals . n), where accidentals is a list of accidental symbols, and n is the number of power accidentals to add to them."
  (aux-add-all-accidental-powers specs)
  (aux-add-accidental-combinations)
  (defparameter *ratio->accidental-table*
    (flip-hash-table *accidental->ratio-table*)
    "Map prime limit ratios to corresponding symbols representing accidentals. The inverse map of *accidental->ratio-table*")
  (defparameter *JI-accidentals*
    (alexandria:hash-table-keys *accidental->ratio-table*))
  (add-accidentals-as-text-attributes))

#|
;; Small extension of accidentals
(add-all-accidental-powers-and-accidental-combinations
 '(((s b
     5K -5K
     7K -7K) . 2)
   ;; ((11K -11K
   ;;   13K -13K
   ;;   17K -17K
   ;;   19K -19K
   ;;   23K -23K
   ;;   29K -29K
   ;;   31K -31K) . 0)
   ))
|#

;; Huge number of power accidentals added (e.g., up to 7k**7) to later help better expressing the
;; intervallic structure of scales etc. Many of these commas or comma combinations may be tempered
;; out in actual tunings, but they are part of the notation.
(add-all-accidental-powers-and-accidental-combinations
 '(((s b
     5K -5K
     7K -7K) . 7)
   ((11K -11K
     13K -13K
     17K -17K
     19K -19K
     23K -23K
     29K -29K
     31K -31K) . 2)
   ))



(defun set-JI-accidental (name ratio)
  "Can be used, e.g., for adding some custom interpretation of the OMN builtin accidentals like + and - or (for adding higher prime limits?)."
  (setf (gethash name *accidental->ratio-table*) ratio)
  (defparameter *ratio->accidental-table*
    (flip-hash-table *accidental->ratio-table*))
  (defparameter *JI-accidentals*
    (alexandria:hash-table-keys *accidental->ratio-table*))
  )


(defun is-JI-accidental (x)
  (member x *JI-accidentals*))


(defconstant +prime->comma-ratio-table+
  (alexandria:alist-hash-table
   `((3 . ,(expt 2187/2048 -1))
     ;; otonal comma goes down
     (5 . ,(expt 81/80 -1))
     (7 . ,(expt 64/63 -1))
     (11 . 33/32)
     (13 . ,(expt 27/26 -1))
     (17 . ,(expt 256/255 -1))
     (19 . 513/512)
     (23 . 736/729)
     (29 . 145/144)
     (31 . ,(expt 1024/1023 -1))
     )))

;; TODO: define version that handles JI pitches expressed by a single symbol (incl. composite accidentals)
(defun ji-pc-symbols-to-ratio (nominal &rest accidentals)
  (apply #'*
   (gethash nominal +nominal->ratio-table+)
   (loop for acc in accidentals
      collect (gethash acc *accidental->ratio-table*))))

#|
(ji-pc-symbols-to-ratio 'C)

(ji-pc-symbols-to-ratio 'E)

;; NOTE: lowering by syntonic comma, but notation confusing suggesting division (instead of multipication) by 5
(ji-pc-symbols-to-ratio 'E '-5K)

(ji-pc-symbols-to-ratio 'E 'b)

(ji-pc-symbols-to-ratio 'E 'b '5K)


(ji-pc-symbols-to-ratio 'B 'b '-7K)

(ji-pc-symbols-to-ratio 'E 'b '-7K)
|#


;; TODO: define version that returns OMN pitches + accidental attributes
;; (defun ratio-to-ji-pc-symbols (ratio)
;;   (let ((monzo (ratio-to-monzo ratio))
;; 	(nominal (alexandria:ensure-gethash ratio +ratio->nominal-table+ NIL)))
;;     (break)
;;     (if nominal
;; 	nominal
;; 	'(TODO))
;;     ))


;; ? TODO: Optimisation: make tail recursive
(defun aux-pythagorean-ratio-to-pc-symbols (limit-3-expt)
  (cond ((and (>= limit-3-expt -1) (<= limit-3-expt 5))
	 (list
	  (gethash (ji-pc-ratio (monzo-to-ratio (list 0 limit-3-expt)))
		   +ratio->nominal-table+)))
	((< limit-3-expt -1)
	 (append (aux-pythagorean-ratio-to-pc-symbols (+ limit-3-expt 7)) '(b)))
	((> limit-3-expt 5)
	 (append (aux-pythagorean-ratio-to-pc-symbols (- limit-3-expt 7)) '(s)))))

(defun pythagorean-ratio-to-pc-symbols (ratio)
  (let* ((monzo (ratio-to-monzo ratio))
	 (limit (monzo-limit monzo)))
    (if (<= limit 2)
	'(C)
	(progn 
	  (assert (<= limit 3))
	  (let ((limit-3-expt (second monzo)))
	    (aux-pythagorean-ratio-to-pc-symbols limit-3-expt))))))
#|
(pythagorean-ratio-to-pc-symbols 9/8)

(pythagorean-ratio-to-pc-symbols 16/9)
=> (B B)

(pythagorean-ratio-to-pc-symbols 2048/2187)
=> (C B)

(pythagorean-ratio-to-pc-symbols 2187/2048)
=> (C S) 

(pythagorean-ratio-to-pc-symbols (* 2187/2048 2187/2048))
=> (C S S)

(pythagorean-ratio-to-pc-symbols (* 9/8 2187/2048 2187/2048))
=> (D B B)

;; assert error
(pythagorean-ratio-to-pc-symbols 5/4)
|#

;; TODO: Define variant for proper OMN pitches. For that, have some new aux (most of this def) that only returns the JI accidentals beyond limit 3.
(defun ratio-to-higher-limit-ji-accidental-symbols (ratio)
  "[Aux for ratio-to-ji-pc-symbols etc] Return list of accidental symbols list beyond limit 5 that corresponds to ratio."
  (let* ((monzo (ratio-to-monzo ratio))
	 (accidentals
	  (loop
	     for exponent in monzo
	     for prime in +primes+
	     append (if (or (= prime 2)
			    (= prime 3))
			NIL 
			(let* ((single-comma-ratio (expt (gethash prime +prime->comma-ratio-table+)
							 ;; whether comma goes up or down
							 (signum exponent)))
			       (accidental (gethash single-comma-ratio *ratio->accidental-table*)))
			  (make-list (abs exponent) :initial-element accidental)))))
	 (factors (loop for acc in accidentals
		     collect (expt (gethash acc *accidental->ratio-table*) -1))))
    (values accidentals factors)))


(defun ratio-to-ji-pc-symbols (ratio)
  "Return PC symbol list that corresponds to ratio (i.e. octaves are ignored)."
  (multiple-value-bind (accidentals factors)
      (ratio-to-higher-limit-ji-accidental-symbols ratio)
    (append (pythagorean-ratio-to-pc-symbols (apply #'* ratio factors))
	    accidentals)))

#|
(ratio-to-ji-pc-symbols 1)
(ratio-to-ji-pc-symbols 4)

(ratio-to-ji-pc-symbols 9/8)

(ratio-to-ji-pc-symbols 5/4)

(ratio-to-ji-pc-symbols 5/3)

(ratio-to-ji-pc-symbols 16/9)

(ratio-to-ji-pc-symbols 7/4)

(ratio-to-ji-pc-symbols 6/5)

(ratio-to-ji-pc-symbols (* 5/4 5/4))

(ratio-to-ji-pc-symbols (* 5/4 5/4 7/4))

(ratio-to-ji-pc-symbols 2187/2048)
|#


(defparameter *1/1-octave* 4
  "Octave in which the frequency ratio 1/1 sounds.")

;; TODO:
(defun get-ratio-octave (ratio)
  ;; BUG: dummy
  *1/1-octave*)


;; !! BUG: Handle octaves properly
(defun ratio-to-OMN (ratio)
  "Return OMN pitch plus accidental attribute that corresponds to ratio (i.e. octaves are ignored)."
  (multiple-value-bind (accidentals factors)
      (ratio-to-higher-limit-ji-accidental-symbols ratio)
    (let* ((pythagorean-pc-symbols (pythagorean-ratio-to-pc-symbols (apply #'* ratio factors)))
	   (omn-pitch (intern (apply #'concatenate 'string
				     (mapcar #'write-to-string
					     (append pythagorean-pc-symbols (list (get-ratio-octave ratio)))))))
	   ;; Assume accidentals are sorted by limit
	   (unique-accidentals (remove-duplicates accidentals))
	   (accidental-power-strings (loop for unique-acc in unique-accidentals
					for acc-count = (count-if (lambda (acc) (eql acc unique-acc))
								  accidentals)
					collect  (if (> acc-count 1)
						     (concatenate 'string
								  (write-to-string unique-acc)
								  "XX"
								  (write-to-string acc-count))
						     (write-to-string unique-acc)))))
      (if accidental-power-strings
	  (list omn-pitch
		(intern 
		 (reduce #'(lambda (a1 a2) (format nil "~AX~A" a1 a2))
			 accidental-power-strings)))
	  (list omn-pitch)))))


#|
(ratio-to-OMN 1)
(ratio-to-OMN 4)

(ratio-to-OMN 9/8)

(ratio-to-OMN 5/4)

(ratio-to-OMN 5/3)

(ratio-to-OMN 16/9)

(ratio-to-OMN 7/4)

(ratio-to-OMN 6/5)

(ratio-to-OMN (* 5/4 5/4))

(ratio-to-OMN (* 5/4 5/4 7/4))

(ratio-to-OMN 2187/2048)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; !! BUG: accidentals ss and bb not supported
(defun parse-OMN-pitch (pitch)
  "Return a plist that contains the components of `pitch' (an OMN pitch symbol), each component as a string.

;;; (parse-OMN-pitch 'c4)
=> (:NOMINAL C :STANDARD-ACCIDENTAL NIL :OCTAVE 4 :MICROTONE-ACCIDENTAL ||)

;;; (parse-OMN-pitch 'cs4+)
=> (:NOMINAL C :STANDARD-ACCIDENTAL S :OCTAVE 4 :MICROTONE-ACCIDENTAL +)
"
  (if (pitchp pitch)
      (let* ((pitch-str (symbol-name pitch))
	     (octave (first (cl-utilities:split-sequence-if-not #'digit-char-p pitch-str
								:remove-empty-subseqs T)))
	     ;; pair (<pitch-class-str> <microtone-accidental-str>)
	     (pc+microtone-strings (cl-utilities:split-sequence-if #'digit-char-p pitch-str))
	     (pc-str (first pc+microtone-strings))
	     (nominal (string (char pc-str 0)))
	     (standard-accidental (if (> (length pc-str) 1)
				      (intern (string (char pc-str 1)))
				      NIL))
	     (microtone-accidental (second pc+microtone-strings)))
	`(:nominal ,(intern nominal) :standard-accidental ,standard-accidental
		   :octave ,(parse-integer octave) :microtone-accidental ,(intern microtone-accidental)))
      (error "Not a pitch")))

#|
(setf test-pitches '(c4 cs4 c4+ c4. cs4+ cs4+. css4 cb4+ cbb4))

(pprint
(mapcar #'parse-OMN-pitch 
	test-pitches))

(elt "C" 1)

(length "C")
|#


(defun get-JI-accidentals (event)
  (let ((attributes (om:disjoin-attributes (fourth event))))
    (remove-if-not #'is-JI-accidental attributes)))



;; TODO: Build-in OMN microtone notation not yet supported -- should be defined with the temperament what these mean
;; TODO: Define variant returning ji-ratio that takes the octave into account
(defun omn-event-to-ji-pc-ratio (event &key (use-JI-accidentals? T))
  (let* ((parsed-OMN-pitch (parse-OMN-pitch (second event)))
	 (JI-accidentals (when use-ji-accidentals?
			   (get-JI-accidentals event)))
	 (standard-accidental (getf parsed-OMN-pitch :standard-accidental))
	 (all-accidentals (append (when standard-accidental
				    (tu:ensure-list standard-accidental))
				  (when use-ji-accidentals?
				    JI-accidentals))))
    (apply #'ji-pc-symbols-to-ratio
	   (getf parsed-OMN-pitch :nominal)
	   all-accidentals)))
#|
(mapcar #'omn-event-to-ji-pc-ratio (single-events '(q c4 c4 -5K cs4)))
|#


(defun omn-event-to-pc-cents (temperament event &key (use-JI-accidentals? T))
  (funcall temperament (omn-event-to-ji-pc-ratio event :use-JI-accidentals? use-JI-accidentals?)))
#|
(mapcar (lambda (event) (omn-event-to-pc-cents #'5-limit-1/4-comma-meantone event))
	(single-events '(q c4 c4 -5K cs4 e4 e4 -5K)))
|#
  

(defun omn-to-tunings (temperament sequence)
  "Return a flat list of tuning values for the given `sequence'. The tuning values are floats (1.0 is 100 cents).

* Arguments:
 - sequence (list, possibly nested): OMN sequence with custom JI accidentals (as defined in *accidental->ratio-table*) as OMN attributes.
"
  (let* ((events (remove-if
		  ;; The returned tuning list should skip rests
		  #'event-restp (single-events
				 ;; The returned tuning list can be flat and processing of a flat
				 ;; list is more easy
				 (flatten sequence)))))
    (loop for event in events
       collect (/ (- (omn-event-to-pc-cents temperament event)
		     ;; Tuning relative to 12-EDO
		     ;; TODO: Optimise (e.g., memoize)
		     (omn-event-to-pc-cents #'31-limit-12-edo event :use-JI-accidentals? NIL))
		  ;; tuning measured in semitones, not cent
		  100))))
#|
(omn-to-tunings '5-limit-1/4-comma-meantone '(q c4 c4 -5K cs4 e4 e4 -5K))

(omn-to-tunings '11-limit-12-EDO '(q c4 c4 -5K cs4 e4 e4 -5K))
|#


;; (defun )


;; (defun aux-tune-track-with-multiple-channels (temperament instr-args)
;;   "Called at compile time"
;;   (let* ((instr (first score-part))
;; 	 (instr-args score-part))
;;     ))





