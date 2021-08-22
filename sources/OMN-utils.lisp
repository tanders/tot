;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OMN Utilities 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: allow to add additional information to sequence, like the metric weight of notes
(defun map-events (fn sequence &key (test #'(lambda (&rest args) (declare (ignore args)) T)) flat section exclude)
  "Every event for which the function `test' returns true is transformed by the function `fn'. In the background, sequence is transformed into a list of events, where each note is represented by a list of the parameters length, pitch, velocity, and articulation. 

  Rests are skipped unprocessed.

* Arguments:
  - fn: function expecting and returning a single event, i.e. the function expects the arguments length, pitch, velocity and articulation of individual elements. 
  - sequence: an OMN sequence
  - test: Boolean function expecting a single event. By default, all elements are processed. 
  - flat (Boolean): whether or not to flatten sequence before processing.
  - section (list of ints): 0-based positions of bars (sublists) in nested `sequence' to which `fn' is applied.
  - exclude (list of ints): 0-based positions of bars (sublists) in nested `sequence' to which `fn' is *not* applied. Only either `section' or `exclude' should be specified, otherwise `exclude' is ignored.

* Examples:

  Reduce all events with velocity p to velocity pp
;;; (map-events
;;;  #'(lambda (l p v a) (list l p 'pp a))
;;;  '((-e s bb3 f marc a3 leg g3 p leg gs3 leg g3 leg a3 leg) (q fs3 f ten -q))
;;;  :test #'(lambda (l p v a) (eql v 'p)))
"
  (let ((result 
	 (if (and (listp (first sequence)) (not flat))
	     ;; if first element in sequence is list assume it is nested and call recursively with map-section
	     (map-section #'(lambda (sublist) (map-events fn sublist :test test))
			  sequence :section section :exclude exclude)
	     ;; sequence is flat list. 
	     (loop for (l p v a) in (single-events (if flat ;; if only for efficiency
						       (flatten sequence)
						       sequence))
		append (remove nil ;; unset parameters in events are nil
			       (cond
				 ;; rests skipped unprocessed. Note that rests cannot have another other params 
				 ((length-restp l) (list l))
				 ;; process matching
				 ((funcall test l p v a) 
				  (funcall fn l p v a))
				 ;; skip unprocessed all others
				 (T (list l p v a)))))
	     )))
    (if flat 
	(copy-time-signature sequence result)
	result)))

;;; TODO: map-consecutive-selected-events -- generalise above function for processing subsequences of consecutive elements



(defun process-element (fn element args)
  "Many Opusmodus functions are defined to work only with lists. This function is intended to help when you want to instead process a single element with such a function.

* Examples:

  Transpose a single element with pitch-transpose
  ;;; (process-element #'pitch-transpose 'c4 '(2 _))

  Without this function, the call above would look as follows.
  ;;; (first (pitch-transpose 2 (list 'c4)))

  It is a matter of taste/style, which approach you prefer :)

  Of course, it might be better if instead Opusmodus functions would simply support single OMN notation elements as well.  
  "
  (first (apply fn (substitute (list element) '_ args))))


(defun map-omn (fn omn-expr)
  "Variant of mapcar for omn expressions, intended for creating variations of omn-expr. Applies function fn to every note in omn-expr (a flat OMN list). fn must exect four arguments (a length, pitch, velocity and articution) and returns a list of four values (a length, pitch, velocity and articution).

  NOTE: This was one of my first Opusmodus function definitions, and while it works it is not as refined as some later functions :)

* Arguments:
  - fn: a function expecting four arguments (a length, pitch, velocity and articulation) and returning a list of four values (a length, pitch, velocity and articulation).
  - omn-expr: an OMN expression


* Examples:

;;; (map-omn #'(lambda (length pitch velocity articulation)
;;;              (list length 
;;;                    pitch 
;;;                    ;; replace tasto dynamics by fff
;;;                    (if (equal articulation 'tasto)
;;;                      'fff
;;;                      velocity)
;;;                    articulation))
;;;          '(e. c4 pppp tasto d4 ponte e4))
;;; => (e. c4 fff tasto d4 pppp ponte e4)

;;; (map-omn #'(lambda (length pitch velocity articulation)
;;;              (list length 
;;;                          (if (member 'slap (disassemble-articulations articulation))
;;;                            'c4
;;;                            pitch)
;;;                          velocity
;;;                          articulation))
;;;          '((q b4 f slap+stacc -h q bb4 slap+stacc -h) (q gs4 slap+stacc -h) (q bb4 slap+stacc c5 mp ord d5 q. f5 e eb5 q d5) (-q c5 g4 h fs4 q eb5 stacc) (q c5 f slap+stacc -h q. g4 mp ord e f5 q e5) (q f5 cs5 f4 h d5 -q)))


* BUGS: 

 Does not work if omn-expr contains rest.
  Problem: omn does not provide any values for rests.
  Possible solution: couple note durations with their respective params, but leave rests without. Then skip rests in the processing unchanged. 
  BTW: This process looses articulations on rests, like fermata.


* Notes: 

  See also the Opusmodus built-in function `single-events': looping (or mapping) over its result has similar effect.


* See Also:

  {defun map-events}

"
  ;;; TMP
  ; (format T "map-omn ~A~%" omn-expr)
  (if (listp (first omn-expr))
    (mapcar #'(lambda (omn) (map-omn fn omn)) omn-expr)
    (destructuring-bind (lengths pitches velocities articulations)
                        (ta-utils:mat-trans 
                         (funcall #'mapcar fn
                                  (omn :length omn-expr)
                                  (omn :pitch omn-expr)
                                  (omn :velocity omn-expr)
                                  (omn :articulation omn-expr)))
      (make-omn :length lengths
                :pitch pitches 
                :velocity velocities
                :articulation articulations))))

#|

(map-omn #'(lambda (length pitch velocity articulation)
             (list length 
                   pitch 
                   ;; replace tasto dynamics by fff
                   (if (equal articulation 'tasto)
                     'fff
                     velocity)
                   articulation))
         '(e. c4 pppp tasto d4 ponte e4))
         
; => (e. c4 fff tasto d4 pppp ponte e4)

(map-omn #'(lambda (length pitch velocity articulation)
             (list length 
                         (if (member 'slap (disassemble-articulations articulation))
                           'c4
                           pitch)
                         velocity
                         articulation))
         '((q b4 f slap+stacc -h q bb4 slap+stacc -h) (q gs4 slap+stacc -h) (q bb4 slap+stacc c5 mp ord d5 q. f5 e eb5 q d5) (-q c5 g4 h fs4 q eb5 stacc) (q c5 f slap+stacc -h q. g4 mp ord e f5 q e5) (q f5 cs5 f4 h d5 -q)))

(map-omn #'(lambda (length pitch velocity articulation)
             (list length 
                         (if (member 'slap (disassemble-articulations articulation))
                           'c4
                           pitch)
                         velocity
                         articulation))
         '(q b4 f slap+stacc -h q bb4 slap+stacc -h))

(setf omn-expr '(q b4 f slap+stacc -h q bb4 slap+stacc -h))
(setf fn #'(lambda (length pitch velocity articulation)
             (list length 
                         (if (member 'slap (disassemble-articulations articulation))
                           'c4
                           pitch)
                         velocity
                         articulation)))

(omn nil omn-expr)

(disassemble-omn omn-expr)

|#


(defun rebar-if (fn sequence)
  "Rebars OMN `sequence', starting a new bar at every single event (as returned by `single-events') for which the Boolean function `fn' returns T.

BUG: Result is unnecessarily verbose including redundant repeated dynamics etc. (such information is introduced by `single-events' and not stripped off again later).
"
  (tu:inner-flat
   (tu:split-if fn
		(single-events (flatten sequence)))))
#|

|#

(defun rebar-at-articulation (sequence articulation)
  "Rebars OMN `sequence', starting a new bar at every event with the given `articulation'."
  (rebar-if (lambda (event)
	      (equal (fourth event) articulation))
	    sequence))
#|
(setf mat '(Q C4 MARC D4 E4 P1 H F4 MARC Q B3 H. C4 MARC))
(rebar-at-articulation mat 'marc)
|#


(defun copy-time-signature (music-with-time-signature music-to-rebar)
  "Rebars `music-to-rebar' so that it fits the meter of `music-with-time-signature'. If music-with-time-signature is a flat list, no rebarring happens."
  ;; only rebar if music-with-time-signature is nested 
  (if (every #'listp music-with-time-signature)
    (omn-to-time-signature music-to-rebar
                           (get-time-signature music-with-time-signature))
    music-to-rebar))
#|
(copy-time-signature '((q c4 d4) (q e4 f4)) '(q c4 d4 e4 f4))
(copy-time-signature '(q c4 d4 q e4 f4) '(q c4 d4 e4 f4))
|#


(defun map-section (function sequence
		    &key section exclude section-args shared-args)
  "Apply a function to only selected bars (sublists) in an OMN sequence. 

* Arguments:
  - function: function to apply to sublists in `sequence'
  - sequence: nested list of OMN parameters or full OMN expressions
  - section (list of ints): 0-based positions of bars (sublists) in `sequence' to which `function' is applied.
  - exclude (list of ints): 0-based positions of bars (sublists) in `sequence' to which `function' is *not* applied. Only either `section' or `exclude' should be specified, otherwise `exclude' is ignored.
  - section-args (list or list of lists): Further arguments to `function' added behind the current sublist of `sequence'. If not a nested list, then only a single additional argument is specified for each bar (sublist) to which `function' is applied.
  - shared-args (list): Further arguments to `function' added behind the current sublist of `sequence' and potentially `section-args'. 

* Examples:
  ;;; (map-section #'(lambda (seq) (pitch-transpose 7 seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) :section '(1 2))

  ;;; (map-section #'(lambda (seq) (pitch-transpose 7 seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) :exclude '(0))

  ;;; (map-section #'(lambda (seq interval) (pitch-transpose interval seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) 
  ;;;              :section '(1 2)
  ;;;              :shared-args '(7))

  ;;; (map-section #'(lambda (seq interval) (pitch-transpose interval seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) 
  ;;;              :section '(1 2)
  ;;;              :section-args '(7 12))

  ;;; (map-section #'(lambda (seq count divide) 
  ;;;                  (length-divide count divide seq))
  ;;;              '((q q q) (q q q) (q q q) (q q q)) 
  ;;;              :section '(1 2 3)
  ;;;              :section-args '((1 2) (2 3)))

  ;;; (map-section #'(lambda (seq count divide &rest args) 
  ;;;                  (apply #'length-divide count divide seq args))
  ;;;              '((q q q) (q q q) (q q q) (h.)) 
  ;;;              :section '(1 2 3)
  ;;;              :section-args '((1 2) (2 3))
  ;;;              :shared-args '(:ignore h.))


* See Also:

This function is a generalised and somewhat more clean variant of the Opusmodus builtin `do-section'.

"
  (let* ((length-sequence (length sequence))
	 ;; whether using section or exclude
	 (using-section? section)
	 (n (if using-section?
		(length section)
		(- length-sequence (length exclude))))
	 ;;; TODO: refactor into array for efficiency
	 (section-args-full (when section-args (circle-repeat (if (notevery #'listp section-args)
								  (mclist section-args)
								  section-args)
							      n))))
    (loop
       for seq in sequence
       for i from 0 to (1- length-sequence)
       for apply-fun? = (if using-section?
			    (member i section)
			    (not (member i exclude)))
       count apply-fun? into sec-no
       collect (if apply-fun?
		 ;;; TODO: revise using shared-args
		   (apply function
			  (append (list seq)
				  (when section-args (nth (1- sec-no) section-args-full))
				  (when shared-args shared-args)))
		   seq)
	 )))

;;; TODO: add support for processing ties properly using omn-merge-ties. e.g., see
;;; https://opusmodus.com/forums/topic/989-length-legato-opposite-function/
(defun edit-omn (type notation fun &key (flat nil) (swallow nil) (section nil) (additional-args nil))
  "Use function `fun', defined for transforming lists of individual OMN parameters of `type' (e.g., :length, or :velocity) to transform omn expression `notation'. This function is intended as a convenient way to generalise your functions to support omn notation as input.

* Arguments:
  - type: a keyword like :length, :pitch, :velocity, :duration, or :articulation (any keyword supported by function omn or make-omn).
  - notation: a omn sequence or a plain parameter list (can be nested).
  - fun: a function expecting a parameter sequence of given type. It is sufficient to support only a flat input list, support for nested lists is added implicitly.
  - flat: whether or not `fun' expects a flat input list.
  - swallow: if `type' is :length, and `fun' turns notes into rests, the argument `swallow' sets whether the pitches of these notes should be shifted to the next note or omitted (swallowed). `swallow' is ignored if notation is a plain parameter list (e.g., a 
  - section: only process the sublists (bars) at the positions given to this argument. Arg is ignored if `flat' is T.
  - additional-args (list of args): `additional-args' allows implementing 'dynamic' arguments, i.e., transformations that change over the sublists of `notation' depending on a list of arguments instead of a plain value. If `additional-args' is nil, then `fun' expects parameter values directly. However, if it is a list, then `fun' expects a list where the parameter values are the first element, and `additional-args' (if `flat' is T) or an element thereof (if `flat' is NIL) the second element in the list expected by `fun'. 

* Examples: 

  Roll your own transposition function.

  First define an aux def supporting only a flat list of pitches.

  ;;; (defun my-transposition-aux (interval pitches)
  ;;;   (midi-to-pitch (loop for p in (pitch-to-midi pitches)
  ;;;                        collect (+ p interval))))
  
  Test that function.
  ;;; (my-transposition-aux 7 '(c4 e4 g4)) 
  ;;;  => (g4 b4 d5)

  Now, based on that aux function, define a function that supports also full OMN sequences. You can later expand this new function further with edit-omn to also support arguments like section and flat (see below).
  ;;; (defun my-transposition (interval omn)
  ;;;   (edit-omn :pitch omn
  ;;;             #'(lambda (ps) (my-transposition-aux interval ps))))

  Test the new function with nested OMN including rests. 
  ;;; (my-transposition 7 '((q c4 mp -q q e4 q f4) (h g4 tr2)))
  ;;;  => ((q g4 mp - b4 c5) (h d5 mp tr2))


  Another example: expand the built-in function `length-rest-series' to support arbitrary OMN expressions (not just length lists), and additionally the arguments `swallow' and `section'.

;;; (defun note-rest-series (positions sequence &key (flat nil) (swallow nil) (section nil))
;;;   (edit-omn :length sequence 
;;;             #'(lambda (ls) (length-rest-series positions ls))
;;;             :swallow swallow
;;; 	        :section section
;;; 	        :flat flat))
;;; 
;;; (setf melody '((s eb6 < leg f5 < leg c5 < leg f5 < leg) (e e6 f - -q)))
;;; (note-rest-series '(1 1) melody :swallow T :section '(0))

  The next example demonstrates how 'dynamic' arguments can be implemented, i.e. arguments that support different values for subsections. Below is a simplified definition of the function rotate-omn. Note how the function argument `n' is handed to the argument `additional-args' of `edit-omn' if `n' is a list. The function given to `edit-omn' also tests whether `n' is a list, and in that case extracts the OMN sublist to rotate as first element of the function argument `xs' and the amount of the rotation of this sublist as second element of `xs'. Further 'dynamic' arguments could be implemented by handing `additional-args' a list of argument lists to use, and by then extracting the relevant elements of such sublists within the function given to `edit-omn'. 

;;; (defun rotate-omn (n sequence &key (parameter :pitch) (flat T) (section nil))
;;;   (let ((n-list-arg? (listp n)))
;;;     (edit-omn parameter sequence
;;; 	          #'(lambda (xs)
;;; 		      (if n-list-arg?
;;; 		          (gen-rotate (second xs) (first xs))
;;; 		          (gen-rotate n xs)))
;;; 	          :section section
;;; 	          :flat flat
;;; 	          :additional-args (when n-list-arg? n))))

  The function rotate-omn can now be called with either giving a single number or a list of numbers to its argument `n'.

;;; (setf melody '((-h e c4 e4) (q. f4 e g4 q a4) (q g4 f4 e e4 d4)))
;;; 
;;; (rotate-omn 1 melody) ; default parameter pitch
;;; 
;;; (rotate-omn '(0 1 2) melody :flat nil) 
;;; 
;;; (rotate-omn '(2 1) melody :section '(1 2) :flat nil :parameter :length)

  "
  ;; (declare (optimize (debug 3)))
  ;; TODO: efficiency: should I assume consistent nesting and only test first list element?
  (if (and (notevery #'listp notation) (not flat))
      ;; TODO: efficiency: just (setf flat T), not calling function recursively.
      ;; If notation is not (consistently) nested then set flat to T
      (edit-omn type notation fun :flat T :swallow swallow :section section :additional-args additional-args)      
      (labels (;; function section-to-binary-better is like built-in section-to-binary, but ensures that resulting 
	       ;; binary list is long enough to span over full nested param seq
	       (section-to-binary-better (seq)
		 (let ((bs (section-to-binary section)))
		   (append bs (gen-repeat (- (length seq) (length bs)) 0))))
	       (process-param-seq (par-seq)
		 (let ((full-args
			(cond ((and flat additional-args) 
			       (list (flatten par-seq) additional-args))			  
			      ((and additional-args section (not flat))
			       ;; `additional-args' may be shorter than `par-seq'. Therefore,
			       ;; - start with full `par-seq' as input for generating full args
			       ;; - substitue elements at positions of `section' with list containing its
			       ;; subsec from `par-seq' and the the element from `additional-args'
			       (reduce #'(lambda (xs sec-n-add-args)
					   (let ((sec (first sec-n-add-args))
						 (add-args (second sec-n-add-args)))
					     ;; replacing and additionally calling nth not efficient, but so what
					     ;;
					     ;; double listing required by tu:replace-element to insert the contained list
					     (tu:replace-element (list (list (nth sec xs) add-args)) sec xs)))
				       (tu:mat-trans (list section additional-args))
				       :initial-value par-seq))
			      ((and additional-args)
			       (tu:mat-trans (list par-seq additional-args)))
			      ((and flat)
			       (flatten par-seq))
			      (T par-seq)
			      )))
		   (cond (flat (funcall fun full-args))
			 ((and section (not flat))
			  ;;; TODO: consider replacing do-section with map-section
			  (do-section (section-to-binary-better par-seq)
			    `(flatten (funcall ,fun X))
			    full-args))
			 ((not flat) (flatten (mapcar fun full-args)))))))
	(if (omn-formp notation)
	    (copy-time-signature notation
				 (let* ((params (omn nil notation))
					(par-seq (getf params type))
					(omn (append  
					      (list type (process-param-seq par-seq))
					      (tu:remove-properties (if (equal type :length)
									'(:length :duration)
									(list type))
								    params)
					      (list :swallow swallow))))
				   (apply #'make-omn omn)))
	    ;; notation must be plain parameter list
	    (let ((pattern (if (some #'length-restp (flatten notation)) ; rests would be skipped in spanning
			       (length-rest-invert notation)
			       notation)))
	      (span pattern (process-param-seq notation) :flat T))
	    ))))


;;; TMP: 
(defun process-omn2 (type function sequence &key flatten flat (span :length) swallow section exclude)
  "Function similar to edit-omn that will soon be built-in in Opusmodus."
  (do-verbose ("process-omn2")
    (labels ((process-omn2* (type function sequence)
               (if (any-itemp '(:length :pitch :velocity :articulation :all) (list! type))
                 (let* ((type-par (list! type))
                        (lval (omn-plist :length sequence))
                        (pval (remove-nils (omn-plist :pitch sequence)))
                        (vval (remove-nils (omn-plist :velocity sequence)))
                        (aval (remove-nils (omn-plist :attribute sequence)))
                        (length (if (any-itemp '(:length :all) type-par)
                                  (let ((plen (assemble-seq (funcall function (list! lval)))))
                                    (cond ((and (listsp lval) flatten)
                                           (length-span (get-span plen)
                                                        (flatten (funcall function (list (flatten lval))))))
                                          ((listsp lval) (funcall function lval))
                                          (t (car (funcall function (lists! lval))))))
                                  lval))
                        (pitch (if (any-itemp '(:pitch :all) type-par)
                                 (cond ((and (listsp pval) flatten)
                                        (flatten (funcall function (list! (flatten pval)))))
                                       ((listsp pval) (car (funcall function  pval)))
                                       (t (car (funcall function (lists! pval)))))
                                 pval))
                        (velocity (if (any-itemp '(:velocity :all) type-par)
                                    (cond ((and (listsp vval) flatten)
                                           (flatten (funcall function (list (flatten vval)))))
                                          ((listsp vval) (funcall function vval))
                                          (t (car (funcall function (lists! vval)))))
                                    vval))
                        (articulation (if (any-itemp '(:articulation :all) type-par)
                                        (cond ((and (listsp aval) flatten)
                                               (flatten (funcall function (list (flatten aval)))))
                                              ((listsp aval) (funcall function aval))
                                              (t (car (funcall function (lists! aval)))))
                                        aval)))
                   (make-omn
                    :length length
                    :pitch pitch
                    :velocity velocity
                    :articulation articulation
                    :flat flat
                    :span span
                    :swallow swallow))
                 (error "Not a known type (~A)." type))))
      
      (maybe-section
       (lambda (x) (process-omn2* type function x))
       sequence section exclude))))

#|

(setf sequence '((s eb6 < leg q e f5 < leg s c5 < leg - f5 < stacc) (-q c4 ff ped1)))
(setf gen-retrograde (lambda (x) (gen-retrograde x)))

(process-omn2 :articulation gen-retrograde '((s eb6 < leg q e f5 < leg s c5 < leg - f5 < stacc) (-q c4 ff ped)) :flatten nil)
(process-omn2 :articulation gen-retrograde '((s eb6 < leg q e f5 < leg s c5 < leg - f5 < stacc) (-q c4 ff ped)) :flatten t)
(process-omn2 :length gen-retrograde '((s eb6 < leg q e f5 < leg s c5 < leg - f5 < stacc) (-q c4 ff ped)) :flatten nil)
(process-omn2 :length gen-retrograde '((s eb6 < leg q e f5 < leg s c5 < leg - f5 < stacc) (-q c4 ff ped)) :flatten t)
|#



;;; TODO: define add-omn when I have variant of disassemble-omn that works with incomplete CMN forms
#|
(defun add-omn (type parameter notation)

  )


(add-omn :pitch 
         '(g4 a4 b4)
         '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))

(omn-formp '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))

(disassemble-omn '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))
|#


;;; TODO: take rests into account. Tmp workaround: if bar starts with a rest then simply skip processing it
(defun map-position-in-bar (position type sequence fun &key (section nil))
  "Transforms in the bars of `sequence' the parameter of `type' (e.g., :length) at `position' with `fun'. 

* Examples:
  Apply the articulation tenuto to every first note in all bars except the last bar.
  ;;; (map-position-in-bar 0 :articulation 
  ;;;                      '((-q c4 c4) (q c4 c4 c4) (q c4 c4 c4)) 
  ;;;                      #'(lambda (ignore) 'ten)
  ;;;                      :section '(0 1))


* Notes: 

Currently, rests are simply not counted when estimating the position of a parameter other then :length. Potential workaround: use argument `section'."
  (edit-omn type sequence 
            #'(lambda (params)
		(when params ; skip in case a bar only contains a rest and some other param except :length is processed
		  (tu:replace-element (funcall fun (nth position params))
				      position params)))
            :flat nil
            :section section
	    :swallow nil))


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
#|
(defun total-duration (sequence)
  "Returns the total duration (sum of all note and rest values) of `sequence'."
  (reduce #'+ (mapcar #'abs (flatten (omn :length sequence))) :initial-value 0))
|#
;; (total-duration '((-h q c4) (q. f4 e g4 q a4) (h. g4)))

(defun flattened-length-adjust (duration sequence)
  "Currently, the built-in function length-adjust has no :flatten argument. This function offers a workaround."
  ;; note: length adjusted sequence is longer or shorter than orig sequence -- how does copy-time-signature cope with that?
  (copy-time-signature 
   sequence
   (length-adjust duration (flatten sequence))))

#|
  (process-omn2 :all
		#'(lambda (seq) (length-adjust duration seq))
		sequence
		:flat T))
|#

;; (flattened-length-adjust 2 '((h g4) (h g4) (h g4)))



#|
(defun note-no (music)
  "Returns the number of notes in music, which is a list of lengths values of OMN expressions.

* Examples:

;;; (note-no '((q c4 c4 c4) (q g4 g4 g4)))
"
  (count-if #'length-notep (flatten (omn :length music))))
|#

(defun count-notes (notes)
  "Returns number of notes (ignoring rests) in length list or other OMN expression.

* Examples:

;;; (count-notes '((q c4 c4 c4) (q g4 g4 g4)))

* BUG:

Counts tied notes as multiple notes.
"
  (get-count notes :length :note :sum T))


(defun phrase-lengths (lengths)
  "Returns the number of notes between rests in the given lengths.
  
* Arguments:
  - length: lengths or OMN (list or list of list)."
  (let ((flat-lengths (length-rest-merge (flatten (omn :length lengths)))))
    (mapcar #'1-
            (tu:x->dx (length-rest-position
                       (append (unless (length-restp (first flat-lengths))
                                 '(-1)) 
                               flat-lengths
                               (unless (length-restp (first (last flat-lengths)))
                                 '(-1))))))))

; (phrase-lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

#|
;;; Unfinished -- rests are currently left out alltogether 
(defun split-into-phrases (lengths)
  "Splits lengths into its phrases (notes between rests). Returns a list of lists, where each sublist is a phrase. Any rest between phrases is retained at the beginning of a phrase.

  length: lengths or OMN (list or list of list)."
  (let* ((events (if (omn-formp lengths)
                   (single-events (length-rest-remove (flatten lengths)))
                   (length-rest-remove (flatten lengths))))
         ;; todo: add these rests one by one at the top of the resulting phrases 
         (rests (if (omn-formp lengths)
                  (single-events (length-note-remove (length-rest-merge (flatten lengths))))
                  (length-note-remove (length-rest-remove (flatten lengths)))))
         ;; test whether first phrase starts with a rest
;         (
;          (if (length-restp (first (flatten lengths)))))          
         (lengths (phrase-lengths lengths)))
    (loop for (start end) on (tu:dx->x lengths 0)
      when end
      ; do (print (list events start end))
      collect (subseq events start end)
      )))
|#

#| ; tests for split-into-phrases
(split-into-phrases '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

(phrase-lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

(length (length-rest-merge (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))))


(setf lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))
(setf phrase-lengths (phrase-lengths lengths))

(single-events (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4))))


(omn-formp '((-3h) (3h c4 mf) (3h c4 mf) (q c4 mf) (q c4 mf) (3h c4 mf) (3h c4 mf) (3h c4 mf) (q c4 mf) (-q) (-5h) (-5h) (5h c4 mf) (5h c4 mf) (5h c4 mf) (q c4 mf) (q c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (q c4 mf) (-q)))

|#



(defun length-subtract (&rest length-values)
  "Subtraction for OMN length values. 

* Examples:
;;; (length-subtract 'w 'q)
;;; => h.
  "
  (first (omn-decode (list (apply #'- (omn-encode length-values))))))


(defun length-add (&rest length-values)
  "Addition of OMN length values.

* Examples:
;;; (length-add 'w 'q)
;;; => wq"
  (first (omn-decode (list (apply #'+ (omn-encode length-values))))))
 

#|
;;; not needed anymore, as CTR-1 does conveniently plot lists/vectors of numbers now
(defun plotter (data &optional (number 100))
  "Aux function for plotting fenvs

  Meanwhile made redundant by new Opusmodus builtin functionality. Instead use {defun fe:fenv->vector} and Tools > Plot > Numbers and friends."
  (list-plot (cond ((fe:fenv? data) (fe:fenv->vector data number))
                   ((and (listp data) (every #'fe:fenv? data))
                    (mapcar #'(lambda (xs) (fe:fenv->vector xs number)) data))
                   (T data))
	     :join-points T :point-radius 2))
|#

#|
(setf *print-pretty* t
      *print-miser-width* 0
      *print-right-margin* 80)
;; based on https://groups.google.com/forum/#!topic/comp.lang.lisp/_NP7Ub6hLsE
(defun pprint-part (part &optional (stream *standard-output*))
  "Pretty prints a part one bar a time, adding a bar line comment before each bar.

  Args: 
  - part: nested OMN list.

* Examples:
  ;;; (pprint-part '((q c4 d4 e4) (h f4 q e4) (h. d2)))

  Meanwhile made redundant by new Opusmodus builtin functionality: Tools > PPrint Expression, and Tools > PPrint Last Score"
  (pprint-logical-block 
      (stream nil :prefix "(" :suffix ")") 
    (pprint-logical-block (stream part) 
      (loop 
        for bar-no from 1 to (length part)
        for bar in part
        do (progn 
             (pprint-indent :block 1 stream)
             (pprint-newline :mandatory stream)
             (format stream ";; Bar ~A" bar-no)
             (pprint-newline :mandatory stream)
             (prin1 bar stream))))
    (pprint-indent :block -1 stream) 
    (pprint-newline :mandatory stream)))
|#

#|
(pprint-part 
 '((s g1 ff gs1 p a1 ff b1) (s c2 mf cs2 ff d2 e2 mf) (e f2 p s fs2 ff g2 p a2)
   (s b2 mf e c3 s cs3 ff eb3) (s e3 mf f3 ff fs3 p a3 mf) (s bb3 ff b3 e c4 p s e4)
   (e e4 ff s f4 e s b4 p) (e b4 s s mf e fs5 ff) (s fs5 mf f5 p mf e cs6)
   (s cs6 c6 p e b5 mf s g6 ff) (s g6 p ff e f6 g6 mf) (s g6 g6 p ff e fs6) (s g6 mf p e s fs6)
   (s fs6 fs6 ff mf ff) (e fs6 p s ff fs6 fs6 mf) (s fs6 fs6 ff p f6) (s fs6 ff mf e e f6)
   (s f6 e fs6 fs6 p s f6) (s f6 mf fs6 fs6 p f6) (e f6 f6 ff s fs6 p f6 ff) (s f6 p e s s)
   (s f6 e mf p s ff) (s f6 mf p mf e ff) (s f6 p e s mf p) (s f6 ff e mf s e p)
   (e f6 s e ff mf) (s f6 p mf e6 ff f6 p) (s f6 ff e6 mf e eb6 p f6 mf) (s f6 e6 d6 f6)
   (s f6 e6 ff d6 e f6 p) (s f6 mf e6 ff cs6 f6 mf) (s f6 ff f6 cs6 mf f6 ff)
   (e f6 mf s s cs6 p fs6 mf) (s f6 ff e e6 p s bb5 mf f6) (s e6 p b5 f5 eb6)
   (s bb5 fs5 ff e cs5 p s a5 mf) (e e5 ff s c5 e gs4 mf s d5) (e bb4 s fs4 d4 e g4)
   (s e4 ff d4 b3 mf e eb4) (s cs4 p bb3 mf e gs3 p s c4 ff) (s a3 a3 mf e b3 ff gs3)
   (s bb3 c4 d4 e b3 mf) (s cs4 p eb4 mf e f4 p s eb4 ff) (s e4 fs4 gs4 p fs4 ff)
   (e g4 p s a4 mf bb4 a4) (s bb4 c5 cs5 p c5 mf) (s cs5 eb5 ff e e5 mf eb5 ff)
   (s e5 mf e fs5 g5 p s fs5 ff) (s g5 mf gs5 a5 p a5) (e bb5 ff b5 mf s c6 ff c6)))

|#

(defun rnd-section (section-range probability &key seed)
  "The function returns a list of random section numbers intended for the argument section of many Opusmodus functions. The list of returned sections is unsorted.

* Arguments:
  - section-range (pair of ints): the range of 0-based section positions (including boundaries) within which sections are returned.
  - probability (float in interval 0-1): the likelyhood by which sections are return, where 0 means the result is nil, 1 means that the result contains all sections within the given range, and, e.g., 0.5 means a 50 percent probability that any section is selected.  

* Examples:
  ;;; (rnd-section '(0 9) 0.5 :seed 1)
"
  (rnd-seed seed)
  (let ((section-no (1+ (- (second section-range) (first section-range)))))
    (rnd-unique (* section-no probability)
		(apply #'gen-integer section-range)
		:seed (seed))
    ))

(defun mk-seed (&optional seed)
  "Generates a random seed value, prints and returns it. Useful for exploring different results of random processes, but then keeping a found solution.

This function is now rather redundant, as Opusmodus automatically prints seed values of all function calls.

* Arguments:
  - seed (int): optionally fixes generated seed.

* Examples:

  ;;; (rnd-sample 3 '(c4 d4 e4) :seed (mk-seed))
  ;;; ; 405621 rnd-sample
  ;;; => (c4 e4 d4) 

  ;;; (rnd-sample 3 '(c4 d4 e4) :seed (mk-seed 13))
  ;;; ; 13 rnd-sample
  ;;; => (e4 d4 e4) 
  "
  (print (if seed seed (rnd-range 1 999999))))


;; TODO: This def is some overlap with tu:ensure-nested-list -- unify and replace all calls.
(defun ensure-double-list (x)
  "Ensures that `x' is a duble-wrapped list. If not, a list (or two) are wrapped around it. 
  
  As a precaution, if `x' is inconsistently nested, then the result is a flattened version of it with a double list wrapped around."
  (if (listp x)
    (if (every #'listp x)
	x
	(progn
	  (warn "ensure-double-list: inconsistent nesting of ~A" x)
	  (list (flatten x))))
    (list (list x))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generalised unfold alternative based on higher-order programming
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun _unfold (seq fn)
  "[aux for fn-unfold and unfold-subseqs] Apply the spec of function call FN to the OMN SEQ. 

* Arguments:
 - fn (list): a function call spec of the form (<omn-fn> &rest <args>), where <omn-fn> is a function expecting an OMN sequence as first argument and arbitrary further argments, and <args> are the further arguments beyond the OMN sequence given to the function.
  - sequence: OMN sequence
"
  (apply (if (functionp (first fn))
	     (first fn)
	     (fdefinition (first fn)))
	 seq (rest fn)))

#|
(setf mat '((q c4 d4 e4) (h f4 q b3)))

;; The first element of function FN can be an OMN function or a symbol naming a function.
(_unfold mat '(*t 12))
(_unfold mat (list #'*t 12))
|#



(defun fn-unfold (fns sequence)
  "Much like the buildin Opusmodus `unfold', but instead works with functions, and additional arguments can be given to the functions. Apply to `sequence' all fns in order.

* Arguments:
  - fns (list of lists): Each sublist has the form (<omn-fn> &rest <args>), where <omn-fn> is a function expecting an OMN sequence as first argument and arbitrary further argments, and <args> are the further arguments beyond the OMN sequence given to the function.
  - sequence: OMN sequence


* Examples:

Some material to use
;;; (setf mat '((q c4 d4 e4) (h f4 q b3)))

Remember: all functions used must expect a OMN sequence as *first* argument.

;;; (fn-unfold '((gen-retrograde :flatten T) (quantum :fraction -0.2)) mat)

Some short-hand versions of common functions are defined for conciseness. These short-hand functions commonly start with an asterisk (*) to stand out and to reduce namespace pollution.

;;; (fn-unfold '((*t 12) (*ld (2 3) :section 1)) mat)
"
  (reduce #'_unfold fns :initial-value sequence))


(defun _get-event-articulations (event)
  "[Aux]"
  (disassemble-articulations (fourth event)))

(defun unfold-subseqs (fn-call-specs sequence &key (end-marker NIL))
  "Variant of Opusmodus' buildin `unfold', but the preset function is much more flexible. Otherwise, the same workflow as documented for `unfold' could be used (multiple transformations concisely applied separately to individual parts and then the parts combined, e.g., with the builtin `ps' or TOT's `preview-score').

Like with `unfold' (and the TOT function `fn-unfold'), multiple Opusmodus transformations can be applied to `sequence' in the given order. However, while `unfold' requires that these transformations are separately declared (using `def-unfold-set'), standard Opusmodus functions are used with `unfold-subseqs' (and with `fn-unfold'). The only condition is that these functions expect an OMN sequence as their first argument (see concisely named functions like `*t', `*mute' and various others below for how such functions can be defined). 

Unlike `unfold', arbitrary arguments can be specified for the transformation functions when using `unfold-subseqs' (and `fn-unfold'). For example, it is sufficient to have only a single transposition function (by contrast, `unfold' requires a separate transformation declaration for each argument combination, e.g., each transposition interval).

Importantly, with `unfold-subseqs' transformation functions can be applied to only subsequences of OMN events in `sequence'. The point where new OMN subsequences start can be marked with standard OMN attributes. Remember that you can define custom OMN attributes with the builtin `add-text-attributes'.

* Arguments:
  - fn-call-specs (plist): A property list of alternating symbols (which are OMN articulations) and nested lists (function call combination specs). Each function call combination spec has the form (<omn-fn> &rest <args>), where <omn-fn> is a function expecting an OMN sequence as first argument and arbitrary further argments, and <args> are the further arguments beyond the OMN sequence given to the function.
  - sequence: OMN sequence
  - end-marker (symbol): OMN attribute that is used for marking subsequence ends.

* Examples:

Define some custom OMN attributes for marking the start points of subsequences to which transformations are applied. Remember that non-sticky attributes are only attached to a single event, following events are not affected.
;;; (add-text-attributes
;;;  '(p1 \"p1\" :non-sticky) ;; p1 stands for phrase one
;;;  '(p2 \"p2\" :non-sticky)
;;;  '(pe \"pe\" :non-sticky)) ;; phrase end

Some material to use with a new subsequence starting at the event marked with the attribute p1.
;;; (setf mat-1 '((q c4 d4 e4) (h f4 q b3 p1) (h. c4)))

Apply two transformations (and octave transposition and note subdivisions), but only to the subsequence starting at p1. Note that parameters are specified for that transformation functions.
;;; (unfold-subseqs '(p1 ((*t 12) (*ld (2 3)))) mat-1)

An OMN sequence consisting of two phrases. The first subsequence p1 ends just before the beginning of the next subsequence, and the second subsequence p2 ends before the explicit subsequence end marker (pe). 
;; (setf mat-2 '((q c4 d4 e4 p1) (h f4 q e4) (h d4 q c4 p2) (h b3 q d4 pe) (h. c4)))

The first subsequence is transposed an octave up, and the second an octave down. `unfold-subseqs' is told that we use the attribute `pe' as subsequence end marker.
;;; (unfold-subseqs '(p1 ((*t 12)) 
;;; 		      p2 ((*t -12)))
;;; 		    mat-2
;;; 		    :end-marker 'pe)

An OMN sequence with two equally named subsequences that are separated by a gap.
;;; (setf mat-3 '((q c4 d4 e4 p1) (h f4 q e4 pe) (h d4 q c4 p1) (h b3 q d4 pe) (h. c4)))

The two equally labelled subsequences are transformed by the same function.
;;; (unfold-subseqs '(p1 ((*t 12)))
;;; 		    mat-3
;;; 		    :end-marker 'pe)

The subsequence marker attributes can be merged with other attributes.
;;; (setf mat-4 '((q c4 d4 e4 p1+marc) (h f4 q b3 ) (h. c4)))
;;; (unfold-subseqs '(p1 ((*t 12))) mat-4
;;; 		    :end-marker 'pe)


NOTE: The transformation functions receive a list of single events (as returned by `single-events'), i.e. the information on bar boundaries is unavailable for the transformations. As a workaround, this information could be preserved, e.g., by marking each event at the beginning of a bar with a custom articulation and then splitting the subsequence into bars again by the first transformation function.
TODO: For unfinished example see comment below.

"
  (let* ((fn-call-specs (if end-marker
			    (append (list end-marker '((identity)))
				    fn-call-specs)
			    fn-call-specs))
	 (split-marker-articulations (remove NIL (tu:properties fn-call-specs)))
	 (events (single-events (flatten sequence)))
	 (split-events (tu:split-if (lambda (event)
				      (some (lambda (articulation) 
					      (member articulation split-marker-articulations))
					    (_get-event-articulations event)))
				    events))
	 (result (loop for events-subseq in split-events
		    for marker = (find-if (lambda (articulation) 
					    (member articulation split-marker-articulations))
					  (_get-event-articulations (first events-subseq)))
		    for fns = (getf fn-call-specs marker)
		    ;; do (break)
		    collect (fn-unfold fns events-subseq))))
    ;; (break)
    (copy-time-signature sequence result)))
#|
(add-text-attributes
 '(p1 "p1" :non-sticky) ;; p1 stands for phrase one
 '(p2 "p2" :non-sticky)
 '(pe "pe" :non-sticky) ;; phrase end
 )

(setf mat-2 '((q c4 d4 e4 p1) (h f4 q e4) (h d4 q c4 p2) (h b3 q d4 pe) (h. c4)))

(unfold-subseqs '(p1 ((*t 12)) 
		  p2 ((*t -12)))
		mat-2
		:end-marker 'pe)

(setf mat-3 '((q c4 d4 e4 p1) (h f4 q e4 pe) (h d4 q c4 p1) (h b3 q d4 pe) (h. c4)))

(unfold-subseqs '(p1 ((*t 12)))
		mat-3
		:end-marker 'pe)

(setf mat-4 '((q c4 d4 e4 p1) (h f4 q b3) (h. c4)))

Mark every first note of a bar with an attribute. 
(setf mat-4 (articulate-bars mat-4 :accent 'marc))


;; BUG: Unfinished. rebar-at-articulation does not work here. 
(unfold-subseqs '(p1 ((rebar-at-articulation 'marc) 
		      (print)
		      (*ld (2 3) :section (0))))
		mat-4)

;; TMP
(rebar-at-articulation (flatten mat-4) 'marc)
|#


#| ;; Example functions that can be used directly (OMN sequence is already first argument) 
;; gen-retrograde
;; length-staccato
;; length-legato
;; quantum
;; rnd-order
;; closest-path
;; comparative-closest-path
;; relative-closest-path
;; ! length-rational-quantize
;; velocity-to-dynamic
|#

(defun swap-args (fn &optional (pos 1))
  "Return a function where the argument at `pos` is moved forward to become the first positional argument. Intended for making arbitrary OMN functions usable for `fn-unfold` (so that their OMN argument becomes the first positional argument)."
  (lambda (&rest args)
    (apply fn (nth pos args) (append (subseq args 0 pos) (subseq args (1+ pos))))))

;;;
;;; Some more concisely named Opusmodus functions compatible with fn-unfold
;;;

(defun *t (sequence transpose &key (section NIL) (exclude NIL) (ambitus 'piano) (omn NIL))
  "Like pitch-transpose, but sequence as first param."
  (pitch-transpose transpose sequence :section section :exclude exclude :ambitus ambitus :omn omn))

(defun *mute (sequence &key (section NIL) (exclude NIL))
  "Alias for gen-pause."
  (gen-pause sequence :section section :exclude exclude))

(defun *a (sequence range &key (type :transpose) (section NIL) (exclude NIL) (omn NIL))
  "Like ambitus, but sequence as first param."
  (ambitus range sequence :type type :section section :exclude exclude :omn omn))

(defun *ac (sequence size &key (unique T) (section NIL) (exclude NIL))
  "Like ambitus-chord, but sequence as first param."
  (ambitus-chord size sequence :unique unique :section section :exclude exclude))

;; ;; TODO: Either chord-interval-remove is buggy or not well documented...
;; ;; Perhaps instead define your own versions of chord-interval-remove and chord-interval-remove when needed?
;; (defun *cir (sequence intervals &key (octaves NIL) (sort :asc) (section NIL) (exclude NIL))
;;   "Like chord-interval-remove, but sequence as first param."
;;   (chord-interval-remove intervals sequence &key :octaves octaves :sort sort :section section :exclude exclude))

;; (defun *cia (sequence intervals &key (chord T) (section NIL) (exclude NIL))
;;   "Like chord-interval-add, but sequence as first param."
;;   (chord-interval-remove intervals sequence &key :chord chord :section section :exclude exclude))

(defun *r (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return retrograde."
  (pitch-variant sequence :variant 'r :transpose transpose :section section :exclude exclude :omn omn))

(defun *i (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return inverse."
  (pitch-variant sequence :variant 'i :transpose transpose :section section :exclude exclude :omn omn))

(defun *ri (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return retrograde inverse."
  (pitch-variant sequence :variant 'ri :transpose transpose :section section :exclude exclude :omn omn))

(defun *> (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return descending."
  (pitch-variant sequence :variant '> :transpose transpose :section section :exclude exclude :omn omn))

(defun *< (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return ascending."
  (pitch-variant sequence :variant '< :transpose transpose :section section :exclude exclude :omn omn))

(defun *>< (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return descending-ascending."
  (pitch-variant sequence :variant '>< :transpose transpose :section section :exclude exclude :omn omn))

(defun *<> (sequence &key (transpose NIL) (section NIL) (exclude NIL) (omn T))
  "Simplification of pitch-variant: return ascending-descending."
  (pitch-variant sequence :variant '<> :transpose transpose :section section :exclude exclude :omn omn))

(defun *ld (sequence values &key set ignore (seed NIL) (section NIL) (exclude NIL) (omn NIL))
  "Like length-divide, but sequence as first param."
  (length-divide values sequence :set set :ignore ignore :section section :exclude exclude :omn omn :seed seed))
#| 
;; NOTE: In case notes processed by length-divide -- or *ld -- carry some articulation, this articulation is preserved for every subdivided note.
(length-divide  '(2 3) '((h c4 marc)))
|#

(defun *frag (sequence count range &key (encode T) (lists NIL) (section NIL) (exclude NIL) (seed NIL))
  "Like gen-fragment, but sequence as first param."
  (gen-fragment count range sequence :encode encode :lists lists :section section :exclude exclude :seed seed))

(defun *rest (sequence value &key (type NIL) (swallow T) (section NIL) (exclude NIL) (omn NIL) (flat NIL) (span :length))
  "Like length-to-rest, but sequence as first param."
  (length-to-rest value sequence :type type :swallow swallow :section section :exclude exclude :omn omn :flat flat :span span))

;; ? TODO: density

;; ? TODO: length-span


