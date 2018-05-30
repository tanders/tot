;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OMN Utilities 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-time-signature (music-with-time-signature music-to-rebar)
  "Rebars `music-to-rebar' so that it fits the meter of `music-with-time-signature'."
  ;; only rebar if music-with-time-signature is nested 
  (if (every #'listp music-with-time-signature)
    (omn-to-time-signature music-to-rebar
                           (get-time-signature music-with-time-signature))
    music-to-rebar))


(defun map-section (function sequence
		    &key section exclude section-args shared-args)
  "Apply a function to only selected bars (sublists) in an OMN sequence. 

  Args:
  - function: function to apply to sublists in `sequence'
  - sequence: nested list of OMN parameters or full OMN expressions
  - section (list of ints): 0-based positions of bars (sublists) in `sequence' to which `function' is applied.
  - exclude (list of ints): 0-based positions of bars (sublists) in `sequence' to which `function' is *not* applied. Only either `section' or `exclude' should be specified, otherwise `exclude' is ignored.
  - shared-args (list): Further arguments to `function' added behind the current sublist of `sequence' and potentially `section-args'. 
  - section-args (list or list of lists): Further arguments to `function' added behind the current sublist of `sequence'. If not a nested list, then only a single additional argument is specified for each bar (sublist) to which `function' is applied.

  Examples:
  (map-section #'(lambda (seq) (pitch-transpose 7 seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) :section '(1 2))

  (map-section #'(lambda (seq) (pitch-transpose 7 seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) :exclude '(0))

  (map-section #'(lambda (seq interval) (pitch-transpose interval seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) 
               :section '(1 2)
               :shared-args '(7))

  (map-section #'(lambda (seq interval) (pitch-transpose interval seq)) '((c4 c4 c4) (c4 c4 c4) (c4 c4 c4)) 
               :section '(1 2)
               :section-args '(7 12))

  (map-section #'(lambda (seq count divide) 
                   (length-divide count divide seq))
               '((q q q) (q q q) (q q q) (q q q)) 
               :section '(1 2 3)
               :section-args '((1 2) (2 3)))

  (map-section #'(lambda (seq count divide &rest args) 
                   (apply #'length-divide count divide seq args))
               '((q q q) (q q q) (q q q) (h.)) 
               :section '(1 2 3)
               :section-args '((1 2) (2 3))
               :shared-args '(:ignore h.))

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

  Args:
  - type: a keyword like :length, :pitch, :velocity, :duration, or :articulation (any keyword supported by function omn or make-omn).
  - notation: a omn sequence or a plain parameter list (can be nested).
  - fun: a function expecting a parameter sequence of given type. It is sufficient to support only a flat input list, support for nested lists is added implicitly.
  - flat: whether or not `fun' expects a flat input list.
  - swallow: if `type' is :length, and `fun' turns notes into rests, the argument `swallow' sets whether the pitches of these notes should be shifted to the next note or omitted (swallowed). `swallow' is ignored if notation is a plain parameter list (e.g., a 
  - section: only process the sublists (bars) of the positions given to this argument. Arg is ignored if `flat' is T.
  - additional-args (list of args): `additional-args' allows implementing 'dynamic' arguments, i.e., transformations that change over the sublists of `notation' depending on a list of arguments instead of a plain value. If `additional-args' is nil, then `fun' expects parameter values directly. However, if it is a list, then `fun' expects a list where the parameter values are the first element, and `additional-args' (if `flat' is T) or an element thereof (if `flat' is NIL) the second element in the list expected by `fun'. 

  Examples: 

  roll your own transposition supporting omn input
  first aux def supporting only pitches
  ;;; (defun my-transposition-aux (interval pitches)
  ;;;   (midi-to-pitch (loop for p in (pitch-to-midi pitches)
  ;;;                        collect (+ p interval))))
  
  test
  ;;; (my-transposition-aux 7 '(c4 e4 g4)) 
  ;;;  => (g4 b4 d5)

  variant supporting also omn expressions
  ;;; (defun my-transposition (interval omn)
  ;;;   (edit-omn :pitch omn
  ;;;             #'(lambda (ps) (my-transposition-aux interval ps))))

  test with nested OMN including a rest
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
  (if (and (notevery #'listp notation) (not flat)) 
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

  Example:
  Apply the articulation tenuto to every first note in all bars except the last bar.
  (map-position-in-bar 0 :articulation 
                       '((q c4 c4 c4) (q c4 c4 c4) (q c4 c4 c4)) 
                       #'(lambda (ignore) 'ten)
                       :section '(0 1))

  NOTE: Currently, rests are simply not counted when estimating the position of a parameter other then :length. Potential workaround: use argument `section'."
  (edit-omn type sequence 
            #'(lambda (params)
		(when params ; skip in case a bar only contains a rest and some other param except :length is processed
		  (tu:replace-element (funcall fun (nth position params))
				      position params)))
            :flat nil
            :section section
	    :swallow nil))


(defun total-duration (sequence)
  "Return the total duration (sum of all note and rest values) of `sequence'."
  (reduce #'+ (mapcar #'abs (flatten (omn :length sequence))) :initial-value 0))

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




;;; TODO: keep as global fun
;;; TODO: see count-notes -- do I need both?
(defun note-no (music)
  "Returns the number of notes in music, which is a list of lengths values of OMN expressions."
  (count-if #'length-notep (flatten (omn :length music))))


;;; TODO: see note-no -- do I need both?
(defun count-notes (notes)
  "Returns number of notes (ignoring rests) in length list or other OMN expression."
  (get-count notes :length :note :sum T))


(defun phrase-lengths (lengths)
  "Returns the number of notes between rests in the given lengths.
  
  Args:
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


(defun insert-at-position (position item list &key seed)
  "Insert item(s) at given position into list.

  Args
  - position: either symbol 's (start), 'e (end) or '? (random position), or integer specifying position.
  - item: value or list of values to be inserted.
  - list: flat list of values.

  Examples:
  (insert-at-position 'e 'x '(a a a a))
  (insert-at-position 's 'x '(a a a a))
  (insert-at-position '? 'x '(a a a a))
  (insert-at-position 'e '(x y) '(a a a a))
  (insert-at-position '0 '(x y) '(a a a a))
"
  (rnd-seed seed)
  (let* ((pos1 (case position
		 (s 0)
		 (e (length list))
		 (? (round (rnd1 :low 0 :high (length list) :seed (seed))))
		 (otherwise (if (numberp position)
				position
				(error "~A is not a valid position" position)))))
	 (pos (if (listp item)
		  (gen-integer pos1 (+ pos1 (1- (length item))))
		  pos1)))
    (position-insert pos item list)))


(defun length-subtract (&rest length-values)
  "Subtraction for OMN length values. 

  Example:
;;; (length-subtract 'w 'q)
;;; => h.
  "
  (first (omn-decode (list (apply #'- (omn-encode length-values))))))


(defun length-add (&rest length-values)
  "Addition of OMN length values.

  Example:
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

  Example:
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


(defun mk-seed (&optional seed)
  "Generates a random seed value, prints and returns it. Useful for exploring different results of random processes, but then keeping a found solution.

  Args:
  - seed (int): optionally fixes generated seed.

  Examples:

  (rnd-sample 3 '(c4 d4 e4) :seed (mk-seed))
  ; 405621 rnd-sample
  => (c4 e4 d4) 

  (rnd-sample 3 '(c4 d4 e4) :seed (mk-seed 13))
  ; 13 rnd-sample
  => (e4 d4 e4) 
  "
  (print (if seed seed (rnd-range 1 999999))))

(defun ensure-double-list (x)
  "Ensures that `x' is a duble-wrapped list. If not, a list (or two) are wrapped around it. 
  
  As a precaution, if `x' is inconsistently nested, then the result is a flattened version of it wth a double list wrapped around."
  (if (listp x)
    (if (every #'listp x)
      x
      (list (flatten x)))
    (list (list x))))
