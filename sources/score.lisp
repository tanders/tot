;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)

;; (declaim (optimize (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Polyphonic processing for creating textures etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE: just some dummy settings for now
(defparameter *default-preview-score-instruments*
  '(:vln (:program 'violin :sound 'gm)
    :vlc (:program 'cello :sound 'gm))
  "Settings for each instrument used by `preview-score'. The format is a plist where keys are the instrument labels, and values a list with the actual settings. For format of these settings are the same as instrument settings for `def-score' with keywords like :sound, :channel etc. -- except for they key :omn.")

;; NOTE: just some dummy settings for now
(defparameter *default-preview-score-header*
  `(:title "Opus magnum"
    :tempo 80
    :layout ((:bracket
	      ,(violin-layout 'vln :flexible-clef t)
	      ,(violoncello-layout 'vlc :flexible-clef t)))
    :time-signature ((4 4 1)))
  "Global score settings used by `preview-score'. The format is a plist where keys are the instrument labels, and values a list with the actual settings. The format is the same as the header settings for `def-score' with keywords like :title, :key-signature etc.")

(defparameter *preview-score-return-value*
  :headerless-score 
  "Controls the return value of function preview-score. 

  Possible values are
  - :headerless-score - the input score to preview-score
  - :full-score - the full def-score expression generated
  - :score - the resulting score object")



(rest (getf *default-preview-score-header* :layout))

(defun preview-score (score &key (name 'test-score)    
			      (instruments *default-preview-score-instruments*)
			      (header *default-preview-score-header*))
  "Notates and plays a score in a format slightly simpler than expected by def-score, i.e., without its header. 
    
    Args: 
    - score (plist): a headerless score. See below for its format.
    - name (symbol): The score name.
    - instruments (plist): Keys are instrument labels, and values a list with the actual settings. These settings have the same format as instrument settings for `def-score' with keywords like :sound, :channel etc. -- except for they key :omn. This list can contain more instruments than actually contained in score (e.g., settings for a full orchestra), but only the instruments actually contained in `score' are actually given to the internal `def-score' call. 
    - header (plist): The format is the same as the header settings for `def-score' with keywords like :title, :composer, :key-signature etc. 
      Note: any time signature sequence given in the `header' that is not long enough for the full score is automatically cycled as a sequence to the required length (i.e., not only the last time signature but the whole sequence is repeated). 

    Score format: 
    ;;; (<part1-name-keyword> <part1-OMN>
    ;;;  <part2-name-keyword> <part2-OMN>
    ;;;  ...) 

    
* Examples:

   Example using the two default instrument names, predefined with `*default-preview-score-instruments*' and `*default-preview-score-header*'.

;;; (preview-score
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))

  Example showing how to define your own instrument specifications. These specifications can either be directly handed to preview-score, as shown below, or you can overwrite `*default-preview-score-instruments*' and `*default-preview-score-header*' accordingly.
  NOTE: when specifying the score :layout as header argument, list instrument names as symbols in the Opusmodus package and *not* keywords as in the headerless score. See the example below.

;;; (preview-score
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
;;;  :instruments '(:vln (:program 'violin :sound 'gm)
;;; 		    :vlc (:program 'cello :sound 'gm))
;;;  :header `(:layout (,(bracket-group (violin-layout 'vln)
;;;                                     (violoncello-layout 'vlc)))))

  The return value is controlled by {defparameter *preview-score-return-value*}

  Polyphonic parts can be expressed by simply using the same instrument name multiple times (again using default instrument names).

;;; (preview-score
;;;  '(:vln ((q g5) (q. c6 e d6 q e6 f6) (h. e6)) ; octave-doubling
;;;    :vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) 
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))


  NOTE: the new builtin Opusmodus function `ps' does something very similar, see 
https://opusmodus.com/forums/topic/1206-opusmodus-1324622/
  "
  ;; Using eval is problematic (https://stackoverflow.com/questions/2571401/why-exactly-is-eval-evil/),
  ;; but hard to avoid for a dynamically created def-score expression that requires splicing with ,@.
  ;; Possible alternative would be to define preview-score as macro, but then arguments are not evaluated.
  (let* ((score-instruments (tu:properties score))
	 ;; only use instruments actually in score
	 (actual-instruments (mappend #'(lambda (p) (list p (getf instruments p)))
				      score-instruments))
	 #| ;;; TMP: not yet working, hence commented
	 (time-sig-lengths (time-signature-length (getf header :time-signature)))
	 (max-part-dur (apply #'max (tu:at-odd-position 
				     (map-parts-equally score
							#'total-duration
							'(_)))))
	 (time-signature (length->time-signature
			  (length-span (/ max-part-dur (apply #'+ time-sig-lengths))				       
				       time-sig-lengths)))
	 |#
	 (full-header (append (tu:update-properties  
			       header
			       ;;; TMP: not yet working, hence commented
			       ;; :time-signature (list time-signature)
			       :layout
			       ;; collect only layouts of instruments actually present in score
			       ;;; TODO: remove also unused parts in brace etc groups
			       (labels ((collect-present-layouts (layouts)
					  (tu:mappend  ;; all return values listed -- nils removed by append
					   #'(lambda (spec)
					       ;; (break "Break lambda in")
					       (cond (;; recursion in case of groups
						      (and (listp spec)
							   (member (first spec)
								   '(:brace :bracket :square)))
						      ;; (break "Break group")
						      (let ((collected-layouts (collect-present-layouts (rest spec))))
							;; (break "Break after recursive collect-present-layouts call")
							;; remove complete group if all its instruments were removed
							(unless
							    ;; only naming details left like (:name "Organ" :abbr "Org.")
							    (every #'(lambda (x) (or (keywordp x) (stringp x)))
								   collected-layouts)
							  (list (cons (first spec) collected-layouts)))))
						     (;; instrument spec like (:treble violin :name "Violin" :abbr "Vln.")
						      (and (listp spec)
							   (keywordp (first spec))
							   (member (intern (symbol-name (second spec))
									   :keyword)
								   score-instruments))
						      ;; (break "Break instrumet")
						      (list spec))							 
						     (;; layouts was only a single instrument layout taken apart by mappend
						      (or (symbolp spec) (stringp spec))
						      ;; (break "Break symbols or string")
						      (list spec))							 
						     (;; everything else is removed by mappend
						      T
						      ;; (break "Break else")
						      nil)))
					   layouts)))
				 ;; wrap in list for append
				 (list (collect-present-layouts (getf header :layout)))
				 ))
			      ;; add default vals of required header args at end -- they are overwritten by args given
			      (tu:remove-properties
			       (tu:properties header)
			       (list :key-signature 'atonal
				     ;; By default, use implicit time signature of 1st part
				     :time-signature (om:get-time-signature (second score))
				     :tempo 70))))
	 ;; (break)
	 (full-score 
	  `(def-score ,name 
	       ;; header args must be quoted...
	       ,(mapcar #'(lambda (x) `',x)					
			full-header)
	     ,@(mapcar #'(lambda (part)
			   (let ((part-symbol (first part))
				 (part-omn (second part)))
			     (list* (keyword-to-om-symbol part-symbol)
				    :omn `(quote ,part-omn)
				    (getf actual-instruments part-symbol))))
		       (tu:plist->pairs (merge-equal-instrument-parts score))))))
    ;; (break)
    (eval full-score)
    (audition-musicxml-last-score)
    (case *preview-score-return-value*
      (:headerless-score score)
      (:full-score full-score)
      (:score *last-score*))))



#|
(preview-score
 '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
   :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
 :instruments '(:vln (:program 'violin :sound 'gm)
	        :vlc (:program 'cello :sound 'gm))
 :header '(:title "Opus magnum"
	   :tempo 80))


'(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
  :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))

|#
  


(defun generate-parts (fn part-args &key 
                          (shared-args nil))
  "Generate multiple parts of a polyphonic score by calling a function returning musical material multiple times for multiple parts.  
  
  Returns a headerless score. {defun preview-score} describes this format. 

* Arguments:
  - fn: A function returning an OMN sequence or a sequence of lengths.
  - part-args (plist): Alternating instrument keywords followed by arguments list for `fn' for that instrument/part. 
  - shared-args (list): For all instruments/parts, these arguments are appended at end end of its part-specific arguments. They are useful, e.g., for keyword arguments. 
  
 
* Examples:

A random rhythm generated by calling gen-white-noise and vector-to-length with different arguments for different parts.
;;; (generate-parts
;;;   #'(lambda (n length low high) 
;;;       (vector-to-length length low high (gen-white-noise n)))
;;;   '(:vln (10 1/12 1 4)
;;;     :vlc (12 1/16 1 4)))

A random rhythm using seed and shared arguments.
;;; (generate-parts
;;;   #'(lambda (seed n length low high) 
;;;       (vector-to-length length low high (gen-white-noise n :seed seed)))
;;;   '(:vln (1)
;;;     :vlc (2))
;;;   :shared-args '(10 1/12 1 4))


* Notes: 

A polyphonic score of only pitches or other parameters without lengths can be generated, but not previewed.

;;; (preview-score
;;;  (generate-parts
;;;   #'(lambda (n range) 
;;;       (vector-to-pitch range (gen-white-noise n)))
;;;   '(:vln (10 (a3 a4))
;;;     :vlc (12 (g2 c4)))))
;;; > Error: No length specified before first pitch 
"
  (tu:pairs->plist 
   (loop 
     for instrument-arg-pair in (tu:plist->pairs part-args) 
     for instrument = (first instrument-arg-pair)
     for fn-args = (second instrument-arg-pair) 
     collect (cons instrument
                   (list (apply fn (append fn-args shared-args)))))))


#|
;; random rhythm 1
(preview-score
 (generate-parts
  #'(lambda (n length low high) 
      (vector-to-length length low high (gen-white-noise n)))
  '(:vln (10 1/12 1 4)
    :vlc (12 1/16 1 4))))

;; random rhythm 2 using seed and shared arguments
(preview-score
 (generate-parts
  #'(lambda (seed n length low high) 
      (vector-to-length length low high (gen-white-noise n :seed seed)))
  '(:vln (1)
    :vlc (2))
  :shared-args '(10 1/12 1 4)))


;; NOTE: a polyphonic score of just pitches and other parameters without lengths can be generated, but not previewed.
(preview-score
 (generate-parts
  #'(lambda (n range) 
      (vector-to-pitch range (gen-white-noise n)))
  '(:vln (10 (a3 a4))
    :vlc (12 (g2 c4)))))
> Error: No length specified before first pitch
 
|#


;;; TODO:
;; - add support for arg swallow: swallow parameters like pitches falling on rests
;; - have keyargs input-parameter and output-parameter separately, at least as options
(defun map-parts (score fn part-args &key 
				       (parameter nil) 
				       (input-parameter nil)
				       (output-parameter nil)
				       (shared-args nil)
				       (flatten nil)
				       (swallow nil))
  "Create or transform a polyphonic score. The original purpose is for creating/transforming musical textures, i.e., relations between polyphonic parts.

  Applies function `fn' to parts in `score': this function is a variant of the standard Lisp function `mapcar', but specialised for scores. A score is represented in the format discussed in the documentation of the function `preview-score'.     
    
    Additional arguments for `fn' can be specified in `part-args', and these argument lists can be different for each part. However, one argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. In the following example, the function `pitch-transpose' is applied to a score with two parts. This function has two required arguments, a transposition interval (measured in semitones), and the pitch sequence or OMN to transpose. The transposition interval for the first part is 4 (major third upwards), and the underscore marks the position of the violin part to transpose, etc. 
 
;;; (map-parts '(:vln ((h e4)) 
;;; 	         :vlc ((h c3))) 
;;; 	   #'pitch-transpose  
;;; 	   '(:vln (4 _)  
;;; 	     :vlc (12 _)))

* Arguments:
  - score (headerless score): See {defun preview-score} for format description. 
  - fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
  - part-args (plist): Alternating instrument keywords (same as in `score') followed by arguments list for `fn' for that instrument/part. If arguments list is :skip for any instrument, then that part is returned unchanged. If `score' contains the same instrument multiple times for expressing polyphony, `part-args' can also specify arguments for these multiple parts with the same name separately.
  - parameter (omn parameter, e.g., :length or :pitch, default nil means processing full OMN expression): If `fn' expects only a single OMN parameter to process, then it can be set here. Otherwise, `fn' expects full OMN sequences. 
  - input-parameter (omn parameter): Same as `parameter', but the parameter is only set for `fn' argument -- the parameter returned by `fn' can be set separately. If both `input-parameter' and `parameter' are nil, or `input-parameter' is set to :all, then `fn' expects full OMN sequences.
  - output-parameter (omn parameter): Same as `parameter', but the parameter is only set for `fn' results that are then inserted into the resulting score -- the parameter expected by `fn' can be set separately. If both `output-parameter' and `parameter' are nil, or `output-parameter' is set to :all, then `fn' returns full OMN sequences.
  - shared-args (list): For all instruments/parts, these arguments are appended at end end of its part-specific arguments. They are useful, e.g., for keyword arguments. 
  - flatten (Boolean): If T, the parameter sequence -- encoded by `_' in the argument lists, is flattened before the `fn' is applied.
  - swallow (Boolean): If T (and `parameter' or `input-parameter' is set to any parameter except :length), then those parameter values that fall on rests are 'swallow', i.e., skipped. For example, if `parameter' is set to :pitch, and there are rests in the score, then generated/transforemd pitches that would fall on rests are left out.
    

* Examples:

Metric shifting of parts, for details see the documentation of {defun metric-shift}

;;; (map-parts '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;              :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
;;;             #'metric-shift
;;;             '(:vln (-q _)
;;;               :vlc (-h _)
;;;              ))

An OMN expression to use by several further examples below.

;;; (setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -) (-5h - g4 pp leg eb4 < leg d4 < leg) (q bb4 < e4 <) (5h g4 mp> leg b4 > leg a4 > leg bb4 > leg d4 > leg) (q gs4 pp -)))

A strict canon formed with the given material (without any counterpoint rules :)  For different parts the material is metrically shifted and transposed.

Note also that map-parts calls can be nested.

;;; (map-parts 
;;;   (map-parts `(:vl1 ,material
;;;                :vl2 ,material  
;;;                :vla ,material
;;;                :vlc ,material)
;;;              #'metric-shift 
;;;              '(:vl1 :skip ;; leave part unchanged
;;;                :vl2 (-q _)  
;;;                :vla (-h _)
;;;                :vlc (-h. _)))
;;;   #'pitch-transpose
;;;   '(:vl1 (6 _) 
;;;     :vl2 (4 _)  
;;;     :vla (2 _)
;;;     :vlc :skip)  
;;;   )

Simple homorhythmic texture created by randomised transpositions. Each part shares a similar overall pitch profile.

Note that calls can be more concise with a (lambda) function that nests calls to transformation functions -- instead of nesting map-parts as shown above.

;;; (map-parts `(:vl1 ,material
;;;              :vl2 ,material  
;;; 	     :vla ,material
;;; 	     :vlc ,material)
;;; 	   #'(lambda (transpose seq)
;;; 	       ;; static transposition for moving parts into different registers
;;; 	       (pitch-transpose 
;;; 		transpose 
;;; 		;; randomised transposition of notes in parts
;;; 		(pitch-transpose-n (rnd 10 :low -2 :high 2) seq)))
;;; 	   '(:vl1 (7 _) 
;;; 	     :vl2 (0 _)  
;;; 	     :vla (-10 _)
;;; 	     :vlc (-20 _)))


Homophonic texture created by random pitch variants (retrograde, inversion etc.). The global pitch profiles of parts differ here, in contrast to the previous example. 

;;; (map-parts 
;;;   `(:vl1 ,material
;;;     :vl2 ,material  
;;;     :vla ,material
;;;     :vlc ,material)
;;;   #'pitch-variant 
;;;   `(:vl1 (_ :transpose 7 :seed 10) 
;;;     :vl2 (_ :transpose 0 :seed 20)  
;;;     :vla (_ :transpose -10 :seed 30)
;;;     :vlc (_ :transpose -20 :seed 40))
;;;    :shared-args '(:variant ?))
    "
  ;; catching hard-to-find user errors...
  (let* ((instruments (get-instruments score))
         (missing-instruments (remove-if #'(lambda (arg-instr) (member arg-instr instruments)) (get-instruments part-args))))
    (assert (not missing-instruments)
            (part-args)
            "map-parts: Some instruments in `part-args' don't have a matching instrument in `score'. ~S.~%" missing-instruments))
  (assert (not (find 'quote (flatten part-args)))
          (part-args)
          "map-parts: Arg `part-args' contains quoted expression. ~S.~%" part-args)
  (assert (not (find 'quote (flatten shared-args)))
          (shared-args)
          "map-parts: Arg `shared-args' contains quoted expression. ~S.~%" shared-args)
  (let ((input-parameter (if input-parameter input-parameter parameter))
        (output-parameter (if output-parameter output-parameter parameter))
        (parts-hash (make-hash-table :test #'equal)))
    ;; Fill hash table, using instrument names as keys
    ;; For supporting multiple parts with same name for polyphony:
    ;; Stored data is plist: (:index <int> :omn <list-of-omn-seqs>), where index points to the current omn to use.
    ;; ... Hm, hash table for efficiency, but then internal data structure with list is not very consequent, but for now sufficient.
    (loop for (instr omn) in (tu:plist->pairs score)
       do (if (gethash instr parts-hash)
	      ;; some omn seq(s) for this instrument exists already -- add the current one
	      (setf (gethash instr parts-hash)
		    (list :current 0 :omns (append (getf (gethash instr parts-hash) :omns) (list omn))))
	      ;; new instrument entry
	      (setf (gethash instr parts-hash)
		    (list :current 0 :omns (list omn)))))
    (tu:pairs->plist 
     (loop 
	for (instrument fn-args) in (tu:plist->pairs part-args) 
	for part-omn = (let* ((data (gethash instrument parts-hash))
			  (idx (getf data :current)))
		     (setf (gethash instrument parts-hash)
			   (tu:update-property data :current (1+ idx))) 
		     (nth idx (getf data :omns)))
	collect (if (equal fn-args :skip)
		    (list instrument part-omn) ; no processing
		    (cons instrument
			  (let* ((seq (if (or (not input-parameter)
					      (eql :all input-parameter))
					  part-omn
					  (omn input-parameter part-omn)))
				 (unswallowed-results (apply fn (append (substitute
									 (if flatten (flatten seq) seq)
									 '_ fn-args)
									shared-args)))
				 (lengths (omn :length part-omn))
				 (result (if (and swallow
						  input-parameter
						  (not (or (eql input-parameter :length)
							   (eql output-parameter :length))))
					     (gen-swallow lengths unswallowed-results
							  :flatten flatten)
					     unswallowed-results)))
			    (list 
			     (if (or (not output-parameter)
				     (eql :all output-parameter))
				 result
				 (omn-replace :pitch (span part-omn result) part-omn)
				 #|
				 (copy-time-signature part-omn
						      (progn
							;; (break)
							;; ? BUG: I should not flatten part-omn, it can squash articulations like acc etc.
							;; However, then I need to perhaps format result to follow spanning of part-omn
;;;         result = ((d5 d5 d5 d5) (d5 d5 d5 d5) (d5 d5 d5) (d5 d5 d5 d5 d5 d5 ...) (d5))
;;;         part-omn = ((-q 5w.. c4 5q 5q 5q) (5w.. c4 5q 5q 5q) (q c4 c4 c4) (q c4 c4 5q 5q 5q ...) (h. c4))
							(omn-replace output-parameter (flatten result) (flatten part-omn))))
				 |#
				 )))))
	  ))))

  
;; testing / generating examples

#|
;; simple test
(preview-score
 (map-parts '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
              :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
            #'length-augmentation 
            '(:vln (1 _)
              :vlc (2/3 _))
            ))


(preview-score
 (map-parts '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
              :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
            #'metric-shift
            '(:vln (-q _)
              :vlc (-h _)
             )
            ))

;; setting sounds (General MIDI string quartet :)
(setf *default-preview-score-instruments*
      '(:vl1 (:program 'violin :sound 'gm)
        :vl2 (:program 'violin :sound 'gm)
        :vla (:program 'viola :sound 'gm)
        :vlc (:program 'cello :sound 'gm)))

(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -) (-5h - g4 pp leg eb4 < leg d4 < leg) (q bb4 < e4 <) (5h g4 mp> leg b4 > leg a4 > leg bb4 > leg d4 > leg) (q gs4 pp -)))

;; create strict canon from given material (without any counterpoint rules :)
;; metrically shift and transpose the given material
;; Note also that map-parts calls can simply be nested.
(preview-score
 (map-parts 
  (map-parts `(:vl1 ,material
               :vl2 ,material  
               :vla ,material
               :vlc ,material)
             #'metric-shift 
             '(:vl1 :skip ;; leave unchanged
               :vl2 (-q _)  
               :vla (-h _)
               :vlc (-h. _)))
  #'pitch-transpose
  '(:vl1 (6 _) 
    :vl2 (4 _)  
    :vla (2 _)
    :vlc :skip)      
  ))

;; Simple homorhythmic texture created by randomised transpositions 
;; Each part shares similar overall pitch profile
;; Also, having a function that nests calls to transformation functions -- instead of nesting map-parts as shown above -- calls can be more concise.
(preview-score
 (map-parts `(:vl1 ,material
              :vl2 ,material  
              :vla ,material
              :vlc ,material)
            #'(lambda (transpose seq)
                ;; static transposition for moving parts into different registers
                (pitch-transpose 
                  transpose 
                  ;; randomised transposition of notes in parts
                  (pitch-transpose-n (rnd 10 :low -2 :high 2) seq)))
            '(:vl1 (7 _) 
              :vl2 (0 _)  
              :vla (-10 _)
              :vlc (-20 _))      
            ))


;; Homophonic texture created by random pitch variants (retrograde, inversion etc.)
;; Overall pitch profiles of parts differ 
(preview-score
 (map-parts 
  `(:vl1 ,material
    :vl2 ,material  
    :vla ,material
    :vlc ,material)
  #'pitch-variant 
  `(:vl1 (_ :transpose 7 :seed 10) 
    :vl2 (_ :transpose 0 :seed 20)  
    :vla (_ :transpose -10 :seed 30)
    :vlc (_ :transpose -20 :seed 40))
   :shared-args '(:variant ?)))



------------------

length-expansion-variant

; (setf material '(h q q h))
; (setf material (tuplet-rhythm '(1/2 1/2 1/2) '(3 4 5)))
(setf material (tuplet-walk-rhythm 7 :seed 1 :rest-distances '(9 1 13) :last-bar '(1/4 -1/4)))
;; randomise some given material
(preview-score
 (map-parts 
  `(:vl1 ,material
    :vl2 ,material  
    :vla ,material
    :vlc ,material)
  #'length-expansion-variant
  '(:vl1 (_ :variant p) 
    :vl2 (_ :variant r)  
    :vla (_ :variant i)
    :vlc (_ :variant ri))      
  :shared-args '(:percent 100)
  ))

(length-expansion-variant '(h q e s) :percent 100 :variant '? :seed 1) 

|#


#|

(preview-score
(map-parts '((:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5)))
             (:vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
           #'length-augmentation 
           '((:vln (2 _))
             (:vlc (2 _)))
           ))

;; old examples, revise

(preview-score
(map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin :sound 'gm)
             (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello :sound 'gm))
           #'length-augmentation 
           '((2 _) 
             (2 _)) 
           :ignore '(1)              
           ))

(preview-score
(map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin :sound 'gm)
             (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello :sound 'gm))
           #'length-augmentation 
           '((2 _) 
             (2 _))
           :parameter :length
           )
)

|#           


(defun map-parts-equally (score fn args &key parameter skip)
  "Variant of map-parts where all args are shared args.

* Arguments:
  - score (headerless score): See {defun preview-score} for format description. 
  - fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
  - args: Arguments list for `fn'. One argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. 
  - parameter (omn parameter, e.g., :length or :pitch, default nil means processing full OMN expression): If `fn' expects only single parameter to process, then it can be set here. 
  - skip (list of keywords): instruments to skip unprocessed. 

* Examples:

;;; (map-parts-equally
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
;;;  #'pitch-transpose
;;;  '(2 _)
;;;  :parameter :pitch)
"
  (mappend #'(lambda (instr-part)
               (let* ((instrument (first instr-part))
                      (part-omn (second instr-part))
		      (skipped? (member instrument skip))
                      (result (unless skipped?
				(apply fn
				       (substitute 
					(if parameter
					    (omn parameter part-omn)
					    part-omn)
					'_ args)))))
                 (list instrument
		       (if skipped?
			   part-omn
			   (if parameter
			       (copy-time-signature part-omn
						    (omn-replace parameter (flatten result) (flatten part-omn)))
			       result))))) 
           (tu:plist->pairs score)))


(defun apply-part (instrument score fn args &key (parameter nil))
  "Function applied to a single part of `score'.
  
* Arguments:
  - instrument (symbol): The label of a part in `score'.
  - score (headerless score): See {defun preview-score} for format description. 
  - fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
  - args: Arguments list for `fn'. One argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. 
  - parameter (omn parameter, e.g., :length or :pitch, default nil means processing full OMN expression): If `fn' expects only single parameter to process, then it can be set here. 
  
  
  Example:

;;; (apply-part
;;;  :vlc
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
;;;  #'pitch-transpose
;;;  '(2 _)
;;;  :parameter :pitch)
"
  (let* ((part-omn (get-part-omn instrument score))
         (result (apply fn
                        (substitute 
                         (if parameter
                           (omn parameter part-omn)
                           part-omn)
                         '_ args))))    
    (replace-part-omn instrument 
                      (if parameter
                        (copy-time-signature part-omn
                                             (omn-replace parameter (flatten result) (flatten part-omn)))
                        result)
                      score)))


(defun number-instruments (score)
  "The instrument keyword labels in `score` are replaced by a pair (<keyword> <int>) with a unique int for each of these instruments counting from 0. This allows to clearly distinguish multiple parts with the same instrument label.

* Examples:

;;; (setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))
;;; (number-instruments 
;;;  `(:vl1 ,material
;;;    :vl1 ,(pitch-transpose 12 material)))
"
  (let* (;; store for each instrument a counter
	 (hash (make-hash-table))
	 (pairs (tu:plist->pairs score))
	 (result (loop for (instrument omn) in pairs 
		    collect (let ((idx (gethash instrument hash)))
			      ;; (break)
			      (if idx
				  (let ((new-idx (1+ idx)))
				    (setf (gethash instrument hash) new-idx)
				    (list (list instrument new-idx) omn))
				  (progn
				    (setf (gethash instrument hash) 0)
				    (list (list instrument 0) omn)))))))
    (tu:pairs->plist result)))

#|
(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))

(number-instruments 
 `(:vl1 ,material
   :vl1 ,(pitch-transpose 12 material)))
|#


(defun un-number-instruments (score)
  "The opposite of `number-instruments`: the numbered instruments labels of the form (<keyword> <int>) are replaced by the corresponding keyword."
  (let* ((pairs (tu:plist->pairs score))
	 (result (loop for (instrument omn) in pairs
		      collect (list (first instrument) omn))))
    (tu:pairs->plist result)))


(defun _append-two-parts (score1 score2)
  (let ((instruments (remove-duplicates 
                      (append (get-instruments score1)
                              (get-instruments score2))))
        ;; dur of longest part in score 1 as rest
        (score1-dur-rest (copy-time-signature 
                          ;;; first instrument, assuming all instruments share same metric structure
                          (second score1)
                          (list (list (- (apply #'max (mapcar #'total-duration 
                                                              (tu:at-odd-position score1))))))))
        (score2-dur-rest (copy-time-signature 
                          (second score2) ;;; first instrument
                          (list (list (- (apply #'max (mapcar #'total-duration 
                                                              (tu:at-odd-position score2)))))))))
    (tu:pairs->plist 
     (loop 
       for instr in instruments
       for part-omn1 = (get-part-omn instr score1)
       for part-omn2 = (get-part-omn instr score2)
       collect (cons instr
                     (list 
                      (append (if part-omn1 
                                part-omn1
                                score1-dur-rest)
                              (if part-omn2 
                                part-omn2
                                score2-dur-rest))))))))

;;; TODO: 
;;; - Introduce offset arg for rests before 2nd score. Offset can be negative, resulting in removing rests or even notes (no polyphony)
;;; - see bug below
(defun append-scores (&rest scores)
  "Concatenate multiple scores so that they form a sequence in the resulting score. The OMN expression of instruments that are shared between input scores are appended, while instruments that are missing in some input score are padded with rests for the duration of that score. 

  Note that the function assumes all parts in each score to contain the same overall duration and metric structure.

* Arguments:
  - scores: see {defun preview-score} for format description of headerless scores. 

* Examples:

;;; (setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))
;;;     
;;; (append-scores `(:vl1 ,material
;;;                  :vl2 ,(gen-retrograde material :flatten T))
;;;                `(:vl1 ,material
;;;                  :vlc ,(gen-retrograde material :flatten T))
;;;                `(:vl2 ,material
;;;                  :vlc ,(gen-retrograde material :flatten T)))

BUG: If one part misses hierarchic nesting in contrast to others, then this lating nesting is preserved, which can lead to inconsistent nesting (some sections of a part being nested, others are not).

BUG: Does not yet work with scores that express polyphony with multiple instances of the same instrument.
"
  (reduce #'_append-two-parts scores))
  

#|
;; testing
(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))

(preview-score
 (append-scores `(:vl1 ,material
                 :vl2 ,(gen-retrograde material :flatten T))
               `(:vl1 ,material
                 :vlc ,(gen-retrograde material :flatten T))
               `(:vl2 ,material
                 :vlc ,(gen-retrograde material :flatten T))))
|#



;;; TODO:
;;; - Consider: support leading rests per subscore? Can I do this with append-parts or metric-shift?
(defun mix-scores (&rest scores)
  "Mix multiple scores together to run in parallel. Useful, e.g., for creating melody and polyphonic accompaniment or different textures separately, and then mixing them together. 

* Arguments:
  - scores: see {defun preview-score} for format description of headerless scores. 

* Examples:

;;; (let ((material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -))))
;;;    (mix-scores `(:vln ,material)
;;;                `(:vlc ,(metric-shift '-h material))))
"
  (apply #'append scores))


#|

(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -) (-5h - g4 pp leg eb4 < leg d4 < leg) (q bb4 < e4 <) (5h g4 mp> leg b4 > leg a4 > leg bb4 > leg d4 > leg) (q gs4 pp -)))

;;; TODO: why pitch repetitions?
(preview-score
 (mix-scores `(:vl1 ,material
	       :vl2 ,material)
	     (map-parts 
	      (map-parts 
	       `(:vla ,material
		 :vlc ,material)
	       #'pitch-variant
	       '(:vla (_ :variant r :transpose -7)
		 :vlc (_ :variant r :transpose -12)))
	      #'length-retrograde
	      '(:vla (_)
		:vlc (_))
	      :shared-args '(:flatten T))))

(preview-score
 (mix-scores `(:vl1 ,material
	       :vl2 ,material)
             (map-parts 
              `(:vla ,material
		:vlc ,material)
              #'(lambda (seq variant transpose)
                  (length-retrograde (pitch-variant seq :variant variant :transpose transpose)
                                     :flatten T))
              '(:vla (_ r -7)
                :vlc (_ r -12)))))

;;; unfinished
(preview-score
 (mix-scores `(:vl1 ,material
	       :vl2 ,material)
	     (map-parts 
	      `(:vla ,material
		:vlc ,material)
	      #'gen-retrograde
	      '(:vla (_ :variant r)
		:vlc (_ :variant r))
	      :shared-args '(:transpose -7)
	      )))

|#




(defun merge-equal-instrument-parts (score)
  "If `score' contains multiple instances of the same instrument, then those multiple voices are merged into a polyphonic line (with the time signature taken from the first voice).

* Examples:
  ;;; (merge-equal-instrument-parts 
  ;;;   '(:rh ((q c4 d4 e4 f4))
  ;;;     :rh ((q c5 b4 g4 d4))
  ;;;     :lh ((h c3 g3))))
  ;;; => (:RH ((Q C4C5 D4B4 E4G4 D4F4)) :LH ((H C3 G3)))
"
  (tu:pairs->plist
   (let ((paired-score (tu:plist->pairs score))
	 already-processed-instruments)
     (loop 
	for remainder-pairs on paired-score
	for (instrument omn) = (first remainder-pairs)
	unless (member instrument already-processed-instruments) 
	collect (let ((other-omns (mapcar #'second 
					  (remove-if-not #'(lambda (x)
							     (eql (first x) instrument)) 
							 (rest remainder-pairs)))))
		  (list instrument (if other-omns
				       (progn
					 (setf already-processed-instruments (cons instrument already-processed-instruments))
					 (apply #'merge-voices (cons omn other-omns)))
				       omn)))))))

#| ; tests

(setf paired-list (tu:plist->pairs '(:a 1 :b 1 :a 1 :c 1)))

;;; BUG: must only look at rest of list
(let (merged)
  (loop 
     for remainder-list on paired-list
     for (key value) = (first remainder-list)
     unless (member key merged) 
       collect (let ((other-values (mapcar #'second 
					   (remove-if-not #'(lambda (x)
							      (eql (first x) key)) 
							  (rest remainder-list)))))
		(list key (if other-values
			      (progn
				(setf merged (cons key merged))
				(apply #'+ (cons value other-values)))
			      value)))))

       



|#




;; source: https://opusmodus.com/forums/topic/894-merge-voices-on-barbeat/?tab=comments#comment-2727
(defun merge-voices2 (seq insert bar/beat)
  "Merges multiple monophonic lines resulting in a polyphonic part. 

* Arguments:
  - seq (OMN sequence, must be nested): Voice to which other voices are added. The result retains the time signatures of SEQ. 
  - insert (list of flat OMN sequences): Voices to merge into SEQ. Their time signatures are overwritten by the meter of SEQ.
  - bar/beat (list): List of start times of inserted sequences. 

  Each INSERT start time is specified in the following format, where <bar-number> is a 1-based bar number (an int), <beat-number> is a 1-based beat number (an int), and <beat-subdivision> is the divisor for the beat number (also an int). 

;;; (<bar-number> (<beat-number> <beat-subdivision>))
  
  For example, (3 (2 4)) indicates the 2nd quarter note in the 3rd bar.  

* Examples:

  Merge two OMN sequences.
  
;;; (merge-voices2 '((q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4))
;;;                 '((q a4 a4 a4))
;;;                 '((2 (2 8))))  

  Merge three sequences.
  
;;; (merge-voices2 '((q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4))
;;;                '((q b5 b5 b5)
;;;                  (e a4 a4 a4))
;;;                '((2 (2 8))
;;;                  (3 (2 16))))

* See Also: 

  The built-in function `merge-voices' is similar, but does not support shifting/offsetting added voices in time. 
  "
   (car 
    (last
     (let ((bar) (beat) (distance))
       (progn
         (setf bar (loop for i in bar/beat collect (car i))
               beat (loop for j in bar/beat collect (cadr j)))
         (loop 
           for ba in bar 
           for be in beat
           for ins in insert
           with time-sign = (get-time-signature seq)
           with ord-time-sign = (get-time-signature seq)
           
          do (setf time-sign (if (listp (car time-sign))
                               (loop for i in time-sign
                                 when (> (caddr i) 1)
                                 append (loop repeat (caddr i)
                                          collect (list (car i) (cadr i)))
                                 else collect (list (car i) (cadr i)))
                               (append time-sign))
                   
                   distance (if (listp (car time-sign))
                              (+ (sum (loop repeat (- ba 1)
                                        for i in time-sign
                                        collect (/ (car i) (cadr i))))
                                 (/ (1- (car be)) (cadr be)))
                              (+ (* (1- ba) (/ (car time-sign) (cadr time-sign)))
                                 (/ (1- (car be)) (cadr be)))))

           do (setf ins (append (list (neg! distance)) ins))
           do (setf seq (omn-to-time-signature 
                         (length-rest-merge 
                          (flatten (merge-voices (omn-merge-ties seq) ins)))
                          ord-time-sign))
           collect seq
           do (setf time-sign ord-time-sign)))))))



#|
(merge-voices2 '((q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4))
                '(((q a4 a4 q a4 a4)))
                '((2 (2 4))))


(merge-voices2 '((q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4))
                '((q a4 a4 a4))
                '((2 (2 8))))

(merge-voices2 '((q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4) (q c4 c4 c4 c4))
               '((q b5 b5 b5)
                 (e a4 a4 a4))
               '((2 (2 8))
                 (3 (2 16))))
|#




#|

;;; Unfinished
;; TODO:
;;; - Consider: support leading rests per subscore? Can I do this with append-parts or metric-shift?
(defun merge-parts (offset-score1 offset-score2)
  ;;; TODO: revise for arbitrary nuymber of scores
  ;; (&rest offset-score-pairs)
  ;;; TODO: revise doc
  "Mix multiple scores together to run in parallel. Useful, e.g., for creating melody and polyphonic accompaniment or different textures separately, and then mixing them together. 

  Args
  - scores: see {defun preview-score} for format description of headerless scores. 

  Example:

;;; (let ((material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -))))
;;;    (mix-parts `(:vln ,material)
;;;               `(:vlc ,(metric-shift '-h material))))
"
  (Let ((score1 (second offset-score1))
        (score2 (second offset-score2)))
    (reduce #'(lambda (myscore instr-omn)
                (let ((instr (first instr-omn))
                      (seq (second instr-omn)))                    
                  (if (find instr myscore)
                    (tu:update-property myscore instr (merge-voices (getf myscore instr) 
                                                                    (getf score2 instr)))
                    (append myscore (list instr seq)))))
            (tu:plist->pairs score2)
            :initial-value score1)
    ))
; metric-shift

; (metric-shift 0 '((q c4)))

;;; TMP test
(merge-parts '(0 (:vln (q e4)))
             '(0 (:vln (q g4) :vlc (q c3))))

|#

#|

(merge-parts 0 '(:vln (q e4)) 0 '(:vln (q g4)) 0 '(:vlc (q c3)))
(merge-parts 0 '(:vln (q e4)) 0 '(:vln (q g4)) 0 '(:vlc (q c3)))


(setf voice1 '(-h w c2)
      voice2 '(h c4 d4)
      voice3 '(-q q g5 = = =))

(merge-voices voice1 voice2 voice3)

|#


(defun get-instruments (score)
  "Returns all instruments of `score', a headerless score (see {defun preview-score} for its format)."
  (tu:at-even-position score))

#|
(get-instruments '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
                   :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
|#


;;; TODO: add optional arg specifying which 0-based number of equally-named parts to return
(defun get-part-omn (instrument score)
  "Returns the part (OMN expression) of `instrument' in `score', a headerless score (see {defun preview-score} for its format).

BUG: does not yet allow access to higher position of equally named parts."
  (getf score instrument))

#|
(get-part-omn  :vlc
'(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
  :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))

(get-part-omn  :test
'(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
  :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
|#

(defun get-parts-omn (score)
  "Returns a list of all OMN parts of `score', a headerless score (see {defun preview-score} for its format)."
  (tu:at-odd-position score))

#|
(get-parts-omn '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
                 :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
|#


;;; TODO: add optional arg specifying which 0-based number of equally-named parts to return
(defun replace-instruments (new old score)
  "Replaces old instruments by new instruments in a score.

* Arguments:
  - new (list of keywords): New instruments to use.
  - old (list of keywords): List of instruments in `score' to be replaced. Must have same length as `new'.
  - score (a headerless score): See {defun preview-score} for its format.

* Examples:

;;; (replace-instruments '(:trp :trb) '(:vl2 :vlc)
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3)))
"
  (reduce #'(lambda (score new-old) (substitute (first new-old) (second new-old) score))
          (matrix-transpose (list new old))
          :initial-value score))

;;; TODO: add optional arg specifying which 0-based number of equally-named parts to return
(defun replace-part-omn (instrument new-part score)
  "Replaces the part (OMN expression) of `instrument' in `score' with `new-part'.

* Examples:

;;; (replace-part-omn  :vlc
;;;  '((h g3))
;;;  '(:vln ((h e4))
;;;    :vlc ((h c3))))
"
  (let ((score-copy (copy-seq score)))
    (setf (getf score-copy instrument) new-part)
    score-copy))

#|
(replace-part-omn  :vlc
 '((h g3))
 '(:vln ((h e4))
   :vlc ((h c3))))
|#



;;; TODO: add optional arg specifying which 0-based number of equally-named parts to return
(defun split-part (instrument orig-score score-to-insert)
  "Replaces part of `instrument' in `orig-score' with `score-to-insert'.

* Examples:

;;; (split-part :vl2
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3))
;;;  '(:vl2_div1 (h f4)
;;;    :vl2_div2 (h e4)))

Split divisi strings into parts

;;; (setf my-score
;;;     '(:vl1 (h g4b4 g4a4)
;;;       :vl2 (h e4 d4)
;;;       :vla (h c4 b3)
;;;       :vlc (h c3 g3)))

;;; (setf divisi-part (single-events (pitch-melodize (get-part-omn :vl1 my-score))))

;;; (preview-score
;;;  (split-part :vl1
;;;              my-score
;;;              `(:vl1_div1 ,(flatten (tu:at-even-position divisi-part))
;;;                :vl1_div2 ,(flatten (tu:at-odd-position divisi-part)))))
"
  (let ((instr-position (position instrument orig-score)))
    (append (subseq orig-score 0 instr-position) 
            score-to-insert 
            (subseq orig-score (+ instr-position 2)))))

#|
(split-part :vl2
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3))
 '(:vl2_div1 (h f4)
   :vl2_div2 (h e4)))

;; split divisi strings into parts
(setf my-score
      '(:vl1 (h g4b4 g4a4)
        :vl2 (h e4 d4)
        :vla (h c4 b3)
        :vlc (h c3 g3)))

(preview-score my-score)

(setf divisi-part (single-events (pitch-melodize (get-part-omn :vl1 my-score))))

(preview-score
 (split-part :vl1
             my-score
             `(:vl1_div1 ,(flatten (tu:at-even-position divisi-part))
               :vl1_div2 ,(flatten (tu:at-odd-position divisi-part)))))


|#

;;; TODO: add optional arg specifying which 0-based number of equally-named parts to return
(defun remove-part (instrument score)
  "Removes `instrument' and its OMN expression from `score'.

* Examples:

;;; (remove-part :vl2
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3)))
"
  (let ((instr-position (position instrument score)))
    (if instr-position
      (append (subseq score 0 instr-position) 
              (subseq score (+ instr-position 2)))
      score)))
  
#|
(remove-part :vl2
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))

(remove-part :test
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))
|#

;;; TODO: add format specifying which 0-based number of equally-named parts to return
(defun remove-parts  (instruments score)
  "Removes all `instruments' and their OMN expressions from `score'.

* Examples:

;;; (remove-parts '(:vl1 :vl2)
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3)))
"
  (reduce #'(lambda (score instrument) (remove-part instrument score)) 
          instruments :initial-value score))

#|
(remove-parts '(:vl1 :vl2)
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))
|#

;;; TODO: add format specifying which 0-based number of equally-named parts to return
(defun extract-parts (instruments score)
 "Extracts all `instruments' and their OMN expressions from `score'. 

* Examples:

;;; (extract-parts '(:vl1 :vla)
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3)))
"
  (apply #'append
         (mapcar #'(lambda (instrument) (list instrument (getf score instrument)))
                 instruments)))

#|
(extract-parts '(:vl1 :vla)
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))
|#


;;; TODO: add format specifying which 0-based number of equally-named parts to return
(defun reorder-parts (instruments-in-order score)
  "Changes the order of instruments in `score' to the order given in `instruments-in-order'. Only the instruments included in `instruments-in-order' are parts of the resulting score."
  (mappend #'(lambda (instr)
	       (list instr (getf score instr)))
	   instruments-in-order))


(defun unify-time-signature (instrument-with-time-signature score)
  "Rebar music of all parts in `score' to match the meter of `instrument-with-time-signature'."
  (let ((part-with-time-sig (get-part-omn instrument-with-time-signature score)))
    (map-parts-equally score
		       #'(lambda (part)
			   (copy-time-signature part-with-time-sig part))
		       '(_)
		       :skip '(instrument-with-time-signature))))


(defun split-score-at-shared-rests (score)
  "Splits headerless `score' into list of headerless scores. `score' is split at the end of every bar, where each part has either a rest at the end of this bar or at the beginning of the next bar.

  The bar positions (zero-based bar numbers) at which the score was split is returned as second value.

  This function is useful to split longer input scores, for which the function `revise-score-harmonically' could take a long time.

  All parts in score must share the same time signatures (nesting). You may want to first use `unify-time-signature' if necessary.

* Examples:

;;; (split-score-at-shared-rests
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q))
;;;    :vlc ((q g3) (q c4 b3 h a3) (-q c3 d3))))
;;; => ((:vln ((q g4) (q. c5 e d5 q e5 -q))
;;;      :vlc ((q g3) (q c4 b3 h a3)))
;;;     (:vln ((h e5 -q))
;;;      :vlc ((-q c3 d3))))
;;;    (0 2)
"
  (let* ((instruments (get-instruments score))
	 (parts (get-parts-omn score))
	 ;; collect for each part in score the position of all bars that end with a rest, or where the next bar starts with a rest
	 (parts-bar-positions-starting-with-rest
	  (mapcar #'(lambda (part)
		      (tu:positions-if #'length-restp (omn :length part) :key #'first))
		  parts))
	 (parts-bar-positions-ending-with-rest
	  (mapcar #'(lambda (part)
		      (tu:positions-if #'length-restp (omn :length part) :key #'(lambda (xs) (first (last xs)))))
		  parts))
	 (parts-splitable-bar-positions
	  (mapcar #'(lambda (part-bar-positions-starting-with-rest part-bar-positions-ending-with-rest)
		      (sort (remove-duplicates (append part-bar-positions-starting-with-rest
						       (mapcar #'1+ part-bar-positions-ending-with-rest)))
			    #'<))
		  parts-bar-positions-starting-with-rest
		  parts-bar-positions-ending-with-rest))
	 ;; extract those positions that are shared by all parts
	 (shared-splitable-bar-positions
	  (cons 0 ; ensure positions start with 0
	   (remove 0
		  (remove nil
			  (mapcar #'(lambda (pos)
				      (when (every #'(lambda (other-parts-splitable-bar-positions)
						       (member pos other-parts-splitable-bar-positions))
						   (rest parts-splitable-bar-positions))
					pos))
				  (first parts-splitable-bar-positions))))))
	 ;; split each part in subseqs at those positions
	 (split-parts
	  (mapcar #'(lambda (part) (tu:subseqs part shared-splitable-bar-positions))
	    parts)))
    ;; construct new scores for those subseqs
    (values 
     (mapcar #'(lambda (section-parts)
		 (tu:pairs->plist (mapcar #'list instruments section-parts)))
	     (tu:mat-trans split-parts))
     shared-splitable-bar-positions)))

; (split-score-at-shared-rests full-score)
;; (multiple-value-list (split-score-at-shared-rests full-score))


(defun split-score-at-bar-boundaries (score &optional positions)
  "Splits headerless `score' into list of headerless scores. `score' is split at the end of bars given at `positions'.

  This function is useful to split longer input scores, for which the function `revise-score-harmonically' could take a long time.

* Arguments:
  - score: a headerless score
  - positions (single integer; list of integers; or NIL): zero-based representation indicating after which bars score should be split.
    If positions is...
   -- A list of integers, the score is split after each indicated bar number.
   -- A single integer `n', the score is split after every `n' bars. For example, if `n' is 2, the score is split after every 2nd bar.
   -- `nil' (default), the score is split after every bar up to the end of the score

  All parts in score must share the same time signatures (nesting). You may want to first use `unify-time-signature' if necessary.

  The bar positions (zero-based bar numbers) at which the score was split is returned as second value.
"
  (let* ((number-of-bars (apply #'max (mapcar #'length (get-parts-omn score))))
	 (actual-positions (cond ((and positions (listp positions) (every #'numberp positions)) ; position is list of numbers
				  positions)
				 ((integerp positions)
				  (om:gen-integer 0 (1- number-of-bars) positions))
				 (T
				  (om:gen-integer 0 (1- number-of-bars)))))
	 (split-parts
	  (mapcar #'(lambda (part) (tu:subseqs part actual-positions))
		  (get-parts-omn score))))
    (values
     (mapcar #'(lambda (section-parts)
		 (tu:pairs->plist (mapcar #'list (get-instruments score) section-parts)))
	     (tu:mat-trans split-parts))
     actual-positions)
    ))


#|
(split-score-at-bar-boundaries 
 '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q)) 
   :lh ((q g3) (q c4 b3 h a3) (-q c3 d3)))
 '(0 1 2))

(split-score-at-bar-boundaries 
 '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q)) 
   :lh ((q g3) (q c4 b3 h a3) (-q c3 d3)))
 2)

(split-score-at-bar-boundaries 
 '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q)) 
   :lh ((q g3) (q c4 b3 h a3) (-q c3 d3))))

|#


#|
;; Fun circle-repeat-length-aux can be replaced by length-span, sigh. 
;; This code could be removed, but use of loop is nice example for later.

(defun circle-repeat-length-aux (lengths duration)
  "Repeats (or cuts) sequence to fit exactly into given duration.

  Arg:
  - lengths (OMN sequence) -- TODO -- currently only flat length list supported
  - duration (OMN length value)"  
  (let* (;;; TODO: hashtable more efficient
	 (flat-lengths (omn :length ;; ensure numeric values -- might be removed later
			    (flatten lengths)))
	 ;; ensure numeric values -- might be removed later
	 (duration-num (first (omn :length (list duration)))) 
	 (length-no (length flat-lengths))
	 (i 0))
    (loop
       for l = (nth (mod i length-no) flat-lengths)
       collecting l into result
       summing l into accum
       when (>= accum duration-num) return (if (= accum duration-num)
					   result
					   (let* ((result-butlast (butlast result))
						  ;; NOTE: accumulating again -- inefficient
						  (accum (apply #'+ result-butlast)))
					     (append result-butlast
						     (list (- duration-num accum)))))
       do
	 (setf i (1+ i)))))

;; (circle-repeat-length-aux '(1/2 1/8 1/8) 19/9)
;; (circle-repeat-length-aux '(h e e) 'wwe.)

; (length-span 'wwe. '(h e e))

(length-span 'wwe. '((h c4) (e d4 e e4)))

|#





(defun score-duration (score)
  "Returns total duration of `score'."
  (apply #'max (mapcar #'total-duration (get-parts-omn score))))

#|
(score-duration
 '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q)) 
   :lh ((q g3) (q c4 b3 h a3) (-q c3 d3))
   :ped ((h c3))))
|#


(defun unify-part-durations (score)
  "Ensure all parts of `score' are of the same duration by effectively looping shorter parts until they are as long as the longest part. The resulting time signatures of all parts follows that of the the first longest part. You may use the function `unify-time-signature' before if you want to enforce time signatures of a different part instead.

An accordingly revised score is returned as first, and the full duration of `score' as a second value."
  (let* ((parts (get-parts-omn score))
	 (part-durs (mapcar #'total-duration parts))	  
	 (dur (apply #'max part-durs))
	 (longest-part (nth (position dur part-durs) parts))	 
	 (time-sigs (get-time-signature longest-part))	 
	 (corrected-parts
	  (mapcar #'(lambda (part)
		      (om:omn-to-time-signature (om:length-span dur (om:flatten part))
						time-sigs))
		  parts)))
    (values
     (tu:mappend #'list		
		 (get-instruments score)
		 corrected-parts)
     dur)))

#|
(unify-part-durations
  '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q -q)) 
    :lh ((q g3) (q c4 b3 h a3) (-q c3 d3))
    :ped ((h c3))))

;; BUG:
;; If some part is only single bar or flat list (e.g., underlying harmony), then other parts may be changed to same time sig. That part must not be longest part.
(unify-part-durations
  '(:rh ((q g4) (q. c5 e d5 q e5 -q) (h e5 -q -q)) 
    :lh ((q g3) (q c4 b3 h a3) (-q c3 d3))
    :ped ((wwww c3))))
|#
