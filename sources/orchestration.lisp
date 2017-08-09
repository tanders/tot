;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


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
  '(:title "Opus magnum"
    :tempo 80)
  "Global score settings used by `preview-score'. The format is a plist where keys are the instrument labels, and values a list with the actual settings. The format is the same as the header settings for `def-score' with keywords like :title, :key-signature etc.")

(defun preview-score (score &key (name 'test-score)    
			    (instruments *default-preview-score-instruments*)
			    (header *default-preview-score-header*))
  "Notates and plays a score in a format slightly simpler than expected by def-score, i.e., without its header. 
    
    Args: 
    - score (plist): a headerless score. See below for its format.
    - name (symbol): The score name.
    - instruments (plist): Keys are instrument labels, and values a list with the actual settings. These settings have the same format as instrument settings for `def-score' with keywords like :sound, :channel etc. -- except for they key :omn.
    - header (plist): The format is the same as the header settings for `def-score' with keywords like :title, :composer, :key-signature etc. 
    

    Score format: 
    ;;; (<part1-name-keyword> <part1-OMN>
    ;;;  <part2-name-keyword> <part2-OMN>
    ;;;  ...) 

    
Example:

;;; (preview-score
;;;  '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
;;;    :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
;;;  :instruments '(:vln (:program 'violin :sound 'gm)
;;; 	            :vlc (:program 'cello :sound 'gm))
;;;  :header '(:title \"Opus magnum\"
;;; 	       :tempo 80))
  "
  ;; Using eval is problematic (https://stackoverflow.com/questions/2571401/why-exactly-is-eval-evil/),
  ;; but hard to avoid for a dynamically created def-score expression that requires splicing with ,@.
  ;; Possible alternative would be to define preview-score as macro, but then arguments are not evaluated. 
 (eval 
   `(def-score ,name 
      ;; quote all header args, because symbol values must be quoted...
      ,(mapcar #'(lambda (x) `',x)					
	       (append header
		       ;; add default vals of required header args at end -- they are overwritten by args given
		       (list :key-signature 'atonal
			     ;; By default, use implicit time signature of 1st part
			     :time-signature (om:get-time-signature (second score))
			     :tempo 70)))
      ,@(mapcar #'(lambda (part)
		    (let ((part-symbol (first part))
			  (part-omn (second part)))
		       (list* part-symbol 
			      :omn `(quote ,part-omn)
			      (getf instruments part-symbol))))
		(tu:plist->pairs score)))
   )
  (audition-musicxml-last-score)
  *last-score*)


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
  

(defun map-parts (score fn part-args &key 
			(parameter nil) 
                        (shared-args nil))
  "Create or transform a polyphonic score. The original purpose is for creating/transforming musical textures, i.e., relations between polyphonic parts.

  Applies function `fn' to parts in `score': this function is a variant of the standard Lisp function `mapcar', but specialised for scores. A score is represented in the format discussed in the documentation of the function `preview-score'.     
    
    Additional arguments for `fn' can be specified in `part-args', and these argument lists can be different for each part. However, one argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. In the following example, the function `pitch-transpose' is applied to a score with two parts. This function has two required arguments, a transposition interval (measured in semitones), and the pitch sequence or OMN to transpose. The transposition interval for the first part is 4 (major third upwards), and the underscore marks the position of the violin part to transpose, etc. 
 
;;; (map-parts '(:vln ((h e4)) 
;;; 	     :vlc ((h c3))) 
;;; 	   #'pitch-transpose  
;;; 	   '(:vln (4 _)  
;;; 	     :vlc (12 _)))

    Args:
    - score (headerless score): See {defun preview-score} for format description. 
    - fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
    - part-args (plist): Alternating instrument keywords (same as in `score') followed by arguments list for `fn' for that instrument/part. If arguments is :skip, then that part is returned unchanged. 
    - parameter (omn parameter, e.g., :length or :pitch, default nil means processing full OMN expression): If `fn' expects only single parameter to process, then it can be set here. 
    - shared-args (list): For all instruments/parts, these arguments are appended at end end of its part-specific arguments. They are useful, e.g., for keyword arguments. 

    
    Examples:

Metric shifting of parts (see {defun metric-shift}).

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
  ;; catching hard-to-find user error...
  (let* ((instruments (get-instruments score))
         (missing-instruments (remove-if #'(lambda (arg-instr) (member arg-instr instruments)) (get-instruments part-args))))
    (assert (not missing-instruments)
            (part-args)
            "map-parts: Some instruments in `part-args' don't have a matching instrument in `score'. ~A.~%" missing-instruments))  
  (let ((parts (make-hash-table :test #'equal)))
    ;; fill hash table, using leading keywords as keys
    (loop for part in (tu:plist->pairs score)
      do (setf (gethash (first part) parts) part))
    (tu:pairs->plist 
     (loop 
       for instrument-arg-pair in (tu:plist->pairs part-args) 
       for instrument = (first instrument-arg-pair)
       for part = (gethash instrument parts)
       for part-omn = (second part)
       for fn-args = (second instrument-arg-pair) 
       collect (if (equal fn-args :skip)
                 part ; no processing
                 (cons instrument
                       (let ((result (apply fn (append (substitute 
                                                        (if parameter
                                                          (omn parameter part-omn)
                                                          part-omn)
                                                        '_ fn-args)
                                                       shared-args))))
                         (list 
                          (if parameter
                            (omn-replace parameter result part-omn)
                            result)))))
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


(defun generate-parts (fn part-args &key 
                          (shared-args nil))
  "Generate multiple parts of a polyphonic score by calling a function returning musical material multiple times for multiple parts.  
  
  Returns a headerless score. {defun preview-score} describes this format. 

  Args:
  - fn: A function returning an OMN sequence or a sequence of lengths.
  - part-args (plist): Alternating instrument keywords followed by arguments list for `fn' for that instrument/part. 
  - shared-args (list): For all instruments/parts, these arguments are appended at end end of its part-specific arguments. They are useful, e.g., for keyword arguments. 
  
 
Examples:

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


NOTE: A polyphonic score of only pitches or other parameters without lengths can be generated, but not previewed.

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
(defun append-parts (&rest scores)
  "Concatenate multiple scores so that they form a sequence in the resulting score. The OMN expression of instruments that are shared between input scores are appended, while instruments that are missing in some input score are padded with rests for the duration of that score. 

  Note that the function assumes all parts in each score to contain the same overall duration and metric structure.

  Args:
  - scores: see {defun preview-score} for format description of headerless scores. 

  Example:

;;; (setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))
;;;     
;;; (append-parts `(:vl1 ,material
;;;                 :vl2 ,(gen-retrograde material :flatten T))
;;;               `(:vl1 ,material
;;;                 :vlc ,(gen-retrograde material :flatten T))
;;;               `(:vl2 ,material
;;;                 :vlc ,(gen-retrograde material :flatten T)))
"
  (reduce #'_append-two-parts scores))
  

#|
;; testing
(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -)))

(preview-score
 (append-parts `(:vl1 ,material
                 :vl2 ,(gen-retrograde material :flatten T))
               `(:vl1 ,material
                 :vlc ,(gen-retrograde material :flatten T))
               `(:vl2 ,material
                 :vlc ,(gen-retrograde material :flatten T))))
|#



;;; TODO:
;;; - Consider: support leading rests per subscore? Can I do this with append-parts or metric-shift?
(defun mix-parts (&rest scores)
  "Mix multiple scores together to run in parallel. Useful, e.g., for creating melody and polyphonic accompaniment or different textures separately, and then mixing them together. 

  Args
  - scores: see {defun preview-score} for format description of headerless scores. 

  Example:

;;; (let ((material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -))))
;;;    (mix-parts `(:vln ,material)
;;;               `(:vlc ,(metric-shift '-h material))))
"
  (apply #'append scores))


#|

(setf material '((-3h fs4 pp eb4 <) (q e4 < fs4 <) (3h gs4 mp> a4 > bb4 >) (q a4 pp -) (-5h - g4 pp leg eb4 < leg d4 < leg) (q bb4 < e4 <) (5h g4 mp> leg b4 > leg a4 > leg bb4 > leg d4 > leg) (q gs4 pp -)))

;;; TODO: why pitch repetitions?
(preview-score
 (mix-parts `(:vl1 ,material
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
 (mix-parts `(:vl1 ,material
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
 (mix-parts `(:vl1 ,material
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


(defun get-instruments (score)
  "Returns all instruments of `score', a headerless score (see {defun preview-score} for its format)."
  (tu:at-even-position score))

#|
(get-instruments '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
                   :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
|#


(defun get-part-omn (instrument score)
  "Returns the part (OMN expression) of `instrument' in `score', a headerless score (see {defun preview-score} for its format)."
  (getf score instrument))

#|
(get-part-omn  :vlc
'(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
  :vlc ((q g3) (q c4 b3 a3 g3) (h. c3))))
|#


(defun replace-instruments (new old score)
  "Replaces old instruments by new instruments in a score.

  Args:
  - new (list of keywords): New instruments to use.
  - old (list of keywords): List of instruments in `score' to be replaced. Must have same length as `new'.
  - score (a headerless score): See {defun preview-score} for its format.

  Example:

;;; (replace-instruments '(:trp :trb) '(:vl2 :vlc)
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3)))
"
  (reduce #'(lambda (score new-old) (substitute (first new-old) (second new-old) score))
          (matrix-transpose (list new old))
          :initial-value score))

(defun replace-part-omn (instrument new-part score)
  "Replaces the part (OMN expression) of `instrument' in `score' with `new-part'.

  Example:

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



(defun split-part (instrument orig-score score-to-insert)
  "Replaces part of `instrument' in `orig-score' with `score-to-insert'.

  Example:

;;; (split-part :vl2
;;; '(:vl1 (h g4)
;;;   :vl2 (h e4)
;;;   :vla (h c4)
;;;   :vlc (h c3))
;;;  '(:vl2_div1 (h f4)
;;;    :vl2_div2 (h e4)))

Split divisi strings into parts

(setf my-score
      '(:vl1 (h g4b4 g4a4)
        :vl2 (h e4 d4)
        :vla (h c4 b3)
        :vlc (h c3 g3)))

(setf divisi-part (single-events (pitch-melodize (get-part-omn :vl1 my-score))))

(preview-score
 (split-part :vl1
             my-score
             `(:vl1_div1 ,(flatten (tu:at-even-position divisi-part))
               :vl1_div2 ,(flatten (tu:at-odd-position divisi-part)))))
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

(defun remove-part (instrument score)
  "Removes `instrument' and its OMN expression from `score'.

  Example:

(remove-part :vl2
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))
"
  (let ((instr-position (position instrument score)))
    (append (subseq score 0 instr-position) 
            (subseq score (+ instr-position 2)))))
  
#|
(remove-part :vl2
'(:vl1 (h g4)
  :vl2 (h e4)
  :vla (h c4)
  :vlc (h c3)))
|#

(defun remove-parts  (instruments score)
  "Removes all `instruments' and their OMN expressions from `score'.

  Example:

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


(defun cluster-engine-score (cluster-engine-score &key (instruments nil))
  "Transforms the results of cluster-engine:clusterengine (https://github.com/tanders/cluster-engine) into a headerless score so that the function `preview-score' can show and play it, and it can be processed by all functions supporting this format.

  Args:
  - cluster-engine-score: The score data in the format returned by ClusterEngine.
  - instruments (list of keywords): Optional instrument labels for score parts -- length should be the same as parts in score.

  Example:

(preview-score
 (cluster-engine-score
  ;; simple polyphonic constraint problem
  (ce::ClusterEngine 10 t nil 
                     ;; single rule: all rhythmic values are equal
                     (ce::R-rhythms-one-voice 
                      #'(lambda (x y) (= x y)) '(0 1) :durations)
                     '((3 4)) 
                     '(((1/4) (1/8))
                       ((60) (61))
                       ((1/4) (1/8))
                       ((60) (61))))
  :instruments '(:vln :vla)))
  "
  (let* ((length-lists (butlast (tu:at-even-position cluster-engine-score)))
         (pitch-lists (tu:at-odd-position cluster-engine-score))
         (time-sigs (first (last cluster-engine-score))))
    (tu:one-level-flat 
    (loop 
      for lengths in length-lists
      for pitches in pitch-lists
      for instrument in (or instruments
                            (mapcar #'(lambda (i) (intern (write-to-string i) :keyword))
                                    (gen-integer 1 (length length-lists))))
      for time-sig in time-sigs
      collect (list instrument
                    (omn-to-time-signature 
                     (make-omn  
                      :length lengths
                      :pitch (midi-to-pitch pitches))
                     time-sig))))))


(defun preview-cluster-engine-score (score)
  "Just shorthand for (preview-score (cluster-engine-score score))"
  (preview-score (cluster-engine-score score)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Orchestration etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun filter-notes-if (test OMN &key (remain T) (section nil))
  "Extracts events in OMN for which a given test function returns true (or keeps only events for which the test function returns nils). All other notes are turned into rests.

  Args:
  - test: Boolean function expecting individual parameters of each note in `OMN'
  - OMN: An OMN sequence
  - remain: Boolean expressing whether only matching notes (T) or non-matching notes (nil) should be kept.
  - section: an integer or list of integers. Selected list or lists to process. The default is NIL.

  See also Opusmodus builtin `filter-events'.

  Examples:

Keep only notes above middle C and turn other notes into rests

;;; (filter-notes-if #'(lambda (dur pitch &rest other-args) 
;;;                      (> (pitch-to-midi pitch) 60))
;;;                  '(e c4 mp -e fermata e. d4 -h e. c4 e e4))

Do the opposite with :remain nil.

;;; (filter-notes-if #'(lambda (dur pitch &rest other-args) 
;;;                      (> (pitch-to-midi pitch) 60))
;;;                  '(e c4 mp -e fermata e. d4 -h e. c4 e e4)
;;;                  :remain nil)

This also works with nested lists and you can process only selected bars (other bars are kept unchanged).

;;; (filter-notes-if #'(lambda (dur pitch &rest other-args) 
;;;                      (> (pitch-to-midi pitch) 60))
;;;                  '((e c4 mp -e fermata e. d4 -s) (-q.. e. c4 e e4))
;;;                  :section 1)

For musical application examples see also {https://opusmodus.com/forums/topic/867-opusmodus-1222292/}.
"
  (if section
    (maybe-section #'(lambda (seq) (filter-notes-if test seq :remain remain)) 
                   OMN section)                   
    (copy-time-signature OMN
                         (flatten 
                          (mapcar #'(lambda (params) 
                                      (if (length-notep (first params))
                                        (if (if remain
                                              (apply test params)
                                              (not (apply test params)))
                                          params
                                          ;; turn non-matching note into rest
                                          ;;; TODO: preserve params relevant for rests (e.g., fermatas)
                                          (- (omn-encode (first params))))
                                        ;; leave rests unchanged
                                        params))
                                  (single-events (flatten OMN)))))))



(defun corresponding-rest (event)
  "Turns a single OMN note into a rest of the same note value. Rests remain rests, and rest articulations are preserved.

  Example: 
  ;;; (corresponding-rest '(h c4))"
  (let ((len (omn-encode (first event))))
    (cons 
     ;; rests should remain rests
     (if (> len 0)
       (* len -1)
       len)
     (omn :rest-articulation event))))


(defun _push-event-and-rests (event matching-position result-omns articulation-sets-length)
  (push event (nth matching-position result-omns))
  (loop for i in (remove matching-position (gen-integer 0 (1- articulation-sets-length)))
	do (push (corresponding-rest event) (nth i result-omns))))

(defun separate-parts (sequence articulation-sets)
  "The function `separate-parts' is useful for customising your sound playback with multiple sound libraries or for algorithmic orchestration. 
    The function breaks an OMN sequence (a single part) into a list of multiple OMN sequences (multiple parts). It basically sorts notes from the OMN sequence into different parts, depending on the articulations of individual notes. All notes with certain articulations go in one resulting parts, and notes with other articulations in another part. In all other resulting parts, notes are substituted by rests, so that timing relations of notes in different parts are preserved. 
    This function can be useful, when you have multiple sound libraries that support different articulations of the same instrument. You can then perform notes with certain articulations on one software instrument (on its own MIDI channel etc.), and notes with other articulations on another instrument. 
    Alternatively, you can use the function for algorithmic orchestration, where you assign custom articulations (typically declared with add-text-attributes first) such as instrument labels with your custom algorithm, and then use this function in a second step to separate your instruments.

    Remember that the result of this function is a list of multiple OMN sequences (multiple parts). You have to split it into its individual parts for use in OMN. 

    See also {https://opusmodus.com/forums/topic/849-towards-algorithmic-orchestration-and-customising-sound-playback-with-multiple-sound-libraries/}

    Args:
    - sequence: OMN sequence, can be nested
    - articulation-sets: list of list of articulations. All notes with articulations contained in the first articulation-set end up in the first resulting part, notes with articulations in the second set end up in the second part and so forth. 
    
    The decision which part a note belongs to is always made based on the first articulation that matches an articulation-set. If a note contains no articulation, or an articulation contained in no set, then it is matched to the first articulation-set. If an articulation is contained in multiple articulation-sets, then the earlier match in articulation-sets is used.


    Examples:     
;;; (separate-parts '(h c4 pizz q arco)
;;;                 '((pizz)
;;;                   (arco)))
;;; => ((h c4 mf pizz -q)   ; part 1 with pizz articulations
;;;     (-h q c4 mf arco))  ; part 2 with arco

;;; (separate-parts '((h c4 pizz q arco) (h trem q h pizz) (h arco+stacc -q fermata))
;;;                 '((pizz arco)
;;;                   (trem)))
;;; => (((h c4 mf pizz q arco) (-h q c4 mf h pizz) (h c4 mf arco+stacc -q fermata)) ; part 1: pizz and arco
;;;     ((-h -q) (h c4 mf trem -q -h) (-h -q fermata)))                             ; part 2: trem


Full score example:

;;; (setf omn-expr '((h c4 pizz q arco) (h trem q h pizz) (h arco+stacc -q fermata)))
;;; (setf parts (separate-parts omn-expr
;;;                             '((pizz arco)
;;;                               (trem))))
;;; (def-score two-violins
;;;   (:title \"Title\"
;;; 	  :composer \"Composer\"
;;; 	  :copyright \"Copyright © \"
;;; 	  :key-signature 'chromatic
;;; 	  :time-signature '((1 1 1 1) 4)
;;; 	  :tempo 100
;;; 	  :layout (bracket-group
;;; 		   (violin1-layout 'violin1)
;;; 		   (violin2-layout 'violin2)))
;;;  
;;;   (violin1
;;;    :omn (nth 0 parts)
;;;    :channel 1
;;;    :sound 'gm
;;;    :program 'violin
;;;    :volume 100
;;;    :pan 54
;;;    :controllers (91 '(48))
;;;    )
;;;  
;;;   (violin2
;;;    :omn (nth 1 parts)
;;;    :channel 2
;;;    :sound 'gm
;;;    :program 'violin
;;;    :volume 100
;;;    :pan 74
;;;    :controllers (91 '(60))
;;;    )
;;;   )
"
    (if (listp (first sequence))
      ;; sequence is nested
      (matrix-transpose 
       (mapcar #'(lambda (seq) (separate-parts seq articulation-sets))
               sequence))
      ;; sequence is flat list
      (let* ((articulation-sets-length (length articulation-sets))
             (result-omns (make-list articulation-sets-length :initial-element nil)))
        (loop for event in  (single-events sequence) 
          do (let ((event-articulation (fourth event)))
               (if event-articulation
                 (let ((matching-position
                        (position-if #'(lambda (articulation-set)
                                         (some #'(lambda (art) 
                                                   (member art (disassemble-articulations event-articulation)))
                                               articulation-set))
                                     articulation-sets)))
                   (if matching-position
                     (_push-event-and-rests event matching-position result-omns articulation-sets-length)
                     ;; if no match, then add event to first omn result                    
                     (_push-event-and-rests event 0 result-omns articulation-sets-length)))
                 ;; if no articulation, then add event to first omn result 
                 (_push-event-and-rests event 0 result-omns articulation-sets-length))))
        (mapcar #'(lambda (result) (flatten-omn (reverse result))) result-omns))))
  
#|
(separate-parts '(h c4 pizz q arco)
                '((pizz)
                  (arco)))
=> ((h c4 mf pizz -q)   ; part 1 with pizz articulations
    (-h q c4 mf arco))  ; part 2 with arco

(separate-parts '((h c4 pizz q arco) (h trem q h pizz) (h arco+stacc -q fermata))
                '((pizz arco)
                  (trem)))
=> (((h c4 mf pizz q arco) (-h q c4 mf h pizz) (h c4 mf arco+stacc -q fermata)) ; part 1: pizz and arco
    ((-h -q) (h c4 mf trem -q -h) (-h -q fermata)))                             ; part 2: trem
|#




(defun insert-articulation (flat-omn-list &rest articulations)
  "Merge in one or more lists of articulations to an OMN expression.

  Example:
  added nil for the rest
  ;;; (insert-articulation '(e c4 mp arco e. d4 -h e. p pizz e e4 arco) 
  ;;;                      '(ponte tasto nil ponte tasto))
  ;;; => (e c4 mp arco+ponte e. d4 mp tasto -h e. d4 p pizz+ponte e e4 p arco+tasto)

  BUG: does not skip rests. Wait for omn-replace supports composite articulations to fix"
  (apply #'append 
         (loop 
           for event in (single-events flat-omn-list)
           for data in (matrix-transpose articulations)
           when (length-notep (first event))
           collect (let ((event-art (fourth event))) 
                     (append (subseq event 0 3)
                             (list (merge-articulations (if event-art 
                                                          (cons event-art data)
                                                          data)))))
           else collect event)))

#| ;; automatic orchestration application
(add-text-attributes
 '(trp "trp")
 '(fl "fl")
 '(clar "clar"))

(insert-articulation '(e c4 mp stacc e. d4 -h e. c4 p ord e e4 stacc)
                     '(trp fl trp trp fl clar)
                     '(flt tr1 tr2 flt tr1 tr2))
; => '(e c4 mp stacc+trp+flt e. d4 mp fl+tr1 -h e. c4 p ord+trp+flt e e4 p stacc+fl+tr1)
|#




(defun remove-unless-parameters (flat-omn-list parameter &key (remove-non-matching? nil))
  "Checks every note whether it contains `parameter'. All notes containing the parameter are preserved, all other notes are turned into rests. If a note contains a combination of articulations, all of them are checked.  

  Args:
  - flat-omn-list: flat OMN list
  - parameter: a length, pitch, OMN velocity or single articulation
  - remove-rests? (default nil): if true, all notes that do not match are removed instead of turned into rests.


  Examples:
  ;;; (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
  ;;;                         'e.)
  ;;; => (-1/8 e. d4 mp tasto -1/2 e. c4 p pizz+ponte -1/8)

  ;;; (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
  ;;;                         'arco)
  ;;; => (e c4 mp arco+ponte -3/16 -1/2 -3/16 e e4 p arco+tasto)
  
  ;;; (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
  ;;;                          'arco
  ;;;                          :remove-non-matching? T)
  ;;; => (e c4 mp arco+ponte e e4 p arco+tasto)
  "
  (remove :not-matching
          (apply #'append
                 (loop for event in (single-events flat-omn-list)
                   when (and (length-notep (first event))
                             (member (omn-encode parameter) 
                                     (append (list (omn-encode (first event)))
                                             (subseq event 1 3) 
                                             (disassemble-articulations (fourth event)))))
                   collect event
                   else collect (if remove-non-matching?
                                  '(:not-matching)
                                  (corresponding-rest event))))))

#| ;; continue automatic orchestration application
(remove-unless-parameters '(e c4 mp stacc+trp+flt e. d4 mp fl+tr1 -h e. c4 p ord+trp+flt e e4 p stacc+fl+tr1)
                        'trp)
; => (e c4 mp stacc+trp+flt -3/16 -1/2 e. c4 p ord+trp+flt -1/8)

(remove-unless-parameters '(e c4 mp stacc+trp+flt e. d4 mp fl+tr1 -h e. c4 p ord+trp+flt e e4 p stacc+fl+tr1)
'e.)
|#




