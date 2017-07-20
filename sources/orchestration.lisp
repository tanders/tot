;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Polyphonic processing for creating textures etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; NOTE: just some dummy settings for now
(defparameter *default-preview-score-instruments*
  '(:vln (:program 'violin :sound 'gm)
    :vlc (:program 'cello :sound 'gm))
  "Settings for each instrument used by `preview-score'. The format is a plist where keys are the instrument labels, and values a list with the actual settings. For format of these settings are the same as instrument settings for `def-score' with keywords like :sound, :channel etc. -- except for they key :omn.")

;;; NOTE: just some dummy settings for now
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
|#
  

;; unfinished def

;;; TODO: finish implementation
;;; - add some meaningful demos
;;; - ? score can be symbol naming score to process 
;;; - OK arg shared-args -- only for keyargs -- or add some extra notation complementing the underscore (_)
;;; - OK args for functions can be allocated by preceeding part name (keyword) instead of positionally (useful for larger scores to read)
;;;   -> If I only use such a format, then I can remove ignore argument (and don't need a corresponding select arg)
;;; - OK allow for some global settings of part args for preview-score
;;; - OK finish doc.
;;; - OK finish arg parameter -- finish testing
;;; - OK arg ignore
(defun map-parts (score fn all-args &key 
			(parameter nil) 
                        (shared-args nil))
  "This function can be used for creating or transforming musical textures, i.e., relations between polyphonic parts.

  Applies function `fn' to parts in `score': this function is a variant of the standard Lisp function `mapcar', but specialised for scores. A score is represented in the format discussed in the documentation of the function `preview-score'.     
    
    Additional arguments for `fn' can be specified in `args', and these argument lists can be different for each part. However, one argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. In the following example, the function `length-augmentation' is applied to a score with two parts. The function `length-augmentation' has two required arguments, a transposition interval (measured in semitones), and the pitch sequence or OMN to transpose. The transposition interval for the first part is 4 (major third upwards), and the underscore marks the position of the violin part to transpose, etc. 
 

TODO: revise example
;;; (preview-score
;;; (map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)))
;;;              (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3))))
;;;            #'pitch-transpose 
;;;            '((4 _) 
;;;              (12 _))))
;;; => ((:violin ((q b4) (q. e5 e fs5 q gs5 a5) (h. gs5))) 
;;;     (:violoncello ((q g4) (q c5 b4 a4 g4) (h. c4))))

    Args:
    - score (headerless score): See {defun preview-score} for format description. 
    - fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
    - all-args (plist): alternating instrument keywords (same as in `score') followed by arguments list for `fn' for that instrument/part. If arguments is :skip, then that part is returned unchanged. 
    - parameter (omn parameter, e.g., :length or :pitch, default nil means processing full OMN expression): If `fn' expects only single parameter to process, then it can be set here. 
    - shared-args (list): For all instruments/parts, these arguments are appended at end end of its part-specific arguments. They are therefore primarily useful for keyword arguments. 


TODO: revise example
    
Examples:

;;; (map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin :sound 'gm)
;;;              (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello :sound 'gm))
;;;            #'length-augmentation 
;;;            '((1 _) 
;;;              (3/2 _)))    
    "
  (let ((parts (make-hash-table :test #'equal)))
    ;; fill hash table, using leading keywords as keys
    (loop for part in (tu:plist->pairs score)
      do (setf (gethash (first part) parts) part))
    (tu:one-level-flat
     (loop 
       for instrument-arg-pair in (tu:plist->pairs all-args) 
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


#|
;;; testing / generating examples

;; simple test
(preview-score
 (map-parts '(:vln ((q g4) (q. c5 e d5 q e5 f5) (h. e5))
              :vlc ((q g3) (q c4 b3 a3 g3) (h. c3)))
            #'length-augmentation 
            '(:vln (1 _)
              :vlc (2/3 _))
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
(preview-score
 (map-parts 
  (map-parts `(:vl1 ,material
               :vl2 ,material  
               :vla ,material
               :vlc ,material)
             #'metric-append 
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

;; Simple homophonic texture created by randomised transpositions 
;; Each part shares similar overall pitch profile
(preview-score
 (map-parts 
  (map-parts `(:vl1 ,material
               :vl2 ,material  
               :vla ,material
               :vlc ,material)
             #'pitch-transpose-n 
             `(:vl1 (,(rnd 10 :low -2 :high 2) _) 
               :vl2 (,(rnd 10 :low -2 :high 2) _)  
               :vla (,(rnd 10 :low -2 :high 2) _)
               :vlc (,(rnd 10 :low -2 :high 2) _)))
  ;; static transposition moving parts into different registers
  #'pitch-transpose 
  '(:vl1 (+7 _) 
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

;;; old examples, revise

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


;;; TODO function: score-append -- simply appending multiphonic scores



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Orchestration etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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




