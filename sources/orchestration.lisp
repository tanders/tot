;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Polyphonic processing for creating textures etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flet
    ;; access data in individual part of headerless score
    ((part-symbol (part) (first part))
     (part-omn (part) (second part))
     (part-args (part) (cddr part)))
  (defun preview-score (score 
                        &rest
                        args
                        &key 
                        (play? T)
                        (display? T)
                        (name 'test-score)      
                        &allow-other-keys
                        )
    "Notates and plays a score in a format slightly simpler than expected by def-score, i.e., without its header. 
    
    Args: 
    score: a headerless score. See below for its format.
    name: The score name.
    play?: A Boolean, whether or not to play the score immediately.
    display?: A Boolean, whether or not to display the notation of the score immediately.
    Additional arguments (keyword-value pairs): all arguments that are part of the header in def-score (e.g., title, composer, key-signature...).
    
    Score format: 
    ((<part1-name-keyword> <part1-OMN> &rest score-args)
    (<part2-name-keyword> <part2-OMN> &rest score-args)
    ...) 
    
Examples:

(preview-score
 '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin)
   (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello))
 :title \"Opus magnum\"
 :tempo 80)

(preview-score
 '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin :sound 'gm)
   (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello :sound 'gm))
 :tempo 80
 :play? T)  
  "
    ;; Using eval is problematic (https://stackoverflow.com/questions/2571401/why-exactly-is-eval-evil/),
    ;; but hard to avoid for a dynamically created def-score expression that requires splicing with ,@.
    ;; Possible alternative would be to define preview-score as macro, but then arguments are not evaluated. 
    (eval 
     `(def-score ,name 
                 ; quote all header args, because symbol values must be quoted...
                 ,(mapcar #'(lambda (x) `',x)
                          ; remove args that don't belong into score header
                          (append (tu:remove-properties '(:play? :display? :name)  
                                                        args)
                                  ; add default vals of required header args at end -- they are overwritten by args given
                                  (list :key-signature 'atonal
                                        ; By default, use implicit time signature of 1st part
                                        :time-signature (om:get-time-signature (second (first score)))
                                        :tempo 70)))
        ,@(mapcar #'(lambda (part) 
                      (list* (part-symbol part) 
                             :omn (list 'quote (list (part-omn part)))
                             (part-args part)))
                  score)))
    (when display? (om:display-musicxml name))
    (when play? (om:display-midi name)))
  

  ;; unfinished def

  ;;; TODO: finish implementation
  ;;; - arg shared-args -- only for keyargs -- or add some extra notation complementing the underscore (_)
  ;;; - add some meaningful demos
  ;;; - score can be symbol naming score to process 
  ;;; - args for functions can be allocated by preceeding part name (keyword) instead of positionally (useful for larger scores to read)
  ;;;   -> If I only use such a format, then I can remove ignore argument (and don't need a corresponding select arg)
  ;;; - OK finish doc.
  ;;; - allow for some global settings of part args for preview-score
  ;;; - OK finish arg parameter -- finish testing
  ;;; - OK arg ignore
  (defun map-parts (score fn args
                          &key 
                          (parameter nil) 
                          ;;; TODO: Only keyword arguments are supported.
                          ; (shared-args nil)
                          ;;; TODO
                          (ignore nil)
                          )
    "Applies function `fn' to parts in `score': this function is a variant of the standard Lisp function `mapcar', but specialised for scores. A score is represented in the format discussed in the documentation of the function `preview-score'.     
    
    Additional arguments for `fn' can be specified in `args', and these argument lists can be different for each part. However, one argument is the part of the score. This argument is marked by an underscore (_) in the argument lists. In the following example, the function `length-augmentation' is applied to a score with two parts. The function `length-augmentation' has two required arguments, a transposition interval (measured in semitones), and the pitch sequence or OMN to transpose. The transposition interval for the first part is 4 (major third upwards), and the underscore marks the position of the violin part to transpose, etc. 
 
(preview-score
(map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)))
             (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3))))
           #'pitch-transpose 
           '((4 _) 
             (12 _))))
-> ((:violin ((q b4) (q. e5 e fs5 q gs5 a5) (h. gs5))) 
    (:violoncello ((q g4) (q c5 b4 a4 g4) (h. c4))))

    Args:
    score (a headerless score): See preview-score doc for format description. 
    fn: A function that expects and returns an OMN sequence or a sequence of parameter values (e.g., lengths, or pitches) as specified in the argument `parameter'. 
    parameter (omn parameter, e.g., :length or :pitch, default is nil for processing full OMN expression): If `fn' expects only single parameter to process, then it can be set here. 
    args: For each part in `score', the arguments for `fn' are set here in the order of the parts.  
    ??? shared-args (a list): As `fn' is applied to every part in score, these arguments are used for every part. 
    ignore (list of integers): positions of parts in `score' that should not be processed.
    
    
    
Furhter examples:

(map-parts '((:violin ((q g4) (q. c5 e d5 q e5 f5) (h. e5)) :program 'violin :sound 'gm)
             (:violoncello ((q g3) (q c4 b3 a3 g3) (h. c3)) :program 'cello :sound 'gm))
           #'length-augmentation 
           '((1 _) 
             (3/2 _)))
    
    "
    ;;; TODO: for implementing ignore define a function remove-position 
    (mapcar #'(lambda (part-n-args) 
                (destructuring-bind (i part args) part-n-args                  
                  (if (member i ignore)
                    ;; no processing
                    part
                    ;; processing
                    (list* (part-symbol part)
                    (let ((result (apply fn (substitute 
                                             (if parameter
                                               (omn parameter (part-omn part))
                                               (part-omn part))
                                             '_ args))))
                      (if parameter
                        (omn-replace parameter result (part-omn part))
                        result)         
                      )
                    (part-args part)))))
            (om:matrix-transpose (list (om:gen-integer (1- (length score))) ; indices
                                       score args))))
) ; flet

;;;; NOTE: I do not know at which position in args the value list pops up -- need to mark that position in args list (e.g., with _)

#|

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
  (corresponding-rest '(h c4))"
  (let ((len (omn-encode (first event))))
    (cons 
     ;; rests should remain rests
     (if (> len 0)
       (* len -1)
       len)
     (omn :rest-articulation event))))

(labels ((push-event-and-rests (event matching-position result-omns articulation-sets-length)
           (push event (nth matching-position result-omns))
           (loop for i in (remove matching-position (gen-integer 0 (1- articulation-sets-length)))
             do (push (corresponding-rest event) (nth i result-omns)))))

  (defun separate-parts (sequence articulation-sets)
    "The function `separate-parts' is useful for customising your sound playback with multiple sound libraries or for algorithmic orchestration. 
    The function breaks an OMN sequence (a single part) into a list of multiple OMN sequences (multiple parts). It basically sorts notes from the OMN sequence into different parts, depending on the articulations of individual notes. All notes with certain articulations go in one resulting parts, and notes with other articulations in another part. In all other resulting parts, notes are substituted by rests, so that timing relations of notes in different parts are preserved. 
    This function can be useful, when you have multiple sound libraries that support different articulations of the same instrument. You can then perform notes with certain articulations on one software instrument (on its own MIDI channel etc.), and notes with other articulations on another instrument. 
    Alternatively, you can use the function for algorithmic orchestration, where you assign custom articulations (typically declared with add-text-attributes first) such as instrument labels with your custom algorithm, and then use this function in a second step to separate your instruments.

    Remember that the result of this function is a list of multiple OMN sequences (multiple parts). You have to split it into its individual parts for use in OMN. 

    See also https://opusmodus.com/forums/topic/849-towards-algorithmic-orchestration-and-customising-sound-playback-with-multiple-sound-libraries/

    Args:
    - sequence: OMN sequence, can be nested
    - articulation-sets: list of list of articulations. All notes with articulations contained in the first articulation-set end up in the first resulting part, notes with articulations in the second set end up in the second part and so forth. 
    
    The decision which part a note belongs to is always made based on the first articulation that matches an articulation-set. If a note contains no articulation, or an articulation contained in no set, then it is matched to the first articulation-set. If an articulation is contained in multiple articulation-sets, then the earlier match in articulation-sets is used.

    Examples: 
    
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


    Full score example:

(setf omn-expr '((h c4 pizz q arco) (h trem q h pizz) (h arco+stacc -q fermata)))
(setf parts (separate-parts omn-expr
                            '((pizz arco)
                              (trem))))

(def-score two-violins
           (:title \"Title\"
            :composer \"Composer\"
            :copyright \"Copyright © \"
            :key-signature 'chromatic
            :time-signature '((1 1 1 1) 4)
            :tempo 100
            :layout (bracket-group
                     (violin1-layout 'violin1)
                     (violin2-layout 'violin2)))
  
  (violin1
   :omn (nth 0 parts)
   :channel 1
   :sound 'gm
   :program 'violin
   :volume 100
   :pan 54
   :controllers (91 '(48))
   )
  
  (violin2
   :omn (nth 1 parts)
   :channel 2
   :sound 'gm
   :program 'violin
   :volume 100
   :pan 74
   :controllers (91 '(60))
   )
  )
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
                     (push-event-and-rests event matching-position result-omns articulation-sets-length)
                     ;; if no match, then add event to first omn result                    
                     (push-event-and-rests event 0 result-omns articulation-sets-length)))
                 ;; if no articulation, then add event to first omn result 
                 (push-event-and-rests event 0 result-omns articulation-sets-length))))
        (mapcar #'(lambda (result) (flatten-omn (reverse result))) result-omns)))))
  
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
  ;; added nil for the rest
  (insert-articulation '(e c4 mp arco e. d4 -h e. p pizz e e4 arco) 
                       '(ponte tasto nil ponte tasto))
  => (e c4 mp arco+ponte e. d4 mp tasto -h e. d4 p pizz+ponte e e4 p arco+tasto)

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
  flat-omn-list: flat OMN list
  parameter: a length, pitch, OMN velocity or single articulation
  remove-rests? (default nil): if true, all notes that do not match are removed instead of turned into rests.

  Examples:
  (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
                          'e.)
  => (-1/8 e. d4 mp tasto -1/2 e. c4 p pizz+ponte -1/8)

  (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
                          'arco)
  => (e c4 mp arco+ponte -3/16 -1/2 -3/16 e e4 p arco+tasto)
  
  (remove-unless-parameters '(e c4 mp arco+ponte e. d4 mp tasto -h e. c4 p pizz+ponte e e4 p arco+tasto) 
                           'arco
                           :remove-non-matching? T)
  => (e c4 mp arco+ponte e e4 p arco+tasto)
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




