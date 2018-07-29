;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Orchestration etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun filter-notes-if (test OMN &key (remain T) (section nil))
  "Extracts events in OMN for which a given test function returns true (or keeps only events for which the test function returns nils). All other notes are turned into rests.

* Arguments:
  - test: Boolean function expecting individual parameters of each note in `OMN'
  - OMN: An OMN sequence
  - remain: Boolean expressing whether only matching notes (T) or non-matching notes (nil) should be kept.
  - section: an integer or list of integers. Selected list or lists to process. The default is NIL.

  See also Opusmodus builtin `filter-events'.

* Examples:

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

* Notes: 

This function could also be useful for Beethoven like motif condensation, where notes are first turned into rests with this function, and then their preceding notes are extended with length-legato, as demonstrated in the following example. 

;;; (setf my-motif '((q. c4 e d4 q. e4 e f4) (h g4 -h)))
;;; (length-legato
;;;  (filter-notes-if #'(lambda (dur pitch &rest other-args)  
;;;                       (> (omn-encode dur) 1/4)) 
;;;                   my-motif))
;;; => ((h c4 e4) (w g4))

* See Also: 

https://opusmodus.com/forums/topic/910-merge-rests-with-preceeding-note/?tab=comments#comment-2713

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

* Examples: 
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

* Arguments:
    - sequence: OMN sequence, can be nested
    - articulation-sets: list of list of articulations. All notes with articulations contained in the first articulation-set end up in the first resulting part, notes with articulations in the second set end up in the second part and so forth. 
    
    The decision which part a note belongs to is always made based on the first articulation that matches an articulation-set. If a note contains no articulation, or an articulation contained in no set, then it is matched to the first articulation-set. If an articulation is contained in multiple articulation-sets, then the earlier match in articulation-sets is used.


* Examples:     

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

* Examples:
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

* Arguments:
  - flat-omn-list: flat OMN list
  - parameter: a length, pitch, OMN velocity or single articulation
  - remove-rests? (default nil): if true, all notes that do not match are removed instead of turned into rests.


* Examples:
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




