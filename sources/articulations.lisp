;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process articulations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-articulations (arts &key (empty-articulations '(- default)))
  "Merges list of OMN articulations to a combined attribute.

* Arguments:
  - arts: a list of OMN articulations
  - empty-attributes: articulations to ignore in a combination. 

* Examples:
  ;;; (merge-articulations '(ten ponte ubow))
  ;;; => ten+ponte+ubow

  ;;; (merge-articulations '(- stacc))
  ;;; => stacc

  ;;; (merge-articulations '(default default))
  ;;; => default

  ;;; (merge-articulations '(- -))
  ;;; => -  
  "
  #|
  (assert (every #'articulationp arts)
          (arts)
          "All values to merge must be OMN attributes: ~A.~%" arts)
  |#
  (intern 
   (reduce #'(lambda (a1 a2) (format nil "~A+~A" a1 a2))
           (let ((interm-result (mappend
                                 #'(lambda (art) (unless (some #'(lambda (a) (eq art a))
                                                               empty-articulations)
                                                   (list (symbol-name art))))
                                 arts)))
             (if interm-result
               interm-result
               (list (symbol-name (first empty-articulations))))))))

#| ; demo
(setf my-rhythm '(1/8 1/8 1/16 1/16 -1/16 1/8 1/8 1/8 1/8))

(setf arts-1 (pattern-map '(((1/8 1/8) (leg stacc)))
                          my-rhythm
                          :otherwise '(default)))
(setf arts-2 (pattern-map '(((1/8) (tasto)))
                          my-rhythm
                          :otherwise '(ord)))

(mapcar #'merge-articulations (matrix-transpose (list (flatten arts-1) (flatten arts-2))))
|#




#|
(merge-articulations '(default stacc))

(merge-articulations '(- 1))

(articulationp '-)

(articulationp 'leg)
|#



(defun disassemble-articulations (art)
  "Splits a combined OMN articulations into a list of its individual attributes.

* Examples:
  ;;; (disassemble-articulations 'leg+ponte)
  ;;; => (leg ponte)"  
  #|
  (assert (articulationp art)
          (art)
          "Value not OMN articulations: ~A.~%" art)
  |#
  (mapcar #'intern (split-string (symbol-name art) :separator "+")))

#|
(disassemble-articulations 'leg+ponte)
(disassemble-articulations '|-7+3.14+16+1/3|)

 (split-string (symbol-name '|-7+3.14+16+1/3|) :separator "+")
|#


;;; TODO: support nested lists -- somehow store nesting structure, then process flattened list and apply nesting back
(defun zip-articulations (&rest arts)
  "Expects lists of articulations and combines them to a single list of merged articulations. Shorter lists are circled to the length of the longest.

  If arts are list of lists, the result follows the nesting organisation of the first list.

* Examples:
  ;;; (zip-articulations '(default leg leg default) '(breathy))
  ;;; (zip-articulations '(- leg leg -) '(breathy))
  ;;; (zip-articulations '(- leg leg -) '(-))
  ;;; => (breathy leg+breathy leg+breathy breathy)"
  (let* ((flat-arts (mapcar #'flatten arts))
         (max-length (apply #'max (mapcar #'length flat-arts))))
    (span (first arts)
          (mapcar #'merge-articulations             
                  (matrix-transpose (mapcar #'(lambda (xs) (circle-repeat xs max-length)) flat-arts))))))

#|
(make-omn 
 :length '(1/8 1/8 1/16 1/16 -1/16 1/8 1/8 1/8 1/8)
 :articulation (zip-articulations '(default stacc stacc default)
                                  '(ubow dbow)
                                  '(ponte))
 :swallow NIL
 :flat T)
|#

;;; BUG: not working for flat lists
(defun get-full-articulations (sequence)
  "[Aux function] Similar to (omn :articulation sequence), but the result includes `leg' and `gliss' symbols for legato 'articulation', which are handled seperately by the function `omn'.

  NOTE: as the implementation of omn is not necessarily stable, this function is breakable.

* Examples: 
  ;;; (get-full-articulations '((q g4 leg+gliss q a4 leg q b4 stacc)
  ;;;                           (q c4 ten q c5 trem)
  ;;;                           ))
  ;;; => ((leg+gliss leg stacc) (ten trem))
"
  (if (listp (first sequence))
      ;; if first nested then assume all are nested
      (mapcar #'get-full-articulations sequence)
      ;; flat sequence
      (let* ((art-list (omn :articulation sequence))
	     ;; numerically encoded format
	     (orig-leg-list (omn :leg sequence))
	     ;; numerically encoded format
	     (orig-gliss-list (omn :gliss sequence))
	     ;; decode orig-legs
	     (leg-list (if orig-leg-list
			   (loop for leg-val in orig-leg-list
			      append (if (> leg-val 0)
					 (gen-repeat leg-val '(leg))
					 (gen-repeat (abs leg-val) '(-))))
			   (gen-repeat (length art-list) '(-))))
	     ;; NOTE: code repetition
	     (gliss-list (if orig-gliss-list
			     (loop for gliss-val in orig-gliss-list
				append (if (> gliss-val 0)
					   (gen-repeat gliss-val '(gliss))
					   (gen-repeat (abs gliss-val) '(-))))
			     (gen-repeat (length art-list) '(-)))))
	(zip-articulations art-list leg-list gliss-list))))

#|
(get-full-articulations '(q g4 leg+gliss q a4 leg q b4 stacc))
|#



;;; BUG: I misunderstood function span -- may not work for OMN
(defun articulation-at-beginning (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' at the beginning and `default' for the rest.
  
* Examples: 
  ;;; (articulation-at-beginning 'pizz '((q q e e) (h.)))
  ;;; => ((pizz default default default) (default))

  BUG: May not work for OMN sequence."
  (position-replace 0 articulation (span sequence (list default)) :section 0))

;;; BUG: I misunderstood function span -- may not work for OMN
(defun articulation-at-end (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' at the beginning and `default' for the rest.
  
* Examples: 
  ;;; (articulation-at-end 'pizz '((q q e e) (h.)))
  ;;; => ((pizz default default default) (default))

* BUGS: 
  - Does not work if last element in sequence is a rest.
  - May not work for OMN sequence."
  (span sequence
        (reverse (cons articulation (gen-repeat (1- (length (flatten sequence))) (list default))))))

(defun articulation-up-to-end (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' from the beginning to the but-last tone, which carries the default. This is useful for legato slurs over a sublist.
  
* Examples:
  ;;; (articulation-up-to-end 'leg '((q q e e) (q q)))
  ;;; => ((leg leg leg leg) (leg default))"
  (span sequence
        (append (gen-repeat (1- (length (flatten sequence))) (list articulation)) (list default))))



;; !? Better don't use text-attribute. Instead, declare piece-specific attributes in score. See also https://opusmodus.com/forums/topic/777-adding-free-text-to-the-score-output/#comment-2278
(defun text-attribute (&rest args)
  "Convenient addition of arbitrary text directly the the score. Function transforms an arbitrary symbol, number or string or a list of  symbols, numbers and strings into a an articulation symbol that is implicitly registered and can directly be added to an OMN note.

  Definition currently only works with multiple integers or floats.
  
* Examples:
  ;;; (text-attribute -7 2 13)
  ;;; => -7+2+13

  ;;; (text-attribute 3.14 42)
  ;;; => 3.14+42

* BUGS: 
  All these examples are not working.
  ;;; (text-attribute \"this is a test\")
  ;;; (text-attribute 42)
  ;;; (text-attribute 'parameters -7 2 13)
  ;;; (text-attribute -7 3.14 16 1/3)
  "
  (flet ((text-attr (x)
           (let ((s (format nil "~A" x)))
             (list (intern s) s))))     
    (merge-articulations
     (apply #'add-text-attributes (mapcar #'text-attr args)))))

#|
;; Multiple integers and floats can be notated
`(q ,(text-attribute -7 2 3.14 1000))
=> '(q -7+2+3.14+1000)


;; Causes error -- why
`(q ,(text-attribute 'espressivo))

`(q ,(text-attribute "1"))

`(q ,(text-attribute 'delay 100))


;; Results in two notes? Why?
`(q ,(text-attribute 'test))

;; not working
`(q a4 ,(text-attribute "finger"))

`(q ,(text-attribute "this is a test"))

`(q ,(text-attribute "delay 100ms"))

'(q c4 p |1 12|)

`(q ,(text-attribute "C_7"))

;; a single integer or fraction is missrepresented as a duration
`(q ,(text-attribute 1))
`(q ,(text-attribute 1/4))
;; single floats cause errors
`(q ,(text-attribute 3.14))

|#


#|
(defmacro add-multiple-attributes (numbers)
  "

The symbol for each attribute starts with 'A_' 
"
  `(add-text-attributes
    ,@(loop for num in numbers
	 collect `(quote (,(intern (format nil "A_~A" num))
			  ,(format nil "~A" num))))
    ))

(add-multiple-attributes (1 2 3 4 5 6 7))


(defun add-multiple-attributes (numbers)
  "

The symbol for each attribute starts with 'A_' 
"
  (apply #'add-text-attributes
	  (loop for num in numbers
	     collect (list (intern (format nil "A_~A" num))
			   (format nil "~A" num)))
	  ))

(add-multiple-attributes '(1 2 3 4 5 6 7))
'(q c4 p a_1)

(add-text-attributes
 '(nr0 "0") 
 '(nr1 "1") 
 '(nr2 "2") 
 '(nr3 "3") 
 '(nr4 "4") 
 '(nr5 "5"))﻿﻿
 
|#


;;; TODO: add arg section (its a bit tricky...)
(defun articulate-bars (sequence &key (accent 'ten) (default '-)
				   (parameter :articulation)
				   )
  "Add articulations at the first notes of bars, e.g, a tenuto on every first beat, and staccato otherwise.
  
* Arguments:

  - sequence (sequence of length values or full OMN expression, must be nested): an accent is positioned on every first note in a bar (sublist).
  - parameter (:articulation or :velocity): which parameter to use for the articulations 
  - accent (symbol): articulation to use on first notes of bars.
  - default (symbol): articulation to use for all other notes. 

* Examples:

  ;;; (articulate-bars '((h h) (q q q q) (q q q q) (-q q) (q q q q)) :accent 'marc)

  An example with a tie: existing articulations are preserved. However, in that case `sequence' must be a full OMN expression (i.e. include length and pitch values).
  ;;; (articulate-bars '((h c4 h) (q q q q tie) (q q q q) (-q q) (q q q q)) :accent 'marc)
  
  ;;; (articulate-bars (gen-repeat 3 '((s s s))) :accent 'f :default 'p :parameter :velocity)
  "
  (assert (if (eql parameter :velocity)
	      (velocityp default)
	      T))
  (let* ((new (gen-swallow (omn :length sequence)		
			   (flatten (loop for bar in (span (length-rest-invert sequence)
							   `(,default))
				       collect (cons accent (rest bar))))))
	 (new2 (case parameter
		 ;; don't overwrite existing articulations
		 (:articulation
		  (if (omn-formp sequence)
		      (zip-articulations (omn parameter sequence) new)
		      new))
		 ;; but overwrite existing dynamics (velocities)
		 (:velocity new))))
    (omn-replace parameter new2 sequence)))

 
#|
;;; OLD:
;;; TODO:
;; - implement arg default
;; - don't overwrite existing articulations (e.g., ties)
;; - implement arg section
;; - process first bar extra (use local function to avoid repetition?)
;; OK - If there is only a single note and a tie them combine them
(defun articulate-phrase (sequence &key (accent 'ten)   
                                    ; (parameter :articulation)
                                    ; (default nil)
                                    ; (section nil)
                                   )
  "Add articulations to phrase for more clear rhythmic accents, e.g, tenuto on every first beat, and stacc otherwise.
  
  NOTE: This function assumes that `sequence' is a purely rhythmic OMN expression with only length values and perhaps ties. A full sequence with added articulations and also (constant) pitches is returned (certain Opusmodus functions do not support an OMN sequence with articulations but without pitches).

  Examples:
  (articulate-phrase '((h h) (q q q q tie) (q q q q) (-q q) (q q q q)) :accent 'marc)
  => ((h marc h) (q marc q q q tie) (q q q q) (-q q) (q marc q q q))
  
  ;; currently not supported anymore
  ; (articulate-phrase (gen-repeat 3 '((s s s))) :accent 'f :default 'p :parameter :velocity)
  "
  (cons 
   ;; First bar
   ;;; NOTE: code repetition...
   (let ((bar1 (first sequence)))
     (if (length-notep (first bar1))
       (if (and (= (count-notes bar1) 1)
                (eql (first (last bar1)) 'tie))
         (tu:replace-element (merge-articulations (list accent 'tie)) 1 bar1)
         (tu:insert-after bar1 0 accent))
       bar1))
   ;; other bars
   (loop :for (bar1 bar2) :on sequence :while bar2
     ;;; NOTE: code repetition
     :collect (if (and (length-notep (first bar2))
                       (not (eql (first (last bar1)) 'tie)))
                (if (and (= (count-notes bar2) 1)
                         (eql (first (last bar2)) 'tie))
                  (tu:replace-element (merge-articulations (list accent 'tie)) 1 bar2)
                  (tu:insert-after bar2 0 accent))
                bar2))))
|#

#|
(articulate-phrase '((1/16 1/16 1/16) (1/16 1/16 1/16) (1/16 1/16 1/16)) :accent 'marc)

(let ((sequence '((h h) (q q q q tie) (q q q q) (-q q) (q q q q))))
  (zip-articulations (omn :articulation (omn-replace :pitch '(c4) sequence))
                     '(stacc)))
|#

;;; TODO:
;; - don't overwrite existing articulations (e.g., ties)
;; - only add articulation on first note of bar if it does not start with rest nor it is a tied over note 
;;

#|
; Very hard to overcome limitation of map-position-in-bar to handle rests and ties, so I just start from scratch
(defun articulate-phrase (phrase &key (accent 'ten) (default nil) (section nil) (parameter :articulation))
  "Add articulations to phrase for more clear rhythmic accents, e.g, tenuto on every first beat, and stacc otherwise.

  Examples:
  (articulate-phrase (gen-repeat 3 '((s s s))) :accent 'marc)
  (articulate-phrase (gen-repeat 3 '((s s s))) :accent 'f :default 'p :parameter :velocity)
  "
  (map-position-in-bar 0 parameter 
                       (if default
                         (omn-replace parameter default phrase)
                         (if (omn-formp phrase)
                           phrase 
                           ;; assume phrase is only lengths
                           (make-omn :length phrase :pitch '(c4))))
                       #'(lambda (ignore)
                           accent)
                       :section section))
|#


