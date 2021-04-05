;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pitch-related utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Called by macro expansion, therefore defined here -- is this really necessary??
(defmethod ji-pc-ratio ((x rational))
  "Transforms the frequency ratio `x' into the interval [1, 2) to represent a corresponding pitch class expressed as a fraction."
  (cond ((>= x 2) (let ((octaves (floor (log x 2))))
		    (/ x (expt 2 octaves))))
	((< x 2) (let ((octaves (abs (floor (log x 2)))))
		   (* x (expt 2 octaves))))
	(T x)))

#|
(ji-pc-ratio 9/4)
=> 9/8

(ji-pc-ratio 2/9)
=> 16/8

(ji-pc-ratio 3/2)
=> 3/2

(ji-pc-ratio 3)
=> 3/2

(ji-pc-ratio 1.5)
; error
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; deftemperament
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG: Documentation unfinished
;; TODO: Consider getting custom syntax colouring for user-defined macros: deftemperament currently not highlighted...
(defmacro deftemperament (name vals generators &optional docstring)
  "Define a temperament with the given `vals' and `generators' bound to the `name'. This results in a method of the given name expecting a ratio and returning the temperament pitch in cents. The `vals' and `generators' can also later be accessed with `get-temperament-vals' and `get-temperament-generators'.

Note that `vals' and `generators' are evaluated (so they can be an expression resulting in the actual values), while the name is not.

* Arguments:


"
  ;; Optimisation: calculate vals and generators from potentially given expressions (at compile time)
  `(let ((my-vals ,vals)
	 (my-generators ,generators)
	 (function-docstring (concatenate 'string
					  "Translate a JI interval (ratio) into the corresponding interval measured in cents of this temperament:
"
					  ,docstring
					  "

* Arguments:
  - ratio (rational): a JI interval for which we want to get a tempered interval")))
     
     (store-temperament ',name my-vals my-generators)
     (setf (documentation ',name 'setf) ,docstring)
     ;; BUG: Doc should only be added to method, not to a plain function, but so far I did not find out how to access the doc from methods. Seems to be special.
     ;; See also http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm
     (setf (documentation ',name 'function) function-docstring)
     (defmethod ,name ((ratio rational))
       ;; TODO: Find out how to access method doc
       function-docstring
       (ratio-to-regular-temperament-cents ratio my-vals my-generators))))


#|
(deftemperament 11-limit-12-EDO
    (list (edo-val 12 '(0 7 4 10 6)))
  '(100.0)
  "12-EDO temperament with an 11-limit mapping.")
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; def-tempered-score
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chord-sequences-to-parts (sequence n)
  "Return `n' monophonic sequences that distribute chords tones in `sequence' across the returned sequences."
  (mapcar (lambda (seq)
	    ;; TODO: optimise: re-barring afterwards is inefficient
	    (copy-time-signature sequence (flatten seq)))
	  (ta-utils:mat-trans
	   (loop
	      for event in (single-events (om:flatten sequence))
	      for pitches = (if (event-restp event)
				NIL
				(melodize (second event)))
	      for joined-attributes = (fourth event)
	      for all-attributes = (when joined-attributes
				     (disjoin-attributes joined-attributes))
	      for ji-accidentals = (remove-if-not #'is-JI-accidental all-attributes)
	      for other-attributes = (remove-if #'is-JI-accidental all-attributes)
	      collect (loop
			 for i from 0 to (1- n)
			 for len = (first event)
			 for pitch = (nth i pitches)
			 for ji-acc = (nth i ji-accidentals)
			 for full-attr = (join-attributes (cons ji-acc other-attributes))
			 collect (if pitch 
				     (list* len
					   pitch
					   (third event) ; dynamics
					   ;; Empty attributes skipped (these are not handled well by copy-time-signature)
					   (when (not (eql full-attr '-))
					     (list full-attr)))
				     (corresponding-rest (list len))
				     ))))))
#|
(pprint
(chord-sequences-to-parts '((h. c5) (h c4e4g4 stacc -q)) 3)
)


;; (ta-utils:mat-trans
;;  '(((Q C4 MF STACC) (Q E4 MF STACC) (Q G4 MF STACC))
;;    ((Q C5 MF) (-1/4) (-1/4))))

;; (ta-utils:mat-trans '(((a 1) (a 2) (a 3)) ((b 1) (b 2) (b 3)) ((c 1) (c 2) (c 3))))

|#


(defun aux-def-tempered-score-mpe-parts-var-name (instr)
  "Return a variable name (symbol) for the quasi MPE parts for the given instr (symbol)."
  (intern (concatenate 'string (symbol-name instr) "-MPE-PARTS")))
; (aux-def-tempered-score-mpe-parts-var-name 'instr1)

(defun aux-def-tempered-score-instruments (temperament score-parts)
  "[Called at compile time by macro] Generate the instruments body for def-score generated by def-tempered-score, where the instruments are split into multiple instruments on different MIDI channels for quasi MPE playback."
  (loop for part in score-parts
     for instr = (first part)
     for MPE-parts-var-name = (aux-def-tempered-score-mpe-parts-var-name instr)
     for instr-args = (rest part)
     ;; for parts = (chord-sequences-to-parts )
     ;; TODO: avoid eval
     ;; ? NOTE: channel param must be given explictly to def-tempered-score for this to work
     for chans = (eval (getf instr-args :channel
			     ''(1)))
     append (loop
	       for chan in (ta-utils:ensure-list chans)
	       for i from 0 
	       collect ;; `(let ((part-omn (get-omn-part ,(getf instr-args :omn) ,i)))
			  `(,instr
			    :channel ,chan
			    ;; TODO: Really inefficient code repetition -- chord-sequences-to-parts
			    ;; called for every part again. Optimise with let in scope around
			    ;; def-score
			    ;; TODO: Vector instead of list element access
			   :omn (nth ,i ,MPE-parts-var-name)
			   ;; :omn (get-omn-part ,(getf instr-args :omn) ,i)
			   :tuning (omn-to-tunings ,temperament
						   ;; TODO: Really inefficient code repetition, but
						   ;; perhaps unavoidable within the generated
						   ;; def-score code for the part -- Or I wrap some
						   ;; let expression for all parts around the whole
						   ;; def-score...
						   (nth ,i ,MPE-parts-var-name))
			   ;; :omn part-omn
			   ;; :tuning (omn-to-tunings ,temperament part-omn)
			   ,@(ta-utils:remove-properties '(:omn :channel)
							 instr-args)))))
;; )
#|
(aux-def-tempered-score-instruments '31-limit-12-EDO 
				    '((instr1
				       :omn '((h c4eb4bb4 1K+-5K+-7K+stacc))
				       ;; :channel '(1)
				       :port "IAC Bus 1"
				       :sound 'gm
				       ;; :program pianoteq
				       )))
|#

#|
(defun aux-def-tempered-score (temperament score-parts)
  "[Called at compile time by macro]"
  (loop for part in score-parts
     for instr = (first part)
     for instr-args = (rest part)
     collect (list* instr
		    :tuning `(omn-to-tunings ,temperament ,(getf instr-args :omn))
		    instr-args)))
|#

;; TODO: Avoid code repetition shared with aux-def-tempered-score-instruments, e.g., val of var chans: more aux functions...
(defun aux-def-tempered-score-let-decls (score-parts)
  "[Called at compile time by macro] Generate the variable declarations around the def-score generated by def-tempered-score, which split the instrument parts into multiple parts for the different MIDI channels. The let is needed for efficiency (value reuse)."
  (loop for part in score-parts
     for instr = (first part)
     for MPE-parts-var-name = (aux-def-tempered-score-mpe-parts-var-name instr)
     for instr-args = (rest part)
     ;; TODO: avoid eval
     for chans = (eval (getf instr-args :channel
			     ''(1)))
     ;; TODO: avoid eval
     ;; TODO: optimise: chord-sequences-to-parts should return vectors instead of lists
     for MPE-parts = (chord-sequences-to-parts (eval (getf instr-args :omn)) (length chans))
     collect `(,MPE-parts-var-name ',MPE-parts)))

#|
(aux-def-tempered-score-let-decls '((instr1
				     :omn '((h c4eb4bb4 1K+-5K+-7K+stacc))
				     :channel '(1 2 3)
				     :port "IAC Bus 1"
				     :sound 'gm
				     ;; :program pianoteq
				     )))
|#

(defmacro def-tempered-score (name global-args &rest instruments)
  "The same as the Opusmodus builtin `def-score', but with the added arg `temperament'.

* Arguments:
 TODO: Allow somehow for temperament to change over time (like arguments such as tempo etc.). 
 - temperament (symbol): a temperament defined with deftemperament.

The other arguments are documented for the builtin `def-score'.
"
  (destructuring-bind (&key title subtitle composer writer copyright
			    time-signature (key-signature ''chromatic) (merge-rests T)
			    (rewrite-lengths T) ignore-tempo ; ignore—velocity
			    octave-shift (accidentals :natural) flexible-clef
			    ignore-time-signature (tempo 60) file
			    start end layout
			    ;; added param
			    (temperament `',*current-temperament*))
      global-args
    (assert time-signature (time-signature) "No time-signature given")
    `(let (,@(aux-def-tempered-score-let-decls instruments))
       (def-score ,name ,(list
			  :title title :subtitle subtitle :composer composer :writer writer :copyright copyright
			  :key-signature key-signature :time-signature time-signature :merge-rests merge-rests
			  :rewrite-lengths rewrite-lengths
			  ;; TODO: Report wrongly documented arg
			  ;; :ignore—velocity ignore—velocity ; Incorrect keyword argument
			  :ignore-tempo ignore-tempo
			  :octave-shift octave-shift :accidentals accidentals :flexible-clef flexible-clef
			  :ignore-time-signature ignore-time-signature :tempo tempo :file file
			  :start start :end end :layout layout
			  )
	 ,@(aux-def-tempered-score-instruments temperament instruments)))))

#|


(def-tempered-score score-name
    (:temperament '31-limit-JI
     :time-signature '(4 4))
    (instr1 :omn '((w c4fs4 c4fs4 1K+-5K c4fs4 1K+-7K c4fs4 1K+-11K))
	    :channel '(1 2 3)
	    :port "IAC Bus 1"
	    :sound 'gm
	    ))


(display-musicxml *last-score* :display :window)


|#


#|
;; NOTE: set for each instrument sufficient number of channels and suitable port
(defparameter *default-preview-score-instruments*
  '(;; TODO: Ideally deduce number of necessary channels automatically -- and use splitting into multiple voices/channels only for playback, not for score notation
    :snippet (:program 'acoustic-grand-piano :sound 'gm :channel '(1 2 3 4 5 6 7 8) :port "IAC Bus 1")
    :vln (:program 'violin :sound 'gm :channel '(1 2 3 4) :port "IAC Bus 1")
    :vlc (:program 'cello :sound 'gm :channel '(5 6 7 8)) :port "IAC Bus 1")
  )

(setf mat '(w c4 c4 -5K c4 -7K c4 -11K))

(setf mat '(w c4fs4 c4fs4 1K+-5K c4fs4 1K+-7K c4fs4 1K+-11K))

(setf mat '(w c4e4 c4e4 1K+-5K c4e4 1K+-7K c4e4 1K+-11K))

(setf mat '((h c4eb4bb4 1K+-5K+-7K+stacc)
	    (h c4eb4bb4 stacc)
	    (h c4eb4bb4 1k+-5K+-7k+stacc)))

(setf mat '((h c4eb4bb4 1K+-5K+-7K+stacc)
	    (h c4eb4bb4)
	    (h c4eb4bb4 1k+-5K+-7k+stacc)))

(def-tempered-score score-name
    (; :temperament '31-limit-JI
     ; :temperament '23-limit-31-EDO
     :time-signature '(4 4))
    (instr1 :omn mat
	    :channel '(1 2 3)
	    :port "IAC Bus 1"
	    :sound 'gm
	    ;; :program pianoteq
	    ))

(display-musicxml *last-score* :display :window)

(setf mat '((q c4 c4 -5K bb4) (q cs4 e4 e4 -5K)))

;; TODO: Collect correct chords from Erlich paper. Perhaps even encode chords by ratios, generate progression algorithmically and then translate them into OMN automatically...
(setf symmetric-decatonic-scale-chords ;; Symmetric decatonic scale
'((h C4E4G4BB4 1K+-5K+1K+-7K) 
  ;; BUG: Unsupported accidental 5K*-7K
  (DB4F4Ab4CB5 5K+1K+5K+5K) ;  CS4 -5K DB4 5K
  ;; NOTE: Fifth A missing
  (D4FS4C5 1K+-5K+-7K) 
  ;; NOTE: Harm 7th missing
  (E4G4B4 -5K+1K+-5K) ;  FB4 5K
  ;; NOTE: Harm 7th missing
  (F4AB4C5 1K+5K+1K)   
  ;; BUG: Unsupported accidental 5K*-7K
  (GB4BB4DB5FB5 5K+1K+5K+5K) ;  FS4 -5K GB4 5K
  (G4B4 1K+-5K) 
  (GS4 -5K) ;  AB4 5K
  (BB4) 
  (B4 -5K) ;  CB4 5K
  (c5)))

(def-tempered-score score-name
    ( 
     ; :temperament '11-limit-12-EDO
     ; :temperament '31-limit-JI
     :temperament '11-limit-POTE-pajara    
     :time-signature '(4 4))
    (instr1 :omn symmetric-decatonic-scale-chords
	    :channel '(1 2 3 4)
	    :sound 'gm
	    :port "IAC Bus 1"
	    ;; :program pianoteq
	    ))

;; BUG: TODO: Ensure that channel is a list...
(def-tempered-score 31-EDO-test
    (:temperament '11-limit-31-edo
     :tempo 50
     :time-signature '(4 4))
  
  (inst1 :omn '((h fs5) (h fs5 -11K) (h fs5) (w fs5 -11K))
  	 :channel 1
  	 :sound 'gm
  	 :program 0
  	 :port 0
  	 )
  (inst2 :omn '((h bb4) (h bb4 -7K) (h bb4)  (w bb4 -7K))
  	 :channel 2
  	 :sound 'gm
  	 :program 0
  	 :port 0
  	 )
  ;; (inst2 :omn '((h eb4) (h eb4) (h eb4) (w eb4))
  ;; 	 :channel 2
  ;; 	 :sound 'gm
  ;; 	 :program 0
  ;; 	 :port 0
  ;; 	 )
  (inst3 :omn '((h eb4) (h eb4 -7K) (h eb4) (w eb4 -7K))
	 :channel 3
	 :sound 'gm
	 :program 0
	 :port 0
	 )
  (inst4 :omn '((h c4) (h c4) (h c4) (w c4))
	 :channel 4
	 :sound 'gm
	 :program 0
	 :port 0
	 )
  )

|#

