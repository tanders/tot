;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;; articulation to display a score that is no solution
(add-text-attributes
 '(no-solution "no solution"))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionality depending on the libraries Cluster Engine and Cluster Rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO:
;;; - ? Arg: split-score? Add support to split score at harmonic changes (instead of shared rests).
;;; - Cut superfluous notes/bars at end (and stop search in time) -- not (always) necessary?
;;; OK - insert all velo and articulations in score into result
;;; OK - Move this function into a new file constraints.lisp into my library tot
;; ... solution can be repeating input score
(defun revise-score-harmonically (score harmonies scales
				  &key
				    (constrain-pitch-profiles? T)
				    ;; BUG: argument ignored and rule not applied?
				    (constrain-pitch-intervals? T)
				    (pitch-domains nil)
				    (pitch-domains-extension 2)
				    (rules :default)
				    (additional-rules nil)
				    (split-score? nil)
				    (length-adjust? T)
				    (print-csp? nil)
				    (unprocessed-cluster-engine-result? nil))
  ;; TMP: unused arg doc
  ;;; TODO:  - pitch-domains (property list): specifying a pitch domain in the Cluster Engine format for every part in score, using the same instrument ID. If no domain is specified for a certain part then a chromatic domain of the ambitus of the input part is automatically generated.
  "CSP transforming the input `score' such that it follows the underlying harmony specified. 
  The rhythm of the input score is left unchanged. The pitches follow the melodic and intervallic profile of the input voices/parts, and various additional constraints are applied.
  
  Args:
  - score (headerless score): See {defun preview-score} for its format. NOTE: the total number of parts is limited to 8 (?) by internal Cluster Engine limitations.
  - harmonies (OMN expression): OMN chords expressing the harmonic rhythm and chord changes of the underlying harmony 
  - scales (OMN expression): OMN chords expressing the rhythm of scales and scale changes of the underlying harmony
  - constrain-pitch-profiles? (Boolean): Whether to constrain the pitch profile
  - constrain-pitch-intervals? (Boolean): Whether to constrain the pitch intervals
  - pitch-domains: Specifies the chromatic pitch domain of every part in `score' in the following format: (<part1-name-keyword> (<lowest-pitch> <highest-pitch>) ...), where pitches are Opusmodus pitch symbols. For example, if your `score' specifies the first and second violin with the keywords :vl1 and :vl2, `pitch-domains' could be (:vl1 (g3 c6) :vl2 (g3 c6)).     
    By default, the ambitus of each part of `score' is used to automatically deduce the chromatic pitch domain for that part. When overwriting this default, the pitch domain of all parts must be given explicitly.
  - pitch-domains-extension: 
     If 0, the pitch domain for each part is the ambitus of pitches in the respective part of `score' (including all semitones within that ambitus). 
     If a positive integer, the pitch domain of each part is the respective ambitus extended by this number of semitones both up an down. For example, if `pitch-domains-extension' is 2, and the ambitus of some part in `score' ranges from C4 to C5, then the pitch domain for that part ranges chromatically from Bb3 (2 semitones down at the lower end) to D5 (2 semitones up at the upper end).
     A pitch domain extension can also be specified for each part separately in either of the following formats: (<part1-name-keyword> <extension> ...) or (<part1-name-keyword> (<lower-extension> <upper-extension>) ...). For example, to specify that the pitch domain of the 1st violin is extended by 0 semitones at the lower, but 7 semitones at the higher end you would write (:vln1 (0 7)), if you are using :vln1 as keyword in `score'. 
      When specifying a pitch domain extension for any part explicitly, the extensions for all parts must be given explicitly.
  - rules (list of cluster-engine rule instances): further rules to apply, in addition to the automatically applied pitch/interval profile constraints. Note that the scales and chords of the underlying harmony are voice 0 and 1, and the actual voices are the sounding score parts. 
    If `rules' is :default, then some default rule set is used. 
  - additional-rules (list of cluster-engine rule instances): convenience argument to add rules without overwriting the default rule set by leaving the argument `rules' untouched.
  - split-score? (Boolean): Feature that can speed up the search for longer scores. If true, the search is performed on score sections one by one. The score is split at shared rests (see `split-score-at-shared-rests').
    NOTE: You cannot use index rules in this modus! (Indices would not be correct)
  - length-adjust? (Boolean): If T, all parts of the output score are forced to be of the same length as the total length of the input score (otherwise the output score can be longer).
  - print-csp? (Boolean): For debugging the CSP: if true, print list of arguments to constraint solver cr:cluster-engine.
  - unprocessed-cluster-engine-result? (Boolean): For debugging the CSP: if true return the result of cluster engine directly, without translating it into an OMN score.

  Example:

Lets first have some input polyphonic score, defined as a headerless score. For previewing this score at the same time saving it into a variable we first ensure that preview-score returns the given headerless score by setting `*preview-score-return-value*' acordingly. 

;;; (setf *preview-score-return-value* :headerless-score)
;;; 
;;; (setf polyphonic-score
;;;       (preview-score 
;;;        '(:vl1 ((-3h fs4 pp a4) (q gs4 fs4) (3h e4 eb4 d4) (h eb4))
;;;          :vl2 ((h d4 pp) (h e4) (h f4 tie) (h f4)))))

The underlying harmony -- both chords and scales -- are declared now.

;;; (setf harmonies
;;;       '((h c4eb4g4a4) (h c4eb4g4a4)       
;;;         (h f4g4a4c4) (h f4g4a4c4)))
;;; 
;;; (setf scales
;;;       (list (append (length-merge (flatten (omn :length harmonies))) 
;;;                     (chordize '(c4 cs4 ds4 f4 g4 a4 b4)))))

Some playback customisation of preview-score is silencing the first two parts (chords and scales are only an underlying analysis).

;;; (setf *default-preview-score-instruments*
;;;       '(;; silent harmony -- :volume 0
;;;         :scales (:program 'violin :sound 'gm :channel 16 :volume 0)
;;;         :chords (:program 'violin :sound 'gm :channel 16 :volume 0)
;;;         :vl1 (:program 'violin :sound 'gm :channel 1)
;;;         :vl2 (:program 'violin :sound 'gm :channel 1)))

The next code snippet shows the actual call to `revise-score-harmonically'. In this example, the resulting score is previewed and at the same time also saved in a variable for later inspection, postprocessing etc.

;;; (setf revised-polyphonic-score
;;;       (preview-score (revise-score-harmonically polyphonic-score harmonies scales)))

The default pitch domains (the ambitus of parts in the score) can be extended. In the following example it is extended by 3 semitones for all parts (:vl1 and :vl2) in both directions (at the upper and lower end of each ambitus). See the discussion of the argument `pitch-domains-extension' above for further options.

;;; (preview-score 
;;;  (revise-score-harmonically polyphonic-score harmonies scales
;;;  			        :pitch-domains-extension 3))


The default pitch domains (the ambitus of parts) can be overwritten explicitly. This argument can be combined with the argument `pitch-domains-extension', which then extends the explicitly given pitch domains.

;;; (preview-score 
;;;  (revise-score-harmonically polyphonic-score harmonies scales
;;; 			        :pitch-domains '(:vl1 (c4 g5) :vl2 (g3 c5))))

TODO: demonstrate how default rules are overwritten.

  "
  (if split-score?
      ;; if split-score? process score section wise 
      (let* ((first-instrument (first score)) ;; used to ensure unified time sigs
	     (first-score-part (second score))
	     ;;; TODO: decide: take out unify-time-signature for efficiency?
	     (aux (multiple-value-list
		   (split-score-at-shared-rests (unify-time-signature first-instrument score))))
	     (split-scores (first aux))
	     (split-positions (second aux))
	     ;; split harmonies at split-positions, but first ensure they follow the same time sigs
	     (split-harmonies (tu:subseqs (copy-time-signature first-score-part harmonies) split-positions))
	     (split-scales (tu:subseqs (copy-time-signature first-score-part scales) split-positions)))
	(apply #'append-parts
	       (mapcar #'(lambda (score-section harmonies-section scales-section)			   
			   (revise-score-harmonically
			    score-section harmonies-section scales-section
			    :constrain-pitch-profiles? constrain-pitch-profiles?
			    :constrain-pitch-intervals? constrain-pitch-intervals?
			    :pitch-domains pitch-domains
			    :pitch-domains-extension pitch-domains-extension
			    :rules rules
			    :additional-rules additional-rules
			    :split-score? nil
			    :print-csp? print-csp?
			    :unprocessed-cluster-engine-result? unprocessed-cluster-engine-result?))
		       split-scores
		       split-harmonies
		       split-scales)))
      ;; 
      (let* ((parts (get-parts-omn score))
	     (pitch-domain-specs (if (listp pitch-domains-extension)
				     pitch-domains-extension
				     (tu:map-plist-vals #'(lambda (_)
							    (declare (ignore _))
							    pitch-domains-extension)
							score)))
	     (first-part (first parts))
	     (time-sigs (PWGL-time-signatures 
			 (get-time-signature first-part)))
	     (csp (list
		   ;; min number of variables with much upper headroom (multiplied with 2), because search is stopped by stop rule
		   (* 2 (apply #'max (cons (length time-sigs) (mapcar #'count-notes parts))))
		   ;; rules
		   (let (;; position of all voices in score starting from 2 after scales and chords
			 (voice-ids (gen-integer 2 (+ (length (get-instruments score)) 1))))
		     (ce::rules->cluster 
		      (ce::rules->cluster 
		       (when constrain-pitch-profiles?
			 (cr:follow-profile-hr 
			  parts :voices voice-ids :mode :pitch :constrain :profile))
		       (when constrain-pitch-intervals?
			 (cr:follow-profile-hr 
			  parts :voices voice-ids :mode :pitch :constrain :intervals))
		       (ce:r-predefine-meter time-sigs)
		       ;; stop search after all parts (including chords and scales) reach the duration of the input score
		       ;; !! NOTE that rule doc says "The stoptime has to be reached in all given voices. Note that the rule ignors the pitch information."
		       (ce:stop-rule-time (append '(0 1) voice-ids) (score-duration score) :and))
		      (apply
		       #'ce::rules->cluster 
		       (if (not (eq :default rules))
			   (append rules additional-rules)
			   (append 
			    (list 
			     ;; more rules
			     (cr:only-scale-pcs :voices voice-ids :input-mode :all 
						:scale-voice 0)
			     (cr:only-chord-pcs :voices voice-ids :input-mode :beat ; :1st-beat
						:chord-voice 1) 
			     (cr:long-notes-chord-pcs :voices voice-ids :max-nonharmonic-dur 1/4)
			     (cr:stepwise-non-chord-tone-resolution
			      :voices voice-ids :input-mode :all :step-size 3)
			     (cr:chord-tone-before/after-rest :voices voice-ids :input-mode :all)
			     (cr:chord-tone-follows-non-chord-tone :voices voice-ids :input-mode :all)
			     (cr:no-repetition :voices voice-ids :window 3)
			     (cr:resolve-skips :voices voice-ids :resolution-size 4)
			     )
			    additional-rules)))))
		   ;; meter domain
		   (remove-duplicates time-sigs :test #'equal) 
		   ;; voice domains
		   `(;; scale rhythm
		     (,(flatten (omn :length scales)))
		     ;; scale pitches
		     (,(pitch-to-midi (flatten (omn :pitch scales))))
		     ;; harmonic rhythm for chords 
		     (,(flatten (omn :length harmonies)))
		     ;; harmony: chords
		     (,(pitch-to-midi (flatten (omn :pitch harmonies))))
		     ,@(mappend #'(lambda (instr)
				    (let* ((part (getf score instr))
					   (pitch-dom-spec (getf pitch-domain-specs instr))
					   (raw-ambitus (if pitch-domains ; inefficient -- checks pitch-ambitus for every instrument, but that should be fine here
							    (mapcar #'pitch-to-integer (getf pitch-domains instr))
							    (if
							     (omn :pitch (flatten part))
							     ;; part contains pitches
							     (find-ambitus part)
							     ;; otherwise return some default domain 
							     '(0 1))))
					   (pitch-dom-ambitus (cond ((integerp pitch-dom-spec)
								     (list (- (first raw-ambitus) pitch-dom-spec)
									   (+ (second raw-ambitus) pitch-dom-spec)))
								    ((listp pitch-dom-spec)
								     (list (- (first raw-ambitus) (first pitch-dom-spec))
									   (+ (second raw-ambitus) (second pitch-dom-spec))))
								    )))
				      `( 
					;; rhythm domain: predefined motif
					(,(omn :length (omn-merge-ties (flatten part))))
					;; pitch domain
					,(mclist
					  (apply #'gen-integer 
						 (mapcar #'(lambda (p) (+ p 60)) 
							 pitch-dom-ambitus)))
					)))
				(get-instruments score))
		     ))))
	(when print-csp?
	  (pprint csp))
	(if unprocessed-cluster-engine-result?
	    (apply #'cr:cluster-engine csp)
	    (let ((score-dur (score-duration score)))
	      ;; reduce total duration of each part in score segment to duration of input score
	      (map-parts-equally 
	       (copy-cluster-engine-pitches-to-score score (apply #'cr:cluster-engine csp))
	       #'(lambda (dur part)
		   ;; (break)
		   (if length-adjust?
		       ;; length-adjust does only work for flat list
		       ;; (length-adjust dur part :flat T)
		       ;; TMP: 
		       (flattened-length-adjust dur part)
		       part))
	       `(,score-dur _)	       
	       ))))))



;;; TODO:
;; - Test grace notes are working
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
 (case cluster-engine-score
  ;; in case of failure return score that displays (preview-score"no solution"
  (:no-solution '(:1 ((w no-solution)))) 
  (otherwise
   (let* ((cluster-engine-score-without-time-sigs (butlast cluster-engine-score))
          (length-lists (tu:at-even-position cluster-engine-score-without-time-sigs))
	  (pitch-lists (tu:at-odd-position cluster-engine-score-without-time-sigs))
	  (time-sigs (mapcar #'(lambda (ts) (append ts '(1)))
			     (first (last cluster-engine-score)))))
     (tu:one-level-flat 
      (loop 
	 for lengths in length-lists
	 for pitches in pitch-lists
	 for instrument in (or instruments
			       (mapcar #'(lambda (i) (intern (write-to-string i) :keyword))
				       (gen-integer 1 (length length-lists))))
					; for time-sig in time-sigs
         ;; TMP: if statement
         ;; if (not (= (length lengths) (length pitches)))
         ;; do
         ;; else do
         collect (list instrument
		       (omn-to-time-signature 
			(make-omn  
			 ;; 0 length are acciaccaturas 
			 :length (mapcar #'(lambda (l) (if (= l 0) 1/8 l))
					 lengths)
                         ;; BUG: articulations also swallowed?
			 :articulation (mapcar #'(lambda (l) (if (= l 0) 'acc '-))
					       lengths)
			 :pitch (tu:mappend #'(lambda (p) 
						(if (listp p)
						    (chordize (midi-to-pitch p))
						    (list (midi-to-pitch p))))
                                            ;; BUG: needed?
					    ;; Hack: replace all nil (pitches of rests, but also at end of score) 
					    (substitute 60 nil pitches))
                         ;; NOTE: changed from T to nil 
			 :swallow T)
			time-sigs))))))))

;;; TODO: 
;;; what if scales-position / chords-position is nil? or no scales/chords in score? 
(defun copy-cluster-engine-pitches-to-score (score cluster-engine-score) 
  "Carries over the pitches of cluster-engine-score found by reharmonisation with the Cluster Engine constraint solver into `score', the score used to shape `cluster-engine-score'. 

  Args:
  - score (headerless score): see {defun preview-score} for format description of headerless scores. 
  - cluster-engine-score: result of `cluster-engine:clusterengine' (https://github.com/tanders/cluster-engine). 

  NOTE: The first two parts in `cluster-engine-score' must be a harmonic analysis (scales and chords parts).

  NOTE: `cluster-engine-score' must contain the same number and order of parts, notes and rhythmic structure as `score'. 
  "
  ;; (declare (optimize (debug 3)))
  (case cluster-engine-score
    ;; in case of failure return score that displays "no solution"
    (:no-solution '(:1 ((w no-solution))))
    (otherwise
     (mix-parts 
      ;; scales and chords part (analysis)
      (cluster-engine-score 
       (append (first-n 4 cluster-engine-score)
	       (last cluster-engine-score))
       :instruments '(:scales :chords))  
      ;; actual score
      (let* ((pitch-lists (tu:at-odd-position cluster-engine-score)))
	(loop
	   for pitches in (cddr pitch-lists) ;; remove pitches of scales and chrods
	   for part-omn in (get-parts-omn score)
	   for instrument in (get-instruments score)
	   append (list instrument
			(copy-time-signature
			 part-omn
			 ;;; omn-replace not work with ties -- use make-omn instead
			 (let ((flat-omn (flatten part-omn)))
			   ;; (break)
			   (make-omn :length (omn-merge-ties flat-omn)
				     :pitch (flatten 
					     (tu:mappend #'(lambda (p) 
							     (if (listp p)
								 (chordize (midi-to-pitch p))
								 (list (midi-to-pitch p))))
							 (remove nil pitches)))
				     :velocity (omn :velocity flat-omn)
				     :articulation (omn :articulation flat-omn)
				     :leg (omn :leg flat-omn)
				     :swallow nil))
			 #|
			 (omn-replace :pitch  (flatten 
					       (tu:mappend #'(lambda (p) 
							       (if (listp p)
								   (chordize (midi-to-pitch p))
								   (list (midi-to-pitch p))))
							   (remove nil pitches)))
				      (flatten part-omn))
			 |#
			 ))))))))

#|
;;; omn-replace does not work for ties
(omn-replace :pitch '((q g4 tie) (q g4) (q a4))
	     '(d4 e4))

|#


(defun preview-cluster-engine-score (score)
  "Just shorthand for (preview-score (cluster-engine-score score))"
  (preview-score (cluster-engine-score score)))

