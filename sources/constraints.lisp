;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;; articulation to display a score that represents no solution
(add-text-attributes
 '(no-solution "no solution")) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionality depending on the libraries Cluster Engine and Cluster Rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (mix-scores
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


(defun chords->domain (chords &key (transpositions '(0)) preview?)
  "Translates `chords' (list of OMN chords) and `transpositions' (list of ints) into a domain of chords for Cluster Engine (list of lists of MIDI note numbers), where every given spectrum is contained in all given transpositions. 

  If `preview?' is T, the resulting domain is returned as list of OMN chords for previewing in Opusmodus.

  Example:
  (chords->domain '(c4e4g4 b3d4g4) :transpositions '(0 2 4))"
  (let ((result (loop for transposition in transpositions
                  append (pitch-transpose transposition chords))))
    (if preview?
      result
      (mclist (pitch-to-midi result)))))

; (chords->domain (list (midic->omn-chord (tr:n-sp-gen 3600 (gen-integer 8 15) 0))) :transpositions (gen-integer 0 11) :preview? T)


;; TODO:
;; - ?? consider alternative to 'app
;; - !! split notation of chords and scales at c4 to notate on two different staffs.
;;   Hm -- why not doing this in revise-score-harmonically as well -- only slightly widens score format by two staffs.
;;   OLD: can be done in preview score, but best if scales are not already transformed into gracenotes before
(defmethod _chords-to-gracenotes ((chord symbol) &optional (grace-length 'e))
  "[Aux] Turns a chord into a a sequence of grace notes.

  Args
  grace-length (OMN length): the notated length assigned to the grace notes.

  Example:
  (_chords-to-gracenotes 'd2a2d3fs3bb3c4d4e4fs4gs4bb4c5)
  (_chords-to-gracenotes 'c4)"
  (let ((chord-ps  (melodize chord)))
    (cons 'app (loop for pitch in chord-ps
                 append (list grace-length pitch)))))

(defun chords-to-gracenotes (sequence &optional (root? T))
  "For notating underlying scales as sequence of grace notes.

  Args: 
  root (OMN pitch): can be overwritten in case scales are notated on separate staffs and the treble clef portion does not contain the actual root."
 (copy-time-signature 
  sequence
   (loop for (len pitch vel art) in (single-events (flatten sequence))
      append (append (list (_chords-to-gracenotes pitch)) 
		     (list len)
		     (if root?
			 (list (first (melodize pitch)))
			 ;; constant placeholder pitch in case root is not shown
			 (list 'c4))
		     (when root? (list vel)) 
		     (cond ((and art root?) (list art))
			   ;; root note place holders to ignore are marked with a "o" like harmonics 
			   ((not root?) (list 'harm))
			   (T nil))))
  ))

#|
;; test
(chords-to-gracenotes '((h. d2a2d3fs3bb3c4d4e4fs4gs4bb4c5)
                        (h d2a2d3fs3bb3c4d4e4fs4gs4bb4c5)
                        (h. d2a2d3fs3bb3c4d4e4fs4gs4bb4c5)
                        (h. d2a2d3fs3bb3c4d4e4fs4gs4bb4c5))
		      nil)
|#

;;; TODO:
;;; - BUG: Ties in input score can be ignored. Tones tied together in input score can turn out with different pitches in result.
;;; - ? Arg: split-score? Add support to split score at harmonic changes (instead of shared rests).
;;; - Cut superfluous notes/bars at end (and stop search in time) -- not (always) necessary?
;;; OK - insert all velo and articulations in score into result
;;; OK - Move this function into a new file constraints.lisp into my library tot
;; ... solution can be repeating input score
(defun revise-score-harmonically (score harmonies scales
					; &optional (scales nil) ;; making scales optional would be nice, but it is not this easy...
				  &key
				    (constrain-pitch-profiles? T) 
				    (constrain-pitch-intervals? T)
				    (pitch-domains nil)
				    (pitch-domains-extension 2)
				    (pitch-domain-limits nil)
				    (rules :default)
				    (additional-rules nil)
				    (split-score? nil)
				    (length-adjust? T)
				    (print-csp? nil)
				    (unprocessed-cluster-engine-result? nil))
  "CSP transforming the input `score' such that it follows the underlying harmony specified. 
  The rhythm of the input score is left unchanged. The pitches follow the melodic and intervallic profile of the input voices/parts, and various additional constraints are applied.
  
  Args:
  - score (headerless score): See {defun preview-score} for its format. NOTE: the total number of parts is limited to 8 (?) by internal Cluster Engine limitations.
  - harmonies (OMN expression): OMN chords expressing the harmonic rhythm and chord changes of the underlying harmony 
  - scales (OMN expression or NIL): OMN chords expressing the rhythm of scales and scale changes of the underlying harmony. If `scales' is NIL (convenience when there are no constrains restricting the underlying scale) then simply the underlying harmonies are doublicated in the scales staff (the staff is not skipped to preserve the order of parts for the constraints that depend on it).
  - constrain-pitch-profiles? (Boolean or int): Whether to constrain the pitch profile. If an int, it sets the weigth offset of the profile rule.
  - constrain-pitch-intervals? (Boolean or int): Whether to constrain the pitch intervals. If an int, it sets the weigth offset of the profile rule.
  - pitch-domains: Specifies the chromatic pitch domain of every part in `score' in the following format: (<part1-name-keyword> (<lowest-pitch> <highest-pitch>) ...), where pitches are Opusmodus pitch symbols. For example, if your `score' specifies the first and second violin with the keywords :vl1 and :vl2, `pitch-domains' could be (:vl1 (g3 c6) :vl2 (g3 c6)).     
    By default, the ambitus of each part of `score' is used to automatically deduce the chromatic pitch domain for that part. When overwriting this default, the pitch domain of all parts must be given explicitly.
  - pitch-domains-extension: 
     If 0, the pitch domain for each part is the ambitus of pitches in the respective part of `score' (including all semitones within that ambitus). 
     If a positive integer, the pitch domain of each part is the respective ambitus extended by this number of semitones both up an down. For example, if `pitch-domains-extension' is 2, and the ambitus of some part in `score' ranges from C4 to C5, then the pitch domain for that part ranges chromatically from Bb3 (2 semitones down at the lower end) to D5 (2 semitones up at the upper end).
     A pitch domain extension can also be specified for each part separately in either of the following formats: (<part1-name-keyword> <extension> ...) or (<part1-name-keyword> (<lower-extension> <upper-extension>) ...). For example, to specify that the pitch domain of the 1st violin is extended by 0 semitones at the lower, but 7 semitones at the higher end you would write (:vln1 (0 7)), if you are using :vln1 as keyword in `score'. 
      When specifying a pitch domain extension for any part explicitly, then extensions for all parts must be given explicitly.
  - pitch-domain-limits: If set, it specifies absolute limits of the pitch domain for some or all parts, which restrict the domain otherwise specified with the pitches of `score' and  `pitch-domains-extension'. This is useful, e.g., to restrict the pitch of a part to the playable range of instruments. This argument in the following format: 
(<part1-name-keyword> (<lowest-pitch> <highest-pitch>) ...), where pitches are Opusmodus pitch symbols or Opusmodus pitch integers (but not MIDI integers!). 
    When specifying a pitch domain limit for any part, then a limit for all parts must be given.
  - rules (list of cluster-engine rule instances): further rules to apply, in addition to the automatically applied pitch/interval profile constraints. Note that the scales and chords of the underlying harmony are voice 0 and 1, and the actual voices are the sounding score parts. 
    If `rules' is :default, then some default rule set is used. 
  - additional-rules (list of cluster-engine rule instances): convenience argument to add rules without overwriting the default rule set by leaving the argument `rules' untouched.
  - split-score? (Boolean or more complex representation, see below): Feature that can speed up the search for longer scores. If true, the search is performed on score sections one by one. 
    If `split-score?' is  
    - :at-shared-rests or simply T, the score is split at shared rests (see function `split-score-at-shared-rests').
    - :at-bar-boundaries, the score is split after every bar.
    - A list '(:at-bar-boundaries <n>), the score is split after every `n' bars (`n' must be an integer).
    - A list '(:at-bar-boundaries <list of integers>), the score is split at the given zero-based bar numbers.
    NOTE: You cannot use index rules if `split-score?' is not NIL -- indices would not be correct! Also, constraints that would cross the boundary of a split point are ignored in this mode. E.g., a melodic constraint between the pitches of notes before and after the split point cannot be applied, as the score ends before/after the split point during the search process. 
    If you want to split your score at different positions (e.g., in order to ensure that melodic rules towards the first note of the next bar are actually followed), then simply rebar your score before calling `revise-score-harmonically' (e.g., by calling `omn-to-time-signature') and rebar the result back to the original time signature afterwards. 
  - length-adjust? (Boolean): If T, all parts of the output score are forced to be of the same length as the total length of the input score (otherwise the output score can be longer).
  - print-csp? (Boolean): For debugging the CSP: if true, print list of arguments to constraint solver cr:cluster-engine.
 - unprocessed-cluster-engine-result? (Boolean): For debugging the CSP: if true return the result of cluster engine directly, without translating it into an OMN score.

For better readability and exportability to notation software, the underlying harmony is output in the score as up to four instruments (if scales is not nil): :chord-treble, :chord-bass, :scales-treble, :scales-bass, i.e., both underlying chords and scales are notated across two staves. For better readability, scale notes are notated as grace notes, with the root (first pitch) as the normal note. (Non-root placeholder tones in the treble are seemingly necessary, but marked like a flagolett tone with an "o"). 
  
Examples:

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
  ;; TMP: unused arg doc
  ;;; TODO:  - pitch-domains (property list): specifying a pitch domain in the Cluster Engine format for every part in score, using the same instrument ID. If no domain is specified for a certain part then a chromatic domain of the ambitus of the input part is automatically generated.
  (let* (;; unify part durations: shorter parts looped until they have same length as longest part of score
         (unified-full-score-aux (multiple-value-list
				  (unify-part-durations
				   (append score
					   (list :harmonies harmonies)
					   (when scales
					     (list :scales scales))))))
	 (unified-full-score (first unified-full-score-aux))
	 (score-dur (second unified-full-score-aux))
	 (unified-score (remove-parts '(:harmonies :scales) unified-full-score))
	 (unified-harmonies (get-part-omn :harmonies unified-full-score))
	 (unified-scales (get-part-omn :scales unified-full-score)))
    (if split-score?
	;; if split-score? process score section wise 
	(let* ((split-scores-aux (multiple-value-list
				  (cond ;; split-score? is... 
				    ;; :at-shared-rests
				    ((or (equal split-score? :at-shared-rests)
					 (equal split-score? T))
				     (split-score-at-shared-rests unified-score))
				    ;; :at-bar-boundaries
				    ((equal split-score? :at-bar-boundaries)
				     (split-score-at-bar-boundaries unified-score))
				    ;; (:at-bar-boundaries <int>) or (:at-bar-boundaries <list of ints>)
				    ((and (listp split-score?)
					  (equal (first split-score?) :at-bar-boundaries))
				     (split-score-at-bar-boundaries unified-score (second split-score?))))))
	       (split-scores (first split-scores-aux))
	       (split-positions (second split-scores-aux))
	       ;; split harmonies at split-positions
	       (split-harmonies (tu:subseqs unified-harmonies split-positions))
	       (split-scales (if scales
				 (tu:subseqs unified-scales split-positions)
				 (om:gen-repeat (length split-positions) '(nil)))))
	  (apply #'append-scores
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
	(let* ((parts (get-parts-omn unified-score))
	       (pitch-domain-extension-specs (if (listp pitch-domains-extension)
						 pitch-domains-extension
						 (tu:map-plist-vals #'(lambda (_)
									(declare (ignore _))
									pitch-domains-extension)
								    unified-score)))
	       (first-part (first parts))
	       (time-sigs (PWGL-time-signatures 
			   (get-time-signature first-part)))
	       (csp (list
		     ;; min number of variables with much upper headroom (multiplied with 2), because search is stopped by stop rule
		     (* 2 (apply #'max (cons (length time-sigs) (mapcar #'count-notes parts))))
		     ;; rules
		     (let (;; position of all voices in score starting from 2 after scales and chords
			   (voice-ids (gen-integer 2 (+ (length (get-instruments unified-score)) 1))))
		       (ce::rules->cluster 
			(ce::rules->cluster 
			 (when constrain-pitch-profiles?
			   (cr:follow-profile-hr
			    parts :voices voice-ids :mode :pitch :constrain :profile :weight-offset (if (numberp constrain-pitch-profiles?)
													constrain-pitch-profiles?
													0)))
			 (when constrain-pitch-intervals?
			   (cr:follow-profile-hr 
			    parts :voices voice-ids :mode :pitch :constrain :intervals :weight-offset (if (numberp constrain-pitch-intervals?)
													  constrain-pitch-intervals?
													  0)))
			 (ce:r-predefine-meter time-sigs)
			 ;; stop search after all parts (including chords and scales) reach the duration of the input score
			 ;; !! NOTE that rule doc says "The stoptime has to be reached in all given voices. Note that the rule ignors the pitch information."
			 (ce:stop-rule-time (append '(0 1) voice-ids) score-dur :and))
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
		       ,(if scales
			    (list (flatten (omn :length unified-scales)))
			    (list (flatten (omn :length unified-harmonies))))
			;; scale pitches
			,(if scales
			     (list (pitch-to-midi (flatten (omn :pitch unified-scales))))
			     (list (pitch-to-midi (flatten (omn :pitch unified-harmonies)))))
			;; harmonic rhythm for chords 
			(,(flatten (omn :length unified-harmonies)))
			;; harmony: chords
			(,(pitch-to-midi (flatten (omn :pitch unified-harmonies))))
			;; parts
			,@(mappend #'(lambda (instr)
				       (let* ((part (getf unified-score instr))
					      (pitch-dom-extension-spec (getf pitch-domain-extension-specs instr))
					      (raw-ambitus (if pitch-domains ; inefficient -- checks pitch-ambitus for aevery instrument, but that should be fine here
							       (mapcar #'pitch-to-integer (getf pitch-domains instr))
							       (if
								(omn :pitch (flatten part))
								;; part contains pitches
								(find-ambitus part)
								;; otherwise return some default domain 
								'(0 1))))
					      (pitch-dom-ambitus (cond ((integerp pitch-dom-extension-spec)
									(list (- (first raw-ambitus) pitch-dom-extension-spec)
									      (+ (second raw-ambitus) pitch-dom-extension-spec)))
								       ((listp pitch-dom-extension-spec)
									(list (- (first raw-ambitus) (first pitch-dom-extension-spec))
									      (+ (second raw-ambitus) (second pitch-dom-extension-spec))))
								       ))
					      (pitch-dom-limit (getf pitch-domain-limits instr))					   
					      (limited-pitch-dom-ambitus (if (not pitch-dom-limit)
									     pitch-dom-ambitus
									     (let ((pitch-dom-limit-ints
										    (cond ((integerp (first pitch-dom-limit))
											   pitch-dom-limit)
											  ((pitchp (first pitch-dom-limit))
											   (pitch-to-integer pitch-dom-limit)))))
									       (list (max (first pitch-dom-limit-ints)
											  (first pitch-dom-ambitus))
										     (min (second pitch-dom-limit-ints)
											  (second pitch-dom-ambitus)))))))
					 `( 
					   ;; rhythm domain: predefined motif
					   (,(omn :length (omn-merge-ties (flatten part))))
					   ;; pitch domain
					   ,(mclist
					     (apply #'gen-integer 
						    (mapcar #'(lambda (p) (+ p 60)) 
							    limited-pitch-dom-ambitus)))
					   )))
				   (get-instruments unified-score))
			))))
	  (when print-csp?
	    (pprint csp))
	  (if unprocessed-cluster-engine-result?
	      (apply #'cr:cluster-engine csp)
	      (let ((result-score
		     (remove-parts '(:chords :scales)
				   ;; reduce total duration of each part in score segment to duration of input score
				   (map-parts-equally 
				    (copy-cluster-engine-pitches-to-score unified-score (apply #'cr:cluster-engine csp))
				    #'(lambda (dur part)
					;; (break)
					(if length-adjust?
					    ;; length-adjust does only work for flat 						;; (length-adjust dur part :flat T)
					    ;; TMP: 
					    (flattened-length-adjust dur part)
					    part))
				    `(,score-dur _)	       
				    ))))
		(mix-scores result-score
			    `(;; ambitus-filter used to split into treble and bass staffs of notation for better readability 
			      :chords-bass ,(ambitus-filter '(c-1 b3) harmonies)
			      :chords-treble ,(ambitus-filter '(c4 g9) harmonies)
			      ,@(when scales
				  (list :scales-bass (chords-to-gracenotes (ambitus-filter '(c-1 b3) scales))
					:scales-treble (chords-to-gracenotes (ambitus-filter '(c4 g9) scales) nil)))
			      ))
		))))))




