;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionality depending on the libraries Cluster Engine and Cluster Rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO:
;;; - Cut superfluous notes/bars at end (and stop search in time) -- not (always) necessary?
;;; OK - insert all velo and articulations in score into result
;;; OK - Move this function into a new file constraints.lisp into my library tot
;; ... solution can be repeating input score
(defun revise-score-harmonically (score harmonies scales &key 
                                        (constrain-pitch-profiles? T)
                                        (constrain-pitch-intervals? T)
                                        ;;; TODO: consider allowing to overwrite pitch domain
                                        ; pitch-domains
                                        (rules nil))
  ;; TMP: unused arg doc
  ; TODO:  - pitch-domains (property list): specifying a pitch domain in the Cluster Engine format for every part in score, using the same instrument ID. If no domain is specified for a certain part then a chromatic domain of the ambitus of the input part is automatically generated.
  "CSP transforming the input `score' such that it follows the underlying harmony specified. 
  The rhythm of the input score is left unchanged. The pitches follow the melodic and intervallic profile of the input voices/parts, and various additional constraints are applied.
  
  Args:
  - score (headerless score): See {defun preview-score} for its format. NOTE: the total number of parts is limited to 8 (?) by internal Cluster Engine limitations.
  - harmonies (OMN expression): OMN chords expressing the harmonic rhythm and chord changes of the underlying harmony 
  - scales (OMN expression): OMN chords expressing the rhythm of scales and scale changes of the underlying harmony
  - constrain-pitch-profiles? (Boolean): Whether to constrain the pitch profile
  - constrain-pitch-intervals? (Boolean): Whether to constrain the pitch intervals
  - rules (list of cluster-engine rule instances): further rules to apply, in addition to the automatically applied pitch/interval profile constraints. Note that the scales and chords of the underlying harmony are voice 0 and 1, and the actual voices are the sounding score parts. 
    If `rules' is nil, then some default rule set is used. 

  Example:

Input score defined by turning a melody into some polyphonic texture. 

;;; (setf galliard-mel 
;;;       '(:|1| 
;;;          ((q e5 f slap+stacc -h q eb5 slap+stacc -h) 
;;;           (q cs5 f slap+stacc eb5 mp ord f5) (q. g5 e gs5 q c6) (h bb5 q a5) (q fs5 - e d5 cs5) (h a5 q b5) (q. cs5 e bb5 q d6) (q cs6 d6 g5) (h e5 q cs6) (q g5 f slap+stacc a5 mp ord bb5) (q. c6 e cs6 q c6) (e b5 a5 q f5 e5) (h b5 q c6) (q eb5 f slap+stacc -e b5 mp ord d6 cs6) (q. eb6 e b5 q e5) (q gs5 d6 b5) (h gs5 q fs5))))

;;; (setf polyphonic-score
;;;       `(:v1 ,(second galliard-mel)
;;;         :v2 ,(append '((-h.)) (butlast (second galliard-mel) 3))
;;;         :v3 ,(append '((-h.) (-w.)) (butlast (second galliard-mel) 4))))

The underlying harmony -- both chords and scales -- are also predefined headerless scores.

;;; (setf galliard-harmonies
;;;       '((h. c4eb4g4a4)
;;;         (w. c4eb4g4a4)        
;;;         (w. f4g4a4c4) (w. cs4f4g4b4) (w. b4eb4g4a4) (w. f4a4c4eb4) (w. eb4g4b4cs4) (w. a4cs4eb4g4) (w. cs4f4g4c4) (h. g4b4cs4f4)))

;;; (setf galliard-scales
;;;       (append (length-merge (flatten (omn :length galliard-harmonies))) 
;;;               (chordize '(c4 cs4 ds4 f4 g4 a4 b4))))

Some playback customisation of preview-score silencing the first two parts.

;;; (setf *default-preview-score-instruments*
;;;       '(;; silent harmony -- :volume 0
;;;         :scales (:program 'violin :sound 'gm :channel 16 :volume 0)
;;;         :chords (:program 'violin :sound 'gm :channel 16 :volume 0)
;;;         :v1 (:program 'violin :sound 'gm :channel 1)
;;;         :v2 (:program 'violin :sound 'gm :channel 1)
;;;         :v3 (:program 'violin :sound 'gm :channel 1))
;;;       )

The actual calls to `revise-score-harmonically', first a monophonic and then a polyphonic example. In the second example, the score is also saved to a variable for later inspection, postprocessing etc. 

;;; (preview-score 
;;;  (revise-score-harmonically galliard-mel galliard-harmonies galliard-scales))
;;;
;;; (progn 
;;;   (setf revised-polyphonic-score
;;;         (revise-score-harmonically polyphonic-score galliard-harmonies galliard-scales))
;;;   (preview-score revised-polyphonic-score))
  "
   (let* ((parts (get-parts-omn score))
          (first-part (first parts))
          (time-sigs (PWGL-time-signatures 
                      (get-time-signature first-part))))
     (copy-cluster-engine-pitches-to-score ; cluster-engine-score 
      score
      (cr:cluster-engine 
       (apply #'max (mapcar #'count-notes parts))
       ;; rules
       (let (;; position of all voices in score starting from 2 after scales and chords
             (voice-ids (gen-integer 2 (+ (length (get-instruments score)) 1))))
         (ce::rules->cluster 
          (append 
           (when constrain-pitch-profiles?
             (cr:follow-profile-hr 
              parts :voices voice-ids :mode :pitch :constrain :profile))
           (when constrain-pitch-intervals?
             (cr:follow-profile-hr 
              parts :voices voice-ids :mode :pitch :constrain :intervals))
           (list (ce:r-predefine-meter time-sigs)))
          (apply
           #'ce::rules->cluster 
           (if rules 
             rules 
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
              )))))
       ;; meter domain
       (remove-duplicates time-sigs :test #'equal) 
       ;; voice domains
       `(
         ; scale rhythm
         (,(flatten (omn :length scales)))
         ; scale pitches
         (,(pitch-to-midi (flatten (omn :pitch scales))))
         ; harmonic rhythm for chords 
         (,(flatten (omn :length harmonies)))
         ; harmony: chords
         (,(pitch-to-midi (flatten (omn :pitch harmonies))))
         ,@(mappend #'(lambda (part)
                        `( 
                          ;; rhythm domain: predefined motif
                          (,(flatten (omn :length part)))
                          ;; pitch domain
                          ,(mclist
                            (apply #'gen-integer 
                                   (mapcar #'(lambda (p) (+ p 60)) 
                                           (find-ambitus part))))
                          ))
                    parts)
         ))
      )))



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
       collect (list instrument
                     (omn-to-time-signature 
                      (make-omn  
                       ;; 0 length are acciaccaturas 
                       :length (mapcar #'(lambda (l) (if (= l 0) 1/8 l))
                                       lengths)
                       :articulation (mapcar #'(lambda (l) (if (= l 0) 'acc '-))
                                             lengths)
                       :pitch (tu:mappend #'(lambda (p) 
                                              (if (listp p)
                                                (chordize (midi-to-pitch p))
                                                (list (midi-to-pitch p))))
                                          ;; Hack: replace all nil (pitches of rests, but also at end of score) 
                                          (substitute 60 nil pitches))
                       :swallow T)
                      time-sigs))))))

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
                     (omn-replace :pitch  (flatten 
                                           (tu:mappend #'(lambda (p) 
                                                           (if (listp p)
                                                             (chordize (midi-to-pitch p))
                                                             (list (midi-to-pitch p))))
                                                       (remove nil pitches)))
                                  (flatten part-omn))))))))


(defun preview-cluster-engine-score (score)
  "Just shorthand for (preview-score (cluster-engine-score score))"
  (preview-score (cluster-engine-score score)))

