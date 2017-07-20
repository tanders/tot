;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interoperate with PWGL (e.g., the library Cluster Engine).
;;; Information between Opusmodus and PWGL is shared simply by textfiles, as PWGL cannot be "remote controlled".
;;; Note: Currently limited to monophoic music.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: revise: expect OMN pitches for harmonies and scales and do conversion within this function. 
(defun export-melody-to-PWGL 
       (omn-mel
        &key 
        ;; directory and file name of exported file
        (dir "/Users/torsten/Compositions/0-Flute-solo/z-Opusmodus-PWGL-communication/")
        (file "melody-raw1.lisp")
        ; default: C major triad
        (harmonies '((60 64 67)))      
        (harm-rhythm '(2))
        ; default: C major scale
        (scales '((60 62 64 65 67 69 71)))                                             
        (scale-rhythm '(2))
        (meter '((4 4)))
        (bar-range nil)
        )
  "Export a melody `omn-mel' alongside harmonic information for further processing by a PWGL patch.  
  For documentation of further arguments see PWGL patch FitInHarmony.

  Args:
  - bar-range (default nil): if non-nil, must be pair (<start-bar-index> <end-bar-index>), that specifies a range of bars from `omn-mel' to export. `omn-mel' must be nested in that case. Indices are 0-based, and start-bar-index/end-bar-index specify the index of the first/last bar to include."
  (print "export-melody-to-PWGL test 1")
  (let* ((omn-mel-range (if bar-range
                          (subseq omn-mel (first bar-range) (1+ (second bar-range)))
                          omn-mel))
         (omn-mel-dur (when bar-range
                        (total-duration omn-mel-range)))
         (harm-rhythm-range (if bar-range
                              (length-adjust omn-mel-dur harm-rhythm)
                              harm-rhythm))
         (scale-rhythm-range (if bar-range
                               (length-adjust omn-mel-dur scale-rhythm)
                               scale-rhythm)))
    (print "export-melody-to-PWGL test 2")
    (st:pprint-to-file (concatenate 'string dir file)
                       (list  
                        ;;; TODO: change length symbols etc into fractions
                        ;;; ? Necessary?
                        (flatten (omn :length omn-mel-range))
                        (pitch-to-midi (flatten (omn :pitch omn-mel-range)))                       
                        harm-rhythm-range (circle-repeat harmonies (length harm-rhythm-range)) ; (pitch-to-midi harmonies) 
                        scale-rhythm-range (circle-repeat scales (length scale-rhythm-range)) ; (pitch-to-midi scales) 
                        meter))))


;; Solved by keeping the orig rhythm for now
; - extract meter from `omn-mel'
; - limit length of result to length of omn-mel 
(defun import-transformed-melody-from-PWGL 
       (omn-mel
        &key 
        ;; directory and file name of exported file
        (dir "/Users/torsten/Compositions/0-Flute-solo/z-Opusmodus-PWGL-communication/")
        (file "melody-transformed1.lisp")
        (bar-range nil))
  "Complements export-melody-to-PWGL to import result from PWGL patch. `omn-mel' should be the same as given to export-melody-to-PWGL (its articulations and dynamics are merged into the result).

  Args:
  - bar-range: should be the same value as the `bar-range' of the corresponding call to `export-melody-to-PWGL'."
  (let* (; (note-no (length (single-events (flatten omn-mel))))
         (data (st:read-lisp-file (concatenate 'string dir file)))
         ;; first four element: sol-scale-durs sol-scale sol-harm-durs sol-harms
         ; (sol-durations (subseq (fifth data) 0 (1- note-no))) ; limit length of sol to length of input
         ; (sol-pitches  (subseq (sixth data) 0 (1- note-no)))
         ; (sol-durations (fifth data)) ; limit length of sol to length of input
         (sol-pitches  (sixth data))
         (sol-time-sigs (seventh data)))
    (format T "sol-pitches: ~A~%" (midi-to-pitch sol-pitches))
    (omn-to-time-signature ; split in bar sublists
     (omn-replace :pitch (midi-to-pitch sol-pitches) 
                  (flatten-omn (if bar-range
                                 (subseq omn-mel (first bar-range) (1+ (second bar-range)))
                                 omn-mel)))
     #|
     (make-omn
      :length (omn :length omn-mel)
      :pitch (midi-to-pitch sol-pitches)
      :velocity (omn :velocity omn-mel)
      :articulation (omn :articulation omn-mel)
      :leg (omn :leg omn-mel)
      )
     |#
     ;; add number of bars to each time signature
     (mapcar #'(lambda (time-sig) (append time-sig '(1)))
             sol-time-sigs))))


(defun import-underlying-harmony-from-PWGL 
       (&key 
        ;; directory and file name of exported file
        (velocity '(ppppp))
        (dir "/Users/torsten/Compositions/0-Flute-solo/z-Opusmodus-PWGL-communication/")
        (file "melody-transformed1.lisp"))
  "Complements import-transformed-melody-from-PWGL to import harmonic result from PWGL patch. `harm-durations' is the "
  (let* ((data (st:read-lisp-file (concatenate 'string dir file)))
         ;; first four element: sol-scale-durs sol-scale sol-harm-durs sol-harms
         (harm-durations (third data)) 
         (harm-pitches  (fourth data))
         (sol-time-sigs (seventh data)))
    (omn-to-time-signature ; split in bar sublists
     (make-omn
      :length harm-durations
      :pitch (mapcar #'chordize (midi-to-pitch harm-pitches))
      :velocity velocity)
     ;; add number of bars to each time signature
     (mapcar #'(lambda (time-sig) (append time-sig '(1)))
             sol-time-sigs))))


(defun PWGL-time-signatures (ts-forms)
  "Translates OMN format of time signatures into very similar but slighly simpler PWGL format.

  Example:
  ;;; (PWGL-time-signatures '((3 4 2) (2 4 1)))
  ;;; => ((3 4) (3 4) (2 4))"
  (loop for ts in (isolate-time-signatures ts-forms)
    collect (subseq ts 0 2)))


#|

(setf mel (make-omn 
           :length (gen-repeat 3 '((1/4 1/8 1/8 1/8 1/8)))
           :pitch (gen-repeat 3 '((c4 d4 f4 g4 a4)))
           :articulation '(stacc)))


(export-melody-to-PWGL mel
                       :meter '((3 4) (4 4) (3 4)))
      
(setf transformed-mel (import-transformed-melody-from-PWGL mel))

|#



