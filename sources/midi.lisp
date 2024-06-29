;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
(in-package :om)

(defun write-midi-from-omn (omn-sequence filename)
  "Writes the given OMN sequence to a MIDI file with the specified filename."
  (compile-score
   (def-score test-score
       (:title "test score"
        :key-signature 'chromatic
        :time-signature '(4 4) ; TODO: from given OMN
        :tempo 72)
     
     (instrument
      :omn omn-sequence
      :channel 1
      :sound 'gm
      :program 'acoustic-grand-piano))
   :output :midi :file filename))

#|
(setf my-omn '((s a4 leg d5 leg fs4 leg d5 leg g4 leg d5)))
(setf path "/Users/torsten/Desktop/test1.midi")

(write-midi-from-omn my-omn path)
|#


(defun write-midi-from-score (score filename)
  "Writes score to a MIDI file with the specified filename."
  (compile-score score :output :midi :file filename))

#|
(def-score test-score
    (:title "test score"
     :key-signature 'chromatic
     :time-signature '(4 4)
     :tempo 72)
  
  (instrument
   :omn my-omn
   :channel 1
   :sound 'gm
   :program 'acoustic-grand-piano))

(setf path "/Users/torsten/Desktop/test2.midi")

(write-midi-from-score 'test-score path)
|#



