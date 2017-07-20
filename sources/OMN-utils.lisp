;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OMN Utilities 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-time-signature (music-with-time-signature music-to-rebar)
  "Rebars `music-to-rebar' so that it fits the meter of `music-with-time-signature'."
  (omn-to-time-signature music-to-rebar
                         (get-time-signature music-with-time-signature)))



(defun edit-omn (type notation fun &key (flat T))
  "Use function `fun', defined for transforming individual OMN parameters of `type' (e.g., :length, or :velocity) to transform omn expression `notation'. This function is intended as a convenient way to generalise functions your functions to support omn notation as input.

  Args:
  - type: a keyword like :length, :pitch, :velocity, :duration, or :articulation (any keyword supported by function omn or make-omn).
  - fun: a function expecting a parameter sequence of given type. It is sufficient to support only a flat input list, support for nested lists is added implicitly.
  - notation: a omn sequence or a plain parameter list (can be nested).
  - flat (default T): whether or not `fun' expects a flat input list.

  Example: roll your own transposition supporting omn input
  first aux def supporting only pitches
  ;;; (defun my-transposition-aux (interval pitches)
  ;;;   (midi-to-pitch (loop for p in (pitch-to-midi pitches)
  ;;;                        collect (+ p interval))))
  
  test
  ;;; (my-transposition-aux 7 '(c4 e4 g4)) 
  ;;;  => (g4 b4 d5)

  variant supporting also omn expressions
  ;;; (defun my-transposition (interval omn)
  ;;;   (edit-omn :pitch omn
  ;;;             #'(lambda (ps) (my-transposition-aux interval ps))))

  test with nested OMN including a rest
  ;;; (my-transposition 7 '((q c4 mp -q q e4 q f4) (h g4 tr2)))
  ;;;  => ((q g4 mp - b4 c5) (h d5 mp tr2))

  More information at {https://opusmodus.com/forums/topic/799-user-functions-supporting-arbitrary-omn-input-–-defining-them-more-easily/}."
  (if (omn-formp notation)
    (let ((params (omn nil notation)))
      (apply #'make-omn 
             (append  
              (list type 
                    (span notation
                          (funcall fun (if flat
                                         (flatten (getf params type))
                                         (getf params type)))))
              (tu:remove-property type params))))
    ;; notation is plain parameter list
    (span notation 
          (funcall fun (if flat
                         (flatten notation)
                         notation)))))
  
;;; TODO: define add-omn when I have variant of disassemble-omn that works with incomplete CMN forms
#|
(defun add-omn (type parameter notation)

  )


(add-omn :pitch 
         '(g4 a4 b4)
         '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))

(omn-formp '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))

(disassemble-omn '((q f jet-up+fermata+comma) (h 0<mp>0 wind-flz+fermata+comma) (q sfffp wind-i+tie q p<ff)))
|#

;;; TODO: keep as global fun
;;; TODO: see count-notes -- do I need both?
(defun note-no (music)
  "Returns the number of notes in music, which is a list of lengths values of OMN expressions."
  (count-if #'length-notep (flatten (omn :length music))))


;;; TODO: see note-no -- do I need both?
(defun count-notes (notes)
  "Returns number of notes (ignoring rests) in length list or other OMN expression."
  (get-count notes :length :note :sum T))


(defun phrase-lengths (lengths)
  "Returns the number of notes between rests in the given lengths.
  
  Args:
  - length: lengths or OMN (list or list of list)."
  (let ((flat-lengths (length-rest-merge (flatten (omn :length lengths)))))
    (mapcar #'1-
            (tu:x->dx (length-rest-position
                       (append (unless (length-restp (first flat-lengths))
                                 '(-1)) 
                               flat-lengths
                               (unless (length-restp (first (last flat-lengths)))
                                 '(-1))))))))

; (phrase-lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

#|
;;; Unfinished -- rests are currently left out alltogether 
(defun split-into-phrases (lengths)
  "Splits lengths into its phrases (notes between rests). Returns a list of lists, where each sublist is a phrase. Any rest between phrases is retained at the beginning of a phrase.

  length: lengths or OMN (list or list of list)."
  (let* ((events (if (omn-formp lengths)
                   (single-events (length-rest-remove (flatten lengths)))
                   (length-rest-remove (flatten lengths))))
         ;; todo: add these rests one by one at the top of the resulting phrases 
         (rests (if (omn-formp lengths)
                  (single-events (length-note-remove (length-rest-merge (flatten lengths))))
                  (length-note-remove (length-rest-remove (flatten lengths)))))
         ;; test whether first phrase starts with a rest
;         (
;          (if (length-restp (first (flatten lengths)))))          
         (lengths (phrase-lengths lengths)))
    (loop for (start end) on (tu:dx->x lengths 0)
      when end
      ; do (print (list events start end))
      collect (subseq events start end)
      )))
|#

#| ; tests for split-into-phrases
(split-into-phrases '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

(phrase-lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))

(length (length-rest-merge (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))))


(setf lengths '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))
(setf phrase-lengths (phrase-lengths lengths))

(single-events (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4))))


(omn-formp '((-3h) (3h c4 mf) (3h c4 mf) (q c4 mf) (q c4 mf) (3h c4 mf) (3h c4 mf) (3h c4 mf) (q c4 mf) (-q) (-5h) (-5h) (5h c4 mf) (5h c4 mf) (5h c4 mf) (q c4 mf) (q c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (5h c4 mf) (q c4 mf) (-q)))

|#


;;; not needed anymore, as CTR-1 does conveniently plot lists/vectors of numbers now
(defun plotter (data &optional (number 100))
  "Aux function for plotting fenvs"
  (list-plot (cond ((fe:fenv? data) (fe:fenv->vector data number))
                   ((and (listp data) (every #'fe:fenv? data))
                    (mapcar #'(lambda (xs) (fe:fenv->vector xs number)) data))
                   (T data))
	     :join-points T :point-radius 2))


(setf *print-pretty* t
      *print-miser-width* 0
      *print-right-margin* 80)
;; based on https://groups.google.com/forum/#!topic/comp.lang.lisp/_NP7Ub6hLsE
(defun pprint-part (part &optional (stream *standard-output*))
  "Pretty prints a part one bar a time, adding a bar line comment before each bar.

  Args: 
  - part: nested OMN list.

  Example:
  ;;; (pprint-part '((q c4 d4 e4) (h f4 q e4) (h. d2)))"
  (pprint-logical-block 
      (stream nil :prefix "(" :suffix ")") 
    (pprint-logical-block (stream part) 
      (loop 
        for bar-no from 1 to (length part)
        for bar in part
        do (progn 
             (pprint-indent :block 1 stream)
             (pprint-newline :mandatory stream)
             (format stream ";; Bar ~A" bar-no)
             (pprint-newline :mandatory stream)
             (prin1 bar stream))))
    (pprint-indent :block -1 stream) 
    (pprint-newline :mandatory stream)))

#|
(pprint-part 
 '((s g1 ff gs1 p a1 ff b1) (s c2 mf cs2 ff d2 e2 mf) (e f2 p s fs2 ff g2 p a2)
   (s b2 mf e c3 s cs3 ff eb3) (s e3 mf f3 ff fs3 p a3 mf) (s bb3 ff b3 e c4 p s e4)
   (e e4 ff s f4 e s b4 p) (e b4 s s mf e fs5 ff) (s fs5 mf f5 p mf e cs6)
   (s cs6 c6 p e b5 mf s g6 ff) (s g6 p ff e f6 g6 mf) (s g6 g6 p ff e fs6) (s g6 mf p e s fs6)
   (s fs6 fs6 ff mf ff) (e fs6 p s ff fs6 fs6 mf) (s fs6 fs6 ff p f6) (s fs6 ff mf e e f6)
   (s f6 e fs6 fs6 p s f6) (s f6 mf fs6 fs6 p f6) (e f6 f6 ff s fs6 p f6 ff) (s f6 p e s s)
   (s f6 e mf p s ff) (s f6 mf p mf e ff) (s f6 p e s mf p) (s f6 ff e mf s e p)
   (e f6 s e ff mf) (s f6 p mf e6 ff f6 p) (s f6 ff e6 mf e eb6 p f6 mf) (s f6 e6 d6 f6)
   (s f6 e6 ff d6 e f6 p) (s f6 mf e6 ff cs6 f6 mf) (s f6 ff f6 cs6 mf f6 ff)
   (e f6 mf s s cs6 p fs6 mf) (s f6 ff e e6 p s bb5 mf f6) (s e6 p b5 f5 eb6)
   (s bb5 fs5 ff e cs5 p s a5 mf) (e e5 ff s c5 e gs4 mf s d5) (e bb4 s fs4 d4 e g4)
   (s e4 ff d4 b3 mf e eb4) (s cs4 p bb3 mf e gs3 p s c4 ff) (s a3 a3 mf e b3 ff gs3)
   (s bb3 c4 d4 e b3 mf) (s cs4 p eb4 mf e f4 p s eb4 ff) (s e4 fs4 gs4 p fs4 ff)
   (e g4 p s a4 mf bb4 a4) (s bb4 c5 cs5 p c5 mf) (s cs5 eb5 ff e e5 mf eb5 ff)
   (s e5 mf e fs5 g5 p s fs5 ff) (s g5 mf gs5 a5 p a5) (e bb5 ff b5 mf s c6 ff c6)))

|#



