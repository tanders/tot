;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process articulations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-articulations (arts &key (empty-articulations '(default -)))
  "Merges list of OMN articulations to a combined attribute.

  Args:
  - arts: a list of OMN articulations
  - empty-attributes: articulations to ignore in a combination. 

  Examples:
  ;;; (merge-articulations '(ten ponte ubow))
  ;;; => ten+ponte+ubow

  ;;; (merge-articulations '(- stacc))
  ;;; => stacc

  ;;; (merge-articulations '(default default))
  ;;; => default
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

  Example:
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

  Example:
  ;;; (zip-articulations '(default leg leg default) '(breathy))
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

;;; BUG: I misunderstood function span -- may not work for OMN
(defun articulation-at-beginning (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' at the beginning and `default' for the rest.
  
  Example: 
  ;;; (articulation-at-beginning 'pizz '((q q e e) (h.)))
  ;;; => ((pizz default default default) (default))

  BUG: May not work for OMN sequence."
  (position-replace 0 articulation (span sequence (list default)) :section 0))

;;; BUG: I misunderstood function span -- may not work for OMN
(defun articulation-at-end (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' at the beginning and `default' for the rest.
  
  Example: 
  ;;; (articulation-at-end 'pizz '((q q e e) (h.)))
  ;;; => ((pizz default default default) (default))

  BUGS: 
  - Does not work if last element in sequence is a rest.
  - May not work for OMN sequence."
  (span sequence
        (reverse (cons articulation (gen-repeat (1- (length (flatten sequence))) (list default))))))

(defun articulation-up-to-end (articulation sequence &key (default 'default))
  "Returns a list of articulations spanning the given `sequence' (can be nested) with `articulation' from the beginning to the but-last tone, which carries the default. This is useful for legato slurs over a sublist.
  
  Example:
  ;;; (articulation-up-to-end 'leg '((q q e e) (q q)))
  ;;; => ((leg leg leg leg) (leg default))"
  (span sequence
        (append (gen-repeat (1- (length (flatten sequence))) (list articulation)) (list default))))



;; !? Better don't use text-attribute. Instead, declare piece-specific attributes in score. See also https://opusmodus.com/forums/topic/777-adding-free-text-to-the-score-output/#comment-2278
(defun text-attribute (&rest args)
  "Convenient addition of arbitrary text directly the the score. Function transforms an arbitrary symbol, number or string or a list of  symbols, numbers and strings into a an articulation symbol that is implicitly registered and can directly be added to an OMN note.

  Definition currently only works with multiple integers or floats.
  
  Examples:
  ;;; (text-attribute -7 2 13)
  ;;; => -7+2+13

  ;;; (text-attribute 3.14 42)
  ;;; => 3.14+42

  BUGS: 
  All these examples are not working.
  (text-attribute \"this is a test\")
  (text-attribute 42)
  (text-attribute 'parameters -7 2 13)
  (text-attribute -7 3.14 16 1/3)
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


