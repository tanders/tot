;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Utilities
;;;
;;; TODO: Consider moving this partly into my general TU utils package instead.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO: consider adding arguments :section and :exclude -- after nested-lists
(defun mapcar-nested (fn &rest nested-lists)
  "A simplified equivalent of mapcar, but expects nested lists (as common for OMN parameter lists). All lists in nested-lists must be nested equally.

  Example: 
  (mapcar-nested #'(lambda (x) (* x 2)) '((1/4 1/8 1/8) (1/2)))
  => ((1/2 1/4 1/4) (1))

  Using the built-in Opusmodus function `span' to ensure an equal nesting of value lists
  (let* ((ls '((1/4 1/8 1/8) (1/2)))
         (factors (span ls '(2 3))))
    (mapcar-nested #'* ls factors))
  => ((1/2 3/8 1/4) (3/2))
  "
  (apply #'mapcar #'(lambda (&rest lists) (apply #'mapcar fn lists))
	 nested-lists))


(defun circle-repeat (pattern n)
  "Circle through elements in pattern (a list) until n elements are collected.

  NOTE: only supports flat list so far.

  Args:
  - pattern: a list or single value treated as a one-value list
  - n: an integer

  Example:

;;; (circle-repeat '(bb4 g4) 10)
;;; => (bb4 g4 bb4 g4 bb4 g4 bb4 g4 bb4 g4)

The function span can do something very similar. 

;;; (span (gen-repeat 10 'x) '(bb4 g4))


See also Opusmodus buildin gen-trim, which does the same, but is overall more flexible.
"  
  ;; (assert pattern
  ;;         (pattern) "circle-repeat: must be of at least length 1: ~A" pattern)
  (let* ((pattern-l (if pattern
			(tu:ensure-list pattern)
			;; if pattern is nil then repeat nil
			(list nil)))
	 (l (length pattern-l)))
    (loop 
      for i from 0 to (- n 1)
      collect (nth (mod i l) 
                   pattern-l))))

; (circle-repeat '(bb4 g4 f4) 20)
; (circle-repeat nil 3)



(defun keyword-to-om-symbol (key)
  "Translates the keyword `key' into a symbol of the Opusmodus package"
  (intern (symbol-name key) :om))



#|
;; function not necessary -- use undocumented rnd-pick arg :encode nil
(defun rnd-pick2 (selections &key (prob 0.5) seed)
  "Randomly selects a value from `selections' (without transforming that value).
  Very similar to rnd-pick, but leaves selected value unchanged (e.g., unflattened).

  Args
  prob: a floating-point number. Probability value. The default is 0.5.

  Example
  (rnd-pick2 '(((h q) (h))
               ((h. q) (h))
               ((h. h) (h))))
  => ((h. q) (h))  
  "
  (rnd-seed seed)
  (nth (rnd1 :low 0 :high (1- (length selections)) :prob prob :seed (seed)) 
       selections))

(rnd-pick '(((h q) (h))
            ((h. q) (h))
            ((h. h) (h))) :encode nil)
|#


