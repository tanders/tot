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

* Examples:
 
  ;;; (mapcar-nested #'(lambda (x) (* x 2)) '((1/4 1/8 1/8) (1/2)))
  ;;; => ((1/2 1/4 1/4) (1))

  Using the built-in Opusmodus function `span' to ensure an equal nesting of value lists
  ;;; (let* ((ls '((1/4 1/8 1/8) (1/2)))
  ;;;        (factors (span ls '(2 3))))
  ;;;   (mapcar-nested #'* ls factors))
  ;;; => ((1/2 3/8 1/4) (3/2))
  "
  (apply #'mapcar #'(lambda (&rest lists) (apply #'mapcar fn lists))
	 nested-lists))

(defun matrix-transpose* (&rest lists)
  "Variant of matrix-transpose that is more allowing in terms of its input. The function performs a matrix transformation, but input lists can be of different length (shorter lists are then circled through) or even mere elements (internally turned into a list by repeating the element).

* Arguments:
  - lists: individual elements or lists

* Examples:

  (matrix-transpose* '(0 1 2 3) 5)
  => ((0 5) (1 5) (2 5) (3 5))


  (matrix-transpose* '(0 1 2 3) '(a b) '_)
  => ((0 a _) (1 b _) (2 a _) (3 b _))

"
  (let* ((full-lists (mapcar #'tu:ensure-list lists))
	 (l (apply #'max (mapcar #'length full-lists))))
    (matrix-transpose (mapcar #'(lambda (list)
				  (circle-repeat list l))
			      full-lists))))


(defun circle-repeat (pattern n)
  "Circle through elements in pattern (a list) until n elements are collected.

  NOTE: only supports flat list so far.

* Arguments:
  - pattern: a list or single value treated as a one-value list
  - n: an integer

* Examples:

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



(defun insert-at-position (position item list &key seed)
  "Insert item(s) at given position into list.

* Arguments:
  - position: either symbol 's (start), 'e (end) or '? (random position), or integer specifying position.
  - item: value or list of values to be inserted.
  - list: flat list of values.

* Examples:
  ;;; (insert-at-position 'e 'x '(a a a a))
  ;;; (insert-at-position 's 'x '(a a a a))
  ;;; (insert-at-position '? 'x '(a a a a))
  ;;; (insert-at-position 'e '(x y) '(a a a a))
  ;;; (insert-at-position '0 '(x y) '(a a a a))
"
  (rnd-seed seed)
  (let* ((pos1 (case position
		 (s 0)
		 (e (length list))
		 (? (round (rnd1 :low 0 :high (length list) :seed (seed))))
		 (otherwise (if (numberp position)
				position
				(error "~A is not a valid position" position)))))
	 (pos (if (listp item)
		  (gen-integer pos1 (+ pos1 (1- (length item))))
		  pos1)))
    (position-insert pos item list)))


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


