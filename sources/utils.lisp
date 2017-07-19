;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Utilities
;;;
;;; TODO: Consider moving this into my general TU utils package instead.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: turn into project independent function with doc etc. It is already general enough.
;; Based on https://groups.google.com/forum/#!topic/comp.lang.lisp/xDaUVFDnp5w
(defun map-neighbours (func list &optional n)
  "Applying `func' to consecutive sublists of `list'. The number of arguments expected by func implicitly specifies the number of consecutive elements to which the function is applied. This can be overwritten by the optional `n'."
  (let ((n2 (if n n (length (ccl:arglist func)))))
    (loop for l on list 
      when (>= (length l) n2)
      collect (apply func (subseq l 0 n2)))))

#|
(map-neighbours #'(lambda (x y) (list x y)) 
                '(1 2 3 4 5 6))
=> ((1 2) (2 3) (3 4) (4 5) (5 6))

(map-neighbours #'+ '(1 2 3 4 5 6)
                3)
=> (6 9 12 15)

Note: doing the same with loop

(loop
  for (key value) on '(1 2 3 4 5 6) by #'cddr
  when value
  collect (list key value))
=> ((1 2) (3 4) (5 6))

|#


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


(defun apropos-function-documentation (my-string &optional (package *package*))
  "Lists all functions that contain `my-string' alongside their documentation in a list of pairs
  (<function-symbol> <doc-string>)

  Examples:
  (apropos-function-documentation \"omn\")"
  (mapcar #'(lambda (x) (list x (documentation x 'function)))
          (remove-if-not #'fboundp (apropos-list my-string package))))

; (apropos-function-documentation "omn")
; (apropos-function-documentation "velocity")
; (apropos-function-documentation "chord")
; (apropos-function-documentation "-ties")

