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


