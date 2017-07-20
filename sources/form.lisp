;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Musical form
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: try to (slightly) generalise further, so that it would be useful for various projects
(defun developing-substitute (map sequence otherwise)
  "Returns a transformation of `sequence', where elements that match entries given in `map' are replaced with given new items, and -- if not contained in map --  with an `otherwise' element. However, this function allows for replacements that develop over time and where the sequence of the developments could be generated algorithmically by other functions: When multiple new and otherwise replacements are given, these are used by and by in their given order (circulating, if the end of the given sequence is reached). 
  
  Replacements in the mapping can also be specified by functions returning the necessary replacement.  

  Note that sequence elements to replace can be arbitrary values, including subsequences.
  

  Args:
  - map:        old-new pairs (list of two-element lists)
  - sequence:   sequence (list) to transform
  - otherwise:  flat list of alternative replacements. Per sublist, only a single replacement value is chosen.

  Examples:
;;; (developing-substitute '((1 (a b c))
;;;   			     (2 (h i j)))
;;;                        '(1 1 2 1 3 2 1)
;;;                        '(x y z))
;;; => (a b h c x i a)"
  (let ((n (length sequence))     ; max number of replacement elements needed
        (mappings (make-hash-table :test #'equal)))
    ;; fill hash table
    (loop for old-new in map
      do (let ((old (first old-new))
               (new (second old-new)))
           (if (consp old)
             (let ((l (length old)))
               (setf (gethash old mappings) 
                     (gen-divide l (circle-repeat new (* n l)))))
             (setf (gethash old mappings) 
                   (circle-repeat new n)))))
    (setf (gethash :otherwise mappings) (circle-repeat otherwise n))
    ;; process sequence
    (loop 
      for x in sequence
      collect (let ((replacement (pop (gethash x mappings))))
                (if replacement 
                  replacement
                  (if (consp x)
                    (circle-repeat (list (pop (gethash :otherwise mappings))) (length x))
                    (pop (gethash :otherwise mappings))))))))


#| ; tests

(developing-substitute '((1 (a b c))
                         (2 (h i j)))
                        '(1 1 2 1 3 2 1)
                        '(x y z))

(setf sequence '((1/8) (1/16 1/16) (1/24 1/24 1/24) (1/8) (3/16) (1/8) (1/16 1/16) (1/24 1/24 1/24) (1/8)))
(make-omn
 :length sequence
 :pitch (developing-substitute 
         ; map
         `(((1/8) (c4 g4)) 
           ((1/24 1/24 1/24)  ,(pitch-figurate 3 '(c6 e6 g6 a6))))
         sequence
         ; otherwise 
         '(fs4 gs4)))

|#



