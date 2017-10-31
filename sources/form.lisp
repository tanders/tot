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



(defun condense (sequence test &key (section nil))
  "Beethoven-like condensation of music. All notes for which a given test function returns true are removed, and their preceeding note is extended by their duration.

  Args:
  - test: Boolean function expecting individual parameters of each note in `OMN'
  - sequence: An OMN sequence

  Example:

;;; (setf my-motif '((q. c4 e d4 q. e4 e f4) (h g4 -h)))

;;; (condense my-motif
;;;           #'(lambda (dur pitch &rest other-args)  
;;;               (> (omn-encode dur) 1/4)))
"
  (length-legato
   (filter-notes-if test sequence :section section)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-omn (n sequence &key (parameter :pitch) (flat T) (section nil))
  "Rotate the OMN `sequence' by `n' positions.

  Args:
  - n: an integer (rotation number, positive or negative). The keywords :left and :right are equivalents of the integer values -1 and 1.
  - sequence: OMN expression.
  - parameter (keyword): which parameter to rotate (e.g., :length, :pitch...). If nil, everything is rotated. 
  - flat (Boolean): whether to rotate the full flattened sequence (T) or subsequences. 
  - section (list of ints): positions of sublists to process. This argument is ignored if flat is T.  

  Examples:

;;; (rotate-omn :right '((-h q c4) (q. f4 e g4 q a4) (h. g4)))
;;; 
;;; (rotate-omn :left '((-h q c4) (q. f4 e g4 q a4) (h. g4)) :parameter :length)
;;; 
;;; (rotate-omn 2 '((-h q c4) (q. f4 e g4 q a4) (h. g4)) :section '(1) :flat nil)
"  
  (edit-omn parameter sequence 
            #'(lambda (ls) (gen-rotate n ls))
            ; :swallow swallow
	    :section section
	    :flat flat))
