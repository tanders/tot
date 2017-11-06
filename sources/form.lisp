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




(defun alternate-omns (ids seqs-of-seqs &key (append? nil))
  "This function alternates between sublists of multiple omn sequences. It can be useful, e.g., to switch between different musical characteristics, but have some kind of development within each characteristic. This is a powerful function for organising musical form on a higher level.

  Args:
  - ids (list of 0-based integers): indicating the position of OMN expressions in seqs-of-seqs.
  - seqs-of-seqs (list of lists of OMN sequences or plain OMN parameters): specifies material between which to switch. The top-level list positions indicate the different materials pointed to by `ids'. The next level specifies the order in which material will be included when `ids' contains repetitions of an id. This order is circling. The lowest level is the actual material either as a flat list (each a single bar), or as a nested list (multiple bars). In case of a nested list at the lowest level you likely want to set `append' to T.
  - append? (Boolean): whether or not nested OMN sequences should be appended instead of been combined in a list. 


Examples:

The following example demonstrates the use of the arguments `ids' and `seqs-of-seqs' with a flat sequence of note lengths. Note the nesting of `seqs-of-seqs', as explained by the comments. 
 
;;; (alternate-omns
;;;  ;; sequence of IDs
;;;  '(0 0 1 1 0 0)
;;;  '(;; ID 0 material/gestures 
;;;    (;; for first occurance of ID 0 (and again, when circling)
;;;     (h q q)
;;;     ;; second occurance of ID 0 etc.
;;;     (h. e e))
;;;    ;; ID 1 material//gestures (only one segment, repeated always)
;;;    ((e e e e e e e e))
;;;    ))
;;; ; => ((h q q) (h. e e) (e e e e e e e e) (e e e e e e e e) (h q q) (h. e e))


Now, the power of algorithmic composition lies in the fact that each of these arguments can be algorithmically generated. The next example demontrates that. It also demonstrates how the materials/gestures can be OMN expressions with multiple parameters. Still, each of these gestures consist in a single bar (gestures are not nested, though they are nested within two list levels).

;;; (alternate-omns 
;;;  ;; random sequence of IDs
;;;  (gen-eval 10 '(rnd-pick '(0 1)))
;;;  (list 
;;;   ;; sequence of first kind of musical gestures
;;;   (make-omn
;;;    :length (length-rest-series
;;; 	    (rnd-sample 7 '(7 8 9))
;;; 	    (length-divide 3 2
;;; 			   (rnd-sample 7 '((q q q) (h e e) (h.) (h q)))))
;;;    :pitch '(d4 e4 f4 g4)
;;;    :velocity '(pp))
;;;   ;; sequence of second kind of musical gestures
;;;   (make-omn
;;;    :length '(s s s s)
;;;    :pitch (gen-rotate :right '(c5 d5 f5 a5 g5 e5) :type :seq)
;;;    :velocity '(ff)
;;;    :span :pitch)))

Remember that resulting OMN expressions can be 're-barred'. This is useful, e.g., for gestures that exceed a single bar but are still stored in a flat list (again, nested within two list levels).

;;; (omn-to-time-signature 
;;;  (alternate-omns
;;;   '(0 0 1 0 1 0 1 1 0 0 1 1 1)
;;;   (list
;;;    (make-omn 
;;;     :length (gen-rotate :left '(-1/20 1/20 1/20 1/20 1/20) :type :seq)
;;;     :pitch '(d4 e4 f4 g4)
;;;     :velocity '(ff))
;;;    (make-omn
;;;     :length '(q e e)
;;;     :pitch (gen-rotate :left '(c5 e5 f5) :type :seq)
;;;     :velocity '(pp pp)
;;;     :attribute '(ten stacc stacc)
;;;     :span :pitch)))
;;;  '(4 4))

Alternatively, it is possible to use gestures that consists of nested lists for representing bars (still nested within two higher list levels). In that case, you typically want to set `append?' to T so that the result is a standard OMN sequence with only a single level of nesting, as shown below. 

;;; (alternate-omns '(0 0 1 1 0 0) 
;;;                 '((;; one nested gestures, circled in case of repeated IDs
;;; 		       ((-h q c4 p leg) (q. f4 leg e g4 leg q a4 leg) (h. g4)))
;;;                   (;; one nested gestures
;;; 		       ((q g4 f leg c5 leg e c4 stacc d4 stacc e4 f4 stacc) (q g4 stacc -h.))))
;;;                 :append? T)
"
  (let ((omn-no (length seqs-of-seqs)))
    (assert (every #'(lambda (x) (and (integerp x) (< x omn-no))) ids)
            (ids) "alternate-omns: must be a list of integers between 0 and (1- (length seqs-of-seqs)): ~A" ids)    
    ;; (assert (and (every #'listp seqs-of-seqs)
    ;;              (every #'(lambda (x) (every #'listp x)) seqs-of-seqs)
    ;;              (every #'(lambda (omn) 
    ;;                         (every #'(lambda (x)
    ;;                                    (every #'listp x)) 
    ;;                                omn)) 
    ;;                     seqs-of-seqs))
    ;;         (seqs-of-seqs) "alternate-omns: must be a list of nested OMN expressions: ~A" seqs-of-seqs)
    (let ((hash (make-hash-table)))
      (loop 
        for i from 0 to (1- omn-no)
        for my-omn in seqs-of-seqs
        ;; span (circular repeat if necessary) omn sublists to number of occurences in specs
        ;; and fill hash table with that as side effect
        do (setf (gethash i hash) (circle-repeat my-omn (count i ids))))    
      (_alternate-omns-aux ids hash append?))))

(defun _alternate-omns-aux (ids hash append?)
  "Aux def" 
  (loop 
     for id in ids
     if append?
       append (pop (gethash id hash))
       else collect (pop (gethash id hash))))

#|

(alternate-omns 
 (gen-eval 10 '(rnd-pick '(0 1)) :seed 1)
 (list
 (make-omn
  :length (length-rest-series (rnd-sample 7 '(7 8 9))
			      (length-divide 3 2
					     (rnd-sample 7 '((q q q) (h e e) (h.) (h q)))))
  :pitch '(d4 e4 f4 g4)
  :velocity '(pp))
 (make-omn
  :length '(s s s s)
  :pitch (gen-rotate :right '(c5 d5 f5 a5 g5 e5) :type :seq)
  :velocity '(ff)
  :span :pitch)))

;; omns can also be plain lengths, or pitches etc.
(alternate-omns
 '(0 0 1 0 1 0 1 1 0 0 1 1 1)
 (list
  (gen-rotate :left '(-1/20 1/20 1/20 1/20 1/20) :type :seq)
  '((q e e))))


;; If omn expressions in omns are flat lists, then alternate-omns switches between individual notes
(alternate-omns 
 (gen-eval 10 '(rnd-pick '(0 1)))
 '((q q q)
   (s s s s)))


;; Test illegal cases

;; ids too large
(alternate-omns 
 (gen-eval 10 '(rnd-pick '(2 3)))
 (list
  (make-omn
   :length '(q q q)
   :pitch '(d4 e4 f4 g4)
   :velocity '(pp))
  (make-omn
   :length '(s s s s)
   :pitch '(c5 d5 f5 a5 g5 e5)
   :velocity '(ff)
   :span :pitch)))

;; OMN expressions not sufficiently nested -- uses individual notes instead of gestures
(alternate-omns 
 (gen-eval 10 '(rnd-pick '(0 1)))
 (list
  (make-omn
   :length '(q q q)
   :pitch '(d4 e4 f4 g4)
   :velocity '(pp))
  (make-omn
   :length '(s s s s)
   :pitch '(c5 d5 f5 a5 g5 e5)
   :velocity '(ff)
   :span :pitch)))

|#




;;; TODO:
;; - allow for keywords in arg-seqs (automatically "multiply" them
;; - add example where 
;;
;; Introduces support for dynamic transformations over nested sequences (incl. double nesting) for arbitrary functions, i.e. transformations where the argument values controlling the transformation can change between sublists.0. 
(defun map-segments (segments fun arg-seqs &key (section nil) (constant-args nil))
  ""

  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-omn (n sequence &key (parameter :pitch) (flat T) (section nil))
  "Rotate the OMN `sequence' by `n' positions.

  Args:
  - n: an integer (rotation number, positive or negative) or a list of ints. The keywords :left and :right are equivalents of the integer values -1 and 1. If `n' is a list then `flat' must be nil.
  - sequence: OMN expression.
  - parameter (keyword): which parameter to rotate (e.g., :length, :pitch...). 
  - flat (Boolean): whether to rotate the full flattened sequence (T) or subsequences. 
  - section (list of ints): positions of sublists to process. This argument is ignored if flat is T.  

  Examples:

;;; (rotate-omn :right '((-h q c4) (q. f4 e g4 q a4) (h. g4)))
;;; 
;;; (rotate-omn :left '((-h q c4) (q. f4 e g4 q a4) (h. g4)) :parameter :length)
;;; 
;;; (rotate-omn 2 '((-h q c4) (q. f4 e g4 q a4) (h. g4)) :section '(1 2) :flat nil)


The following examples are rotating subsequences separately.

;;; (rotate-omn '(0 1 2) '((-h e c4 e4) (q. f4 e g4 q a4) (q g4 f4 e e4 d4)) :flat nil) ; default parameter pitch
;;; 
;;; (rotate-omn '(0 1 2) '((-h e c4 e4) (q. f4 e g4 q a4) (q g4 f4 e e4 d4)) :flat nil :parameter :length)
;;; 
;;; (rotate-omn '(2 1) '((-h e c4 e4) (q. f4 e g4 q a4) (q g4 f4 e e4 d4)) :section '(1 2) :flat nil :parameter :length)
"
  (let ((n-list-arg? (listp n)))
    (when n-list-arg?
      (assert (not flat)
	      (n flat)
	      "If N is a list then FLAT must be nil."))
    (edit-omn parameter sequence
	      #'(lambda (xs)
		  (if n-list-arg?
		      (gen-rotate (second xs) (first xs))
		      (gen-rotate n xs)))
	      ;; :swallow swallow
	      :section section
	      :flat flat
	      :additional-args (when n-list-arg? n))))

#|
;; with :additional-args and :section give, does do-section work correctly? Seemingly, there are errors in the number of resulting processed sublists and their nesting

(rotate-omn '(2 1) '((-h e c4 e4) (q. f4 e g4 q a4) (q g4 f4 e e4 d4)) :section '(1 2) :flat nil :parameter :length)
=> (make-omn :LENGTH (((-1/2 1/8 1/8) 2) (1/4 3/8 1/8)) :PITCH ((C4 E4) (F4 G4 A4) (G4 F4 E4 D4)) :VELOCITY ((MF MF) (MF MF MF) (MF MF MF MF)) ...)

|#

#|
(let ((n-list-arg? (listp n)))
    (when n-list-arg?
      (assert (not flat)
	      (n flat)
	      "If N is a list then FLAT must be nil."))
    (edit-omn parameter (if n-list-arg?
			    (tu:mat-trans (list (gen-repeat (length sequence) n)
						sequence))
			    sequence)
	      #'(lambda (xs)
		  (if n-list-arg?
		      (gen-rotate (first xs) (second xs))
		      (gen-rotate n xs)))
	      ;; :swallow swallow
	      :section section
	      :flat flat))
|#
