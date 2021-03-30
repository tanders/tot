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
  

* Arguments:
  - map:        old-new pairs (list of two-element lists)
  - sequence:   sequence (list) to transform
  - otherwise:  flat list of alternative replacements. Per sublist, only a single replacement value is chosen.

* Examples:
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
  "Beethoven-like condensation of music. All notes for which a given test function returns `nil'
are removed, and their preceding note is extended by their duration.

NOTE: The first note of sequence cannot be removed (turned into a rest) by condensation.

* Arguments:
  - sequence: An OMN sequence
  - test: Boolean function expecting individual parameters of each note in `OMN'

* Examples:

;;; (setf my-motif '((q. c4 e d4 q. e4 e f4) (h g4 -h)))

;;; (condense my-motif
;;;           #'(lambda (dur pitch &rest other-args)  
;;;               (> (omn-encode dur) 1/4)))


* Bugs:

Currently, when first note does not meet the test, the result starts with a rest. It might be better to revise the function that in that case the first note is still preserved, as otherwise the result is too much changed compared with the input.
"
  (length-legato
   (filter-notes-if test sequence :section section)))


(defun section-id-seq (pairs)
  "Expects a list of pairs (<id> <no of repetitions>) and generates out of that a flat list of integers intended as ids for {defun alternate-omns} .

* Examples: 

;;; (section-id-seq '((0 2) (1 1) (0 3)))
;;; -> (0 0 1 0 0 0)
"
  (mappend #'(lambda (sym+par) ; write out repetitions
               (gen-repeat (second sym+par) (first sym+par)))
           pairs))


(defun vary-motif (motif fn argss)
  "Generates one or more variations of given motif or phrase. As this function generates multiple variations, its result has one more list nesting level than `motif'.

The result can be used, e.g., as input for {defun alternate-omns} (alongside other motifs and their variations, which are possibly also generated with `vary-motif').  

* Arguments:
  
  - motif (OMN sequence or plain OMN parameter, depends on fn): a motif to vary
  - fn (function): arbitrary function transforming motif multiple times
  - argss (list of lists): lists of argument lists for `fn'. The argument `_' is a placeholder for `motif'.


* Examples:

;;; (tu:one-level-flat 
;;;  (vary-motif '((h. b3g4 fp tie 3q g4 3e f4 stacc+leg g4 stacc+leg a4 stacc+leg b4 stacc+leg) (h. e4c5 marc))
;;;              #'pitch-transpose
;;;              '((-4 _) (-2 _) (0 _))))
"
  (let ((result (loop 
                  repeat (length argss)
                  for args in argss
                  collect (apply fn (substitute motif '_ args)))))
    result))


;;; TODO: revise to allow for seqs-of-seqs only nested once (i.e. no variations of motifs)
(defun alternate-omns (ids seqs-of-seqs &key (append? nil))
  "This function alternates between sublists of multiple omn sequences. It can be useful, e.g., to switch between different musical characteristics, but have some kind of development within each characteristic. This is a powerful function for organising musical form on a higher level.

  The argument `seqs-of-seqs' can be written by hand or generated with other functions such as {defun vary-motif} and `ids' with functions like `gen-binary-rnd' or {defun section-id-seq} .

* Arguments:
  - ids (list of 0-based integers): indicating the position of OMN expressions in seqs-of-seqs.
  - seqs-of-seqs (list of lists of OMN sequences or plain OMN parameters): specifies material between which to switch. The top-level list positions indicate the different materials pointed to by `ids'. The next level specifies the order in which material will be included when `ids' contains repetitions of an id. This order is circling. The lowest level is the actual material either as a flat list (each a single bar), or as a nested list (multiple bars). In case of a nested list at the lowest level you likely want to set `append' to T.
  - append? (Boolean): whether or not nested OMN sequences should be appended instead of been combined in a list. 


* Examples:

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


Now, the power of algorithmic composition lies in the fact that each of these arguments can be algorithmically generated. The next example demonstrates that. It also demonstrates how the materials/gestures can be OMN expressions with multiple parameters. Still, each of these gestures consist in a single bar (gestures are not nested, though they are nested within two list levels).

;;; (alternate-omns 
;;;  ;; random sequence of IDs
;;;  (gen-eval 10 '(rnd-pick '(0 1)))
;;;  (list 
;;;   ;; sequence of first kind of musical gestures
;;;   (make-omn
;;;    :length (length-rest-series
;;; 	    (rnd-sample 7 '(7 8 9))
;;; 	    (length-divide '(3 2)
;;; 			   (rnd-sample 7 '((q q q) (h e e) (h.) (h q)))))
;;;    :pitch '(d4 e4 f4 g4)
;;;    :velocity '(pp))
;;;   ;; sequence of second kind of musical gestures
;;;   (make-omn
;;;    :length '(s s s s)
;;;    :pitch (gen-rotate :right '(c5 d5 f5 a5 g5 e5) :type :seq)
;;;    :velocity '(ff)
;;;    :span :pitch)))


Below is a more concise algorithmic example demonstrating motivic variation. Here, IDs are generated randomly and motifs/phrases are specified manually. The function {defun vary-motif} is then used for transforming these motifs multiple times. Note that each motif consists of multiple bars, and results are appended using the argument `append?' (see below for more details on this argument).

;;; (alternate-omns (gen-binary-rnd 1 10 3 1 :start 0)
;;;                 `(,(vary-motif
;;;                     '((q c5 p leg q e4 stacc) (q. f4 leg e g4 leg) (h a4))
;;;                     #'rotate-omn
;;;                     '((0 _) (-1 _) (-2 _)))
;;;                   ,(vary-motif
;;;                     '((q e3c4 ff marc+stacc s c3 stacc e3 stacc f3 stacc g3 stacc))
;;;                     #'pitch-transpose
;;;                     '((-6 _) (-4 _) (-2 _) (0 _) (2 _) (4 _))))
;;;                 :append? T)


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
            (ids) "alternate-omns: ids must be a list of integers between 0 and (1- (length seqs-of-seqs)): ~A" ids)    
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


(defun alternate-scores (ids seqs-of-scores)
  "Variant of alternate-omns for scores.

* Arguments:
  - ids (list of 0-based integers): indicating the position of scores in `seqs-of-scores'.
  - seqs-of-seqs (list of lists of scores): specifies material between which to switch. The top-level list positions indicate the different materials pointed to by `ids'. The next level specifies the order in which material will be included when `ids' contains repetitions of an id. This order is circling.

* Examples

;;; (alternate-scores '(0 0 1)
;;; 		  '(((:vln ((h c4 q d4 e4))) (:vln ((h e4 q d4 c4))))
;;; 		    ((:vln ((e b3d4g4 stacc stacc stacc stacc))))))
"
  (apply #'append-scores (alternate-omns ids seqs-of-scores)))


(defun alternate-fenvs (ids ns fenv-lists &key (interpolation :steps))
  "Alternate between fenvs and sample the fenvs; the result is a list of lists of numbers (fenv values). A fenv is a particularly flexible envelope, see my fenv library for details. 

  For convenience, fenvs can also be specified simply as lists of numbers (x-values). 

* Arguments: 
  - ids (list of 0-based integers): indicating the position of fenvs.
  - ns (list of integers): indicates number of samples per fenv. `ids' and `ns' should have the same length.
  - fenv-lists (list of fenvs, or list of lists of fenvs): specifies fenvs between which to switch. If `fenv-lists' is a flat list of fenvs, then `ids' simply access the fenvs at those postions. If `fenv-lists' is nested, then `ids' indicate the top-level list positions of `fenv-lists'. The lower-level list of fenvs indicates alternatives from which to choose in order. So, when there is a repetition of integers in `ids', always the next fenv in the respective list of alternatives is choses. This order is circling.
    Remember that fenvs can be specified as a number sequence as well. 
  - interpolation (either :steps or :linear): in case fenvs are specified as lists of numbers, the `interpolation' indicates the type of fenv created.

* Examples:
Using lists of integers, internally translated into fenvs. The result here is rather similar to the input, but with lists of different lengths. 
;;; (alternate-fenvs '(0 1 0) '(3 3 4) 
;;; 		 '((1 2 3 4)
;;; 		   (10 8 6 4)))
;;; => ((1 3 4) (10 6 4) (1 2 3 4))

For ns greater than the length of the number lists representing the fenvs, the interpolation method makes a difference.
;;; (alternate-fenvs '(0 1 0) '(6 8 6) 
;;; 		 '((1 3 2)
;;; 		   (10 4)))
;;; => ((1 1 3 3 2 2) (10 10 10 10 4 4 4 4) (1 1 3 3 2 2))

;;; (alternate-fenvs '(0 1 0) '(6 8 6) 
;;; 		 '((1 3 2)
;;; 		   (10 4))
;;; 		 :interpolation :linear)
;;; => ((1 9/5 13/5 14/5 12/5 2) (10 64/7 58/7 52/7 46/7 40/7 34/7 4) (1 9/5 13/5 14/5 12/5 2))


When specifying fenvs directly, the full range of fenvs are available.
;;; (alternate-fenvs '(0 1 0) '(3 3 4) 
;;; 		 (list (fenv:sin-fenv (0 1) (1 0))
;;; 		       (fenv:sin-fenv (0 0) (1 1))))

An example with double-nested fenvs
;;; (alternate-fenvs '(0 0 1 1 0 0) '(4 4 4 4 4 4)  
;;;      		 '(((1 2 3 4) (2 3 4 5) (3 4 5 6)) 
;;;      		   ((10 8 6 4) (11 9 7 5))))
;;; => ((1 2 3 4) (2 3 4 5) (10 8 6 4) (11 9 7 5) (3 4 5 6) (1 2 3 4))

For more examples with nested lists of fenvs in `fenv-lists' compare the use of (double) nested OMN sequences in `alternate-omns' above. 
"
  (let ((omn-no (length fenv-lists))
	(full-fenv-lists (if (every #'fenv:fenv? fenv-lists)
			     ;; flat list of fenvs
			     (mclist fenv-lists)
			     ;; assume elements in fenv-lists are lists -- test only first list for efficiency
			     (let ((first-list (first fenv-lists)))
			       (cond
				 ;; nested list of fenvs
				 ((every #'fenv:fenv? first-list) fenv-lists)
				 ;; flat list of number seqs
				 ((every #'numberp first-list) 								  
				  (mapcar #'(lambda (fenv) (list (fenv:list->fenv fenv :type interpolation))) fenv-lists))
				 ;; Assume nested list of number seqs
				 (T (mapcar #'(lambda (fenvs)
						(mapcar #'(lambda (fenv)
							    (fenv:list->fenv fenv :type interpolation))
							fenvs))
					    fenv-lists)))))))    
    (assert (every #'(lambda (x) (and (integerp x) (< x omn-no))) ids)
            (ids) "alternate-fenvs: ids must be a list of integers between 0 and (1- (length seqs-of-seqs)): ~A" ids)  
    (let ((hash (make-hash-table)))
      (loop 
        for i from 0 to (1- omn-no)
        for my-fenv in full-fenv-lists
        do (setf (gethash i hash) (circle-repeat my-fenv (count i ids))))
      (loop 
	 for id in ids
	 for n in ns
	 collect (fenv:fenv->list (pop (gethash id hash)) n :first)))))


;; TODO: not tail recursive
(defun _map-sublist-subseqs (no-of-sublists sequence fn)
  "aux
  fn is a function expecting a list of sublists of sequence"
  (when (and no-of-sublists sequence)
    (let ((n (first no-of-sublists)))
      (cons (funcall fn (first-n n sequence))
	    (_map-sublist-subseqs (rest no-of-sublists) (subseq sequence n) fn)))))
#|  ; test
(_map-sublist-subseqs '(2 1 2 1) (gen-repeat 3 '((h) (q q)))
		     #'(lambda (sublists) (count-notes sublists)))

(_map-sublist-subseqs '(2) (gen-repeat 3 '((q q)))
		     #'(lambda (sublists) (count-notes sublists)))
|#


;;; TODO:
;;; - add more examples to doc?
;;; - Make more efficient: internally translate articulation-maps into vector for faster access at index
;;; - OK "Protect" arg no-of-sublists -- ensure it is of same length as ids
;;; - OK Allow for option to add articulation parameters without overwriting existing articulations
(defun alternate-subseq-fenvs (ids no-of-sublists fenv-lists parameter sequence
			       &key (min-vel 'pp) (max-vel 'ff)
				 (articulation-maps nil)
				 (keep-articulations? T)
				 (interpolation :steps)
				 (hairpins? nil))
  "This function adds (or replaces) a given `parameter' to (of) an OMN `sequence', where this new parameter sequence follows a concatenation of fenvs. A fenv is a particularly flexible envelope, see my fenv library for details. 

  Like `alternate-omns', the present function is useful for switching between different musical characteristics, while having some kind of development within each characteristic. It is a powerful function for organising musical form on a higher level. 

  One of the particular expressive powers of this function is that characteristics defined by fenvs (`fenv-lists') can be applied to sublists (bars) in `sequence' of different lengths. For example, the same overall gestus can be applied to a bar of three but all four or five notes. (Nevertheless, it does not allow to change the number of elements in `sequence'.)

  For convenience, fenvs can also be specified simply as lists of numbers (x-values). 

  See also the function `alternate-omn-fenvs', which is similar but more easy to use.

* Arguments: 
  - ids (list of 0-based integers): indicating order of positions of fenvs to choose from `fenv-lists'.
  - no-of-sublists (integer or list of integers): indicates how many consecutive sublists (quasi bars) of `sequence' each fenv shapes. The number of samples created from each fenv is the product of the length of the sublist in question and the respective `no-of-sublists' value.
  - fenv-lists (list of fenvs, or list of lists of fenvs): specifies fenvs between which to switch. If `fenv-lists' is a flat list of fenvs, then `ids' simply access the fenvs at those positions. If `fenv-lists' is nested, then `ids' indicate the top-level list positions of `fenv-lists'. The lower-level list of fenvs indicates alternatives from which to choose in order. So, when there is a repetition of integers in `ids', always the next fenv in the respective list of alternatives is choses. This order is circling.
  - parameter (keyword): the parameter the fenvs overwrite in `sequence', can be :length, :pitch, :velocity or :articulation. As fenv values are always numeric, they have to be translated into the corresponding parameter. Fenvs values for length values should be ratios (they are translated to ratios internally, but complex ratios can lead to difficulties with notation); fenv values for pitches are MIDI note numbers; fenv values for velocities depend on `min-vel' and `max-vel'; and articulations are the rounded position of elements in the `articulation-maps'.
  - sequence: OMN sequence to transform, must be nested.
  - min-vel/max-vel (OMN velocity symbol like 'p or 'ff): in case `parameter' is :velocity, these args set the minimum and maximum velocity in the result.
  - articulation-maps (list of list of OMN articulation symbols): in case `parameter' is :articulation, this arg specifies the articulations that rounded fenv values mean. For example, if `articulation-maps' is '(stacc ten) then the fenv value 0 results in a staccato and 1 in a tenuto.
  - keep-articulations? (Boolean): if T, existing articulations of `sequence' are retained and new articulations are added, otherwise new articulations overwrite the existing ones.
  - interpolation (either `:steps' or `:linear'): in case fenvs are specified as lists of numbers, the `interpolation' indicates the type of fenv created.
  - hairpins? (Boolean): In case `parameter' is `:velocity' you can set that the dynamics are connected with crescendo and diminuendo hairpins, which may make particularly sense if `interpolation' is set to `:linear'.

* Examples:

Lists of integers can be used instead of fenvs for convenience. 
;;; (alternate-subseq-fenvs '(0 1 1 0)
;;; 			'(2 1 2 1)
;;; 			;; two envelopes, defined as linearly interpolated number lists
;;; 			'((72 48) (60 84))
;;; 			:pitch
;;; 			(gen-repeat 3 '((h h) (q q q q)))
;;; 			:interpolation :linear)


When specifying fenvs directly, the full range of fenvs and their transformations are available.
;;; (alternate-subseq-fenvs '(0 1 1 0)
;;; 			'(2 1 2 1)
;;; 			(list (fenv:sin-fenv (0 72) (1 48))
;;; 			      (fenv:sin-fenv (0 60) (1 84)))
;;; 			:pitch
;;; 			(gen-repeat 3 '((h h) (q q q q))))


An example with velocities
;;; (alternate-subseq-fenvs '(0 1 1 0)
;;; 			'(1 1 1 1)
;;; 			(list (omn->fenv :velocity '(p p p ff) :type :steps)
;;; 			      (omn->fenv :velocity '(mf pp pp pp) :type :steps))
;;; 			:velocity
;;; 			(gen-repeat 3 '((h h) (q q q q))))

An example with double-nested lists.
;;; (alternate-subseq-fenvs '(0 0 1 1 0 0) 
;;;      			'(1 1 1 1 1 1) 
;;;      			;; two envelopes, defined as linearly interpolated number lists 
;;;      			'(((72 60) (71 59) (70 58))
;;;                               ((72 84) (74 86)))
;;;      			:pitch 
;;;      			(gen-repeat 3 '((h h) (q q q q))) 
;;;      			:interpolation :linear)
;;; => ((h c5 c3) (q b4 eb4 g3 b2) (h c4 c6) (q d4 bb4 fs5 d6) (h bb4 bb2) (q c5 e4 gs3 c3))
"
  (let* ((full-no-of-sublists (circle-repeat no-of-sublists (length ids)))
	 (fenv-ns (_map-sublist-subseqs full-no-of-sublists sequence
					#'(lambda (sublists) (count-notes sublists))))
	 (fenv-val-lists (alternate-fenvs ids fenv-ns fenv-lists :interpolation interpolation))
	 ;; efficiency: not needed when parameter is :articulation
	 (flat-fenv-vals (unless (eql parameter :articulation)
			   (flatten fenv-val-lists)))
	 ;; fenv vals translated into OMN params
	 (fenv-params (ccase parameter
			(:length (mapcar #'rationalize flat-fenv-vals))
		        (:pitch (midi-to-pitch (mapcar #'round flat-fenv-vals)))
			(:velocity (if hairpins?
				       (velocity-to-dynamic (vector-to-velocity min-vel max-vel flat-fenv-vals))
				       (vector-to-velocity min-vel max-vel flat-fenv-vals :type :symbol)))
			(:articulation
			 (mapcar #'(lambda (fenv-vals id)
				     (let* (;; NOTE: inefficient: access with nth
					    (articulation-map (nth id articulation-maps))
					    (art-map-length (length articulation-map)))
				       (mapcar #'(lambda (val)
						   (let ((round-val (round val)))
						     (assert (< -1 round-val art-map-length)
							     (round-val)
							     "alternate-fenvs-for-subseqs: when generating articulations, all fenv values must be within the range of the length of ;TODO: he articulation-maps. Current fenv value: ~A, articulation-maps: ~A" round-val articulation-maps)
						     ;; NOTE: inefficient: access with nth
						     (nth round-val articulation-map)))
					       fenv-vals)))
				 fenv-val-lists
				 ids))))
	 )
    (omn-replace parameter (span (omn :length sequence)
				 (if (and (eql parameter :articulation)
				      keep-articulations?)
				     (zip-articulations
				      (flatten fenv-params)
				      (flatten (get-full-articulations sequence)))
				     fenv-params))
		 sequence)
    ))




(defun _counters-to-1s (xs)
  "[Aux] Expects a list returned by an expression like (omn :leg sequence), which expresses TRUE with 1 and FALSE with -1, but consecutive TRUE or FALSE values are accuulated, so that '(1 1 1 -1 1) is represented more concisely as '(3 -1 1). The present function translates this more concise representation into the longer representation consisting only of 1 and -1, so that results of an expression like (omn :leg sequence) can be turned into a fenv."
  (loop for x in xs
    append (if (> (abs x) 1)
             (gen-repeat (abs x) (signum x)) 
             (list x)
             )))
; (_counters-to-1s '(-1 8))
; (_counters-to-1s nil)


(defun omn->fenv (parameter sequence &key (type :steps) (x-values :rhythm))
  "Translates one of the parameters of the OMN `sequence' into a number sequence, which is then translated into a fenv. A fenv is a particularly flexible envelope, see my fenv library for details. 

  This function is useful, e.g., for quasi motific variation, where the number of notes in the result changes but the result still follows the overall shape (profile) of the given music. 

  Usually, you do not want to use this function directly, but instead use the function {defun alternate-omn-fenvs} , which is less low-level and therefore more easy to use.

* Arguments:
  - parameter: an OMN parameter keyword like :length or :pitch
  - sequence: either full OMN sequence or sequence of the respective OMN parameters
  - type (either :steps or :linear): how to interpolate between parameter values. Can be :steps (a step function, i.e. parameter values are hold by the fenv until the next value) or :linear (linear interpolation). 
  - x-values (either :rhythm or :equidistant): whether the x-values of the resulting fenv follow the rhythm of `sequence' or are equidistant, where points are spread evenly across the x axis.

* Examples:

  A six-note OMN sequence with multiple parameters used for several examples
  ;;; (setf sequence '((h c5 f ten q g4 mp stacc) (q b4 ff ten q g4 f stacc q d4 mp stacc) (h. g4 f tr2)))

  Translate the note values of this six-note sequence into a sequence of five note values.  
  ;;; (fenv:fenv->list (omn->fenv :length sequence) 5)

  Translate the pitches of this six-note sequence into a sequence of five pitches. 
  ;;; (midi-to-pitch (fenv:fenv->list (omn->fenv :pitch sequence) 5))

  Translate the pitches of this six-note sequence into a sequence of eight pitches. 
  ;;; (midi-to-pitch (mapcar #'round (fenv:fenv->list (omn->fenv :pitch sequence :type :linear) 8)))  

  Translate the note dynamics of this six-note sequence into a sequence of 9 dynamics without interpolation.
  ;;; (get-velocity (mapcar #'round (fenv:fenv->list (omn->fenv :velocity sequence :type :steps) 9)) :type :symbol)

  Translate the note dynamics of this six-note sequence into a sequence of 5 dynamics, but linearily interpolating between the original xones.
  ;;; (get-velocity (mapcar #'round (fenv:fenv->list (omn->fenv :velocity sequence :type :linear) 5)) :type :symbol)


  Translate the articulations of this six-note sequence into a sequence of 7 articulations.
  ;;; (multiple-value-bind (fenv arts-set)
  ;;;     (omn->fenv :articulation (flatten sequence))
  ;;;   ;; inefficient with list
  ;;;   (mapcar #'(lambda (i) (nth i arts-set)) 
  ;;; 	    (fenv:fenv->list fenv 7)))
"
  (let ((flat-seq (flatten sequence)))
    (multiple-value-bind (num-seq arts-set)
	;; in case parameter is :articulation (and only in that case), I have to additionally store the "set" of articulations that are possible.
	(ccase parameter
	  (:length (if (every #'lengthp flat-seq)
		       flat-seq
		       (omn parameter flat-seq)))
	  (:pitch (pitch-to-midi (if (every #'pitchp flat-seq)
				     flat-seq
				     (omn parameter flat-seq))))
	  (:velocity (get-velocity flat-seq :type :midi))
	  (:articulation (let* ((arts
				 (if (every #'articulationp flat-seq)
				     flat-seq
				     (get-full-articulations flat-seq)))
				  ;; (omn :articulation flat-seq))
				(arts-set (remove-duplicates arts)))
			   (values 
			    (loop for art in arts
			       collect (position art arts-set))
			    arts-set)))
	 ;; NOTE: :gliss variants like gliss2, gliss3, gliss4, kgliss and kgliss-ch currently not supported
	  ((:leg :gliss) (_counters-to-1s (omn parameter flat-seq)))
	  )
      (let ((fenv (when num-seq ;; params like :leg can be nil
		    (ccase x-values
		      (:equidistant (fenv:list->fenv num-seq :type type))
		      ;; !!! TODO: for fenvs of lengths, lengths are best processed positionally as before to preserve the rests?
		      (:rhythm (let* (;; Rests merged in, they should not count as extra rhythmic values in fenv
				      (lengths (omn :length (merge-rest-into-note flat-seq)))
				      (xs (ccase type
					    (:linear (tu:dx->x (tu:scale-sum lengths 1) 0))
					    (:steps (butlast (tu:dx->x (tu:scale-sum lengths 1) 0))))))
				 (fenv:list->fenv (ccase type
						    ;; NOTE: Hack: last fenv value is constant over last note dur (otherwise number of y values one less than x values because I had to perform tu:dx->x on rhythmic values representing x values)
						    (:linear (append num-seq (last num-seq)))
						    (:steps num-seq))
						  :type type :xs xs)))))))
	(case parameter
	  (:articulation (values fenv arts-set))
	  (otherwise fenv))))))

; (omn->fenv :articulation '(q g4 f legno q c5 p stacc q c3 pp stacc) :type :steps)

;; ;;; TODO:
;; ;; - scale rhythmic values so that their sum = 1: tu:scale-sum   
;; ;; - find a way to have values for all rhythmic positions -- should last value be repeated, so that end of fenv is some section with last value?? OK if type is steps, but not for linear interpolation...
;; (setf seq '(h g4 f legno q c5 p stacc q c3 pp stacc))
;; ;; use this method for :type :steps
;; (butlast (tu:dx->x (tu:scale-sum (omn :length seq) 1) 0))
;; (omn :pitch seq)
;; ;; for :type :linear instead use
;; ;;; BUG: not correct (same as above)
;; (let ((time-positions (tu:dx->x (omn :length seq) 0)))
;;   (tu:rescale time-positions 0 (first (last time-positions)) 0 1))

(defun omn->fenvs (sequence &key (parameters '(:length :pitch :velocity)) (type :steps) (x-values :rhythm))
   "Translates all parameters of the OMN `sequence' into a number sequences, which are then translated into a fenvs. Returns a plist of pairs <parameter> <fenv>. For further details on how the individual fenvs are generated see function `omn->fenv'

* Arguments:
  - sequence: OMN sequence
  - parameters: which parameters to translate. By default, leaves out :articulation, because interpreting articulations in a fenv takes some extra measures.
  - type: see doc of omn->fenv above  
  - x-values: see doc of omn->fenv above  
"

  (loop for param in parameters
     append (let ((fenv (omn->fenv param sequence :type type :x-values x-values)))
	      (when fenv 
		(list param fenv)))))

; (omn->fenvs '(q g4 f legno q c5 p stacc q c3 pp stacc) :type :steps)


(defun fenv->omn-parameter (fenv parameter n &key arts-set)
  "Translates a fenv into one of the OMN parameters. This is the inverse of the above function {defun omn->fenv} . For translating fenvs into OMN attributes, a list of attributes must be given. 

* Arguments:
  - fenv (a fenv)
  - parameter: an OMN parameter keyword like :length or :pitch
  - n (integer): how many values to generate from fenv.
  - arts-set (list of articulation symbols): this list maps number (fenv values) into the attributes at the corresponding position in `arts-set'.
"
  (ccase parameter
    ;;; TODO: rationalise
    ((:length :leg :gliss) (fenv:fenv->list fenv n))
    (:pitch (midi-to-pitch (mapcar #'round (fenv:fenv->list fenv n))))
    (:velocity (get-velocity (mapcar #'round (fenv:fenv->list fenv n)) :type :symbol))
    (:articulation ;; NOTE: inefficient with long list arts-set, array might be better
     (mapcar #'(lambda (i) (nth i arts-set)) 
	     (mapcar #'round (fenv:fenv->list fenv n))))
     ))


;;; TODO: allow for nested omn-fenv-lists for articulations as well
;;; BUG: if no-of-sublists values are not all 1 for parameter :articulation, then results are not correct -- possibly wrapping messed up?
;;; TODO: Args swallow for rests?
;;; TODO: Consider interpreting the rhythm of omn-fenv-lists as fenv y values (distances between them)
;;; TODO Dynamics currently only be static values here, because behind the scene they are translated into fenvs with only one velocity number per note
;;;      I could at a later stage also allow for cresc. and dim. by extending the definition of alternate-omn-fenvs and introducing dynamics-maps, much like articulation-maps
;;; OK TODO: Allow for option to add articulation parameters without overwriting existing articulations
;;; OK TODO: make it recursive for multiple params
;;; OK TODO: add support for param velocity 
;;; OK TODO: add support for param articulation
;;; OK TODO: parameters: ensure list, but only once in recursive function
(defun alternate-omn-fenvs (ids no-of-sublists omn-fenv-lists parameters sequence
			    &key (keep-articulations? T) (interpolation :steps) (hairpins? nil))  
  "This function adds (or replaces) one or more given `parameters' to (of) an OMN `sequence', where this new parameter sequence follows a concatenation of fenvs. A fenv is a particularly flexible envelope, see my fenv library for details. However, for this function musical characteristics in `omn-fenv-lists' are defined by OMN expressions instead, and fenvs are only used in the background.

  Like `alternate-omns', the present function is useful for switching between different musical characteristics, while having some kind of development within each characteristic. It is a powerful function for organising musical form on a higher level. 

  One of the particular expressive powers of this function is that characteristics defined by fenvs (`fenv-lists') can be applied to sublists (bars) in `sequence' of different lengths. For example, the same overall gestus can be applied to a bar of three but all four or five notes. (Nevertheless, it does not allow to change the number of elements in `sequence'.)

* Arguments:
  - ids (list of 0-based integers): indicating order of positions of fenvs to choose from `omn-fenv-lists'.
  - no-of-sublists (integer or list of integers): indicates how many consecutive sublists (quasi bars) of `sequence' each fenv shapes. The number of samples created from each fenv is the product of the length of the sublist in question and the respective `no-of-sublists' value.
  - omn-fenv-lists (list of flat OMN expression lists, or list of lists of flat OMN expression lists): specifies characteristics between which to switch (transformed into fenvs in the background). If `omn-fenv-lists' is a list of OMN expression lists, then `ids' simply access the fenvs at those positions. If `omn-fenv-lists' is further nested, then `ids' indicate the top-level list positions of `omn-fenv-lists'. The lower-level list of fenvs indicates alternatives from which to choose in order. So, when there is a repetition of integers in `ids', always the next fenv in the respective list of alternatives is choses. This order is circling.
  - parameter (keyword or list of keywords): the parameter the fenvs overwrite in `sequence', can be :length, :pitch, :velocity or :articulation. 
  - sequence: OMN sequence to transform, must be nested.
  - keep-articulations? (Boolean): if T, existing articulations of `sequence' are retained and new articulations are added, otherwise new articulations overwrite the existing ones.
  - interpolation (either `:steps' or `:linear'): `interpolation' indicates the type of fenv created (interpolation between the numeric representation of OMN parameter values or not).
  - hairpins? (Boolean): In case `parameter' is `:velocity' you can set that the dynamics are connected with crescendo and diminuendo hairpins, which may make particularly sense if `interpolation' is set to `:linear'.

* Examples:

TODO:

"
  (if parameters
      (let ((params (tu:ensure-list parameters)))
	(alternate-omn-fenvs
	 ids no-of-sublists omn-fenv-lists (rest params)       
	 (let* ((param (first params))
		;; skip if (eql param :articulation)
		(fenv-lists (unless (eql param :articulation)
			      (mapcar #'(lambda (omn-env)
					  (cond ((every #'listp omn-env)
						 (mapcar #'(lambda (omn-sub-env)
							     (omn->fenv param omn-sub-env :type interpolation))
							 omn-env))
						((every #'atom omn-env)
						 (omn->fenv param omn-env :type interpolation))))
				      omn-fenv-lists))))
	   (ccase param
	     ((:length :pitch)
	      (alternate-subseq-fenvs ids no-of-sublists fenv-lists param sequence
				      :interpolation interpolation :hairpins? hairpins?))
	     ((:velocity)
	      (let ((flat-omn-fenv-lists (flatten omn-fenv-lists)))
		(alternate-subseq-fenvs ids no-of-sublists fenv-lists param sequence
					:interpolation interpolation
					:min-vel (min-velocity flat-omn-fenv-lists)
					:max-vel (max-velocity flat-omn-fenv-lists)
					:hairpins? hairpins?)))
	     ((:articulation)
	      (let* (;; TODO: currently only list of flat omn-fenv-lists for :articulation supported, but not a list of alternatives
		     (fenvs_and_articulation-map
		      (tu:mat-trans
		       (mapcar #'(lambda (omn-env)
				   (multiple-value-list
				    (omn->fenv :articulation omn-env :type interpolation)))
			       omn-fenv-lists)))
		     (fenv-lists (first fenvs_and_articulation-map))
		     (articulation-maps (second fenvs_and_articulation-map)))
		(alternate-subseq-fenvs ids no-of-sublists fenv-lists param sequence
					:articulation-maps articulation-maps
					:keep-articulations? keep-articulations?
					:hairpins? hairpins?)))))       
	 :keep-articulations? keep-articulations? 
	 :interpolation interpolation
	 :hairpins? hairpins?))
      sequence))



#|
;;; tests
(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two note value envelopes, defined by parameter seqs
		     '((h q q h h)
		       (e e e e))
		     '(:length)
		     (gen-repeat 3 '((h h) (q q q q))))


(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two pitch envelopes, defined by parameter seqs
		     '((g4 c5 c3)
		       (c4 c6))
		     '(:pitch)
		     (gen-repeat 3 '((h h) (q q q q)))
		     :interpolation :linear)
=> ((H G4 A4) (Q B4 G4 BB3 C3) (H C4 C6) (Q C4 F4 BB4 D5) (H G5 C6) (Q G4 BB4 E4 C3)) 

(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two pitch envelopes, defined by OMN seqs
		     '((q c5 q c3)
		       (q c4 q c6))
		     '(:pitch)
		     (gen-repeat 3 '((h h) (q q q q)))
		     :interpolation :linear)

(alternate-omn-fenvs '(0 1 1 0)
		     '(1 1 1 1)
		     ; '(2 1 2 1)
		     ;; two envelopes consisting of multiple parameters
		     '((q g4 f q c5 pp q c3)
		       (q c4 f q c6 pp)
		       )
		     '(:velocity)
		     (gen-repeat 3 '((h h) (q q q q)))
		     :interpolation :steps)

;;; BUG: simplify-dynamics results not correct
(simplify-dynamics 
 (alternate-omn-fenvs '(0 1 1 0)
                      '(1 1 1 1)
                      ; '(2 1 2 1)
                      ;; two envelopes consisting of multiple parameters
                      '((q g4 f q c5 pp q c3)
                        (q c4 f q c6 pp)
                        )
                      '(:velocity)
                      (gen-repeat 3 '((h h) (q q q q)))
                      :interpolation :linear
                      :hairpins? T))


(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two envelopes consisting of multiple parameters
		     '((q g4 f q c5 pp q c3)
		       (q c4 f q c6 pp)
		       )
		     ;; '(:pitch)
		     '(:pitch :velocity)
		     (gen-repeat 3 '((h h) (q q q q)))
		     :interpolation :linear)

;;; BUG: fenv values "circled"
;;; BUG: legato articulation ignored?
(alternate-omn-fenvs '(0 1 1 0)
		     '(1 1 1 1)
		     ;; two envelopes consisting of multiple parameters
		     '((q g4 f legno q c5 p stacc q c3 pp stacc)
		       (q c4 f ten+ord q c6 pp stacc)
		       )
		     '(:articulation)
		     (gen-repeat 2 '((h c4 q e4 q g4) (q f4 q e4 q d4 q b3)))
		     :interpolation :steps
                     )

;;; BUG: causes error -- :articulation not yet supported
(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two envelopes consisting of multiple parameters
		     '((q g4 f ten q c5 p stacc q c3 pp stacc)
		       (q c4 f leg q c6 pp stacc)
		       )
		     '(:articulation)
		     (gen-repeat 3 '((h h) (q q q q)))
		     ;;; BUG: can I allow for :linear for :articulation?
		     :interpolation :linear)


;;; BUG: causes error -- :articulation not yet supported
(alternate-omn-fenvs '(0 1 1 0)
		     '(2 1 2 1)
		     ;; two envelopes consisting of multiple parameters
		     '((q g4 f ten q c5 p stacc q c3 pp stacc)
		       (q c4 f leg q c6 pp stacc)
		       )
		     ;; '(:pitch)
		     '(:pitch :velocity :articulation)
		     (gen-repeat 3 '((h h) (q q q q)))
		     :interpolation :linear)

|#






#| ; unfinished
;;; TODO:
;; - allow for keywords in arg-seqs (automatically "multiply" them
;; - add example where 
;;
;; Introduces support for dynamic transformations over nested sequences (incl. double nesting) for arbitrary functions, i.e. transformations where the argument values controlling the transformation can change between sublists.0. 
(defun map-segments (segments fun arg-seqs &key (section nil) (constant-args nil))
  ""

  
  )
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-omn (n sequence &key (parameter :pitch) (flat T) (section nil))
  "Rotate the OMN `sequence' by `n' positions.

* Arguments:
  - n: an integer (rotation number, positive or negative) or a list of ints. The keywords :left and :right are equivalents of the integer values -1 and 1. If `n' is a list then `flat' must be nil.
  - sequence: OMN expression.
  - parameter (keyword): which parameter to rotate (e.g., :length, :pitch...). 
  - flat (Boolean): whether to rotate the full flattened sequence (T) or subsequences. 
  - section (list of ints): positions of sublists to process. This argument is ignored if flat is T.  

* Examples:

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
;; (let ((n-list-arg? (listp n)))
;;     (when n-list-arg?
;;       (assert (not flat)
;; 	      (n flat)
;; 	      "If N is a list then FLAT must be nil."))
;;     (edit-omn parameter (if n-list-arg?
;; 			    (tu:mat-trans (list (gen-repeat (length sequence) n)
;; 						sequence))
;; 			    sequence)
;; 	      #'(lambda (xs)
;; 		  (if n-list-arg?
;; 		      (gen-rotate (first xs) (second xs))
;; 		      (gen-rotate n xs)))
;; 	      ;; :swallow swallow
;; 	      :section section
;; 	      :flat flat))
|#

