;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Opusmodus package
(in-package :om)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process velocities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun velocity-fenvs (lengths &rest dynamics-fenvs)
  "Generates dynamics markers (velocities) for OMN `lengths'. Dynamics for each phrase (notes between rests) are defined by their own fenv (if fewer fenvs are defined, they are circled through). Returns a sublist for each phrase.

* Arguments:
  - lengths: OMN `lengths' (list or list of list)
  - each dynamics-fenv has the form (<min-vel> <max-vel> &rest points), where `min-vel' and `max-vel' are dynamic indicators like pp or f, and points are pairs of linear fenv points. 

* Examples:
  ;;; (velocity-fenvs  '((1/4 1/8 1/8) (1/4 -1/4) (1/4 1/4) (1/4 -1/4))
  ;;;                  '(pp mp (0 0) (0.7 1) (1 0)))
  ;;; => ((pp< p< > pp) (pp< > pp))
  "
  (let ((phrase-lengths (phrase-lengths lengths)))
    (mapcar #'simplify-dynamics
            (mapcar #'(lambda (dynamics-fenv phrase-length)
                        (destructuring-bind (min-vel max-vel &rest points)
                                            dynamics-fenv
                          (velocity-to-dynamic 
                           (vector-to-velocity  min-vel max-vel 
                                                (fenv:v (fenv:linear-fenv-fn points) phrase-length)))))
                    (circle-repeat dynamics-fenvs (length phrase-lengths))
                    phrase-lengths))))

          
#|
(velocity-fenvs  '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4))
'(pp mp (0 0) (0.7 1) (1 0))
'(pp mp (0 0) (0.65 1) (1 0)))

(velocity-fenvs  (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))
'(pp mp (0 0) (0.7 1) (1 0))
'(pp mp (0 0) (0.65 1) (1 0)))
|#


;;; extended variant of velocity-fenvs -- at some stage replace former in all calls with this.
(defun velocity-fenvs2 (sequence dynamics-fenvs &key (omn T))
  "Generates dynamics markers (velocities) for OMN `lengths'. Dynamics for each phrase (notes between rests) are defined by their own fenv (if fewer fenvs are defined, they are circled through). Returns a sublist for each phrase.

* Arguments:
  - lengths (list): OMN length values or OMN sequence, can be nested
  - dynamics-fenvs: One dynamics-fenv or list of them. Each dynamics-fenv has the form (<min-vel> <max-vel> &rest points), where `min-vel' and `max-vel' are dynamic indicators like pp or f, and points are pairs of linear fenv points. 
  - omn (Boolean): whether to return a plain velocity list or OMN expression

* Examples:
  ;;; (velocity-fenvs  '((1/4 1/8 1/8) (1/4 -1/4) (1/4 1/4) (1/4 -1/4))
  ;;;                  '(pp mp (0 0) (0.7 1) (1 0)))
  ;;; => ((pp< p< > pp) (pp< > pp))

  BUG: not working yet
  "
  (let* ((phrase-lengths (phrase-lengths sequence))
         (my-dynamics-fenvs (if (velocityp (first dynamics-fenvs)) ; only single dynamics-fenv (crude test)
                              (list dynamics-fenvs)
                              dynamics-fenvs))
         (result (mapcar #'simplify-dynamics
                         (mapcar #'(lambda (dynamics-fenv phrase-length)
                                     (destructuring-bind (min-vel max-vel &rest points)
                                                         dynamics-fenv
                                       (velocity-to-dynamic 
                                        (vector-to-velocity  min-vel max-vel 
                                                             (fenv:v (fenv:linear-fenv-fn points) phrase-length)))))
                                 (circle-repeat my-dynamics-fenvs (length phrase-lengths))
                                 phrase-lengths))))
    (if omn
      (copy-time-signature 
       sequence (omn-replace :velocity (flatten result) (flatten sequence)))                           
      result)))

#|
(setf part-length '((1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28 1/28) (1/28 1/28 1/28 -1/28 1/28 1/28 1/28 1/28 1/28 -1/28 1/28 1/28 1/28 1/28) (-1/28 -1/28 1/28 1/28 1/28 1/28 -1/28 -1/28 1/28 1/28 1/28 -1/28 -1/28 -1/28) (1/28 1/28 1/28 -1/28 -1/28 -1/28 1/28 1/28 -1/28 -1/28 -1/28 -1/28 1/28 1/28)))

;;; BUG: why is this generating several short sublists (bars)
(velocity-fenvs2 (flatten part-length) '(pp ff (0 1) (1 0)) :omn nil)
|#


#|
;;; BUG:  error
(velocity-fenvs  
 '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4))
 '((pp mp (0 0) (0.7 1) (1 0))
   (pp mp (0 0) (0.65 1) (1 0))))

(velocity-fenvs  (flatten '((-1/6 1/6 1/6) (1/4 1/4) (1/6 1/6 1/6) (1/4 -1/4) (-1/10 -1/10 1/10 1/10 1/10) (1/4 1/4) (1/10 1/10 1/10 1/10 1/10) (1/4 -1/4)))
'(pp mp (0 0) (0.7 1) (1 0))
'(pp mp (0 0) (0.65 1) (1 0)))
|#


;;;
;;; NOTE: the following definitions are to be replaced by OMN build-in velocity-intermediate
;;; See https://opusmodus.com/forums/topic/771-velocity-to-dynamic-limitation/
;;;

#|
(defun cresc-p (dyn)
  "Example: 
  (cresc-p 'p<)
  => T

  BUG: too simple: there are dynamic symbols where this is not working for. See *one-note-dynamic-symbol*."
  (and (velocityp dyn)
       (let ((str (symbol-name dyn)))
         (equal (subseq str (1- (length str)))
                "<"))))

(defun dim-p (dyn)
  "Example: 
  (dim-p 'f>)
  => T

  BUG: too simple: there are dynamic symbols where this is not working for. See *one-note-dynamic-symbol*."
  (and (velocityp dyn)
       (let ((str (symbol-name dyn)))
         (equal (subseq str (1- (length str)))
                ">"))))
|#


#|
(defun simplify-dynamics (dynamics)
  "Removes intermediate textual dynamic indicators from longer hairpins (e.g., generated by velocity-to-dynamic or gen-dynamic).
  
* Examples:
  (simplify-dynamics '(pppp< < ppp< pp< < p< < mp< mf< < f< < ff> > mf> mp> p> ppp> pppp))
  => (pppp< < < < < < < < < < < < ff> > > > > > pppp)
  "
  (append (list (first dynamics))
          (loop
            for (d1 d2 d3) on dynamics 
            when (and d2 d3)
            collect (cond ((or (and (cresc-p d1) (dim-p d3))
                               (and (dim-p d1) (cresc-p d3)))
                           d2)
                          ((or (and (cresc-p d1) (cresc-p d3))
                               (and (not (member d2 '(< >)))
                                    (not (member d3 '(< >)))
                                    (not (member d2 *one-note-dynamic-symbol*))
                                    (not (member d3 *one-note-dynamic-symbol*))
                                    (< (get-velocity d2) (get-velocity d3))))
                           '<)
                          ((or (and (dim-p d1) (dim-p d3))
                               (and (not (member d2 '(< >)))
                                    (not (member d3 '(< >)))
                                    (not (member d2 *one-note-dynamic-symbol*))
                                    (not (member d3 *one-note-dynamic-symbol*))
                                    (> (get-velocity d2) (get-velocity d3))))
                           '>)
                          (t d2)))
          (last dynamics)))
|#


#| ;;; TMP
(defun simplify-dynamics-numbers (dynamics &key (flat T))
  "Transforming integers (MIDI velocities) or floats into symbols with cresc. and decres."
   (if (or flat 
          (not (some #'listp dynamics)))
    (let ((flat-dynamics (flatten dynamics))) 
      (span dynamics
            (append (list (first flat-dynamics))
                    (loop
                      for (d1 d2 d3) on flat-dynamics 
                      when (and d2 d3)
                      collect (cond ((or (and (cresc-p d1) (dim-p d3))
                                         (and (dim-p d1) (cresc-p d3)))
                                     d2)
                                    ((or (and (cresc-p d1) (cresc-p d3))
                                         (and (not (member d2 '(< >)))
                                              (not (member d3 '(< >)))
                                              (not (member d2 *one-note-dynamic-symbol*))
                                              (not (member d3 *one-note-dynamic-symbol*))
                                              (< (get-velocity d2) (get-velocity d3))))
                                     '<)
                                    ((or (and (dim-p d1) (dim-p d3))
                                         (and (not (member d2 '(< >)))
                                              (not (member d3 '(< >)))
                                              (not (member d2 *one-note-dynamic-symbol*))
                                              (not (member d3 *one-note-dynamic-symbol*))
                                              (> (get-velocity d2) (get-velocity d3))))
                                     '>)
                                    (t d2)))
                    (last flat-dynamics))))
    (mapcar #'simplify-dynamics-numbers dynamics))
  )

|#

(defun _simplify-dynamics (dynamics &key (flat T))
  (if (or flat 
          (not (some #'listp dynamics)))
    (let ((flat-dynamics (flatten dynamics))) 
      (span dynamics
            (append (list (get-velocity (first flat-dynamics) :type :symbol))
                    (loop
                      for (d1 d2 d3) on flat-dynamics 
                      when (and d2 d3)
                      collect (cond 
                               ;;; numbers
                               ((numberp d2)
                                (cond ((< d1 d2 d3) '<)
                                      ((> d1 d2 d3) '>)
                                      (T (get-velocity d2 :type :symbol))))
                               ;;; symbols
                               ((or (and (cresc-p d1) (dim-p d3))
                                    (and (dim-p d1) (cresc-p d3)))
                                d2)
                               ((or (and (cresc-p d1) (cresc-p d3))
                                    (and (not (member d2 '(< >)))
                                         (not (member d3 '(< >)))
                                         (not (member d2 *one-note-dynamic-symbol*))
                                         (not (member d3 *one-note-dynamic-symbol*))
                                         (< (get-velocity d2) (get-velocity d3))))
                                '<)
                               ((or (and (dim-p d1) (dim-p d3))
                                    (and (not (member d2 '(< >)))
                                         (not (member d3 '(< >)))
                                         (not (member d2 *one-note-dynamic-symbol*))
                                         (not (member d3 *one-note-dynamic-symbol*))
                                         (> (get-velocity d2) (get-velocity d3))))
                                '>)
                               (t d2)))
                    (get-velocity (last flat-dynamics) :type :symbol)
                    )))
    (mapcar #'_simplify-dynamics dynamics)))


(defun simplify-dynamics (notation &key (flat T))
  "Removes intermediate textual dynamic indicators from longer hairpins (e.g., generated by velocity-to-dynamic or gen-dynamic).
  
* Arguments:
  - notation: list of OMN dynamics values in multiple formats (can be nested)
  - flat (default T): whether or not to simplify dynamics across sublists.
  

* Examples:

  ;;; (simplify-dynamics '(pppp< < ppp< pp< < p< < mp< mf< < f< < ff> > mf> mp> p> ppp> pppp))
  ;;; => (pppp< < < < < < < < < < < < ff> > > > > > pppp)

  ;;; (simplify-dynamics '((pppp< < ppp< pp< < p< <) (mp< mf< < f< < ff> > mf> mp> p> ppp> pppp)))
  ;;; => ((pppp< < < < < < <) (< < < < < ff> > > > > > pppp))

  ;;; (simplify-dynamics '((pppp< < ppp< pp< < p< <) (mp< mf< < f< < ff> > mf> mp> p> ppp> pppp))
  ;;;                     :flat nil)
  ;;; => ((pppp< < < < < < <) (mp< < < < < ff> > > > > > pppp))

  using omn input
  ;;; (simplify-dynamics '((e c4 p< d4 < e4 mf< f4 < g4 f)))
  ;;; => ((e c4 p< d4 < e4 < f4 < g4 f))

  processing MIDI velocity integers
  ;;; (simplify-dynamics '((10 20 30 40) (50 60 50 30)))
  ;;; => ((pppp < < <) (< mp > ppp))

  processing floats
  ;;; (simplify-dynamics '((0.1 0.2 0.3 0.4) (0.5 0.6 0.5 0.3)))
  ;;; => ((pppp < < <) (< f > pp))


* BUGS: 

This is not correct -- various problems.
  ;;; (simplify-dynamics
  ;;;  '((h c4 f> pp<) (q c4 f> mf> p> pp<) (h c4 f> pp<) (q c4 f> p> pp c4) (h c4 f> pp<) (q c4 f> mf> p> pp<)))
  "
  (edit-omn :velocity notation
            #'(lambda (vs) (_simplify-dynamics vs :flat flat))
            :flat flat))


#|
(defvar *cresc-dynamic-symbol*
  '(< cresc cresc-molto cresc-poco cresc-poco-poco
      ppppp< pppp< ppp< pp< p< mp< mf< f< ff< fff< ffff< fffff<))

(defvar *dim-dynamic-symbol*
  '(> dim dim-molto dim-poco dim-poco-poco
      fffff> ffff> fff> ff> f> mf> mp> p> pp> ppp> pppp> ppppp>))


(defun cresc-p (x)
  (memq x *cresc-dynamic-symbol*))

(defun dim-p (x)
  (memq x *dim-dynamic-symbol*))

(cresc-p 'p<)
(cresc-p 'f)
(dim-p 'f>)
|#

#|
;;; Function by Janusz Podrazik, seemingly not quite finished yet, at least it does not do what it is documented to do

;;; https://opusmodus.com/forums/topic/771-velocity-to-dynamic-limitation/#comment-2237
(defun velocity-intermediate (velocity &key end (flatten t) section exclude)
  "Removes intermediate textual dynamic indicators from longer hairpins (e.g., generated by velocity-to-dynamic or gen-dynamic).
  
* Examples:
  (velocity-intermediate '(pppp< < ppp< pp< < p< < mp< mf< < f< < ff> > mf> mp> p> ppp> pppp))
  => (pppp< < < < < < < < < < < < ff> > > > > > pppp)
  "
  (do-verbose ("velocity-intermediate")
    (labels ((velocity-intermediate-l (velocity &key end (flatten t))
               (let ((dyn (velocity-to-dynamic velocity :end end :flatten flatten)))
                 (append
                  (list (first dyn))
                  (loop
                    for (v1 v2) on dyn 
                    when (and v1 v2)
                    collect (cond ((and (cresc-p v1) (cresc-p v2)) '<)
                                  ((and (dim-p v1) (dim-p v2)) '>)
                                  (t v2))))))
             
             (%velocity-intermediate (velocity &key end (flatten t))
               (if (listsp velocity)
                 (loop for v in velocity
                   collect (velocity-intermediate-l v :end end :flatten flatten))
                 (velocity-intermediate-l velocity :end end :flatten flatten)))
             
             (velocity-intermediate* (velocity &key end (flatten t))
               (if (and (listsp velocity) flatten)
                 (gen-divide
                  (get-count velocity)
                  (%velocity-intermediate (flatten velocity) :end end :flatten flatten))
                 (%velocity-intermediate velocity :end end :flatten flatten))))
      
      (disassembling-omn ((velocity plist) velocity :velocity)
        (let ((velocity (remove-nils velocity)))
          (maybe-section
           (lambda (x) (velocity-intermediate* x :end end :flatten flatten))
           velocity section exclude))))))
|#

#|
(velocity-intermediate '(p pppp f p pp ppp mp ff))
=> (p> pppp< f> > > ppp< < ff)

(velocity-intermediate '((e c4 f cs5 f d4 f ds5 p f4 p fs5 p c5 p pp)
                         (e cs4 p f d4 f eb5 p f4 p eb4 f d3 p ppppp)))
=> ((e c4 f> cs5 > d4 > ds5 > f4 > fs5 > c5 > pp<)
    (e cs4 < f> d4 > eb5 p< f4 < eb4 f> d3 > ppppp))

(velocity-intermediate '(p pp ppp pppp f ff fff p pp ppp f ff
                         fff ffff f p pp ppp mp ff))
=> (p> > > pppp< < < fff> > > ppp< < < < ffff> > > > ppp< < ff)

(velocity-intermediate '((p pp ppp pppp f ff fff p pp ppp f ff)
                         (fff ffff f p pp ppp mp ff)))
=> ((p> > > pppp< < < fff> > > > pppp< ff) (p> pppp< f> > > ppp< < ff))

(velocity-intermediate '(ppppp< pppp< ppp< < < < mf> > > p f< ff< fff< ffff< fffff))
=> (ppppp< < < < < < mf> > > p< < < < < fffff)
BUG: returns (ppppp pppp ppp < < < mf > > p f ff fff ffff fffff)

(simplify-dynamics '(ppppp< pppp< ppp< < < < mf> > > p f< ff< fff< ffff< fffff))
=> (ppppp< < < < < < mf> > > p < < < < fffff)
|#



(defun velocity-apply (fun velocities &key (type :float))
  "Applies a numeric function `fun' to a list of symbolic velocity values `velocities', which are translated into floats in the background. 

* Examples:
  (velocity-apply #'min '(p mf pp ff) :type :symbol)
  =>pp"
  (first (get-velocity (list (apply fun (get-velocity velocities)))
		       :type type)))

(defun min-velocity (velocities &key (type :float))
  (velocity-apply #'min velocities :type type))


(defun max-velocity (velocities &key (type :float))
  (velocity-apply #'max velocities :type type))

#|
(min-velocity '(p mf pp ff))

(min-velocity '(p mf pp ff) :type :symbol)

(max-velocity '(p mf pp ff))
|#

(defun velocity-transform (fun args &key (simplify T))
  "Higher-order function for transforming velocities by processing them as an Openmodus vector in the background.
  
* Arguments:
  - fun: a function expecting a vector and possibly more arguments.
  - args: the arguments for `fun'. Velocities should be explicitly transformed into numeric values. 
    Example args value: (get-velocity '(mf> > > > > pp)) 
  - simplify: whether or not to simplify cresc. and dim. in the result with simplify-dynamics.

* Examples:
  ;;; (velocity-transform #'vector-add (list (get-velocity '(mf> > > > > pp)) '(0.1)))"
  (let* ((vel-vector (apply fun args))
         (result (velocity-to-dynamic 
                  (vector-to-velocity (apply #'min vel-vector) (apply #'max vel-vector)
                                      vel-vector))))
    (if simplify
      (simplify-dynamics result)
      result)))


(defun _velocity-add (offset velocities &key (simplify T))
  (velocity-transform #'vector-add 
                      (list (get-velocity velocities)
                            (if (listp offset)
                              (mapcar #'get-velocity offset)
                              (list (get-velocity offset))))
                      :simplify simplify))
; (_velocity-add 0.1 '(mf> > > > > pp))
; => (f> > > > > p)

;;; BUG: not quite working yet
(defun velocity-add (offset velocities &key (simplify T))
  "Adds an offset to a list of OMN velocities. Quasi the dynamics equivalent of pitch transposition.

* Arguments:
  - offset: an offset added to `velocities', can be numeric (between 0 and 1) or a velocity symbol (e.g., pp), and also a list of either. 
  - velocities: list of velocities, can be nested.
  - simplify: whether or not to simplify cresc. and dim. in the result with simplify-dynamics.

* Examples:
  ;;; (velocity-add 0.1 '(mf> > > > > pp))
  ;;; => (f> > > > > p)

  ;;; (velocity-add 'pppp '(mf> > > > > pp))
  ;;; => (f> > > > > p)

  ;;; (velocity-add 0.1 '((mf> > >) (> > pp)))
  ;;; => ((f> > >) (> > p))

  ;;; (velocity-add '(0.1 0.2 0.3 0.4 0.5 0.6) '(mf> > > > > pp))
  ;;; => (f< < < < < ffff)

  omn input
  ;;; (velocity-add 0.15 '((e c4 p< d4 < e4 mf< f4 < g4 f)))
  ;;; => ((e c4 mf< d4 < e4 < f4 < g4 fff))
  "
  (edit-omn :velocity velocities
            #'(lambda (vs) (_velocity-add offset vs :simplify simplify))))


(defun _velocity-smooth (alfa velocities &key (simplify T))
  (velocity-transform #'vector-smooth 
                      (list alfa (get-velocity velocities))
                      :simplify simplify))

(defun velocity-smooth (alfa velocities &key (simplify T))
  "Smoothes velocity values.
  
* Arguments:
  - alfa: parameter controlling the degree of exponential smoothing (usually  0 < alpha < 1).
  - velocities: list of velocities, can be nested.
  - simplify: whether or not to simplify cresc. and dim. in the result with simplify-dynamics.
  

* Examples:
  ;;; (velocity-smooth 0.7 '((ppp p mf ff p< < <) (fff> > f><p ff mf p ppp)))
  ;;; => ((ppp< < < mp< < mp< <) (f< ff> > < ff> > mp))

  ;;; (velocity-smooth 0.2 '((ppp p mf ff p< < <) (fff> > f><p ff mf p ppp)))
  ;;; => ((ppp< < < < < < <) (< mf mf mf mf mf mf))
  "
  (edit-omn :velocity velocities
            #'(lambda (vs) (_velocity-smooth alfa vs :simplify simplify))))


