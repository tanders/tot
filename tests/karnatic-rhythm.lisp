;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#|

;; Run different random tests each time
(let ((*random-state* (make-random-state T)))
  (run! 'karnatic-rhythm))

(progn
  (asdf:load-system :tot)
  (run! 'karnatic-rhythm))

(progn
  (asdf:load-system :tot)
  (run! 'anga))

|#


(in-package :om)
;; (in-package :tot/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:def-suite karnatic-rhythm :in tot)
(5am:in-suite karnatic-rhythm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Karnatic rhythmical techniques
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Simplistic tests, less intended for thoroughly testing the definitions, more as regression tests
;; in case things change in Opusmodus. For many functions (incl. this one) there is no dependency
;; to Opusmodus, though. These tests are secondary, just in case.
(5am:test anga
  (5am:is (equal '(1/4) (anga :a)))
  (5am:is (equal '(1/2) (anga :d)))
  (5am:is (equal '(5/4) (anga :l 5))))

(5am:test tala
  (5am:is (equal '((1/2) (3/4))
		 (tala '(:d :l))))
  (5am:is (equal '((1/2) (5/4))
		 (tala '(:d :l) 5)))
  (5am:is (equal '((1/4) (5/8))
		 (tala '(:d :l) 5 1/8))))


(5am:test tala-time-signatures
  ;; The given duration 5/2 lasts exactly the length of two cycles of the given tala.
  (5am:is (equal '((2 4 1) (3 4 1) (2 4 1) (3 4 1))
		 (tala-time-signatures '(10/4)
				       (tala '(:d :l) 3))))
  ;; If a duration is given that does not exactly fit into a repetition of the tala cycle, then the last returned time signature is correspondingly shorter.
  (5am:is (equal '((2 4 1) (3 4 1) (2 4 1) (2 4 1))
		 (tala-time-signatures '(9/4)
				       (tala '(:d :l) 3)))))

(5am:test complete-phrase-in-tala
  (5am:is (equal '((H C4 TIE) (W C4 TIE+TIE Q TIE) (H C4 TIE) (W C4 TIE Q))
		 (complete-phrase-in-tala '((7/2 c4)) :tala (tala '(:d :l) 5))))
  (5am:is (equal '((H C4 TIE) (W C4 TIE Q) (H C4))
		 (complete-phrase-in-tala '((7/4 c4)) :tala (tala '(:d :l) 5) :append-sam? T))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gen-karnatic-cell: central function, therefore must params are tested here with randomised tests
;;

(5am:test gen-karnatic-cell_overall-length
  (5am:for-all ((gati (5am:gen-one-element 3 4 5 7))
		(jathi (5am:gen-integer :min 3 :max 7))
		;; TODO: add guard excluding large positions depending on jathi
		(position (5am:gen-integer :min 0 :max 44))
		(accented? (5am:gen-one-element T NIL)))
    (5am:is (= jathi (* 4 gati (apply #'+ (gen-karnatic-cell gati jathi position :accented? accented?))))
	    "The found cell has the wrong overall length.")))

;; TODO: Consider: define a macro similar to for-all that allows for tests like this, where
;; there could be arbitrary and possibly nested conditions on generated arguments and the result
;; applied to decide whether to test, but still it is ensured that tests are run 5am:*num-trials*
;; times.
(5am:test gen-karnatic-cell_min/max-number
  (let ((i 0))
    (loop while (< i 5am:*num-trials*)
       for gati = (funcall (5am:gen-one-element 3 4 5 7))
       for jathi = (funcall (5am:gen-integer :min 3 :max 7))
       ;; Lower max position setting
       for position = (funcall (5am:gen-integer :min 0 :max 5))
       for accented? = (funcall (5am:gen-one-element T NIL))
       for max-number = (funcall (5am:gen-integer :min 1 :max 10))
       for min-number = (funcall (5am:gen-integer :min 1 :max 10))
       do (when (<= min-number max-number)
	    (let ((result (gen-karnatic-cell gati jathi position :accented? accented?
					     :min-number min-number :max-number max-number)))
	      (when result ;; ensure there is a result with these args
		(5am:is (<= min-number (length result))
			"Result too short with arg min-number for (gen-karnatic-cell ~A ~A ~A :accented? ~A :min-number ~A :max-number ~A)"
			gati jathi position accented? min-number max-number)
		(5am:is (>= max-number (length result))
			"Result too long with arg max-number for (gen-karnatic-cell ~A ~A ~A :accented? ~A :min-number ~A :max-number ~A)"
			gati jathi position accented? min-number max-number)
		(setf i (+ i 1))))))))

(5am:test gen-karnatic-cell_first-length
  (let ((i 0))
    (loop while (< i 5am:*num-trials*)
       for gati = (funcall (5am:gen-one-element 3 4 5 7))
       for jathi = (funcall (5am:gen-integer :min 3 :max 7))
       ;; Lower max position setting
       for position = (funcall (5am:gen-integer :min 0 :max 5))
       for accented? = (funcall (5am:gen-one-element T NIL))
       for first-length = (funcall (5am::gen-ratio :numerator (5am:gen-one-element 1 2 3 4 5 6)
						   :denominator (5am:gen-one-element 8 16)))
       do (let ((result (gen-karnatic-cell gati jathi position :accented? accented?
					   :first-length first-length)))
	    (when result ;; ensure there is a result with these args
	      (5am:is (= first-length (first result)))
	      (setf i (+ i 1)))))))

(5am:test gen-karnatic-cell_include/exclude-length
  (let ((i 0))
    (loop while (< i 5am:*num-trials*)
       for gati = (funcall (5am:gen-one-element 3 4 5 7))
       for jathi = (funcall (5am:gen-integer :min 3 :max 7))
       ;; Lower max position setting
       for position = (funcall (5am:gen-integer :min 0 :max 5))
       for accented? = (funcall (5am:gen-one-element T NIL))
       for include-length = (funcall (5am::gen-ratio :numerator (5am:gen-one-element 1 2 3 4 5 6)
						     :denominator (5am:gen-one-element 8 16)))
       for exclude-length = (funcall (5am::gen-ratio :numerator (5am:gen-one-element 1 2 3 4 5 6)
						      :denominator (5am:gen-one-element 8 16)))
       do (when (/= include-length exclude-length)
	    (let ((result (gen-karnatic-cell gati jathi position :accented? accented?
					     :include-length include-length :exclude-length exclude-length)))
	      (when result ;; ensure there is a result with these args
		(5am:is (member include-length result))
		(5am:is (not (member exclude-length result)))
		(setf i (+ i 1))))))))

