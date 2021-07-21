;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :5am)
;; (in-package :om)
;; (in-package :tot/tests)


#|
;; ASDF interface for running all tests
(asdf:test-system :tot)

;; Run different random tests each time
(let ((*random-state* (make-random-state T)))
  (run! 'tot))

(progn
  (asdf:load-system :tot)
  (run! 'tot))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 0) (space 0) (debug 3)))

;; Interactive debugging 
(setf *on-error* :DEBUG)
;; (setf *on-error* :BACKTRACE)
;; (setf *on-error* NIL)
(setf *on-failure* :debug)

;; NOTE: reduce number of trials for speeding up during test developments
;; (setf *num-trials* 10)
(setf *num-trials* 100)
;; (setf *num-trials* 1000)

;; When non-NIL tests are run as soon as they are defined.
;; (setf *run-test-when-defined* T)
(setf *run-test-when-defined* NIL)

;; T if we should print the expression failing, NIL otherwise.
(setf *verbose-failures* T)


;; Copied from cluster-engine-test-utils.lisp
(defun gen-selection (&key (length (gen-integer :min 0 :max 10))
			elements)
  "Return a generator that picks LENGTH values (int or generator) from ELEMENTS (list of ints or generator) without repeating them. Must be called less often than length of ELEMENTS."
  (lambda ()
    (let ((length* (if (functionp length)
		       (funcall length)
		       length))
	  (elements-copy (if (functionp elements)
			     (copy-list (funcall elements))
			     (copy-list elements))))
      (assert (<= length* (length elements-copy))
	      (length elements)
	      "Cannot pick ~A different elements from ~A." length* elements)
      (loop for i from length* downto 1
	 for pos = (random (length elements-copy))
	 collect (tu:pop-nth elements-copy pos)))))
#|
(setf my-gen (gen-selection :length (gen-integer :min 1 :max 3) :elements '((-1/2) (-1/4) (-1/8) (1/8) (1/4) (1/2))))
(funcall my-gen)
|#

;; Copied from cluster-engine-test-utils.lisp
(defun gen-ratio (&key
		    ;; (numerator (gen-integer :min -7 :max 7))
		    ;; no grace-notes for now
		    (numerator (gen-one-element -5 -4 -3 -2 -1 1 2 3 4 5 6))
		    (denominator (gen-one-element 1 2 4 8 16)))
  "Return a generator that produces a ratio. NUMERATOR and DENOMINATOR are both integer generators with default values suitable for standard Cluster Engine rhythmic values."
  (lambda ()
    (/ (funcall numerator) (funcall denominator))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

(5am:def-suite tot
    :description "Top-level test suite")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(in-package :5am)

;; BUG: Always returns the same value
(funcall (gen-one-element -5 -4 -3 -2 -1 1 2 3 4 5 6))
(funcall (gen-one-element 1 2 4 8 16))

(setf my-gen (gen-ratio))
(funcall my-gen)

;; Generate a rhythmic motif
(setf my-gen (gen-list :length (gen-integer :min 1 :max 5) :elements (gen-ratio)))
(funcall my-gen)

;; Generate a rhythmic domain
(setf my-gen (gen-list :length (gen-integer :min 1 :max 5) 
		       :elements (gen-list :length (gen-integer :min 1 :max 5) :elements (gen-ratio))))
(funcall my-gen)
|#


#|
(loop repeat 100
   collect (funcall (gen-ratio)))


(setf *random-state* (make-random-state T))
(loop repeat 10
   collect (random 1000))

(let ((*random-state* (make-random-state T)))
  (loop repeat 10
     collect (random 1000)))

(loop repeat 10
     collect (random 1000 (make-random-state)))

(loop repeat 10
     collect (random 1000 (make-random-state T)))


(funcall (gen-integer))
(funcall (gen-one-element 1 2 3 4 5))

;; BUG: Always the same value
(setf min 1 max 1000)
(+ min (random (1+ (- max min))))
|#



#|
(in-package :5am)

(test dummy
  (for-all ((xs (gen-list)))
    (let ((result (reverse xs)))
      (when result
	(is (equal xs (reverse result)))))))

(test dummy
  (for-all ((xs (gen-list)))
    (for-all ((result (lambda () (reverse xs))
		      ;; xs is not empty
		      xs))
      (is (equal xs (reverse result))))))


;; Error: Slot TEST-EXPR is unbound in #<FOR-ALL-TEST-NO-TESTS #x30200388375D>
(test dummy
  (for-all ((xs (gen-list)))
    (when xs
      (is (< 0 (length xs))))))

;; Slot TEST-EXPR is unbound in #<FOR-ALL-TEST-NEVER-RUN #x3020035D7E5D>
(test dummy
  (for-all ((l (gen-integer :min 0 :max 10)))
    (for-all ((xs (gen-list :length (lambda () l))
		  xs))
      (is (equal l (length xs))))))


;; Cases above made working by replacing for-all with a loop
(test dummy
  (loop repeat 100
     for xs = (funcall (gen-list))
     do (when xs
	  (is (< 0 (length xs))))))

(test dummy
  (loop repeat 100
     for l = (funcall (gen-integer :min 0 :max 10))
     for xs = (funcall (gen-list :length (lambda () l)))
     do (when xs
	  (is (< 0 (length xs))))))

;; Working test with dependency between generator values
(test dummy
  (for-all ((l (gen-integer :min 0 :max 10)))
    (for-all ((xs (gen-list :length (lambda () l))))
      (is (equal l (length xs))))))


;; Working
(test dummy
  (for-all ((xs (gen-list)))
    (let ((result (reverse xs)))
      (is (equal xs (reverse result))))))

(let ((*random-state* (make-random-state T)))
  (run!' dummy))
|#

