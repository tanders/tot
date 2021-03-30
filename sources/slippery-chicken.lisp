;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;
;;; This file contains definitions from the composition system Slippery Chicken by Michael Edwards
;;;

(defpackage :slippery-chicken
  (:documentation "This package contains selected definitions from the composition system Slippery Chicken by Michael Edwards.")
  (:nicknames :sc)
  (:use :common-lisp)
  (:export fibonacci-transition))

(in-package :sc)

;;;
;;; Implementation of Fibonacci Transition by Michael Edwards copied from
;;; http://www.moz.ac.at/sem/lehre/lib/mat/text/notes9.html
;;; This (or a variation therefore) is part of Slippery Chicken, hence the package name.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Aux defs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci (max-sum)
  "Return the fibonacci numbers in a list ending at 0 that add up to a maximum less than <max-sum>.  Returns the fibonacci number < max-sum as a second value."
  (loop 
     ;; our result will be in descending order
     with result = '(1 0) 
     ;; the running total of sums
     with cumulative-sum = 1
     for x = 0 
     for y = 0 
     ;; the sum of our two most recent numbers.
     for sum = 0 
     do
     (setf x (first result)
           y (second result)
           sum (+ x y))
     (incf cumulative-sum sum)
     (when (> cumulative-sum max-sum)
       ;; we're not using sum this time as we're over our limit.
       ;; return can be used in loops to exit immediately
       (return (values result (1+ (- cumulative-sum sum)))))
     (push sum result)))


;;; Same as fibonacci but eliminates the final 0 and 1s; can also reach max-sum
;;; rather than having to be < it.
;;; (fibonacci 20) -> (8 5 3 2 1 1 0) 20
;;; (_fibonacci-start-at-2 20) -> (8 5 3 2) 18
(defun _fibonacci-start-at-2 (max-sum)
  "Aux def"
  (multiple-value-bind
      (series sum)
      (fibonacci (+ 2 max-sum)) ; + 2 so we can hit max-sum if need be
    ;; subseq returns a sequence out of our list
    (values (subseq series 0 (- (length series) 3))
            (- sum 2))))


;;; Once we have the numbers e.g. (8 5 3 2 1) we convert into indices e.g. 
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1)
;;;                8         5     3   2 1
(defun _fibonacci-transition-aux2 (list item1 item2)
  "Aux def"
  (let ((result '()))
    (loop for num in list do 
       ;; so each time we have 'num' items, all but one of which are item1
         (loop repeat (1- num) do 
              (push item1 result))
         (push item2 result))
    ;; we've used push so we need to reverse the list before returning
    (nreverse result)))


;;; Say you want a transition between two repeating states over a period of x
;;; repetitions; this gives you a gradual break in of the second state using
;;; fibinacci relationships.
;;; <item1> is the start item, <item2> the item we want to transition towards
;;; e.g. (fibonacci-transition-aux1 21 0 1) ->
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1)   
(defun _fibonacci-transition-aux1 (num-items &optional
                                  (item1 0)
                                  (item2 1))
  "Aux def"
  ;; local function: usually done with flet but you can't call flet functions
  ;; recursively...
  (labels ((ftar (num) 
             ;; lisp functions can return more than one value (e.g. (floor
             ;; 3.24) usually you will only want the first value (as in the
             ;; case of floor) but we can get them all using
             ;; multiple-value-bind and friends.
             (multiple-value-bind
                   (series sum)
                 ;; returns a list of descending fib numbers and their sum--this
                 ;; will be < num-items
                 (_fibonacci-start-at-2 num)
               (let ((remainder (- num sum)))
                 (if (> remainder 2)
                     ;; recursive call: what we're looking for is a descending
                     ;; list of fib numbers that total <num-items> exactly,
                     ;; hence we have to keep doing this until we've got
                     ;; num-items
                     (append series (ftar remainder))
                     ;; we're done so just store the remainder and return
                     (progn
                       (when (> remainder 0) 
                         (push remainder series))
                       series))))))
    ;; we might have something like (2 5 3 2 8 5 3 2) so make sure we sort them
    ;; in descending order.  Note that our sort algorithm takes a function as
    ;; argument.
    (_fibonacci-transition-aux2 
     (stable-sort (ftar num-items) #'>)
     item1 item2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top-level def
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fibonacci-transition (num-items &optional
                                       (item1 0)
                                       (item2 1))
  "Generates a list containing only instances of item1 and and item2, where initially item1 dominates, but then item1 gradually decreases and item2 increases, until item2 completely dominates. The transition follows Fibonacci numbers.

* Examples:
  ;;; (fibonacci-transition 35 0 1)
  ;;; => (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 0 1 0 1 1 0 1 1 1 1 1)

  Function defined by Michael Edwards for his algorithmic composition system Slippery Chicken, see 
  - Edwards, M. (2011) Algorithmic composition: computational thinking in music. Communications of the ACM. 54 (7), 58-67.
  - Edwards, M. (2012) An Introduction to Slippery Chicken, ICMC 2012.

  Slippery Chicken also defines a variant of this function for more than two items."
  ;; just some sanity checks
  (unless item1
    (setf item1 0))
  (unless item2
    (setf item2 1))
  ;; we use the aux1 function to first move towards more of item2, but then
  ;; again for less of item1.  The point at which this shift occurs is at the
  ;; golden section (where else?).
  (let* ((left-num (round (* num-items .618)))
         (right-num (- num-items left-num))
         ;; get the two transitions.
         (left (_fibonacci-transition-aux1 left-num item1 item2))
         ;; this one will be reversed
         (right (_fibonacci-transition-aux1 right-num item2 item1)))
    ;; avoid two item1s at the crossover. we use equal as it can handle number
    ;; and symbol comparison
    (when (equal (first (last right))
                 item1)
      ;; butlast returns it's argument minus the last element
      ;; e.g. (butlast '(1 2 3 4)) -> (1 2 3)
      (setf right (butlast right))
      (push item2 right))
    ;; append the two lists and return.  we can use nreverse (which is more
    ;; efficient) rather than reverse as we won't need the original version of
    ;; result
    (append left (nreverse right))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Import defs into Opusmodus package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| ;; Don't import any further packages into the already very busy OM package
(in-package :om)

;; not sure why this is necessary
(unintern 'fibonacci-transition)

(use-package :sc :om)
|#


#|
(sc:fibonacci-transition 35 0 1)

(om::fibonacci-transition 35 0 1)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF 
