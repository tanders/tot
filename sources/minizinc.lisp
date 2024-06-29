;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; ! TODO Consider moving this file into a separate new project with a separate package.

;; Opusmodus package
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definitions that depend on Minizinc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG TMP: Use the full path to the MiniZinc executable
(defparameter *minizinc-exec* "/Applications/MiniZincIDE.app/Contents/Resources/minizinc")

; ;; Use Minizinc executable in the PATH
; (defparameter *minizinc-exec* "minizinc")

;; TODO Get this working for a portable way to find the MiniZinc executable
(defun load-path-from-profile ()
  (with-open-file (stream "~/.profile")
    (loop for line = (read-line stream nil)
          while line
          when (and (> (length line) 11)
                    (string= "export PATH=" line :start2 0 :end2 11))
            do (let* ((path (subseq line 12))
                      (current-path (uiop:getenv "PATH"))
                      (new-path (if current-path
                                    (concatenate 'string path ":" current-path)
                                    path)))
                 (setf (uiop:getenv "PATH") new-path)
                 (return new-path)))))

; (load-path-from-profile)

; ;; Call the function and store the result
; (defparameter *updated-path* (load-path-from-profile))


(defun check-minizinc-exec ()
  "Ensure that the MiniZinc executable is in the current PATH or report a warning if not."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program `("which" ,*minizinc-exec*) :output :string :error-output :string :ignore-error-status t)
    (if (zerop exit-code)
        (setf *minizinc-exec* (string-trim '(#\Newline #\Return) output))
        (warn "MiniZinc executable not found in PATH. Please ensure it is installed and available. You can obtain it from here: https://www.minizinc.org"))))


(defun call-minizinc (model-file data-file)
  "Run MiniZinc executable on given model and data files. Return the solution it prints as a string, or raise an error if it fails."
  (check-minizinc-exec)
  (uiop:run-program (format nil "~a ~a ~a" *minizinc-exec* model-file data-file)
                    :output :string
                    :error-output :string))

;; TODO Somehow generalise
(defun minizinc-solve-int-list (model data solution-var-name)
  "Solve a Minizinc CSP over a list of integers and return result parsed as Lisp value.

* Arguments:
 - model (str): The CSP definition in Minizinc syntax.
 - data (str): The CSP input data in Minizinc datafile syntax.
 - solution-var-name (str): The variable to solve in the model.
"
  (uiop:with-temporary-file (:stream model-file :pathname model-path :type "mzn")
    (write-string model model-file)
    (finish-output model-file)
    (uiop:with-temporary-file (:stream data-file :pathname data-path :type "dzn")
      (write-string data data-file)
      (finish-output data-file)
      (let ((output (call-minizinc (namestring model-path) (namestring data-path))))
	(ppcre:register-groups-bind (solution)
				    ((format nil "~a = \\[(\\d+(?:, \\d+)*)\\]" solution-var-name)
				     output)
				    (mapcar #'parse-integer (ppcre:split ", " solution)))))))

;; TODO
;; - ! Distances between ints in result xs should not be too uneven. Example: for (split-int-CSP 10 3) it should return (5 3 2) and not (5 4 1).
;; - ? Explicitly handle no solution: make sure Minizinc returns a certain value in that case. Seemingly uiop:run-program does return nil in that case?
(defun split-int-CSP (n parts &key (order #'>))
  "Return a list of PARTS integers. All integers in the result are different, and their sum is N.

This function uses Minizinc in the background.

? Return nil if there is no solution.

* Arguments:
 - N (int): input integer to 'split'.
 - PARTS (int): number of integers to return. 
 - ORDER (numerical comparison function): in which order to return the result.

* Examples:

;;; (split-int-CSP 12 3)
;;; => (5 4 3)  

;;; (split-int-CSP 9 3)
;;; => (4 3 2)

;;; (split-int-CSP 12 3 :order #'<)
;;; => (3 4 5)

No solution
;;; (split-int-CSP 2 3)
;;; => nil
"
  (let* ((model "
include \"alldifferent.mzn\"; 
int: n;
int: parts;
array[1..parts] of var 1..n: xs;
constraint alldifferent(xs);
constraint sum(xs) = n;
solve ::int_search(xs, input_order, indomain_median) satisfy;")
	 (data (format nil "n=~d; parts=~d;" n parts))
	 (sol-val "xs")
	 (result (minizinc-solve-int-list model data sol-val)))
    (sort result order)))

#|
(split-int-CSP 11 3)
(split-int-CSP 10 3)
(split-int-CSP 9 3)
|#

