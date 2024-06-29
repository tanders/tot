;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf-user)

#-opusmodus
(error "This library depends on the commercial composition system Opusmodus, http://opusmodus.com.")

(asdf:defsystem tot
  :description "Torsten's Opusmodus Tools (TOT) is a collection of definitions that extend the algorithmic composition system Opusmodus (http://opusmodus.com/). Note that these tools have been developed for personal use for specific projects, and therefore their generality (or even applicabiliy) for other projects might be limited. Nevertheless, at least I tried to document their purpose and limitations." 
  :author "Torsten Anders <torstenanders@gmx.de>"
  :licence "GNU General Public License, Version 3"
  :source-control "https://github.com/tanders/tot"
  :version "0.3"
  :components  ((:module "sources"
			 :serial t
			 :components (; (:file "package")
				      (:file "macros")
				      ;; (:file "slippery-chicken") ;; independent
				      (:file "utils")
					  (:file "minizinc")
				      (:file "OMN-utils")
				      (:file "score")	       
				      (:file "rhythm")       
				      (:file "karnatic-rhythm")
				      (:file "pitch")
				      (:file "tuning")
				      (:file "velocity")
				      (:file "articulations")	       
				      (:file "PWGL")
				      (:file "constraints")
				      (:file "form")
				      (:file "orchestration")
				      ;; (:file "sources/export")
				      )))
  ;; :if-feature :opusmodus
  ;;; !! NOTE: This code additionally depends on the commercial system Opusmodus
  :depends-on (;; https://common-lisp.net/project/alexandria/
	       ;; Alexandria should already be loaded with Opusmodus
	       "alexandria"
	       ;; https://common-lisp.net/project/cl-utilities/
	       "cl-utilities"
	       ;; Libraries by Torsten Anders, see https://github.com/tanders?tab=repositories
	       "string-tools"
	       "ta-utilities"
	       "fenv"
	       "cluster-rules")
  :in-order-to ((test-op (test-op #:tot/tests))))

(defsystem #:tot/tests
  :depends-on (:FiveAM :tot) 
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "setup-tests")
				     (:file "karnatic-rhythm")
				     (:file "articulations")
				     (:file "OMN-utils")
				     )))
  :perform (test-op (o s)
		    ;; Find and run top-level test suite
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* '#:tot :om)
				      ;; (uiop:find-symbol* '#:tot :tot/tests)
				      )))

;;
;; Opusmodus setup (this cannot be set in ~/Opusmodus/Extensions for some reason.
;;

;; For details see https://opusmodus.com/forums/topic/1391-disabling-do-verbose/
(defparameter om::*do-verbose* nil
  "Enable or disable traces printed by do-verbose.")


