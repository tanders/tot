;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem tot
  :description "Torsten's Opusmodus Tools (TOT) is a collection of definitions that extend the algorithmic composition system Opusmodus (http://opusmodus.com/). Note that these tools have been developed for personal use for specific projects, and therefore their generality (or even applicabiliy) for other projects might be limited. Nevertheless, at least I tried to document their purpose and limitations." 
  :author "Torsten Anders"
  :version "0.1"
  :serial t ;; the dependencies are linear.
  :components ((:file "sources/package")
	       ;; (:file "sources/macros")
	       (:file "sources/utils")
	       (:file "sources/OMN-utils")
	       (:file "sources/rhythm")
	       (:file "sources/pitch")
	       (:file "sources/velocity")
	       (:file "sources/articulations")	       
	       (:file "sources/PWGL")
	       (:file "sources/form")
	       (:file "sources/orchestration")
	       ;; (:file "sources/export")
	       )
  ;;; !! NOTE: This code additionally depends on the commercial system Opusmodus
  :depends-on ("string-tools"
	       "ta-utilities"
	       "fenv"
	       ))
