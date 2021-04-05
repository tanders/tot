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
  :serial t ;; the dependencies are linear.
  :components (; (:file "sources/package")
	       (:file "sources/macros")
	       ;; (:file "sources/slippery-chicken") ;; independent
	       (:file "sources/utils")
	       (:file "sources/OMN-utils")
	       (:file "sources/score")	       
	       (:file "sources/rhythm")       
	       (:file "sources/karnatic-rhythm")
	       (:file "sources/pitch")
	       (:file "sources/tuning")
	       (:file "sources/velocity")
	       (:file "sources/articulations")	       
	       (:file "sources/PWGL")
	       (:file "sources/constraints")
	       (:file "sources/form")
	       (:file "sources/orchestration")
	       ;; (:file "sources/export")
	       )
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
	       "cluster-rules"
	       ))


