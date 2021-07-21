;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package :om)

#|
;; Packages OM and FiveAM have name conflicts for external OM symbols GEN-INTEGER and RUN and internal symbols TEST, PASS and SKIP (the latter have no value nor function value)
;;
;; Import the defs of FiveAM.
(shadowing-import '(5AM:GEN-INTEGER 5AM:RUN 5AM:TEST 5AM:PASS 5AM:SKIP) :OM)
(use-package '(:FiveAM) :OM)
 
;; (describe 'om::test)


;; BUG: This is not working, because some tests depend on tot functions that call the Opusmodus function gen-integer.
;; Two possible solutions:
;; - Use OM package directly for TOT, and do not import FiveAM. Always define tests with 5am:test etc.
;; - Define a proper TOT package that imports OM and exports all its relevant symbols, incl. all the automatically created microtonal accidentals.
;; The first option for now is simpler, which is whatg I am currently doing...
;;

|#

#|
;; NOTE: The symbols of my library TOT are not exported, and therefore these symbols would not be used/imported by the tot/test package created here.
;; TMP solution: for now define tests in OM package. 
;; TODO: define a custom package for both tot and tot/tests, and export all relevant symbols from tot, incl. all the automatically created microtonal accidentals.
(defpackage #:tot/tests
  (:use #:cl
        #:FiveAM
        #:om)
  ;; Packages OM and FiveAM have name conflicts for symbols GEN-INTEGER and RUN. Import the defs of FiveAM.
  (:shadowing-import-from #:FiveAM #:GEN-INTEGER #:RUN)
  #|
  (:export #:run!
	   #:all-tests
	   )
  |#
  )
|#




#|
(defpackage #:tot/tests
  (:use #:cl
        #:om
        #:rove))
|#

