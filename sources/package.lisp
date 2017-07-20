;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; openmusic package
(in-package :om)

;; (require :fenv)
; (asdf:load-system :fenv)
;; no idea why these unintern calls are necessary -- possibly because these functions are called in the actual source code of this project?
(unintern 'v)
(unintern 'y)
(use-package :fenv)

#|
; (ql:quickload :fn)
(require :fn)
(unintern 'fn*)
(use-package :fn)
|#

;; (require :ta-utilities)

;; (require :string-tools)


#|
;;; NOTE: careful with shadowing-import 
(shadowing-import ; import all external symbols of :fenv
 (loop for s being the external-symbols of (find-package :fenv)
      collect s))
|#

