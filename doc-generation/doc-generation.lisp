;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cldoc -- documentation using 
;;; works well :)  Minor doc string formatting supported. Perhaps only downside: internal and exported symbols are not distinguished

(require :cldoc)

(cldoc:extract-documentation 'cludg:html "/Users/torsten/common-lisp/tot/doc/" 
    (asdf:find-system :tot)
    :table-of-contents-title 
    "Torsten's Opusmodus Tools (TOT)")

