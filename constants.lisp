;; Constants used in the program

(defpackage :HP67-INTERNALS
  (:use :COMMON-LISP)
  (:export :STACK-REGISTERS :STACK-MEMORY       ;; stack.lisp
           :STACK-PROGRAM-MEMORY :STACK-ERROR-STATE

           ;; engine.lisp
           :HANDLE-ONE-KEYPRESS :GET-NEW-STACK-OBJECT
           :GET-NEW-MODE-OBJECT

           ;; key-structs.lisp
           :KEY-STRUCT-ABBREV
           :KEY-STRUCT-KEY-LOCATION
           :LOCATION-CATEGORY-1 :LOCATION-CATEGORY-2
           :LOCATION-COL :LOCATION-ROW :LOCATION-NARROW-KEY
           :GET-KEY-STRUCTS

           ;; display.lisp
           :FORMAT-FOR-PRINTING
           ))

(in-package :HP67-INTERNALS)


(defparameter *digits-in-display* 10)
(defparameter *overflow-exponent* 99)  ;; larger than this, and
                                       ;; there's an error

(defparameter *debug-stack* '())  ;; use this to push things onto an
                                  ;; examination stack for later
                                  ;; perusal (ncurses really messes up
                                  ;; the screen for bread-crumb
                                  ;; debugging or backtraces)


