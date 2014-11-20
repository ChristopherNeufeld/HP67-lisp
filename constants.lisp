;; Constants used in the program

(defpackage :HP67-INTERNALS
  (:use :COMMON-LISP)
  )

(in-package :HP67-INTERNALS)


(defparameter *digits-in-display* 10)
(defparameter *overflow-exponent* 99)  ;; larger than this, and
                                       ;; there's an error



