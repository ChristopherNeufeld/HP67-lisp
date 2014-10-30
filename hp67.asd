(defpackage hp67
  (:use :common-lisp :asdf))

(in-package :hp67)

(defsystem "hp67"
  :description "hp67:  a programmable RPN calculator emulator."
  :version "0.1"
  :author "Christopher Neufeld"
  :licence "GPL v3"
  :components ((:file "stack")
               (:file "modes")
               (:file "key-structs"
                      :depends-on ("stack" "modes"))
               (:file "calc1"
                      :depends-on ("key-structs"))))
           
