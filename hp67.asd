(defpackage hp67
  (:use :common-lisp :asdf))

(in-package :hp67)

(defsystem "hp67"
  :description "hp67:  a programmable RPN calculator emulator."
  :version "0.1"
  :author "Christopher Neufeld"
  :licence "GPL v3"
  :components ((:file "stack" :depends-on ("constants"))
               (:file "constants")
               (:file "modes")
               (:file "display" :depends-on ("modes" "constants"))
               (:file "key-structs"
                      :depends-on ("stack" "modes" "display"))
               (:file "calc1"
                      :depends-on ("key-structs"))
               (:file "ui")
               (:file "engine"
                      :depends-on ("stack" "modes" "key-structs"
                                           "calc1" "ui"))))
                                           
           
