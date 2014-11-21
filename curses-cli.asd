(defpackage curses-cli
  (:use :common-lisp :asdf))

(in-package :curses-cli)

(defsystem "curses-cli"
  :description "The NCurses CLI to HP-67."
  :version "0.1"
  :author "Christopher Neufeld"
  :licence "GPL v3"
  :depends-on ("hp67" "cl-charms")
  :components ((:file "curses-cli")))
