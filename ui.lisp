;; The user interface base class and generic function definitions.

(defpackage :HP67-UI
  (:use :COMMON-LISP)
  (:export
   ))

(in-package :HP67-UI)

(defclass ui-base ()
  ((active-keys		:reader get-active-keys
                        :initform nil)
   (active-shift	:reader get-active-shift
                        :initform nil)
   (active-mode		:reader get-active-mode
                        :initform nil)
   (display-mode	:reader get-display-mode
                        :initform nil)
   (display-digits	:reader get-display-digits
                        :initform nil)
   (error-text		:reader get-error-text
                        :initform "")
   (stack-real-contents	:reader get-stack-real-contents
                        :initform nil)
   (stack-imag-contents	:reader get-stack-imag-contents
                        :initform nil)
   (memory-contents	:reader get-memory-contents
                        :initform nil)
   (program-contents	:reader get-program-contents
                        :initform nil)
   (program-counter	:reader get-program-counter
                        :initform nil)))



(defgeneric ui-set-active-keys (ui active-key-list)
  (:documentation "Used to inform the UI of what keys can be
pressed given the current state of the calculator.  The engine
will attempt not to call this function unless the active keys
list has changed."))

(defgeneric ui-set-active-shift (ui active-shift)
  (:documentation "Used to inform the UI that a shift key, (F, G,
or H) has been pressed."))

(defgeneric ui-set-active-mode (ui active-mode)
  (:documentation "Used to inform the UI of any change in
mode (interactive, program, running).  The engine will attempt
not to call this function unless the active mode has changed."))

(defgeneric ui-set-display-mode (ui display-mode display-digits)
  (:documentation "Used to inform the UI what the display mode
is.  The engine will attempt not to call this function unless the
display mode has changed."))

(defgeneric ui-set-complex-mode (ui how)
  (:documentation "Used to inform the UI whether or not we want
to display complext numbers.  If 'how' is non-nil, complex
display is requested.  The engine will attempt not to call this
function unless the complex mode has changed."))

(defgeneric ui-set-error-text (ui error-text)
  (:documentation "Used to inform the UI when an error has
occured.  The engine will not call this function unless an error
has just been set, or just been cleared."))

(defgeneric ui-has-error-text (ui)
  (:documentation "Used by the UI to establish whether there is
an error message to display."))

(defgeneric ui-clear-stack-contents (ui max-depth)
  (:documentation "Erase the UI's knowledge of the stack
contents, and allocate space for up values with depth up to
max-depth.  The engine will call this when setting up for a
paint."))

(defgeneric ui-add-stack-value (ui stack-depth
                                real-part-string
                                &optional imag-part-string)
  (:documentation "Inform the UI of the contents of the stack.  X
is at depth=0.  The engine will call this at least once when
setting up for a paint."))

(defgeneric ui-clear-memory-contents (ui)
  (:documentation "Erase the UI's knowledge of the memory
contents.  The engine will call this when setting up for a
paint."))

(defgeneric ui-add-memory-value (ui precedence label
                                 real-part-string
                                 &optional imag-part-string)
  (:documentation "Inform the UI of the contents of memory.  When
not enough space is available to display all memory contents,
those with higher precedence should be displayed ahead of those
with lower.  The engine may call this one or more times when
setting up for a paint."))

(defgeneric ui-clear-program-contents (ui)
  (:documentation "Erase the UI's knowledge of program memory.
The engine will normally not call this unless program steps have
been deleted."))

(defgeneric ui-add-program-step (ui step-num display-string)
  (:documentation "Inform the UI of the contents of a single step
of memory.  Step-num will be unique.  The engine will call
whenever new program steps are present."))

(defgeneric ui-get-program-step-string (ui step-num)
  (:documentation "Used by the UI to retrieve a particular step
number from the program memory."))

(defgeneric ui-set-program-counter (ui)
  (:documentation "Inform the UI of what program step would next
be executed if program execution were to begin.  The engine will
attempt to call this only when it has changed."))

(defgeneric ui-paint (ui)
  (:documentation "Ask the UI to repaint itself based on its new
settings."))



(defmethod ui-set-active-keys ((ui ui-base) active-key-list)
  (setf (slot-value ui 'active-keys) active-key-list))

(defmethod ui-set-active-shift ((ui ui-base) active-shift)
  (setf (slot-value ui 'active-shift) active-shift))

(defmethod ui-set-active-mode ((ui ui-base) active-mode)
  (setf (slot-value ui 'active-mode) active-mode))

(defmethod ui-set-display-mode ((ui ui-base) display-mode display-digits)
  (setf (slot-value ui 'display-mode) display-mode
        (slot-value ui 'display-digits) display-digits))

(defmethod ui-set-error-text ((ui ui-base) error-text)
  (unless error-text
    (setf error-text ""))
  (setf (slot-value ui 'error-text) error-text))

(defmethod ui-has-error-text ((ui ui-base))
  (let ((et (get-error-text ui)))
    (and et (> (length et) 0))))

(defmethod ui-clear-stack-contents ((ui ui-base) max-depth)
  (setf (slot-value ui 'stack-real-contents)
        (make-array (1+ max-depth) :initial-element ""))
  (setf (slot-value ui 'stack-imag-contents)
        (make-array (1+ max-depth) :initial-element "")))

(defmethod ui-add-stack-value ((ui ui-base) stack-depth real-part-string &optional imag-part-string)
  (let ((realpart (get-stack-real-contents ui))
        (imagpart (get-stack-imag-contents ui)))
    (when (< stack-depth (array-dimension realpart 0))
      (setf (aref realpart stack-depth) real-part-string)
      (setf (aref imagpart stack-depth) imag-part-string))))
