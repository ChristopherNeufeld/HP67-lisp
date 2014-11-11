;; Code related to the modes of operation of the calculator.

(defstruct (modes)
  (angles		:RADIANS)
  (run/prog		:RUN-MODE-NO-PROG)
  (complex		nil)

  (digits		2)
  (display-mode		:FIXED))


(defun set-angle-units-mode (mode angle-mode)
  (assert (member angle-mode '(:RADIANS :GRADIANS :DEGREES)))
  (setf (modes-angles mode) angle-mode))


(defun convert-angle-to-radians (angle angle-mode)
  (ecase angle-mode
    (:RADIANS angle)
    (:GRADIANS (* PI (/ angle 200.0d0)))
    (:DEGREES (* PI (/ angle 180.0d0)))))


(defun convert-angle-from-radians (angle angle-mode)
  (ecase angle-mode
    (:RADIANS angle)
    (:GRADIANS (* 200.0d0 (/ angle PI)))
    (:DEGREES (* 180.0d0 (/ angle PI)))))

(defun convert-number-to-rational (num rational-mode)
  "Convert the number, only if 'rational-mode' is non-nil"
  (if rational-mode
      (rational num)
      num))

(defun set-display-digits (mode num)
  (assert (<= 0 num 9))
  (setf (modes-digits mode) num))

(defun get-display-digits (mode)
  (modes-digits mode))

(defun set-display-output-mode (mode-struct how)
  (assert (member how '(:FIXED :SCIENTIFIC :ENGINEERING)))
  (setf (modes-display-mode mode-struct) how))

(defun get-display-output-mode (mode-struct)
  (modes-display-mode mode-struct))



;; Behavioural modes of the calculator.  There are some keys that are
;; only active in certain contexts, or which have different behaviours
;; in certain contexts.  These contexts are:

;; NUMERIC-INPUT
;;	The CHS and EEX keys behave in different ways in numeric input
;;	mode.  In this mode, EEX adds an exponent, and CHS changes the
;;	sign of the number being constructed, or of its exponent,
;;	depending on context.
;;
;; RUN-MODE
;;	The calculator is receiving interactive commands from the user.
;;
;; RUN-MODE-NO-PROG
;;	The calculator is receiving interactive commands from the
;;	user, and there are no program steps defined.
;;
;; PROGRAM-EXECUTION
;;	In program execution mode, commands are being drawn from a
;;	program in memory, and the flow-control operations can cause
;;	recorded keys to be skipped
;;
;; PROGRAMMING-MODE
;;	In programming mode, commands are being stored for later replay.
;;
;; The default assumption will be that a key is the same in RUN-MODE,
;; RUN-MODE-NO-PROG, PROGRAM-EXECUTION, and PROGRAMMING-MODE.  Keys
;; can override this explicitly in their definitions.
