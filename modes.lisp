;; Code related to the modes of operation of the calculator.

(defstruct (modes)
  (angles		:RADIANS)
  (run/prog		:RUN-MODE)
  (complex		nil)
  (rational		nil)

  (digits		2)
  (display-mode		:FIXED))


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
