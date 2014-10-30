;; Code related to the modes of operation of the calculator.

(defstruct (modes)
  (angles		:RADIANS)
  (run/prog		:RUN-MODE)
  (complex		nil)
  (rational		nil)
  
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


