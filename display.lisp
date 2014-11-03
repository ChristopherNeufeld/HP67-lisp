;; Code for rendering numbers for display

(defparameter *digits-in-display* 10)

;; This isn't actually that simple a problem.  Note, for instance,
;; that the format directive is permitted to round up or round down
;; from 0.5, but the calculator rounds 0.5 to 1, and -0.5 to -1.
;; There is also some automatic mode switching if the number can't be
;; displayed in the 'fixed' display mode.

(defun format-for-printing (state val &key readable)
  (let ((digits-after-decimal (get-display-digits state)))
    (ecase (get-display-output-mode state)
      (:FIXED
       (format-for-printing-fix val
                                digits-after-decimal
                                :readable readable))
      (:SCIENTIFIC
       (format-for-printing-sci val
                                digits-after-decimal
                                :readable readable))
      (:ENGINEERING
       (format-for-printing-eng val
                                digits-after-decimal
                                :readable readable)))))


(defun string-contains-non-zero-digit (string)
  (dotimes (i (length string))
    (let ((one-char (char string i)))
      (when (and (digit-char-p one-char)
                 (char/= one-char #\0))
        (return-from string-contains-non-zero-digit t))))
  nil)

(defun break-down-sci-notation (string)
  (let* ((negative (char= (char string 0) #\-))
         (epos (position-if #'(lambda (x)
                                (or (char= x #\e)
                                    (char= x #\d))) string))
         (neg-expt (char= (char string (1+ epos)) #\-))
         (mantissa (subseq string
                           (if negative 1 0)
                           epos))
         (expt (subseq string
                       (if neg-expt
                           (+ 2 epos)
                           (1+ epos)))))
    (when (char= (char expt 0) #\+)
      (setf expt (subseq expt 1)))
    (list (if negative "-" " ")
          mantissa
          (if neg-expt "-" " ")
          expt)))


(defun shift-char-to-right (string start-pos n-shift
                            &key (padding #\0))
  "Moves the character at start-pos n-shift to the right"
  (let ((workspace (copy-seq string))
        (moved (char string start-pos))
        (pad-len (- (+ 1 start-pos n-shift) (length string))))

    (when (> pad-len 0)
      (setf workspace
            (concatenate 'string
                         workspace
                         (make-sequence 'string
                                        pad-len
                                        :initial-element padding))))
    (dotimes (i n-shift)
      (setf (char workspace (+ i start-pos))
            (char workspace (+ i 1 start-pos)))
      (setf (char workspace (+ i 1 start-pos)) moved))
    workspace))


(defun round-sci-notation-to-digits (string n-dig)
  (destructuring-bind (sign mantissa e-sign expt)
      (break-down-sci-notation string)
    (let* ((d-pos (position #\. mantissa))
           (shifted (shift-char-to-right mantissa d-pos
                                         (- n-dig d-pos -1)))
           (coerced (format nil "~Ad0" shifted))
           (m-val (floor (+ 0.5d0 (read-from-string coerced)))))
      (setf mantissa (format nil "~,v,vF" n-dig (- n-dig) m-val))

      (when (= d-pos 2)
        (let ((expt-val (read-from-string expt)))
          (if (string= e-sign "-")
              (decf expt-val)
              (incf expt-val))
          (setf expt (format nil "~2,'0D" expt-val)))))

    (format nil "~A~Ad~A~A"
            (if (string-equal sign "-")
                sign
                "")
            mantissa
            (if (string-equal e-sign "-")
                e-sign
                "")
            expt)))


(defun format-for-printing-fix (val digits-after-decimal
                                &key readable)

  (when (= val 0)
    (return-from format-for-printing-fix
      (format nil "~,vF" digits-after-decimal 0.0d0)))
  
  (let* ((negmult (if (< val 0) -1.0d0 1.0d0))
         (scaleup (expt 10.0d0 digits-after-decimal))
         (magnitude (abs val))
         (rounded (* negmult
                     (floor (+ 0.50000000004d0
                               (* magnitude scaleup)))))
         (first-try (format nil "~,v,vF"
                            digits-after-decimal
                            (- digits-after-decimal)
                            rounded))
         (max-width (+ 1 *digits-in-display*
                       (if (< val 0) 1 0))))

    (let ((overrun (- (length first-try) max-width)))
      (cond
        ((and (> overrun 0)
              (<= overrun digits-after-decimal))
         (format-for-printing-fix val
                                  (- digits-after-decimal
                                     overrun)
                                  :readable readable))
        ((> overrun 0)
         (format-for-printing-sci val digits-after-decimal
                                  :readable readable))
        ((and (/= val 0)
              (not (string-contains-non-zero-digit first-try)))
         (format-for-printing-sci val digits-after-decimal
                                  :readable readable))
        (t
         first-try)))))



(defun format-for-printing-sci (val digits-after-decimal
                                &key readable)
  (when (= 0 val)
    (return-from format-for-printing-sci
      (if readable
          "0.0d0"
          (format nil "~,vE" digits-after-decimal 0.0d0))))
  
  (let* ((magnitude (abs val))
         (first-try (format nil "~A~,v,2E"
                            (if (< val 0) "-" "")
                            digits-after-decimal
                            magnitude))
         formatted)

    (setf first-try (round-sci-notation-to-digits first-try
                                                  digits-after-decimal))

    (unless readable
      (destructuring-bind (sign mantissa e-sign exponent)
          (break-down-sci-notation first-try)

        (setf formatted
              (format nil "~A~vA~A~A"
                      sign
                      (1+ *digits-in-display*)
                      mantissa
                      e-sign
                      exponent))))

    (if readable
        (values first-try first-try)
        (values formatted first-try))))
    
    
(defun format-for-printing-eng (val digits-after-decimal
                                &key readable)
  (multiple-value-bind (junk parsed)
      (format-for-printing-sci val digits-after-decimal
                               :readable readable)
    (declare (ignore junk))
    (when readable
      (return-from format-for-printing-eng parsed))

    (destructuring-bind (sign mantissa e-sign exponent)
        (break-down-sci-notation parsed)

      (let* ((e-num (read-from-string exponent))
             (man-len (length mantissa))
             (shift-num (mod e-num 3)))
        (when (string= e-sign "-")
          (setf shift-num (mod (- 3 shift-num) 3)))
        (when (and (= man-len 3) (= shift-num 2))
          (setf mantissa (format nil "~A0" mantissa)))

        (dotimes (i shift-num)
          (psetf (char mantissa (1+ i)) (char mantissa (+ 2 i))
                 (char mantissa (+ 2 i)) #\.))

        (when (string= e-sign "-")
          (setf e-num (* -1 e-num)))
        (decf e-num shift-num)

        (format nil "~A~vA~A~2,'0D"
                sign
                (1+ *digits-in-display*)
                mantissa
                e-sign
                (abs e-num))))))

        
