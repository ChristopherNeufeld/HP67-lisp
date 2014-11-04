;; Code for rendering numbers for display

(defparameter *digits-in-display* 10)

;; This isn't actually that simple a problem.  Note, for instance,
;; that the format directive is permitted to round up or round down
;; from 0.5, but the calculator rounds 0.5 to 1, and -0.5 to -1.
;; There is also some automatic mode switching if the number can't be
;; displayed in the 'fixed' display mode.

(defun format-for-printing (state val)
  (let ((digits-after-decimal (get-display-digits state)))
    (ecase (get-display-output-mode state)
      (:FIXED
       (format-for-printing-fix (rational val)
                                digits-after-decimal))
      (:SCIENTIFIC
       (format-for-printing-sci (rational val)
                                digits-after-decimal))
      (:ENGINEERING
       (format-for-printing-eng (rational val)
                                digits-after-decimal)))))


;; n-digits is the number after the decimal
(defun render-rational-as-sci (rval n-digits)
  "In the end, we can't trust format because of its rounding rules, and the coercion.  Even going to double-float can't guarantee that we won't slip a digit in the last place.  So, here we 'display' a rational number by longhand division."
  (when (= rval 0)
    (return-from render-rational-as-sci
      (format nil "~,vE" n-digits 0.0)))

  (let* ((result '())
         (rv (make-string-output-stream))
         (sign (if (> rval 0) 1 -1))
         (num (numerator (abs rval)))
         (den (denominator (abs rval)))
         (exponent 0))
    ;; First, adjust the numerator or denominator so that the rational
    ;; is at least 1 and less than 10.
    
    (do ()
        ((>= num den))
      (setf num (* 10 num))
      (decf exponent))
    (do ()
        ((< num (* 10 den)))
      (setf den (* 10 den))
      (incf exponent))

    (dotimes (i (1+ n-digits))
      (let ((digit (floor (/ num den))))
        (push digit result)
        (decf num (* digit den))
        (setf num (* 10 num))))

    (let ((carry (if (>= (/ num den) (/ 1 2)) 1 0)))
      (setf result (mapcar #'(lambda (x)
                               (setf x (+ carry x))
                               (cond
                                 ((= 10 x)
                                  (setf x 0))
                                 (t
                                  (setf carry 0)))
                               x) result))

      (cond
        ((= carry 1)
         ;; rounded all the way to the beginning. Fix.
         (pop result)
         (setf result (reverse result))
         (incf exponent)
         (push 1 result))
        (t
         (setf result (reverse result)))))

    (format rv "~A~D."
            (if (= sign -1) "-" "")
            (car result))
    (format rv "~{~D~}" (cdr result))
    (format rv "e~A~2,'0D"
            (if (< exponent 0) "-" "")
            (abs exponent))

    (get-output-stream-string rv)))




(defun make-rational-from-float-string (string)
  "This all has to happen without passing through floats"
  (let ((d-pos (position #\. string))
        whole-part frac-part)
    (cond
      (d-pos
       (setf whole-part (subseq string 0 d-pos))
       (when (= (length whole-part) 0)
         (setf whole-part "0"))
       (setf frac-part (subseq string (1+ d-pos))))
      (t
       (setf whole-part (copy-seq string))))

    ;; switch it from string to integer
    (setf whole-part (read-from-string whole-part))
    (cond
      (frac-part
       (let ((frac-len (length frac-part))
             (divisor 1))
         (dotimes (i frac-len)
           (setf divisor (* 10 divisor)))
         (setf frac-part (/ (read-from-string frac-part)
                            divisor)))
       (when (< whole-part 0)
         (setf frac-part (* -1 frac-part))))
      (t
       (setf frac-part 0)))

    (+ whole-part frac-part)))


(defun make-rational-from-sci-string (string)
  (destructuring-bind (sign mantissa e-sign expt)
      (break-down-sci-notation string)
    (let ((mant-rat (make-rational-from-float-string mantissa))
          (neg-mant (string= sign "-"))
          (neg-expt (string= e-sign "-"))
          (expt-int (read-from-string expt))
          (expt-factor 1))

      (if neg-expt
          (dotimes (i expt-int)
            (setf expt-factor (/ expt-factor 10)))
          (dotimes (i expt-int)
            (setf expt-factor (* expt-factor 10))))

      (* mant-rat expt-factor (if neg-mant -1 1)))))



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


(defun format-for-printing-fix (val digits-after-decimal)
  ;; 'val' arrives as rational, and we do our best to represent it
  (assert (rationalp val))
  (when (= val 0)
    (return-from format-for-printing-fix
      (values (format nil "~,vF" digits-after-decimal 0.0d0)
              0)))

  (let* ((negmult (if (< val 0) -1 1))
         (scaleup (expt 10 digits-after-decimal))
         (magnitude (abs val))
         (rounded (* negmult
                     (floor (+ (/ 1 2)
                               (* magnitude scaleup)))))
         (first-try (format nil "~,v,vF"
                            digits-after-decimal
                            (- digits-after-decimal)
                            (coerce rounded 'double-float)))
         (max-width (+ 1 *digits-in-display*
                       (if (< val 0) 1 0))))

    (let ((overrun (- (length first-try) max-width)))
      (cond
        ((and (> overrun 0)
              (<= overrun digits-after-decimal))
         (format-for-printing-fix val
                                  (- digits-after-decimal
                                     overrun)))
        ((> overrun 0)
         (format-for-printing-sci val digits-after-decimal))
        ((and (/= val 0)
              (not (string-contains-non-zero-digit first-try)))
         (format-for-printing-sci val digits-after-decimal))
        (t
         (values first-try
                 (make-rational-from-float-string first-try)))))))



(defun format-for-printing-sci (val digits-after-decimal)
  (assert (rationalp val))
  (when (= 0 val)
    (return-from format-for-printing-sci
      (values (format nil "~,vE" digits-after-decimal 0.0) 0)))
  
  (let* ((magnitude (abs val))
         (maglog (floor (log magnitude 10.0d0)))
         (scaleup (- digits-after-decimal maglog))
         (rounded (* (floor (+ (/ 1 2)
                               (* magnitude (expt 10 scaleup))))
                     (expt 10 (- scaleup))))

         (first-try (format nil "~A~,v,2E"
                            (if (< val 0) "-" "")
                            digits-after-decimal
                            (coerce rounded 'double-float)))
         formatted f-rat)

    (setf first-try (round-sci-notation-to-digits first-try
                                                  digits-after-decimal))

    (destructuring-bind (sign mantissa e-sign exponent)
        (break-down-sci-notation first-try)

      (setf formatted
            (format nil "~A~vA~A~A"
                    sign
                    (1+ *digits-in-display*)
                    mantissa
                    e-sign
                    exponent))

      (setf f-rat
            (format nil "~A~Ad~A~A"
                    (if (string= sign "-")
                        "-"
                        "")
                    mantissa
                    (if (string= e-sign "-")
                        "-"
                        "")
                    exponent)))
    

    (values formatted (make-rational-from-sci-string f-rat))))
    
    
(defun format-for-printing-eng (val digits-after-decimal)
  (when (= 0 val)
    (return-from format-for-printing-eng
      (values (format nil "~,vE" digits-after-decimal 0.0) 0)))

  (let* ((magnitude (abs val))
         (first-try (format nil "~A~,v,2E"
                            (if (< val 0) "-" "")
                            digits-after-decimal
                            magnitude))
         formatted f-rat)

    (setf first-try (round-sci-notation-to-digits first-try
                                                  digits-after-decimal))

    (destructuring-bind (sign mantissa e-sign exponent)
        (break-down-sci-notation first-try)

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

        (setf formatted (format nil "~A~vA~A~2,'0D"
                                sign
                                (1+ *digits-in-display*)
                                mantissa
                                e-sign
                                (abs e-num)))
        (setf f-rat
              (format nil "~A~Ad~A~A"
                      (if (string= sign "-")
                          "-"
                          "")
                      mantissa
                      (if (string= e-sign "-")
                          "-"
                          "")
                      e-num))))

    (values formatted (make-rational-from-sci-string f-rat))))

        
