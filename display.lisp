;; Code for rendering numbers for display

(declaim (optimize (debug 3) (safety 3)))

;; This isn't actually that simple a problem.  Note, for instance,
;; that the format directive is permitted to round up or round down
;; from 0.5, but the calculator rounds 0.5 to 1, and -0.5 to -1.
;; There is also some automatic mode switching if the number can't be
;; displayed in the 'fixed' display mode.

(defun format-for-printing (state val)
  (let ((digits-after-decimal (get-display-digits state)))
    (ecase (get-display-output-mode state)
      (:FIXED
       (render-rational-as-fix (rational val)
                               digits-after-decimal))
      (:SCIENTIFIC
       (render-rational-as-sci (rational val)
                               digits-after-decimal))
      (:ENGINEERING
       (render-rational-as-sci (rational val)
                               digits-after-decimal
                               :engineering t)))))


(defun convert-string-rep-to-rational (string)
  (if (position-of-exponent-marker string)
      (make-rational-from-sci-string string)
      (make-rational-from-float-string string)))


(defun round-to-ultimate-precision (rval)
  "Takes a rational and returns a new one, in the exact precision of the calculator (10 digits)"
  (assert (rational rval))
  (multiple-value-bind (rep in-bounds)
      (render-rational-as-sci rval (1- *digits-in-display*))
    (values (convert-string-rep-to-rational rep)
            in-bounds)))


(defun position-of-exponent-marker (string)
  (position-if #'(lambda (x)
                   (or (char-equal x #\e)
                       (char-equal x #\d))) string))

(defun perform-long-division (ratval n-digits)
  "Computes the decimal expansion of the positive rational to n total digits, with appropriate rounding.  Returns a list of three values.  The first is a list of digits, from most to least significant.  The second is the power of 10 on the first digit.  The third is 1 if the least-significant digit was rounded up to that value (0 means it rounded down)."
  (assert (>= ratval 0))

  (assert (>= n-digits 0))
  (when (= ratval 0)
    (return-from perform-long-division
      (list (make-list n-digits :initial-element 0) 0)))

  (let ((result '())
        (num (numerator ratval))
        (den (denominator ratval))
        (exponent 0)
        carryflag)
    
    (do ()
        ((>= num den))
      (setf num (* 10 num))
      (decf exponent))
    (do ()
        ((< num (* 10 den)))
      (setf den (* 10 den))
      (incf exponent))

    (cond
      ((> n-digits 0)
       (dotimes (i n-digits)
         (let ((digit (floor (/ num den))))
           (push digit result)
           (decf num (* digit den))
           (setf num (* 10 num)))))
      ((= n-digits 0)
       (incf exponent)
       (setf result (list 0))))

    (setf num (/ num 10))  ;; erase the extra power of 10 we put in
    (let ((carry (if (>= (/ num den) (/ 1 2)) 1 0)))
      (setf carryflag carry)
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

    (list result exponent carryflag)))


(defun round-long-division-result (ld-res)
  "Rounds to remove the least-significant digit."
  (labels
      ((round-up (remaining-reversed-digits expt)
         (let ((carry 1)
               unreversed)
           (setf remaining-reversed-digits
                 (mapcar #'(lambda (x)
                             (incf x carry)
                             (setf carry 0)
                             (when (= x 10)
                               (setf x 0)
                               (setf carry 1))
                             x) remaining-reversed-digits))

           (setf unreversed (reverse remaining-reversed-digits))
           (when (= carry 1)
             (push 1 unreversed)
             (incf expt))

           (list unreversed expt 1))))

    (let* ((digits (reverse (first ld-res)))
           (exponent (second ld-res))
           (carryflag (third ld-res))
           (least-sig (pop digits)))

      (cond
        (digits
         (cond
           ((< least-sig 5)
            (list (reverse digits) exponent 0))
           ((> least-sig 5)
            (round-up digits exponent))
           ((= carryflag 1)
            (list (reverse digits) exponent 0))
           ((= carryflag 0)
            (round-up digits exponent))
           (t
            (error "bad flow, shouldn't get here"))))

        ;; When we reach the final digit, we round to 0 or 1, not to
        ;; nil
        (t
         (cond
           ((or (< least-sig 5)
                (and (= least-sig 5)
                     (= carryflag 1)))
            (list (list 0) 0 0))
           (t
            (list (list 1) 0 1))))))))



;; n-digits is the number after the decimal
(defun render-rational-as-sci (rval n-digits
                               &key
                                 engineering
                                 (truncating t))
  "In the end, we can't trust format because of its rounding rules, and the coercion.  Even going to double-float can't guarantee that we won't slip a digit in the last place.  So, here we 'display' a rational number by longhand division."
  (when (and truncating
             (> (1+ n-digits) *digits-in-display*))
    (setf n-digits (1- *digits-in-display*)))

  (when (= rval 0)
    (return-from render-rational-as-sci
      (values (format nil "~,vE" n-digits 0.0) t)))

  (let* ((rv (make-string-output-stream))
         (sign (if (> rval 0) 1 -1))
         (ld (perform-long-division (abs rval) (1+ n-digits)))
         (digits (first ld))
         (exponent (second ld))
         (decimal-pos 1))

    (when engineering
      (let ((shift-right (mod exponent 3)))
        (incf decimal-pos shift-right)
        (decf exponent shift-right)))

    (when (= sign -1)
      (format rv "-"))
    
    (dolist (dig digits)
      (format rv "~D" dig)
      (decf decimal-pos)
      (when (= 0 decimal-pos)
        (format rv ".")))

    (format rv "e~A~2,'0D"
            (if (< exponent 0) "-" "")
            (abs exponent))

    (values (get-output-stream-string rv)
            (<= exponent *overflow-exponent*))))


;; The 'truncating' keyword, if non-nil, bounds the total number of
;; digits to *digits-in-display*
(defun render-rational-as-fix (rval n-digits-after-decimal
                               &key (truncating t))

  (assert (>= n-digits-after-decimal 0))

  (when (= rval 0)
    (return-from render-rational-as-fix
      (format nil "~,vF" n-digits-after-decimal 0.0)))

  (let ((sign (if (> rval 0) 1 -1))
        (rabs (abs rval))
        magnitude
        digits-needed)
        

    ;; Check to see if, following rounding, the number won't fit in
    ;; the width of the display.  If so, use the scientific notation
    ;; function instead
    (when (and truncating
               (or (>= rabs (- (expt 10 *digits-in-display*) (/ 1 2)))
                   (< rabs (/ (expt 10 (- n-digits-after-decimal)) 2))))
      (return-from render-rational-as-fix
        (render-rational-as-sci rval n-digits-after-decimal
                                :truncating truncating)))


    ;; We'll use a log to estimate the magnitude, but may correct it
    ;; if rounding errors give the wrong result
    (setf magnitude (floor (log (coerce rabs 'double-float) 10)))
    (when (>= rabs (expt 10 (1+ magnitude)))
      (incf magnitude))
    (when (< rabs (expt 10 magnitude))
      (decf magnitude))

    (setf digits-needed (+ magnitude n-digits-after-decimal 1))

    (let* ((ld (perform-long-division rabs digits-needed))
           (digits (first ld))
           (exponent (second ld))
           (rv (make-string-output-stream)))

      ;; Cut off digits that are past the end of the display, by
      ;; rounding exactly
      (when (and truncating
                 (< rabs 1))
        (let ((width (+ -1 (length digits) (- exponent))))
          (dotimes (i (- width (min n-digits-after-decimal
                                    (1- *digits-in-display*))))
            (setf ld (round-long-division-result ld))))
        (setf digits (first ld))
        (setf exponent (second ld)))

      (when (and truncating
                 (> rabs 1))
        (let ((width (length digits)))
          (dotimes (i (- width *digits-in-display*))
            (setf ld (round-long-division-result ld))))
        (setf digits (first ld))
        (setf exponent (second ld)))

      ;; put in leading zeroes, they aren't in the division result
      (when (< sign 0)
        (format rv "-"))
      (when (< exponent 0)
        (format rv "0.")
        (dotimes (i (- -1 exponent))
          (format rv "0")))

      (dotimes (i (length digits))
        (format rv "~D" (nth i digits))
        (when (= i exponent)
          (format rv ".")))

      (let ((result (get-output-stream-string rv)))
        (if (not (string-contains-non-zero-digit result))
            (render-rational-as-sci rval n-digits-after-decimal
                                    :truncating truncating)
            result)))))



(defun make-rational-from-float-string (string)
  "This all has to happen without passing through floats"
  (let ((d-pos (position #\. string))
        whole-part frac-part)
    (cond
      (d-pos
       (setf whole-part (subseq string 0 d-pos))
       (when (= (length whole-part) 0)
         (setf whole-part "0"))
       (when (< (1+ d-pos) (length string))
         (setf frac-part (subseq string (1+ d-pos)))))
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
         (epos (position-of-exponent-marker string))
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
