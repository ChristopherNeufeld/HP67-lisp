;; Stack operations for the HP-67 emulator
;;

(declaim (optimize (debug 3) (safety 3)))

(in-package :HP67-INTERNALS)


(defparameter *unlimited-indirection* nil
  "If non-nil, the I-register is allowed to modify memory/flags outside the normally-permitted range")


(defconstant +initial-flag-settings+
  (if (boundp '+initial-flag-settings+)
      (symbol-value '+initial-flag-settings+)
      '(("0" . nil) ("1" . nil) ("2" . nil) ("3" . nil))))


(define-condition not-real-number (error)
  ((val		:initarg :value
                :reader get-val))
  (:documentation "Complex number encountered in real-only mode.")
  (:report (lambda (c s)
             (format s "The complex value ~A was encountered."
                     (get-val c)))))

(define-condition overflow (error)
  ()
  (:documentation "Calculator register overflowed.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "A number too large for the calculator was encountered"))))


(define-condition invalid-float-arrived (error)
  ((val		:initarg :value
                :reader get-val))
  (:documentation "A floating-point number was pushed when rationals were promised.  This is a coding bug and should be fixed.")
  (:report (lambda (c s)
             (format s "The float value ~A was encountered."
                     (get-val c)))))

(define-condition single-precision-float (error)
  ((val		:initarg :value
                :reader get-val))
  (:documentation "A single-precision floating-point number was seen.  This is below the promised precision of the calculator, and is a programming bug.")
  (:report (lambda (c s)
             (format s "The single-precision float value ~A was encountered."
                     (get-val c)))))

(define-condition i-register-range-error (error)
  ((val		:initarg :value
                :reader get-val)
   (min		:initarg :min-allowed
                :reader get-min)
   (max		:initarg :max-allowed
                :reader get-max))
  (:documentation "I-register out of range for operation.")
  (:report (lambda (c s)
             (format s "The I-register was ~D.  In this context, it must lie between ~D and ~D"
                     (get-val c) (get-min c) (get-max c)))))


(defun fix-input-val (val rflag)
  (when (eq rflag :DOUBLE-FLOAT)
    (when (eq (type-of val) 'single-float)
      (error (make-condition 'single-precision-float
                             :value val)))
         (setf val (rational val)))

  (unless (rationalp val)
    (error (make-condition 'invalid-float-arrived
                           :value val)))

  (multiple-value-bind (number in-bounds)
      (round-to-ultimate-precision val)
    (unless in-bounds
      (error (make-condition 'overflow)))
    number))


(defun fix-output-val (val rflag)
  (if (eq rflag :DOUBLE-FLOAT)
      (coerce val 'double-float)
      val))


(defstruct (token-assembler)
  (mantissa-sign	1)
  (mantissa		(make-string-output-stream))
  (mantissa-digits	0)
  (exponent-sign	1)
  (exponent		(make-array 2))
  (exponent-digits	0)

  (translation		'((:0 . "0") (:1 . "1") (:2 . "2")
                          (:3 . "3") (:4 . "4") (:5 . "5")
                          (:6 . "6") (:7 . "7") (:8 . "8")
                          (:9 . "9") (:DOT . ".") (:EEX . "d")
                          (:ENTER . :ENTER) (:CLX . :CLX)
                          (:CHS . :CHS)))

  (seen-dot		nil)
  (seen-expt		nil)
  (finalized		nil))


(defun produce-result (assembler)
  (let ((s (make-string-output-stream))
        (e-digs (token-assembler-exponent-digits assembler)))
    (when (= (token-assembler-mantissa-sign assembler) -1)
      (format s "-"))
    (format s "~A" (get-output-stream-string (token-assembler-mantissa assembler)))
    (format s "d")
    (when (= (token-assembler-exponent-sign assembler) -1)
      (format s "-"))
    (cond
      ((= e-digs 0) (format s "0"))
      (t
       (dotimes (i e-digs)
         (format s (aref (token-assembler-exponent assembler) i)))))
    (make-rational-from-sci-string (get-output-stream-string s))))
      

(defun add-token (stack token)
  (let ((assembler (stack-assembler stack)))
    (case token
      (:CLX
       (reset-token-assembler stack)
       (return-from add-token :RESET))
      (:ENTER
       (cond
         ((= (token-assembler-mantissa-digits assembler) 0)
          (let ((x-val (pop-stack stack :RATIONAL)))
            (push-stack stack x-val :RATIONAL)
            (push-stack stack x-val :RATIONAL)))
         (t
          (push-stack stack (produce-result assembler) :RATIONAL)))
       (reset-token-assembler stack)
       (return-from add-token :FINALIZE))
      (t
       (if (token-assembler-seen-expt assembler)
           (add-token-to-exponent assembler token)
           (add-token-to-mantissa assembler token))
       :CONSTRUCTING))))


(defun add-token-to-exponent (assembler token)
  (with-slots ((sign exponent-sign)
               (exp exponent)
               (exp-n exponent-digits)
               (table translation))
      assembler
    (let ((val (cdr (assoc token table))))
      (ecase token
        ((:0 :1 :2 :3 :4 :5 :6 :7 :8 :9)
         (cond
           ((= exp-n 2)
            (setf (aref exp 0) (aref exp 1))
            (setf (aref exp 1) val))
           (t
            (setf (aref exp exp-n) val)
            (incf exp-n))))
        ((:DOT :EEX)
         ;; do nothing
         )
        (:CHS
         (setf sign (* -1 sign)))))))


(defun add-token-to-mantissa (assembler token)
  (with-slots ((sign mantissa-sign)
               (mant mantissa)
               (mant-n mantissa-digits)
               (seen-dot seen-dot)
               (seen-expt seen-expt)
               (table translation))
      assembler
    (let ((val (cdr (assoc token table))))
      (ecase token
        ((:0 :1 :2 :3 :4 :5 :6 :7 :8 :9)
         (when (< mant-n *digits-in-display*)
           (format mant "~A" val)
           (incf mant-n)))
        (:DOT
         (unless seen-dot
           (setf seen-dot t)
           (format mant ".")))
        (:EEX
         (when (= mant-n 0)
           (format mant "1")
           (incf mant-n))
         (setf seen-expt t))
        (:CHS
         (setf sign (* -1 sign)))))))



(defstruct (stack)
  (registers		(list 0 0 0 0))
  (registers-copy	nil)
  (num-registers	4)
  (last-x		0)

  (assembler		(make-token-assembler))

  (memory		nil)
  (flags		(copy-alist +initial-flag-settings+))

  (program-memory	nil)

  (complex-allowed-p	nil)
  (error-state		nil))



(defun clear-primary-memory-registers (stack)
  (let ((primary-names '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                         "A" "B" "C" "D" "E" "(i)")))
    (mapcar #'(lambda (x)
                (store-memory-by-name stack x 0)) primary-names)))

(defun clear-program-memory (stack mode)
  (setf (stack-program-memory stack) nil)
  (set-angle-units-mode mode :DEGREES)
  (set-display-digits mode 2)
  (clear-all-flags stack))


(defun store-program-step (stack index command-string)
  "Stores the string in program memory at offset 'index'.  As a
   special case, if index is -1, it appends."
  (unless (stack-program-memory stack)
    (setf (stack-program-memory stack) (hp67-ropes:make-rope))
    ;; program step 0 is unassigned
    (hp67-ropes:append-object (stack-program-memory stack) nil))
  (when (= index -1)
    (setf index
          (hp67-ropes:rope-length (stack-program-memory stack))))
  
  (multiple-value-bind (unused success)
      (hp67-ropes:insert-object (stack-program-memory stack)
                                index
                                command-string)
    (declare (ignore unused))
    success))


(defun retrieve-program-step (stack index)
  (hp67-ropes:retrieve-object (stack-program-memory stack) index))

(defun get-num-program-steps (stack)
  "Remember, steps number from 1."
  (1- (hp67-ropes:rope-length (stack-program-memory stack))))


(defun canonicalize-memory-name (stack mem-name)
  (when (integerp mem-name)
    (setf mem-name (format nil "~D" mem-name)))
  (assert (stringp mem-name))
  (cond
    ((string-equal mem-name "(i)")
     (multiple-value-bind (junk int-val str-val)
         (get-i-register stack)
       (declare (ignore junk))
       (cond
         ((and (not *unlimited-indirection*)
               (or (< int-val 0) (> int-val 25)))
          (error (make-condition 'i-register-range-error
                                 :value int-val
                                 :min-allowed 0
                                 :max-allowed 25)))
         ((= int-val 25)
          "(i)")
         ((> int-val 19)
          (subseq "ABCDE"
                  (- int-val 20)
                  (- int-val 19)))
         (t
          str-val))))
    (t
     mem-name)))


(defun store-memory-by-name (stack name val)
  "Does no indirection, just stores under the name."
  (setf (stack-memory stack)
        (delete-duplicates
         (push (cons name val)
               (stack-memory stack))
         :key 'car
         :test 'string=
         :from-end t))
  val)

(defun recall-memory-by-name (stack name)
  "Does no indirection, just recalls from the name."
  (let ((record (assoc name
                       (stack-memory stack)
                       :test 'string=)))
    (if record
        (cdr record)
        0)))


(defun store-memory (stack name val rflag)
  (when (and (complexp val)
             (not (stack-complex-allowed-p stack)))
    (error (make-condition 'not-real-number
                           :value val)))
  (setf name (canonicalize-memory-name stack name))
  (setf val (fix-input-val val rflag))
  (store-memory-by-name stack name val)
  val)


(defun recall-memory (stack name rflag)
  (setf name (canonicalize-memory-name stack name))
  (fix-output-val (recall-memory-by-name stack name)
                  rflag))



(defun canonicalize-flag-name (stack flag-name)
  (when (integerp flag-name)
    (setf flag-name (format nil "~D" flag-name)))
  (assert (stringp flag-name))
  (cond
    ((string-equal flag-name "(i)")
     (multiple-value-bind (junk int-val str-val)
         (get-i-register stack)
       (declare (ignore junk))
       (cond
         ((and (not *unlimited-indirection*)
               (or (< int-val 0) (> int-val 3)))
          (error (make-condition 'i-register-range-error
                                 :value int-val
                                 :min-allowed 0
                                 :max-allowed 3)))
         (t
          str-val))))
    (t
     flag-name)))
  

(defun set-flag-by-name (stack name &key clear)
  (let ((record (assoc name (stack-flags stack)
                       :test 'string=)))
    (cond
      (record
       (setf (cdr record) (not clear)))
      (t
       (setf (stack-flags stack)
             (push (cons name (not clear))
                   (stack-flags stack)))))))

(defun get-flag-by-name (stack name)
  (let* ((record (assoc name (stack-flags stack)
                       :test 'string=))
         (rval (cdr record)))
    (when (or (string= name "2")
              (string= name "3"))
      (set-flag-by-name stack name :clear t))
    rval))


(defun set-flag-fcn (stack name &key clear)
  (setf name (canonicalize-flag-name stack name))
  (set-flag-by-name stack name :clear clear))

(defun clear-flag-fcn (stack name)
  (set-flag-fcn stack name :clear t))

(defun get-flag-fcn (stack name)
  (setf name (canonicalize-flag-name stack name))
  (get-flag-by-name stack name))

(defun clear-all-flags (stack)
  (setf (stack-flags stack) (copy-alist +initial-flag-settings+)))




(defun set-i-register (stack value)
  (store-memory-by-name stack "(i)" value))

;; Returns 3 values.  The unmodified value of I, the greatest-integer
;; value, and a string holding the greatest-integer value
(defun get-i-register (stack)
  (let ((rval (recall-memory-by-name stack "(i)")))
    (values
     rval
     (floor rval)
     (format nil "~D" (floor rval)))))



(defun swap-primary-secondary (stack)
  (dotimes (i 10)
    (let ((val-prim (recall-memory stack i :RATIONAL))
          (val-second (recall-memory stack (+ i 10) :RATIONAL)))
      (store-memory stack i val-second :RATIONAL)
      (store-memory stack (+ i 10) val-prim :RATIONAL))))
  


(defun backup-stack (stack)
  (setf (stack-registers-copy stack)
        (copy-tree (stack-registers stack))))

(defun recover-stack (stack)
  (setf (stack-registers stack)
        (copy-tree (stack-registers-copy stack))))

(defun set-error-state (stack c)
  (setf (stack-error-state stack) c))

(defun clear-error-state (stack)
  (setf (stack-error-state stack) nil))


(defun trim-list-to-length (list num)
  (assert (<= num (length list)))
  (dotimes (i (1- num))
    (setf list (cdr list)))
  (setf (cdr list) '()))


(defun set-stack-size (stack num)
  "Changes the size of the stack.  Size 0 means unlimited."
  (assert (and (integerp num)
               (>= num 0)))
  (when (and (/= num 0)
             (/= (stack-num-registers stack) num))
    (let ((num-new-entries (- num
                              (length (stack-registers stack)))))
      (cond
        ((> num-new-entries 0)
         (setf (stack-registers stack)
               (append (stack-registers stack)
                       (make-sequence 'list
                                      num-new-entries
                                      :initial-element 0))))
        (t
         (trim-list-to-length (stack-registers stack) num)))))

  (setf (stack-num-registers stack) num))


(defun pop-stack (stack rflag)
  "Returns the first element from the stack."
  (unless (stack-error-state stack)
    (let (rv)
      (cond
        ((and (= 0 (stack-num-registers stack))
              (null stack))
         (setf rv 0))
        (t
         (let ((previous-contents (stack-registers stack)))
           (setf rv (pop previous-contents))
           (when (/= (stack-num-registers stack) 0)
             (setf (stack-registers stack)
                   (append previous-contents (last previous-contents)))))))
      (fix-output-val rv rflag))))


(defun push-stack (stack val rflag)
  "Pushes an element on the stack."
  (unless (stack-error-state stack)
    (when (and (complexp val)
               (not (stack-complex-allowed-p stack)))
      (error (make-condition 'not-real-number
                             :value val)))

    (setf val (fix-input-val val rflag))
    (push val (stack-registers stack))
    (when (/= 0 (stack-num-registers stack))
      (trim-list-to-length (stack-registers stack)
                           (stack-num-registers stack))))
  val)



(defun reset-token-assembler (stack)
  (setf (stack-assembler stack) (make-token-assembler)))



(defun rolldown-stack (stack)
  (unless (stack-error-state stack)
    (let ((x-val (pop-stack stack :RATIONAL)))
      (setf (stack-registers stack)
            (append (stack-registers stack) (list x-val))))
    (first (stack-registers stack))))


(defun rollup-stack (stack)
  (unless (stack-error-state stack)
    (cond
      ((= (stack-num-registers stack) 0)
       (let* ((slen (length (stack-num-registers stack)))
              (last-val (nth (1- slen) (stack-num-registers stack))))
         (trim-list-to-length (stack-registers stack) (1- slen))
         (push-stack stack last-val :RATIONAL)))
      (t
       (let ((last-val (or (nth (1- (stack-num-registers stack))
                                (stack-registers stack))
                           0)))
         (push-stack stack last-val :RATIONAL)
         (trim-list-to-length (stack-registers stack)
                              (stack-num-registers stack)))))
    (first (stack-registers stack))))


(defun update-last-x (stack)
  (let ((contents (stack-registers stack)))
    (setf (stack-last-x stack)
          (if contents
              (first contents)
              0))))


(defun retrieve-last-x-value (stack)
  (stack-last-x stack))
