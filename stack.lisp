;; Stack operations for the HP-67 emulator
;;

(defparameter *unlimited-indirection* nil
  "If non-nil, the I-register is allowed to modify memory/flags outside the normally-permitted range")



(define-condition not-real-number (error)
  ((val		:initarg value
                :reader get-val))
  (:documentation "Complex number encountered in real-only mode.")
  (:report (lambda (c s)
             (format s "The complex value ~A was encountered."
                     (get-val c)))))

(define-condition i-register-range-error (error)
  ((val		:initarg value
                :reader get-val)
   (min		:initarg min-allowed
                :reader get-min)
   (max		:initarg max-allowed
                :reader get-max))
  (:documentation "I-register out of range for operation.")
  (:report (lambda (c s)
             (format s "The I-register was ~D.  In this context, it must lie between ~D and ~D"
                     (get-val c) (get-min c) (get-max c)))))


(defstruct (stack)
  (registers		(list 0 0 0 0))
  (registers-copy	nil)
  (num-registers	4)
  (last-x		nil)

  (memory		nil)
  (flags		'(("0" . nil)
                          ("1" . nil)
                          ("2" . nil)
                          ("3" . nil)))

  (use-rationals-p	nil)
  (complex-allowed-p	nil)
  (error-state		nil))



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


(defun store-memory (stack name val)
  (setf name (canonicalize-memory-name stack name))
  (store-memory-by-name stack name val))


(defun recall-memory (stack name)
  (setf name (canonicalize-memory-name stack name))
  (recall-memory-by-name stack name))



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
    (let ((val-prim (recall-memory stack i))
          (val-second (recall-memory stack (+ i 10))))
      (store-memory stack i val-second)
      (store-memory stack (+ i 10) val-prim))))
  


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


(defun pop-stack (stack)
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
      (if (stack-use-rationals-p stack)
          (rational rv)
          rv))))


(defun push-stack (stack val)
  "Pushes an element on the stack."
  (assert (numberp val))
  (unless (stack-error-state stack)
    (when (and (complexp val)
               (not (stack-complex-allowed-p stack)))
      (error (make-condition 'not-real-number
                             :value val)))
    (when (stack-use-rationals-p stack)
      (setf val (rational val)))

    (push val (stack-registers stack))
    (when (/= 0 (stack-num-registers stack))
      (trim-list-to-length (stack-registers stack)
                           (stack-num-registers stack))))
  val)


(defun rolldown-stack (stack)
  (unless (stack-error-state stack)
    (let ((x-val (pop-stack stack)))
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
         (push-stack stack last-val)))
      (t
       (let ((last-val (or (nth (1- (stack-num-registers stack))
                                (stack-registers stack))
                           0)))
         (push-stack stack last-val)
         (trim-list-to-length (stack-registers stack)
                              (stack-num-registers stack)))))
    (first (stack-registers stack))))


(defun update-last-x (stack)
  (let ((contents (stack-registers stack)))
    (setf (stack-last-x stack)
          (if contents
              (first contents)
              0))))


