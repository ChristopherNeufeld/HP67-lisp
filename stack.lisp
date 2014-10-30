;; Stack operations for the HP-67 emulator
;;

(define-condition not-real-number (error)
  ((val		:initarg value
                :reader get-val))
  (:documentation "Complex number encountered in real-only mode.")
  (:report (lambda (c s)
             (format s "The complex value ~A was encountered."
                     (get-val c)))))

(defstruct (stack)
  (registers		(list 0 0 0 0))
  (registers-copy	nil)
  (num-registers	4)
  (last-x		nil)

  (memory		nil)
  (register-i		0)
  (flags		'(("0" . nil)
                          ("1" . nil)
                          ("2" . nil)
                          ("3" . nil)))

  (use-rationals-p	nil)
  (complex-allowed-p	nil)
  (error-state		nil))



(defun memory-name (name)
  (etypecase name
    (string (copy-seq name))
    (integer (format nil "~D" name))))

(defun convert-indirection-name (name)
  (assert (numberp name))
  (let ((num (floor name)))
    (cond
      ((or (< num 0) (> num 25))
       "")
      ((= num 25)
       (values t t))
      ((> num 19)
       (subseq "ABCDE" (- num 20) (- num 19)))
      (t
       (format nil "~D" num)))))



(defun store-memory (stack name val &key indirection)
  (let (converted use-i-reg)
    (cond
      (indirection
       (multiple-value-bind (c-name special-i-reg)
           (convert-indirection-name name)
         (setf use-i-reg special-i-reg
               converted c-name)))
      (t
       (setf converted (memory-name name))))

    (cond
      (use-i-reg
       (setf (stack-register-i stack) val))
      (t
       (setf (stack-memory stack)
             (delete-duplicates
              (push (cons converted val)
                    (stack-memory stack))
              :key 'car
              :test 'string=
              :from-end t)))))
  val)


(defun recall-memory (stack name &key indirection)
  (let (converted use-i-reg)
    (cond
      (indirection
       (multiple-value-bind (c-name special-i-reg)
           (convert-indirection-name name)
         (setf use-i-reg special-i-reg
               converted c-name)))
      (t
       (setf converted (memory-name name))))
    (cond
      (use-i-reg
       (stack-register-i stack))
      (t
       (let ((record (assoc converted
                            (stack-memory stack)
                            :test 'string=)))
         (if record
             (cdr record)
             0))))))


(defun set-flag-fcn (stack name &key clear)
  (let* ((converted (memory-name name))
         (record (assoc converted (stack-flags stack)
                        :test 'string=)))
    (cond
      (record
       (setf (cdr record) (not clear)))
      (t
       (setf (stack-flags stack)
             (push (cons converted (not clear))
                   (stack-flags stack)))))))

(defun clear-flag-fcn (stack name)
  (set-flag-fcn stack name :clear t))

(defun get-flag-fcn (stack name)
  (let* ((converted (memory-name name))
         (record (assoc converted (stack-flags stack)
                        :test 'string=))
         (rval (cdr record)))
    (when (or (string= converted "2")
              (string= converted "3"))
      (clear-flag-fcn stack name))
    rval))


(defun set-i-register (stack value)
  (setf (stack-register-i stack) value))

(defun get-i-register (stack)
  (stack-register-i stack))

(defun swap-primary-secondary (stack)
  (dotimes (i 10)
    (let ((val-prim (recall-memory stack i
                                   :indirection t))
          (val-second (recall-memory stack (+ i 10)
                                     :indirection t)))
      (store-memory stack i val-second
                    :indirection t)
      (store-memory stack (+ i 10) val-prim
                    :indirection t))))
  


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


