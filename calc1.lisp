;; The stack labels the bottom four entries.  In order from first
;; popped to last: X, Y, Z, W
;;
;; The general format of key operator rules is this:
;;
;; X <- (sqrt (* X X) (* Y Y))
;; Y <- (atan Y X)
;;
;; However...
;; If the function produces only a single result, it can be abbreviated as:
;;
;; (* Y X)
;;
;;
;; so, we need some code to rewrite this, when it arrives as macro
;; arguments.
;;
;; Behind the scenes, the macro will look roughly like:
;; 
;; (defmacro ( [options] ) &body rules)
;;
;; So these things will arrive in a list
;;


(defstruct (location)
  (row		nil)     ;; 1-offset row number
  (column	nil)     ;; 1-offset column number
  (shift	:UNSHIFTED)
  (width	1)
  (category-1	nil)
  (category-2	nil))


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

  (use-rationals-p	nil)
  (complex-allowed-p	nil)
  (error-state		nil))


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


(defun update-last-x (stack)
  (let ((contents (stack-registers stack)))
    (setf (stack-last-x stack)
          (if contents
              (first contents)
              0))))


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


(defstruct (key-struct)
  (key-location		nil)
  (key-id		nil)
  (avail-modes		:RUN-MODE)
  (abbrev		nil)
  (run-mode-fcn		nil)
  (doc-string		nil))
  

(let ((keys '())
      (next-id 0))

  (defun make-new-id ()
    (prog1
        next-id
      (incf next-id)))

  (defun register-key-structure (ks)
    (let ((this-id (key-struct-key-id ks)))
      (when (>= this-id next-id)
        (setf next-id (1+ this-id))))
    (push ks keys))

  (defun get-keys ()
    keys))

           


(defun get-symbols-in-list (rlist)
  (let ((rval '()))
    (dolist (element rlist)
      (cond
        ((listp element)
         (setf rval (append rval (get-symbols-in-list element))))
        ((symbolp element)
         (push element rval))))
    (delete-duplicates rval)))

(defun get-vars-used (rules-list varnames)
  (let ((symbols-used (get-symbols-in-list rules-list))
        (vlen (length varnames)))
    (dotimes (i vlen)
      (let ((check (nth (- vlen i 1) varnames)))
        (when (member check symbols-used)
          (return-from get-vars-used 
            (subseq varnames 0 (- vlen i))))))))

(defun get-vars-assigned (rules-list varnames)
  (let ((rv '()))
    (labels
        ((worker (rl)
           (do ((v rl (cdr v)))
               ((not v) rv)
             (cond
               ((listp (first v))
                (setf rv (append rv (worker (first v)))))
               ((and (symbolp (first v))
                     (eq (second v) '<-))
                (push (first v) rv))))))

      (setf rv (worker rules-list))

      (remove-if #'(lambda (x)
                     (not (member x varnames))) rv)
      (if (not rv)
          (list (first varnames))
          (delete-duplicates rv)))))


(defun convert-to-setf-forms (rules-list 
                              vars-used 
                              output-varnames)
  (let (rv)
    (do ((pos rules-list (cdr pos)))
        ((not pos) rv)
      (cond
        ((and (member (first pos) vars-used)
              (eq (second pos) '<-)
              (third pos))
         (setf rv
               (append rv 
                       `((setf ,(nth (position (first pos)
                                               vars-used)
                                     output-varnames)
                               ,(third pos)))))
         (setf pos (cddr pos)))
        ((listp (first pos))
         (setf rv 
               (append 
                rv 
                (list 
                 (convert-to-setf-forms (first pos)
                                        vars-used
                                        output-varnames)))))
        (t
         (setf rv (append rv (list (first pos)))))))))


;; This is going to change a basic rules list into explicit pops,
;; pushes, and exception handling
(defun expand-rules (rules-list &key update-last-x)
  (let* ((varnames '(X Y Z W))
         (stack-var (gensym))
         (state-var (gensym))
         (vars-used (get-vars-used rules-list
                                   varnames))
         (vars-assigned (get-vars-assigned rules-list
                                           varnames)))

    ;; If this is an implicit X <- form, make it explicit so the setf
    ;; substitution will work later
    (when (and (= 1 (length vars-assigned))
               (not (member '<- (get-symbols-in-list
                                 rules-list)))
               (= 1 (length rules-list)))
      (setf rules-list 
            (append (list (first varnames) '<-)
                    rules-list)))

    ;; We need new symbols to hold the assigned values of the stack
    ;; variables, to avoid side-effects on multiple assignments.
    (let (gensyms-output)
      (dolist (v vars-assigned)
        (declare (ignore v))
        (push (gensym) gensyms-output))

      (setf rules-list 
            (convert-to-setf-forms 
             rules-list vars-assigned gensyms-output))

      `(lambda (,stack-var ,state-var)
         (declare (ignorable ,stack-var ,state-var))
         (labels
             ((to-radians (angle)
                (convert-angle-to-radians 
                 angle 
                 (modes-angles ,state-var)))
              (from-radians (angle)
                (convert-angle-from-radians 
                 angle 
                 (modes-angles ,state-var)))
              (to-rational (num)
                (convert-number-to-rational 
                 num 
                 (modes-rational ,state-var)))
              (to-double-fp (num)
                (coerce num 'double-float)))

           ,(when update-last-x
                  `(update-last-x ,stack-var))
           (backup-stack ,stack-var)
           (let (,@(mapcar #'(lambda (x) 
                               `(,x (pop-stack ,stack-var))) 
                           vars-used)
                 ,@(mapcar #'(lambda (x) 
                               (list x 0))
                           gensyms-output))
             (handler-case
                 (progn
                   ,@rules-list
                   ,@(mapcar #'(lambda (x)
                                 `(push-stack ,stack-var ,x)) 
                             gensyms-output))

               ((or arithmetic-error simple-error not-real-number) (c)
                 (set-error-state ,stack-var c)
                (recover-stack ,stack-var)))))))))




(defmacro define-op-key ((&key
                          location
                          (id (make-new-id))
                          (mode :RUN-MODE)
                          abbreviation
                          (updates-last-x t)
                          documentation)
                         &body run-mode-forms)
  (register-key-structure
   (make-key-struct :key-location location
                    :key-id id
                    :avail-modes mode
                    :abbrev abbreviation
                    :doc-string documentation
                    :run-mode-fcn
                    (eval (expand-rules 
                           `(,@run-mode-forms)
                           :update-last-x updates-last-x))))
  (values))


(define-op-key 
    (:location (make-location
                :row 5
                :col 1
                :category-1 :ARITHMETIC)
               :abbreviation "-" 
               :documentation "Subtracts X from Y")
    X <- (- Y X))

(define-op-key 
    (:location (make-location
                :row 6
                :col 1
                :category-1 :ARITHMETIC)
               :abbreviation "+" 
               :documentation "Adds X to Y")
    X <- (+ Y X))

(define-op-key 
    (:location (make-location
                :row 7
                :col 1
                :category-1 :ARITHMETIC)
               :abbreviation "*" 
               :documentation "Multiplies Y by X")
    X <- (* Y X))

(define-op-key 
    (:location (make-location
                :row 8
                :col 1
                :category-1 :ARITHMETIC)
               :abbreviation "/" 
               :documentation "Divides Y by X")
    X <- (/ Y X))

(define-op-key 
    (:location (make-location
                :row 8
                :col 1
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :abbreviation "!" 
               :documentation "Computes X factorial")
    (assert (and (integerp X)
                 (>= X 0)))
  (let ((result 1))
    (dotimes (i X)
      (setf result (* result (1+ i))))
    X <- result))
