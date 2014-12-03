;; The CL-CHARMS (NCurses) interface to the HP-67 emulator
;;

(declaim (optimize (debug 3) (safety 3)))

(defpackage :CURSES-CLI
  (:use :COMMON-LISP :ASDF/INTERFACE)
  (:import-from :HP67-INTERNALS
                :GET-NEW-STACK-OBJECT
                :STACK-REGISTERS
                :STACK-MEMORY
                :STACK-ERROR-STATE
                :GET-NEW-MODE-OBJECT
                :GET-KEY-ABBREVS
                :HANDLE-ONE-KEYPRESS
                :KEY-STRUCT-ABBREV
                :KEY-STRUCT-KEY-LOCATION
                :FORMAT-FOR-PRINTING
                :LOCATION-CATEGORY-1
                :LOCATION-CATEGORY-2
                :LOCATION-COL :LOCATION-ROW
                :LOCATION-NARROW-KEY)
  (:import-from :HP67-UI
                :GET-ACTIVE-KEYS
                :GET-ACTIVE-SHIFT
                :GET-ACTIVE-MODE
                :GET-DISPLAY-MODE
                :GET-DISPLAY-DIGITS
                :GET-ERROR-TEXT
                :GET-STACK-REAL-CONTENTS
                :GET-STACK-IMAG-CONTENTS
                :GET-MEMORY-CONTENTS
                :GET-PROGRAM-CONTENTS
                :GET-PROGRAM-COUNTER
                :GET-QUIT-REQUESTED
                :GET-COMPLEX-MODE
                :UI-HAS-ERROR-TEXT
                :UI-SET-ACTIVE-MODE
                :UI-PAINT
                :UI-GET-INPUT
                )

  (:export :main))


(in-package :CURSES-CLI)



(defparameter *excluded-keys*
  '("0" "1" "2" "3" "4"
    "5" "6" "7" "8" "9"
    "eex" "enter" "(i)" "."
    "GSB-A" "GSB-B" "GSB-C" "GSB-D" "GSB-E"
    "GSB-a" "GSB-b" "GSB-c" "GSB-d" "GSB-e"
    "label-L"))

;; keys that, if the first entry in a line, immediately take effect
(defparameter *hot-keys*
  '(#\+ #\- #\* #\/))


(defparameter *category-precedence*
  '(:ARITHMETIC :ALGEBRAIC
    :STATISTICS :TRIGONOMETRY
    :TRANSCENDENTAL :STACK-MANIPULATION
    :MEMORY :INDIRECTION :MODE-SWITCH
    :FLAGS :FLOW_CONTROL :PROGRAM-MEMORY
    :EXTERNAL-I-O))


(defparameter *max-mem-label-length* 10)

(defclass curses-cli (hp67-ui:ui-base)
  ((window		:accessor get-window
                        :initarg :window
                        :initform nil)
   (rows		:accessor get-rows
                        :initarg :rows
                        :initform 0)
   (cols		:accessor get-cols
                        :initarg :cols
                        :initform 0)

   (all-keys		:accessor get-all-keys)
   (active-keys		:accessor get-active-keys)

   (button-rows		:accessor get-button-rows
                        :initform 0)
   (button-width	:accessor get-button-width
                        :initform 0)
   (keys-per-row	:accessor get-keys-per-row
                        :initform 0)
   (error-text-row	:accessor get-error-row
                        :initform 0)

   (stack-disp-size	:accessor get-stack-display-size
                        :initform 0)
   (stack-row-1		:accessor get-stack-first-row
                        :initform 0)
   (stack-col-1		:accessor get-stack-column
                        :initform 0)
   
   (mem-disp-size	:accessor get-mem-display-size
                        :initform 0)
   (mem-row-1		:accessor get-mem-first-row
                        :initform 0)
   (mem-label-col	:accessor get-mem-label-column
                        :initform 0)
   (mem-value-col	:accessor get-mem-value-column
                        :initform (+ 2 *max-mem-label-length*))

   (input-row		:accessor get-input-row
                        :initform 0)
   (input-col		:accessor get-input-col
                        :initform 0)))


(defun fill-in-parameters (object all-keys key-width)
  "Given a curses-cli object with window, rows, and columns set,
fills in the rest of the parameters appropriately."
  (let* ((num-keys (length all-keys))
         (key-rows (1+ (floor num-keys (1+ key-width)))))
    
    (setf (get-button-rows object) key-rows)
    (setf (get-button-width object) (1+ key-width))
    (setf (get-keys-per-row object) (floor (get-cols object) key-width))
    (setf (get-all-keys object) all-keys)

    (setf (get-error-row object) (+ 2 key-rows))

    (setf (get-input-row object) (- (get-rows object) 2)
          (get-input-col object) 0)

    (setf (get-stack-display-size object)
          (- (get-rows object) key-rows 6))
    (setf (get-stack-first-row object)
          (+ key-rows 4))
    (setf (get-stack-column object) 0)

    (setf (get-mem-display-size object)
          (- (get-rows object) key-rows 6))
    (setf (get-mem-first-row object)
          (+ key-rows 4))
    (setf (get-mem-label-column object) (floor (get-cols object) 2))
    (setf (get-mem-value-column object) (+ 2 *max-mem-label-length*
                                           (get-mem-label-column object)))))
    

(defun main-2 ()
  (charms:with-curses ()
    (let* ((w charms:*standard-window*)
           (dims (multiple-value-list (charms:window-dimensions w)))
           (curses-obj
            (make-instance 'curses-cli
                           :window w
                           :rows (second dims)
                           :cols (first dims)))

           (all-keys (get-key-abbrevs nil
                                      :sort-fcn #'comp<
                                      :veto-list *excluded-keys*))
           (maxlen (apply 'max
                          (mapcar 'length all-keys))))

      (fill-in-parameters curses-obj all-keys maxlen)))

  (error "continue"))


(defmethod hp67-ui:ui-set-active-mode :after ((ui curses-cli) active-mode)
  (setf (get-active-keys ui)
        (get-key-abbrevs active-mode
                         :sort-fcn #'comp<
                         :veto-list *excluded-keys*
                         :limit-to-mode t)))

(defmethod hp67-ui:ui-paint ((ui curses-cli))
  (charms:clear-window (get-window ui))
  (let ((i 0))
    (dolist (candidate (get-all-keys ui))
      (when (member candidate (get-active-keys ui)
                    :test 'string=)
        (multiple-value-bind (r c)
            (floor i (get-keys-per-row ui))
          (charms:move-cursor (get-window ui)
                              (* c (get-button-width ui))
                              r))
        (charms:write-string-at-cursor (get-window ui) candidate))
      (incf i)))

  (when (ui-has-error-text ui)
    (charms:move-cursor (get-window ui) 0 (get-error-row ui))
    (charms:write-string-at-cursor (get-window ui)
                                   (get-error-text ui)))

  (when (get-complex-mode ui)
    (error "Have to write complex stack output code."))

  (let* ((stack-contents (get-stack-real-contents ui))
         (n-entries (array-dimension stack-contents 0)))
    (dotimes (i n-entries)
      (let ((display-row (- (+ (get-stack-first-row ui)
                               (get-stack-display-size ui))
                            (1+ i))))
      (when (< i (get-stack-display-size ui))
        (charms:move-cursor (get-window ui)
                            (get-stack-column ui)
                            display-row)
        (charms:write-string-at-cursor (get-window ui)
                                       (aref stack-contents i))))))
  
  (when (get-complex-mode ui)
    (error "Have to write complex memory output code."))

  (let* ((mem-contents (get-memory-contents ui))
         (n-entries (length mem-contents)))
    (dotimes (i n-entries)
      (let ((display-row (- (+ (get-mem-first-row ui)
                               (get-mem-display-size ui))
                            (1+ i))))
      (when (< i (get-mem-display-size ui))
        (charms:move-cursor (get-window ui)
                            (get-mem-label-column ui)
                            display-row)
        (let ((label (copy-seq (second (aref mem-contents i)))))
          (when (> (length label) *max-mem-label-length*)
            (setf label (subseq label 0 *max-mem-label-length*)))
          (charms:write-string-at-cursor (get-window ui) label))

        (charms:move-cursor (get-window ui)
                            (get-mem-value-column ui)
                            display-row)
        (charms:write-string-at-cursor (get-window ui)
                                       (third (aref mem-contents i)))))))

  (charms:move-cursor (get-window ui) (get-input-col ui) (get-input-row ui))
  (charms:refresh-window))
  

(defmethod hp67-ui:ui-get-input ((ui curses-cli))
  (let ((output (make-array 0 :element-type 'character :adjustable t))
        (pos 0))
    (do ((c (charms:get-char (get-window ui))
            (charms:get-char (get-window ui))))
        ((char= c #\Newline))
      (cond
        ((rubout-character c)
         (when (> pos 0)
           (adjust-array output (1- pos))
           (charms:move-cursor (get-window ui)
                               (1- pos)
                               (get-input-row ui))
           (charms:write-string-at-cursor (get-window ui) " ")
           (decf pos)
           (charms:move-cursor (get-window ui) pos (get-input-row ui))))
        ((quit-character c)
         (setf (get-quit-requested ui) t))
        ((allowed-character c)
         (adjust-array output (1+ pos))
         (setf (aref output pos) c)
         (incf pos)))

      (when (and (= pos 1)
                 (member c *hot-keys* :test 'char=))
        (return)))

    output))
        
  



#+sbcl
(defun allowed-character (c)
  (let ((c-num (char-int c)))
    (or (char= c #\Newline)
        (char= c #\Space)
        (= c-num 4)
        (<= 32 c-num 127))))

#+sbcl
(defun quit-character (c)
  (= (char-int c) 4))

#+sbcl
(defun rubout-character (c)
  (or (char= c #\Rubout)
      (= (char-int c) 263)))



(defun comp< (key1 key2)
  (let* ((loc1 (key-struct-key-location key1))
         (loc2 (key-struct-key-location key2))

         (narrow-1 (location-narrow-key loc1))
         (narrow-2 (location-narrow-key loc2))

         (cat1-1 (location-category-1 loc1))
         (cat2-1 (location-category-2 loc1))
         (cat1-2 (location-category-1 loc2))
         (cat2-2 (location-category-2 loc2))

         (row1 (location-row loc1))
         (row2 (location-row loc2))
         (col1 (location-col loc1))
         (col2 (location-col loc2)))

    (cond
      ((not (eq cat1-1 cat1-2))  ;; different categories
       (> (length (member cat1-1 *category-precedence*))
          (length (member cat1-2 *category-precedence*))))
      ((and (not cat2-1) cat2-2) ;; 1 has a sub-category
       t)
      ((and cat2-1 (not cat2-2)) ;; 2 has a sub-category
       nil)
      ((and cat2-1 cat2-2)  ;; both have sub-categories
       (string< (symbol-name cat2-1)
                (symbol-name cat2-2)))
      ((and narrow-1 (not narrow-2))
       t)
      ((and narrow-2 (not narrow-1))
       nil)
      ((and row1 row2)
       (or (< row1 row2)
           (and (= row1 row2)
                (< col1 col2))))
      (t
       (string< (key-struct-abbrev key1)
                (key-struct-abbrev key2))))))


        
(defun main()
  (charms:with-curses ()
    (let ((w charms:*standard-window*)
          n-rows n-cols)

      (multiple-value-setq
          (n-cols n-rows)
        (charms:window-dimensions w))

      (macrolet
          ((wsc (ostring)
             `(charms:write-string-at-cursor w ,ostring)))
        
        (charms:enable-echoing)
        (charms:enable-extra-keys w)
        (charms:disable-non-blocking-mode w)
        (charms:enable-raw-input)

        (let* ((stack (get-new-stack-object 4))
               (mode (get-new-mode-object))
               (all-keys (get-key-abbrevs mode
                                          :sort-fcn #'comp<
                                          :veto-list *excluded-keys*))
               (maxlen (apply 'max
                              (mapcar 'length all-keys)))
               (keys-per-row (floor (/ n-cols (1+ maxlen))))
               (key-rows (1+ (floor (/ (length all-keys)
                                       keys-per-row)))))

          (do (exit-requested)
              (exit-requested)

            (charms:clear-window w)

            (let ((active-keys (get-key-abbrevs mode
                                                :sort-fcn #'comp<
                                                :veto-list *excluded-keys*
                                                :limit-to-mode t))
                  (i 0)
                  (accumulator (make-string-output-stream)))

              (dolist (candidate all-keys)
                (when (member candidate active-keys
                              :test 'string=)
                  (multiple-value-bind (r c)
                      (floor i keys-per-row)
                    (charms:move-cursor w (* c (1+ maxlen)) r))
                  (wsc candidate))
                (incf i))

              (when (stack-error-state stack)
                (charms:move-cursor w 0 (+ 2 key-rows))
                (wsc (format nil "~A" (stack-error-state stack))))

              (dotimes (j 4)
                (let ((entry (nth j (stack-registers stack))))
                  (when entry
                    (charms:move-cursor w 0 (- n-rows j 4))
                    (if (and (= j 0) (stack-error-state stack))
                        (wsc "<<<ERROR>>>")
                        (wsc (format-for-printing mode entry))))))

              (dotimes (j 4)
                (let ((entry (nth j (stack-memory stack))))
                  (when entry
                    (charms:move-cursor w 20 (- n-rows j 4))
                    (wsc (format nil "~A  ~A"
                                 (car entry)
                                 (format-for-printing mode (cdr entry)))))))
              
              (charms:move-cursor w 0 (- n-rows 2))

              (charms:refresh-window w)

              (do ((pos 0)
                   (c (charms:get-char w) (charms:get-char w)))
                  ((char= c #\Newline))

                (cond
                  ((rubout-character c)
                   (when (> pos 0)
                     (let ((contents (get-output-stream-string accumulator)))
                       (format accumulator
                               "~A"
                               (subseq contents
                                       0
                                       (- (length contents) 1))))
                     (charms:move-cursor w (1- pos) (- n-rows 2))
                     (wsc " ")
                     (decf pos)
                     (charms:move-cursor w pos (- n-rows 2))))
                  ((quit-character c)
                   (return-from main))
                  ((allowed-character c)
                   (format accumulator "~C" c)
                   (incf pos)))

                (when (and (= pos 1)
                           (member c *hot-keys* :test 'char=))
                  (return)))

              (let ((result (get-output-stream-string accumulator)))
                (handle-one-keypress result nil nil stack mode
                                     :arg-is-num t))))

              )))))
