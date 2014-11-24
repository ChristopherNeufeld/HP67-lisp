;; The CL-CHARMS (NCurses) interface to the HP-67 emulator
;;

(declaim (optimize (debug 3) (safety 3)))

(defpackage :CURSES-CLI
  (:use :COMMON-LISP :ASDF/INTERFACE)
  (:import-from :HP67-INTERNALS
                :GET-NEW-STACK-OBJECT
                :STACK-REGISTERS
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
        (charms:disable-extra-keys w)
        (charms:disable-non-blocking-mode w)
        (charms:enable-raw-input :interpret-control-characters t)

        (let* ((stack (get-new-stack-object 4))
               (mode (get-new-mode-object))
               (all-keys (get-key-abbrevs mode
                                          :sort-fcn #'comp<
                                          :veto-list *excluded-keys*))
               (maxlen (apply 'max
                              (mapcar 'length all-keys)))
               (keys-per-row (floor (/ n-cols (1+ maxlen)))))

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

              (dotimes (j 4)
                (let ((entry (nth j (stack-registers stack))))
                  (when entry
                    (charms:move-cursor w 0 (- n-rows j 4))
                    (wsc (format-for-printing mode entry)))))

              (charms:move-cursor w 0 (- n-rows 2))

              (charms:refresh-window w)

              (do ((pos 0)
                   (c (charms:get-char w) (charms:get-char w)))
                  ((char= c #\Newline))

                (cond
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
