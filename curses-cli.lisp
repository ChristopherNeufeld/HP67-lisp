;; The CL-CHARMS (NCurses) interface to the HP-67 emulator
;;

(declaim (optimize (debug 3) (safety 3)))

(defpackage :CURSES-CLI
  (:use :COMMON-LISP :ASDF/INTERFACE)
  (:import-from :HP67-INTERNALS
                :GET-NEW-STACK-OBJECT
                :GET-NEW-MODE-OBJECT
                :GET-KEY-ABBREVS
                :KEY-STRUCT-ABBREV
                :KEY-STRUCT-KEY-LOCATION
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


(defparameter *category-precedence*
  '(:ARITHMETIC :ALGEBRAIC
    :STATISTICS :TRIGONOMETRY
    :TRANSCENDENTAL :STACK-MANIPULATION
    :MEMORY :INDIRECTION :MODE-SWITCH
    :FLAGS :FLOW_CONTROL :PROGRAM-MEMORY
    :EXTERNAL-I-O))
                                      


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
    (charms:disable-echoing)
    (charms:disable-extra-keys charms:*standard-window*)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (charms:enable-raw-input :interpret-control-characters t)

    (multiple-value-bind (n-cols n-rows)
        (charms:window-dimensions charms:*standard-window*)

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

          (charms:clear-window charms:*standard-window*)

          (let ((active-keys (get-key-abbrevs mode
                                              :sort-fcn #'comp<
                                              :veto-list *excluded-keys*
                                              :limit-to-mode t))
                (i 0))

            (dolist (candidate all-keys)
              (when (member candidate active-keys
                            :test 'string=)
                (multiple-value-bind (r c)
                    (floor i keys-per-row)
                  (charms:move-cursor charms:*standard-window*
                                      (* c (1+ maxlen))
                                      r))
                (charms:write-string-at-cursor charms:*standard-window*
                                               candidate))
              (incf i))

            (charms:refresh-window charms:*standard-window*)

            (sleep 4)

            (setf exit-requested t)

            ))))))
