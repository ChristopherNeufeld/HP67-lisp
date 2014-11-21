;; The CL-CHARMS (NCurses) interface to the HP-67 emulator
;;

(defpackage :CURSES-CLI
  (:use :COMMON-LISP :ASDF/INTERFACE)
  (:import-from :HP67-INTERNALS
                :GET-NEW-STACK-OBJECT
                :GET-NEW-MODE-OBJECT
                :GET-KEY-ABBREVS
                :GET-ACTIVE-KEY-ABBREVS
                :KEY-STRUCT-ABBREV
                :KEY-STRUCT-KEY-LOCATION
                :LOCATION-CATEGORY-1
                :LOCATION-CATEGORY-2
                :LOCATION-COL :LOCATION-ROW)
  (:export :main))


(in-package :CURSES-CLI)

(defparameter *excluded-keys*
  '("0" "1" "2" "3" "4"
    "5" "6" "7" "8" "9"
    "eex" "enter" "(i)"))

(defparameter *category-precedence*
  '(:ARITHMETIC :ALGEBRAIC
    :STATISTICS :TRIGONOMETRY
    :TRANSCENDENTAL :STACK-MANIPULATION
    :MEMORY :INDIRECTION :MODE-SWITCH
    :FLAGS :FLOW_CONTROL :PROGRAM-MEMORY
    :EXTERNAL-I-O))
                                      


(defun remove-excluded-keys (list)
  (remove-if #'(lambda (x)
                 (member (key-struct-abbrev x)
                         *excluded-keys*))
             list))


(defun sort-keys (keylist)
  (labels
      ((comp< (key1 key2)
         (let* ((loc1 (key-struct-key-location key1))
                (loc2 (key-struct-key-location key2))

                (cat1-1 (location-category-1 loc1))
                (cat2-1 (location-category-2 loc1))
                (cat1-2 (location-category-1 loc2))
                (cat2-2 (location-category-2 loc2))

                (row1 (location-row loc1))
                (row2 (location-row loc2))
                (col1 (location-col loc1))
                (col2 (location-col loc2)))

           (cond
             ((not (eq cat1-1 cat1-2))
              (> (length (member cat1-1 *category-precedence*))
                 (length (member cat1-2 *category-precedence*))))
             ((and (not cat2-1) cat2-2)
              t)
             ((and cat2-1 (not cat2-2))
              nil)
             ((and cat2-1 cat2-2)
              (string< (symbol-name cat2-1)
                       (symbol-name cat2-2)))
             ((and row1 row2)
              (or (< row1 row2)
                  (and (= row1 row2)
                       (< col1 col2))))
             (t
              (string< (key-struct-abbrev key1)
                       (key-struct-abbrev key2)))))))

    (sort keylist #'comp<)))

              



        
(defun main()
  (charms:with-curses ()
    (charms:enable-echoing)
    (charms:disable-extra-keys charms:*standard-window*)
    (charms:disable-non-blocking-mode charms:*standard-window*)
    (charms:enable-raw-input :interpret-control-characters t)

    (let* ((stack (get-new-stack-object 4))
           (mode (get-new-mode-object))
           (all-keys (sort-keys
                      (remove-excluded-keys
                       (get-key-abbrevs))))
           (maxlen (apply 'max
                          (mapcar
                           #'(lambda (x)
                               (length (key-struct-abbrev x)))
                           all-keys))))

      (do (exit-requested)
          (exit-requested)

        (let ((active-keys (sort-keys
                            (remove-excluded-keys
                             (get-active-key-abbrevs mode)))))
          
          
    
        )))))
