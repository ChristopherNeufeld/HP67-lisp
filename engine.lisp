;; The working engine for the calculator emulator

(declaim (optimize (debug 3) (safety 3)))

(in-package :HP67-INTERNALS)

(defun get-new-stack-object (stack-size)
  (make-stack :num-registers stack-size))

(defun get-new-mode-object ()
  (make-modes))


(defun string-may-contain-number (text)
  (let ((extras '(#\e #\E #\d #\D #\. #\-)))
    (dotimes (i (length text))
      (let ((c (char text i)))
        (unless (or (digit-char-p c)
                    (member c extras))
          (return-from string-may-contain-number nil)))))
  t)



(defun tokenize (text-string)
  (labels
      ((find-non-space (seq pos)
         (when pos
           (position-if #'(lambda (x)
                            (char/= x #\ ))
                        seq :start pos)))

       (find-space (seq pos)
         (when pos
           (position-if #'(lambda (x)
                            (char= x #\ ))
                        seq :start pos))))

    (let ((begin-token 0)
          (end-token 0)
          rv)
      (do ()
          ((not begin-token) (reverse rv))
        (setf begin-token (find-non-space text-string end-token))
        (setf end-token (find-space text-string begin-token))

        (cond
          ((and begin-token end-token)
           (push (subseq text-string begin-token end-token) rv))
          (begin-token
           (push (subseq text-string begin-token) rv)))))))


;; Here we handle run-mode operations, the most common case.  We will
;; pass key-presses by their abbreviation strings, and will support a
;; space-separated argument after the abbreviation.  The
;; fetch-argument-closure is used when an argument is needed and it
;; isn't found within the key-string.  It will be called with a single
;; argument, the key abbreviation, and is expected to return the
;; argument as a string.  The check-for-interrupt-closure is used to
;; halt execution of a running closure.  If the user interface
;; provides a way to signal the desire to halt, it will be checked
;; with this closure.  It takes no arguments, and if it returns
;; non-nil, program execution is halted.  The stack and mode arguments
;; must be built and owned by the user interface.
;;
;; Returns :UNKNOWN-COMMAND on a bad match
;; Returns :MISSING-ARGUMENT if no argument was found when needed
;; Returns :ERROR if there is an error state
;; Returns :NORMAL-EXIT in the unexceptional case
(defun handle-one-keypress (key-string
                            fetch-argument-closure
                            check-for-interrupt-closure
                            stack mode
                            &key arg-is-num)

  (declare (ignorable check-for-interrupt-closure))

  (labels
      ((specific-match (abbrev mode ks)
         (and (string= abbrev (key-struct-abbrev ks))
              (member mode (key-struct-avail-modes ks))))

       (wildcard-match (abbrev mode ks)
         (declare (ignore mode))
         (string= abbrev (key-struct-abbrev ks)))

       (set-run-mode (stack mode)
         (setf (modes-run/prog mode)
               (if (stack-program-memory stack)
                   :RUN-MODE
                   :RUN-MODE-NO-PROG))))

    (let* ((current-mode (modes-run/prog mode))
           (all-keys (get-keys))
           (tokenized (tokenize key-string))
           (abbrev (first tokenized))
           (arg (second tokenized))
           (key (car (or (member-if
                          #'(lambda (x)
                              (specific-match abbrev
                                              current-mode
                                              x))
                          all-keys)
                         (member-if
                          #'(lambda (x)
                              (wildcard-match abbrev
                                              current-mode
                                              x))
                          all-keys)))))

      ;; Simple case when entire numbers might be sent in by the UI
      (when arg-is-num
        (when (and (string-may-contain-number abbrev)
                   (numberp (read-from-string abbrev)))
          (push-stack stack
                      (convert-string-rep-to-rational abbrev)
                      :RATIONAL)
          (return-from handle-one-keypress :NORMAL-EXIT)))

      ;; If we get here, it's a key press.

      (unless key
        (return-from handle-one-keypress :UNKNOWN-COMMAND))

      (cond
        ((key-struct-token-key key)
         (let* ((token (second (funcall (key-struct-run-mode-fcn key)
                                        stack mode)))
                (result (add-token stack token)))
         (case result
           (:FINALIZE
            (set-run-mode stack mode))
           (:CONSTRUCTING
            (setf (modes-run/prog mode) :NUMERIC-INPUT))))
         (return-from handle-one-keypress :NORMAL-EXIT))
        (t
         ;; if there's a number in progress, push it and continue to
         ;; process the keypress
         (when (eq (modes-run/prog mode) :NUMERIC-INPUT)
           (add-token stack :ENTER))))

      (when (and (key-struct-takes-arg key)
                 (not arg))
        (setf arg (funcall fetch-argument-closure abbrev))
        (unless arg
          (return-from handle-one-keypress :MISSING-ARGUMENT)))


      ;; Look for compound keys
      (dolist (k (get-compound-keys))
        (let* ((location (key-struct-key-location k))
               (ck (location-compound-key location)))
          (when (and (string= abbrev (first ck))
                     (string= arg (second ck)))
            (setf key k)
            (setf arg (funcall fetch-argument-closure
                               (key-struct-abbrev key)))
            (unless arg
              (return-from handle-one-keypress :MISSING-ARGUMENT))
            (return))))

      (when (or (not (stack-error-state stack))
                (key-struct-can-clear-errors key))
        
        
        (if (key-struct-takes-arg key)
            (funcall (key-struct-run-mode-fcn key)
                     stack mode arg)
            (funcall (key-struct-run-mode-fcn key)
                     stack mode)))

      (if (stack-error-state stack)
          :ERROR
          :NORMAL-EXIT))))


(defun run-engine (ui &key (stacksize 4))
  (let* ((stack (get-new-stack-object stacksize))
         (mode (get-new-mode-object))
         (prev-active-mode nil)
         (prev-complex-mode nil)
         (prev-display-mode nil)
         (prev-display-digits -1))

    (do (quit-requested)
        (quit-requested)

      (let ((current-active-mode (modes-run/prog mode))
            (current-complex-mode (modes-complex mode))
            (current-error-text (stack-error-state stack))
            (current-display-mode (modes-display-mode mode))
            (current-display-digits (modes-digits mode)))

        (unless (eq prev-active-mode current-active-mode)
          (hp67-ui:ui-set-active-mode ui current-active-mode)
          (hp67-ui:ui-set-active-keys ui (get-key-structs current-active-mode
                                                          :limit-to-mode t))
          (setf prev-active-mode current-active-mode))

        (unless (and (eq current-display-mode prev-display-mode)
                     (= current-display-digits prev-display-digits))
          (hp67-ui:ui-set-display-mode ui current-display-mode current-display-digits)
          (setf prev-display-mode current-display-mode
                prev-display-digits current-display-digits))

        (unless (eq prev-complex-mode current-complex-mode)
          (hp67-ui:ui-set-complex-mode ui current-complex-mode)
          (setf prev-complex-mode current-complex-mode))

        (when current-error-text
          (hp67-ui:ui-set-error-text ui current-error-text))

        (let ((num-stack-to-pass (if (> stacksize 0)
                                     stacksize
                                     (length (stack-registers stack)))))

          (hp67-ui:ui-clear-stack-contents ui num-stack-to-pass)
          (dotimes (i num-stack-to-pass)
            (let ((entry (nth i (stack-registers stack))))
              (unless entry
                (setf entry 0))
              (hp67-ui:ui-add-stack-value ui i
                                          (format-for-printing mode entry)))))

        (let ((mem (stack-memory stack)))
          (hp67-ui:ui-clear-memory-contents ui)
          (dotimes (i (length mem))
            (hp67-ui:ui-add-memory-value ui i
                                         (car (nth i mem))
                                         (format-for-printing mode (cdr (nth i mem))))))

        (hp67-ui:ui-paint ui)
        (let ((response (hp67-ui:ui-get-input ui)))
          (setf quit-requested (hp67-ui:get-quit-requested ui))
          (unless quit-requested
            (etypecase response
              (string
               (when (string= response "")
                 (setf response "enter"))
               (handle-one-keypress response nil nil stack mode
                                    :arg-is-num t))
              (key-struct
               (handle-one-keypress (key-struct-abbrev response)
                                    nil nil stack mode
                                    :arg-is-num nil)))))))))


        
        
