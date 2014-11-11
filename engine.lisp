;; The working engine for the calculator emulator


(defun get-new-stack-object (stack-size)
  (make-stack :num-registers stack-size))

(defun get-new-mode-object ()
  (make-modes))


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
                            stack mode)

  (declare (ignorable check-for-interrupt-closure))

  (labels
      ((matching-key-struct (abbrev mode ks)
         (and (string= abbrev (key-struct-abbrev ks))
              (member mode (key-struct-avail-modes ks))))

       (matching-key-fallback (abbrev mode ks)
         (when (eq mode :RUN-MODE-NO-PROG)
           (matching-key-struct abbrev :RUN-MODE ks))))

  (let* ((current-mode (modes-run/prog mode))
         (all-keys (get-keys))
         (tokenized (tokenize key-string))
         (abbrev (first tokenized))
         (arg (second tokenized))
         (key (car (or (member-if
                        #'(lambda (x)
                            (matching-key-struct abbrev
                                                 current-mode
                                                 x))
                        all-keys)
                       (member-if
                        #'(lambda (x)
                            (matching-key-fallback abbrev
                                                   current-mode
                                                   x))
                        all-keys)))))

    (unless key
      (return-from handle-one-keypress :UNKNOWN-COMMAND))

    (when (and (key-struct-takes-arg key)
               (not arg))
      (setf arg (funcall fetch-argument-closure abbrev))
      (unless arg
        (return-from handle-one-keypress :MISSING-ARGUMENT)))

    (if (key-struct-takes-arg key)
        (funcall (key-struct-run-mode-fcn key)
                 stack mode arg)
        (funcall (key-struct-run-mode-fcn key)
                 stack mode))

    (if (stack-error-state stack)
        :ERROR
        :NORMAL-EXIT))))
