;; Code related to the parsing and construction of keys

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


(defparameter *rcode* :RETCODE)
(defparameter *assign* '<-)


(defstruct (location)
  (row		nil)     ;; 1-offset row number
  (col		nil)     ;; 1-offset column number
  (shift	:UNSHIFTED)
  (width	1)
  (narrow-key   nil)     ;; some keys are square, not rectangular
  (compound-key nil)
  (category-1	nil)
  (category-2	nil))


;; Here are the category-1 types, and their category-2 types, if defined:
;;
;; :NUMERIC
;; :ARITHMETIC
;; :STATISTICS
;; :ALGEBRAIC
;; :TRIGONOMETRY	(nil :INVERSE-TRIG)
;; :TRANSCENDENTAL
;; :STACK-MANIPULATION
;; :MODE-SWITCH
;; :INDIRECTION
;; :EXTERNAL-I-O
;; :MEMORY		(nil :MEMORY-RECALL :MEMORY-STORE)
;; :FLAGS
;; :FLOW-CONTROL	(nil :MEMORY-STORE)
;; :PROGRAM-MEMORY


;; Keys can return one of the following:
;;
;; :NORMAL-EXIT    no unusual behaviour.  If in numeric input mode,
;;                 switches back to run mode
;; :GOTO "label"   move program counter to the next forward occurence
;; 		   of label, or back nnn steps if "label" is a
;; 		   negative number
;; :GOSUB "label"  execute the program starting at label, until RTN is
;;                 encountered
;; :RETURN-FROM-SUBROUTINE   pop the stack and return from gosub
;; :TOKEN "<token>"...  start numeric input mode if not already there
;; :SINGLE-STEP    execute the next program step
;; :LABEL "label"  record a label, a target for GOTO or GOSUB
;; :BACK-STEP      back up the program counter by one step
;; :SKIP-NEXT-STEP   jump over the next program step
;; :CARD-OPERATION   use the calculator's card reader/writer
;; :DELETE-CURRENT-STEP  delete a program step
;; :PAUSE-1-SECOND   display the X register for 1 second, then continue
;; :REVIEW-REGISTERS display the primary registers, then continue
;; :RUN-STOP       start or stop the program execution
;; :PAUSE-5-SECONDS  display the X register for 5 seconds, then continue
;; :DISPLAY-STACK    display the stack, then continue
;; :NO-OP          insert a null instruction in the program space


(defstruct (key-struct)
  (key-location		nil)
  (key-id		nil)
  (avail-modes		'(:RUN-MODE :RUN-MODE-NO-PROG
                          :PROGRAM-EXECUTION :PROGRAMMING-MODE))
  (abbrev		nil)
  (run-mode-form	nil)
  (run-mode-fcn		nil)
  (takes-arg		nil)
  (doc-string		nil))
  

(let ((keys '())
      (next-id 0))

  (defun erase-keys ()
    (setf keys '()))

  (defun make-new-id ()
    (prog1
        next-id
      (incf next-id)))

  (defun register-key-structure (ks)
    (let ((this-id (key-struct-key-id ks)))
      (when (>= this-id next-id)
        (setf next-id (1+ this-id))))

    (when (member-if
           #'(lambda (x)
               (and (string= (key-struct-abbrev x)
                             (key-struct-abbrev ks))
                    (intersection (key-struct-avail-modes x)
                                  (key-struct-avail-modes ks))))
           keys)
      (error "Key collision: redundant definition for ~A~%"
             (key-struct-abbrev ks)))
    
    (push ks keys))

  (defun get-keys ()
    keys))

           

(defun show-forms-on-key (abbreviation)
  (dolist (k (get-keys))
    (when (string= abbreviation (key-struct-abbrev k))
      (format t "~S~%" (key-struct-run-mode-form k)))))


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

(defun get-vars-assigned (rules-list varnames &key no-implicit-x)
  (let ((rv '()))
    (labels
        ((worker (rl)
           (do ((v rl (cdr v)))
               ((not v) rv)
             (cond
               ((listp (first v))
                (setf rv (append rv (worker (first v)))))
               ((and (symbolp (first v))
                     (eq (second v) *assign*))
                (push (first v) rv))))))

      (setf rv (worker rules-list))

      (setf rv (remove-if #'(lambda (x)
                              (not (member x varnames))) rv))

      (if (and no-implicit-x (not rv))
          (list (first varnames))
          (delete-duplicates rv)))))


(defun convert-to-setf-forms (rules-list 
                              vars-used 
                              output-varnames
                              return-code-symbol
                              return-code-var)
  (let (rv)
    (do ((pos rules-list (cdr pos)))
        ((not pos) rv)
      (cond
        ((and (eq (second pos) *assign*)
              (eq (first pos) return-code-symbol))
         (setf rv (append rv `((setf ,return-code-var ,(third pos)))))
         (setf pos (cddr pos)))
        ((and (member (first pos) vars-used)
              (eq (second pos) *assign*)
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
                                        output-varnames
                                        return-code-symbol
                                        return-code-var)))))
        (t
         (setf rv (append rv (list (first pos)))))))))


;; This is going to change a basic rules list into explicit pops,
;; pushes, and exception handling
(defun expand-rules (rules-list &key
                                  no-implicit-x
                                  update-last-x
                                  rational-safe
                                  op-takes-arg)
  (let* ((varnames '(X Y Z W))
         (rflag (if rational-safe :RATIONAL :DOUBLE-FLOAT))
         (stack-var (gensym))
         (state-var (gensym))
         (ret-code-var (gensym))
         with-implicit
         (vars-used (get-vars-used rules-list
                                   varnames))
         (vars-assigned (get-vars-assigned rules-list
                                           varnames)))

    ;; If this is an implicit X <- form, make it explicit so the setf
    ;; substitution will work later.  Overridden by a keyword.
    (when (and (not no-implicit-x)
               (not (member *assign* (get-symbols-in-list
                                      rules-list)))
               (= 1 (length rules-list)))
      (setf with-implicit t)
      (setf rules-list 
            (append (list (first varnames) *assign*)
                    rules-list))

      (setf vars-assigned (get-vars-assigned rules-list varnames)))
      

    ;; We need new symbols to hold the assigned values of the stack
    ;; variables, to avoid side-effects on multiple assignments.
    (let (gensyms-output)
      (dolist (v vars-assigned)
        (declare (ignore v))
        (push (gensym) gensyms-output))

      (setf rules-list 
            (convert-to-setf-forms 
             rules-list
             vars-assigned
             gensyms-output
             *rcode* ret-code-var))

      `(lambda ,(if op-takes-arg
                    `(,stack-var ,state-var ARG)
                    `(,stack-var ,state-var))
         (declare (ignorable ,stack-var ,state-var))
         (macrolet
             ((to-radians (angle)
                `(convert-angle-to-radians 
                  ,angle 
                  (modes-angles ,',state-var)))
              (from-radians (angle)
                `(convert-angle-from-radians 
                  ,angle 
                  (modes-angles ,',state-var)))

              (set-flag (name)
                `(set-flag-fcn ,',stack-var ,name))
              (clear-flag (name)
                `(clear-flag-fcn ,',stack-var ,name))
              (get-flag (name)
                `(get-flag-fcn ,',stack-var ,name))

              (push-val (val)
                `(push-stack ,',stack-var ,val ,,rflag))
              (roll-stack-up ()
                `(rollup-stack ,',stack-var))
              (roll-stack-down ()
                `(rolldown-stack ,',stack-var))
              (get-last-x ()
                `(retrieve-last-x-value ,',stack-var))

              (round-to-display-precision (num)
                `(convert-string-rep-to-rational
                  (format-for-printing ,',state-var ,num)))

              (store-mem (name val)
                `(store-memory ,',stack-var ,name ,val ,,rflag))
              (recall-mem (name)
                `(recall-memory ,',stack-var ,name ,,rflag))
              (swap-registers ()
                `(swap-primary-secondary ,',stack-var))
              (clear-registers ()
                `(clear-primary-memory-registers ,',stack-var))

              (clear-program ()
                `(clear-program-memory ,',stack-var ,',state-var))

              (store-i-val (val)
                `(set-i-register ,',stack-var ,val))
              (recall-i-val ()
                `(get-i-register ,',stack-var))

              (get-i-int-val ()
                `(second (multiple-value-list
                          (get-i-register ,',stack-var))))

              (set-angle-mode (how)
                `(set-angle-units-mode ,',state-var ,how))

              (set-display-width (width)
                `(set-display-digits ,',state-var ,width))
              (set-display-mode (how)
                `(set-display-output-mode ,',state-var ,how)))

           ,(when update-last-x
                  `(update-last-x ,stack-var))
           (backup-stack ,stack-var)
           (let (,@(mapcar #'(lambda (x) 
                               `(,x (pop-stack ,stack-var ,rflag))) 
                           vars-used)
                 ,@(mapcar #'(lambda (x) 
                               (list x 0))
                           gensyms-output)
                   (,ret-code-var '(:NORMAL-EXIT)))
             (declare (ignorable ,@vars-used))

             (handler-case
                 (progn
                   ,@rules-list
                   ,@(mapcar #'(lambda (x)
                                 `(push-stack ,stack-var ,x ,rflag)) 
                             gensyms-output))

               ((or
                 arithmetic-error simple-error
                 not-real-number i-register-range-error) (c)
                 (set-error-state ,stack-var c)
                (setf ,ret-code-var '(:ERROR))
                (recover-stack ,stack-var)))
             ,ret-code-var))))))




(defmacro define-op-key ((&key
                          location
                          (id (make-new-id))
                          (modelist '(:RUN-MODE :RUN-MODE-NO-PROG
                                      :PROGRAM-EXECUTION
                                      :PROGRAMMING-MODE))
                          abbreviation
                          (updates-last-x t)
                          rational-safe
                          takes-argument
                          (implicit-x t)
                          documentation)
                         &body run-mode-forms)
  (let ((run-forms
         (expand-rules
          `(,@run-mode-forms)
          :update-last-x updates-last-x
          :no-implicit-x (not implicit-x)
          :rational-safe rational-safe
          :op-takes-arg takes-argument)))
    `(register-key-structure
      (make-key-struct :key-location ,location
                       :key-id ,id
                       :avail-modes ',modelist
                       :abbrev ,abbreviation
                       :takes-arg ,takes-argument
                       :doc-string ,documentation
                       :run-mode-form ',run-forms
                       :run-mode-fcn ,run-forms))))


(defmacro define-toprow-key ((col letter abbreviation doc
                                  &key
                                  category-1
                                  implicit-x
                                  (updates-last-x t)
                                  rational-safe)
                             &body arith-forms)
  `(progn
     (define-op-key
         (:location (make-location
                     :row 1
                     :col ,col
                     :category-1 ,category-1)
                    :modelist '(:RUN-MODE-NO-PROG)
                    :abbreviation ,abbreviation
                    :updates-last-x ,updates-last-x
                    :rational-safe ,rational-safe
                    :implicit-x ,implicit-x
                    :documentation ,(format nil
                                            "~S (when no program exists)"
                                            doc))
         ,@arith-forms)
     
     (define-op-key
         (:location (make-location
                     :row 1
                     :col ,col
                     :category-1 :FLOW-CONTROL)
                    :modelist '(:RUN-MODE :PROGRAM-EXECUTION
                                :PROGRAMMING-MODE)
                    :abbreviation ,(format nil
                                           "GSB-~C"
                                           letter)
                    :implicit-x ,implicit-x
                    :updates-last-x nil
                    :documentation ,(format nil
                                            "Call program label ~C"
                                            letter))
         :RETCODE <- '(:GOSUB ,(format nil "~C" letter))
         X <- X)
                    
     (define-op-key
         (:location (make-location
                     :row 1
                     :col ,col
                     :category-1 :FLOW-CONTROL)
                    :modelist '(:RUN-MODE :PROGRAM-EXECUTION
                                :PROGRAMMING-MODE)
                    :abbreviation ,(format nil
                                           "GSB-~C"
                                           (char-downcase letter))
                    :implicit-x ,implicit-x
                    :updates-last-x nil
                    :documentation ,(format nil
                                            "Call program label ~C"
                                            (char-downcase letter)))
         :RETCODE <- '(:GOSUB ,(format nil "~C"
                                       (char-downcase letter)))
         X <- X)))
                    


