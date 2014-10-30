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
  (category-1	nil)
  (category-2	nil))


(defstruct (key-struct)
  (key-location		nil)
  (key-id		nil)
  (avail-modes		'(:RUN-MODE))
  (abbrev		nil)
  (run-mode-form	nil)
  (run-mode-fcn		nil)
  (takes-arg		nil)
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

      (remove-if #'(lambda (x)
                     (not (member x varnames))) rv)

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
         (append rv `((setf ,return-code-var ,(third pos))))
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
                                  op-takes-arg)
  (let* ((varnames '(X Y Z W))
         (stack-var (gensym))
         (state-var (gensym))
         (ret-code-var (gensym))
         (vars-used (get-vars-used rules-list
                                   varnames))
         (vars-assigned (get-vars-assigned rules-list
                                           varnames)))

    ;; If this is an implicit X <- form, make it explicit so the setf
    ;; substitution will work later.  Overridden by a keyword.
    (when (and (not no-implicit-x)
               (= 1 (length vars-assigned))
               (not (member *assign* (get-symbols-in-list
                                      rules-list)))
               (= 1 (length rules-list)))
      (setf rules-list 
            (append (list (first varnames) *assign*)
                    rules-list)))

    ;; We need new symbols to hold the assigned values of the stack
    ;; variables, to avoid side-effects on multiple assignments.
    (let (gensyms-output)
      (dolist (v vars-assigned)
        (declare (ignore v))
        (push (gensym) gensyms-output))

      (setf rules-list 
            (convert-to-setf-forms 
             rules-list vars-assigned gensyms-output
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
                `(push-stack ,',stack-var ,val))
              (roll-stack-up ()
                `(rollup-stack ,',stack-var))
              (roll-stack-down ()
                `(rolldown-stack ,',stack-var))

              (store-mem (name val)
                `(cond
                   ((string-equal ,name "(i)")
                    (store-memory ,',stack-var
                                  (get-i-register ,',stack-var)
                                  ,val
                                  :indirection t))
                   (t
                    (store-memory ,',stack-var ,name ,val))))
              (recall-mem (name)
                `(cond
                   ((string-equal ,name "(i)")
                    (recall-memory ,',stack-var
                                   (get-i-register ,',stack-var)
                                   :indirection t))
                   (t
                    (recall-memory ,',stack-var ,name))))

              (to-rational (num)
                `(convert-number-to-rational 
                  ,num 
                  (modes-rational ,',state-var)))
              (to-double-fp (num)
                `(coerce ,num 'double-float)))

           ,(when update-last-x
                  `(update-last-x ,stack-var))
           (backup-stack ,stack-var)
           (let (,@(mapcar #'(lambda (x) 
                               `(,x (pop-stack ,stack-var))) 
                           vars-used)
                 ,@(mapcar #'(lambda (x) 
                               (list x 0))
                           gensyms-output)
                   (,ret-code-var '(:NORMAL-EXIT)))

             (handler-case
                 (progn
                   ,@rules-list
                   ,@(mapcar #'(lambda (x)
                                 `(push-stack ,stack-var ,x)) 
                             gensyms-output))

               ((or arithmetic-error simple-error not-real-number) (c)
                 (set-error-state ,stack-var c)
                (setf ,ret-code-var '(:ERROR))
                (recover-stack ,stack-var)))
             ,ret-code-var))))))




(defmacro define-op-key ((&key
                          location
                          (id (make-new-id))
                          (modelist '(:RUN-MODE))
                          abbreviation
                          (updates-last-x t)
                          takes-argument
                          (implicit-x t)
                          documentation)
                         &body run-mode-forms)
  (let ((run-forms
         (expand-rules
          `(,@run-mode-forms)
          :update-last-x updates-last-x
          :no-implicit-x (not implicit-x)
          :op-takes-arg takes-argument)))
    `(register-key-structure
      (make-key-struct :key-location ,location
                       :key-id ,id
                       :avail-modes ',modelist
                       :abbrev ,abbreviation
                       :takes-arg ,takes-argument
                       :doc-string ,documentation
                       :run-mode-form ,run-forms
                       :run-mode-fcn (eval ,run-forms)))))


(defmacro define-toprow-key ((col letter abbreviation doc
                                  &key implicit-x (updates-last-x t))
                             &body arith-forms)
  `(progn
     (define-op-key
         (:location (make-location
                     :row 1
                     :col ,col
                     :category-1 :ARITHMETIC)
                    :modelist '(:RUN-NO-PROG)
                    :abbreviation ,abbreviation
                    :updates-last-x ,updates-last-x
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
                    :modelist '(:RUN-WITH-PROG)
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
                    :modelist '(:RUN-WITH-PROG)
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
                    


