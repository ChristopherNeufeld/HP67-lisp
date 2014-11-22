;; Defining the keys in the calculator.

(declaim (optimize (debug 3) (safety 3)))

(in-package :HP67-INTERNALS)

(erase-keys)

(define-toprow-key (1 #\A "1/x" "Reciprocal"
                      :rational-safe t
                      :category-1 :ARITHMETIC)
  X <- (/ 1 X))

(define-toprow-key (2 #\B "sqrt" "Square root"
                      :category-1 :ALGEBRAIC)
  X <- (sqrt X))

(define-toprow-key (3 #\C "y^x" "Power"
                      :category-1 :TRANSCENDENTAL)
  X <- (expt Y X))

(define-toprow-key (4 #\D "rolld" "Roll stack down"
                      :implicit-x nil
                      :updates-last-x nil
                      :category-1 :STACK-MANIPULATION)
  (roll-stack-down))

(define-toprow-key (5 #\E "x<>y" "Exchange X and Y"
                      :updates-last-x nil
                      :rational-safe t
                      :category-1 :STACK-MANIPULATION)
  X <- Y
  Y <- X)


(define-op-key
    (:location (make-location
                :row 2
                :col 1
                :category-1 :STATISTICS)
               :abbreviation "sigma+"
               :rational-safe t
               :documentation "Adds X and Y to the statistical registers")
    (let ((n-pts (recall-mem "19"))
          (sum-xy (recall-mem "18"))
          (sum-y2 (recall-mem "17"))
          (sum-y (recall-mem "16"))
          (sum-x2 (recall-mem "15"))
          (sum-x (recall-mem "14")))
      (incf n-pts)
      (incf sum-xy (* X Y))
      (incf sum-y2 (* Y Y))
      (incf sum-y Y)
      (incf sum-x2 (* X X))
      (incf sum-x X)

      (store-mem "19" n-pts)
      (store-mem "18" sum-xy)
      (store-mem "17" sum-y2)
      (store-mem "16" sum-y)
      (store-mem "15" sum-x2)
      (store-mem "14" sum-x)

      Y <- Y
      X <- n-pts))


(define-op-key
    (:location (make-location
                :row 2
                :col 1
                :shift :F-YELLOW
                :category-1 :STATISTICS)
               :abbreviation "avg"
               :rational-safe t
               :documentation "Computes statistical mean X and Y")
    (let ((n-pts (recall-mem "19"))
          (sum-y (recall-mem "16"))
          (sum-x (recall-mem "14")))

      (assert (> n-pts 0))
      Y <- (/ sum-y n-pts)
      X <- (/ sum-x n-pts)))



(define-op-key
    (:location (make-location
                :row 2
                :col 1
                :shift :G-BLUE
                :category-1 :STATISTICS)
               :abbreviation "stdev"
               :rational-safe nil
               :documentation "Computes sample statistical deviations of X and Y")
  (let ((n-pts (recall-mem "19"))
        (sum-y2 (recall-mem "17"))
        (sum-y (recall-mem "16"))
        (sum-x2 (recall-mem "15"))
        (sum-x (recall-mem "14")))

    (assert (> n-pts 1))
    Y <- (sqrt (/ (- sum-y2 (/ (* sum-y sum-y) n-pts)) (1- n-pts)))
    X <- (sqrt (/ (- sum-x2 (/ (* sum-x sum-x) n-pts)) (1- n-pts)))))



(define-op-key
    (:location (make-location
                :row 2
                :col 1
                :shift :H-BLACK
                :category-1 :STATISTICS)
               :abbreviation "sigma-"
               :rational-safe t
               :documentation "Subtracts X and Y from the statistical registers")
    (let ((n-pts (recall-mem "19"))
          (sum-xy (recall-mem "18"))
          (sum-y2 (recall-mem "17"))
          (sum-y (recall-mem "16"))
          (sum-x2 (recall-mem "15"))
          (sum-x (recall-mem "14")))
      (decf n-pts)
      (decf sum-xy (* X Y))
      (decf sum-y2 (* Y Y))
      (decf sum-y Y)
      (decf sum-x2 (* X X))
      (decf sum-x X)

      (store-mem "19" n-pts)
      (store-mem "18" sum-xy)
      (store-mem "17" sum-y2)
      (store-mem "16" sum-y)
      (store-mem "15" sum-x2)
      (store-mem "14" sum-x)

      Y <- Y
      X <- n-pts))



(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :category-1 :FLOW-CONTROL)
               :takes-argument t
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "goto"
               :documentation "Branches to location specified")
  (let (ival)
    (when (string-equal ARG "(i)")
      (setf ival (floor (recall-mem "(i)"))))
    (cond
      ((< ival 0)
       :RETCODE <- (list :GOTO (format nil "~D" ival)))  ;; jump back
      ((and ival (<= 0 ival 9))
       :RETCODE <- (list :GOTO (format nil "~D" ival)))
      ((and ival (<= 10 ival 14))
       :RETCODE <- (list :GOTO (subseq "ABCDE" (- ival 10) (- ival 9))))
      ((and ival (<= 15 ival 19))
       :RETCODE <- (list :GOTO (subseq "abcde" (- ival 10) (- ival 9))))
      (t
       :RETCODE <- (list :GOTO ARG)))))


(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL)
               :takes-argument t
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "gosub"
               :documentation "JSRs to location specified")
  (let (ival)
    (when (string-equal ARG "(i)")
      (setf ival (floor (recall-mem "(i)"))))
    (cond
      ((and ival (<= 0 ival 9))
       :RETCODE <- (list :GOSUB (format nil "~D" ival)))
      ((and ival (<= 10 ival 14))
       :RETCODE <- (list :GOSUB (subseq "ABCDE" (- ival 10) (- ival 9))))
      ((and ival (<= 15 ival 19))
       :RETCODE <- (list :GOSUB (subseq "abcde" (- ival 10) (- ival 9))))
      (t
       :RETCODE <- (list :GOSUB ARG)))))


;; (define-op-key
;;     (:location (make-location
;;                 :row 2
;;                 :col 2
;;                 :shift :G-BLUE
;;                 :category-1 :FLOW-CONTROL)
;;                :takes-argument t
;;                :updates-last-x nil
;;                :implicit-x nil
;;                :abbreviation "gosub"
;;                :documentation "JSRs to location specified")
;;   (assert (not (string-equal "(i)" ARG)))
;;   :RETCODE <- (list :GOSUB ARG))


(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :shift :H-BLACK
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "rtn"
               :documentation "Returns from subroutine")
  :RETCODE <- '(:RETURN-FROM-SUBROUTINE))


(define-op-key
    (:location (make-location
                :row 2
                :col 3
                :category-1 :MODE-SWITCH)
               :takes-argument t
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "dsp"
               :documentation "Changes display width")
  (let ((width ARG))
    (when (string-equal ARG "(i)")
      (setf width (get-i-int-val)))
    (set-display-width width)))

(define-op-key
    (:location (make-location
                :row 2
                :col 3
                :shift :F-YELLOW
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "fix"
               :documentation "Changes display to fixed-point")
  (set-display-mode :FIXED))


(define-op-key
    (:location (make-location
                :row 2
                :col 3
                :shift :G-BLUE
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "sci"
               :documentation "Changes display to scientific notation")
  (set-display-mode :SCIENTIFIC))


(define-op-key
    (:location (make-location
                :row 2
                :col 3
                :shift :H-BLACK
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "eng"
               :documentation "Changes display to engineering notation")
  (set-display-mode :ENGINEERING))


(define-op-key
    (:location (make-location
                :row 2
                :col 4
                :category-1 :INDIRECTION)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "(i)"
               :documentation "Inserts the indirection-I argument")
  :RETCODE <- '(:SPECIAL "(i)"))


(define-op-key
    (:location (make-location
                :row 2
                :col 4
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :abbreviation "rnd"
               :rational-safe t
               :documentation "Rounds X to the displayed precision")

  X <- (round-to-display-precision X))


(define-op-key
    (:location (make-location
                :row 2
                :col 4
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "x<>i"
               :rational-safe t
               :documentation "Swaps registers X and I")
  (let ((tmp (recall-mem "(i)")))
    (store-mem "(i)" X)
    X <- tmp))


(define-op-key
    (:location (make-location
                :row 2
                :col 5
                :category-1 :FLOW-CONTROL)
               :modelist (:RUN-MODE :NUMERIC-INPUT)
               :updates-last-x nil
               :abbreviation "sst"
               :implicit-x nil
               :documentation "Single-steps the program")
  :RETCODE <- '(:SINGLE-STEP))


(define-op-key
    (:location (make-location
                :row 2
                :col 5
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL)
               :takes-argument t
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "label"
               :implicit-x nil
               :documentation "Creates an upper-case label")
  :RETCODE <- (list :LABEL ARG))


(define-op-key
    (:location (make-location
                :row 2
                :col 5
                :shift :G-BLUE
                :category-1 :FLOW-CONTROL)
               :takes-argument t
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "label-L"
               :implicit-x nil
               :documentation "Creates a lower-case label")
  :RETCODE <- (list :LABEL ARG))


(define-op-key
    (:location (make-location
                :row 2
                :col 5
                :shift :H-BLACK
                :category-1 :FLOW-CONTROL)
               :modelist (:RUN-MODE :NUMERIC-INPUT)
               :updates-last-x nil
               :abbreviation "bst"
               :implicit-x nil
               :documentation "Backs up one program step (does not execute)")
  :RETCODE <- '(:BACK-STEP))


(define-op-key
    (:location (make-location
                :row 3
                :col 1
                :width 2
                :category-1 :NUMERIC)
               :abbreviation "enter"
               :updates-last-x nil
               :implicit-x nil
               :documentation "Pushes value onto stack")
  :RETCODE <- '(:TOKEN :ENTER))


(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "sto"
               :rational-safe t
               :documentation "Saves a memory register")
  (store-mem ARG X))


(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "dsz"
               :rational-safe t
               :implicit-x nil
               :documentation "Decrement register, skip next step if zero")
  (let ((v (recall-mem ARG)))
    (decf v)
    (when (= v 0)
      :RETCODE <- '(:SKIP-NEXT-STEP))
    (store-mem ARG v)))


(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :shift :G-BLUE
                :category-1 :FLOW-CONTROL
                :category-2 :MEMORY-STORE)
               :abbreviation "dsz(i)"
               :updates-last-x nil
               :rational-safe t
               :implicit-x nil
               :documentation "Decrement indirect, skip next step if zero")
  (let ((v (recall-mem "(i)")))
    (decf v)
    (when (= v 0)
      :RETCODE <- '(:SKIP-NEXT-STEP))
    (store-mem "(i)" v)))



(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :shift :H-BLACK
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :abbreviation "sti"
               :updates-last-x nil
               :implicit-x nil
               :rational-safe t
               :documentation "Saves to I register")
  (store-i-val X))

(define-op-key
    (:location (make-location
                :row 3
                :col 4
                :category-1 :MEMORY
                :category-2 :MEMORY-RECALL)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "rcl"
               :rational-safe t
               :documentation "Saves a memory register")
  (recall-mem ARG))


(define-op-key
    (:location (make-location
                :row 3
                :col 4
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "isz"
               :rational-safe t
               :implicit-x nil
               :documentation "Increment register, skip next step if zero")
  (let ((v (recall-mem ARG)))
    (incf v)
    (when (= v 0)
      :RETCODE <- '(:SKIP-NEXT-STEP))
    (store-mem ARG v)))


(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :shift :G-BLUE
                :category-1 :FLOW-CONTROL
                :category-2 :MEMORY-STORE)
               :updates-last-x nil
               :abbreviation "isz(i)"
               :rational-safe t
               :implicit-x nil
               :documentation "Increment indirect, skip next step if zero")
  (let ((v (recall-mem "(i)")))
    (incf v)
    (when (= v 0)
      :RETCODE <- '(:SKIP-NEXT-STEP))
    (store-mem "(i)" v)))



(define-op-key
    (:location (make-location
                :row 3
                :col 4
                :shift :H-BLACK
                :category-1 :MEMORY
                :category-2 :MEMORY-RECALL)
               :updates-last-x nil
               :abbreviation "rci"
               :rational-safe t
               :documentation "Recalls the I register")
  (recall-i-val))


(define-op-key
    (:location (make-location
                :row 4
                :col 1
                :width 2
                :shift :F-YELLOW
                :category-1 :EXTERNAL-I-O)
               :modelist (:RUN-MODE :NUMERIC-INPUT)
               :updates-last-x nil
               :abbreviation "w/data"
               :implicit-x nil
               :documentation "Writes memory registers to data card")
  :RETCODE <- '(:CARD-OPERATION :WRITE-DATA))


(define-op-key
    (:location (make-location
                :row 4
                :col 1
                :width 2
                :shift :G-BLUE
                :category-1 :EXTERNAL-I-O)
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "merge"
               :implicit-x nil
               :documentation "Appends program after current point")
  :RETCODE <- '(:CARD-OPERATION :MERGE-PROGRAM))


(define-op-key
    (:location (make-location
                :row 4
                :col 1
                :width 2
                :shift :H-BLACK
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :abbreviation "deg"
               :implicit-x nil
               :documentation "Sets angle mode to degrees")
  (set-angle-mode :DEGREES))



(define-op-key
    (:location (make-location
                :row 4
                :col 3
                :category-1 :ARITHMETIC)
               :modelist (:NUMERIC-INPUT)
               :abbreviation "chs"
               :implicit-x nil
               :documentation "Changes the sign of number being formed")
  :RETCODE <- '(:TOKEN :CHS))


(define-op-key
    (:location (make-location
                :row 4
                :col 3
                :category-1 :ARITHMETIC)
               :abbreviation "chs"
               :rational-safe t
               :documentation "Changes the sign of X")
  X <- (- X))


(define-op-key
    (:location (make-location
                :row 4
                :col 3
                :shift :F-YELLOW
                :category-1 :MEMORY)
               :updates-last-x nil
               :abbreviation "p<>s"
               :implicit-x nil
               :documentation "Swaps primary and secondary memory registers")
  (swap-registers))


(define-op-key
    (:location (make-location
                :row 4
                :col 3
                :shift :H-BLACK
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :abbreviation "rad"
               :implicit-x nil
               :documentation "Sets angle mode to radians")
  (set-angle-mode :RADIANS))


(define-op-key
    (:location (make-location
                :row 4
                :col 4
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "eex"
               :implicit-x nil
               :documentation "Starts exponent entry")
  :RETCODE <- '(:TOKEN :EEX))


(define-op-key
    (:location (make-location
                :row 4
                :col 4
                :shift :F-YELLOW
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :updates-last-x nil
               :abbreviation "clreg"
               :implicit-x nil
               :documentation "Erases all primary memory registers")
  (clear-registers))


(define-op-key
    (:location (make-location
                :row 4
                :col 4
                :shift :H-BLACK
                :category-1 :MODE-SWITCH)
               :updates-last-x nil
               :abbreviation "grad"
               :implicit-x nil
               :documentation "Sets angle mode to gradians")
  (set-angle-mode :GRADIANS))



(define-op-key
    (:location (make-location
                :row 4
                :col 5
                :category-1 :STACK-MANIPULATION)
               :modelist (:NUMERIC-INPUT)
               :updates-last-x nil
               :abbreviation "clx"
               :implicit-x nil
               :rational-safe t
               :documentation "Resets the input line")
  :RETCODE <- '(:TOKEN :CLX))


(define-op-key
    (:location (make-location
                :row 4
                :col 5
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "clx"
               :rational-safe t
               :documentation "Erases the X register")
  X <- 0)


(define-op-key
    (:location (make-location
                :row 4
                :col 4
                :shift :F-YELLOW
                :category-1 :PROGRAM-MEMORY)
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "clprog"
               :implicit-x nil
               :documentation "Erases all program space, and resets modes")
  (clear-program))


(define-op-key
    (:location (make-location
                :row 4
                :col 4
                :shift :H-BLACK
                :category-1 :PROGRAM-MEMORY)
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "del"
               :implicit-x nil
               :documentation "Deletes current program step")
  :RETCODE <- '(:DELETE-CURRENT-STEP))



(define-op-key 
    (:location (make-location
                :row 5
                :col 1
                :narrow-key t
                :category-1 :ARITHMETIC)
               :abbreviation "-" 
               :rational-safe t
               :documentation "Subtracts X from Y")
  X <- (- Y X))

(define-op-key 
    (:location (make-location
                :row 5
                :col 1
                :narrow-key t
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x=0" 
               :rational-safe t
               :documentation "Skips next step unless X == 0")
  (unless (= X 0)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  X <- X)


(define-op-key 
    (:location (make-location
                :row 5
                :col 1
                :narrow-key t
                :shift :G-BLUE
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x=y" 
               :rational-safe t
               :documentation "Skips next step unless X == Y")
  (unless (= X Y)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  Y <- Y
  X <- X)


(define-op-key
    (:location (make-location
                :row 5
                :col 1
                :narrow-key t
                :shift :H-BLACK
                :category-1 :FLAGS)
               :updates-last-x nil
               :takes-argument t
               :implicit-x nil
               :abbreviation "sf"
               :documentation "Sets a flag")
  (set-flag ARG))



(define-op-key 
    (:location (make-location
                :row 5
                :col 2
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "7" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 7")
  :RETCODE <- '(:TOKEN :7))


(define-op-key 
    (:location (make-location
                :row 5
                :col 2
                :shift :F-YELLOW
                :category-1 :TRANSCENDENTAL)
               :abbreviation "ln" 
               :rational-safe nil
               :documentation "The natural logarithm of X")
  X <- (log X))

(define-op-key 
    (:location (make-location
                :row 5
                :col 2
                :shift :G-BLUE
                :category-1 :TRANSCENDENTAL)
               :abbreviation "e^x" 
               :rational-safe nil
               :documentation "e to the power of X")
  X <- (exp X))


(define-op-key 
    (:location (make-location
                :row 5
                :col 2
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "x<>y"
               :rational-safe t
               :documentation "Swap X and Y")
  X <- Y
  Y <- X)


(define-op-key 
    (:location (make-location
                :row 5
                :col 3
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "8" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 8")
  :RETCODE <- '(:TOKEN :8))


(define-op-key 
    (:location (make-location
                :row 5
                :col 3
                :shift :F-YELLOW
                :category-1 :TRANSCENDENTAL)
               :abbreviation "log" 
               :rational-safe nil
               :documentation "The base-10 logarithm of X")
  X <- (log X 10.0d0))

(define-op-key 
    (:location (make-location
                :row 5
                :col 3
                :shift :G-BLUE
                :category-1 :TRANSCENDENTAL)
               :abbreviation "10^x" 
               :rational-safe nil
               :documentation "10 to the power of X")
  X <- (expt 10.0d0 X))


(define-op-key 
    (:location (make-location
                :row 5
                :col 3
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "rolld"
               :rational-safe t
               :implicit-x nil
               :documentation "Roll stack down")
  (roll-stack-down))


(define-op-key 
    (:location (make-location
                :row 5
                :col 4
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "9" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 9")
  :RETCODE <- '(:TOKEN :9))


(define-op-key 
    (:location (make-location
                :row 5
                :col 4
                :shift :F-YELLOW
                :category-1 :ALGRBRAIC)
               :abbreviation "sqrt" 
               :rational-safe nil
               :documentation "The square root of X")
  X <- (sqrt X))

(define-op-key 
    (:location (make-location
                :row 5
                :col 4
                :shift :G-BLUE
                :category-1 :ALGEBRAIC)
               :abbreviation "x^2" 
               :rational-safe t
               :documentation "The square of X")
  X <- (* X X))


(define-op-key 
    (:location (make-location
                :row 5
                :col 4
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "rollup"
               :rational-safe t
               :implicit-x nil
               :documentation "Roll stack up")
  (roll-stack-up))




(define-op-key 
    (:location (make-location
                :row 6
                :col 1
                :narrow-key t
                :category-1 :ARITHMETIC)
               :abbreviation "+"
               :rational-safe t
               :documentation "Adds X to Y")
  X <- (+ Y X))

(define-op-key 
    (:location (make-location
                :row 6
                :col 1
                :shift :F-YELLOW
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x/=0"
               :rational-safe t
               :documentation "Skips next step unless X is not zero")
  (unless (/= X 0)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  X <- X)


(define-op-key 
    (:location (make-location
                :row 6
                :col 1
                :shift :G-BLUE
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x/=y"
               :rational-safe t
               :documentation "Skips next step unless X is not equal to Y")
  (unless (/= X Y)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  Y <- Y
  X <- X)


(define-op-key
    (:location (make-location
                :row 6
                :col 1
                :shift :H-BLACK
                :category-1 :FLAGS)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "cf"
               :implicit-x nil
               :documentation "Clears a flag")
  (clear-flag ARG))


(define-op-key 
    (:location (make-location
                :row 6
                :col 2
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "4" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 4")
  :RETCODE <- '(:TOKEN :4))


(define-op-key 
    (:location (make-location
                :row 6
                :col 2
                :shift :F-YELLOW
                :category-1 :TRIGONOMETRY)
               :abbreviation "sin" 
               :rational-safe nil
               :documentation "The sine of X")
  X <- (sin (to-radians X)))

(define-op-key 
    (:location (make-location
                :row 6
                :col 2
                :shift :G-BLUE
                :category-1 :TRIGONOMETRY
                :category-2 :INVERSE-TRIG)
               :abbreviation "asin" 
               :rational-safe nil
               :documentation "The arcsine of X")
  X <- (from-radians (asin X)))


(define-op-key 
    (:location (make-location
                :row 6
                :col 2
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :abbreviation "1/x"
               :rational-safe t
               :documentation "The reciprocal of X")
  X <- (/ 1 X))




(define-op-key 
    (:location (make-location
                :row 6
                :col 3
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "5" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 5")
  :RETCODE <- '(:TOKEN :5))


(define-op-key 
    (:location (make-location
                :row 6
                :col 3
                :shift :F-YELLOW
                :category-1 :TRIGONOMETRY)
               :abbreviation "cos" 
               :rational-safe nil
               :documentation "The cosine of X")
  X <- (cos (to-radians X)))

(define-op-key 
    (:location (make-location
                :row 6
                :col 3
                :shift :G-BLUE
                :category-1 :TRIGONOMETRY
                :category-2 :INVERSE-TRIG)
               :abbreviation "acos" 
               :rational-safe nil
               :documentation "The arccosine of X")
  X <- (from-radians (acos X)))


(define-op-key 
    (:location (make-location
                :row 6
                :col 3
                :shift :H-BLACK
                :category-1 :TRANSCENDENTAL)
               :abbreviation "y^x"
               :rational-safe nil
               :documentation "Power")
  X <- (expt Y X))



(define-op-key 
    (:location (make-location
                :row 6
                :col 4
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "6" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 6")
  :RETCODE <- '(:TOKEN :6))


(define-op-key 
    (:location (make-location
                :row 6
                :col 4
                :shift :F-YELLOW
                :category-1 :TRIGONOMETRY)
               :abbreviation "tan" 
               :rational-safe nil
               :documentation "The tangent of X")
  X <- (tan (to-radians X)))

(define-op-key 
    (:location (make-location
                :row 6
                :col 4
                :shift :G-BLUE
                :category-1 :TRIGNOMETRY
                :category-2 :INVERSE-TRIG)
               :abbreviation "atan" 
               :rational-safe nil
               :documentation "The arctangent of X")
  X <- (from-radians (atan X)))


(define-op-key 
    (:location (make-location
                :row 6
                :col 4
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :abbreviation "abs"
               :rational-safe t
               :documentation "Absolute value")
  X <- (abs X))





(define-op-key 
    (:location (make-location
                :row 7
                :col 1
                :narrow-key t
                :category-1 :ARITHMETIC)
               :abbreviation "*"
               :rational-safe t
               :documentation "Multiplies Y by X")
  X <- (* Y X))

(define-op-key 
    (:location (make-location
                :row 7
                :col 1
                :shift :F-YELLOW
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x<0"
               :rational-safe t
               :documentation "Skips next step unless X is less than zero")
  (unless (< X 0)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  X <- X)


(define-op-key 
    (:location (make-location
                :row 7
                :col 1
                :shift :G-BLUE
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x<=y"
               :rational-safe t
               :documentation "Skips next step unless X is <= Y")
  (unless (<= X Y)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  Y <- Y
  X <- X)


(define-op-key
    (:location (make-location
                :row 7
                :col 1
                :shift :H-BLACK
                :category-1 :FLAGS)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :takes-argument t
               :abbreviation "f?"
               :rational-safe t
               :implicit-x nil
               :documentation "Tests a flag")
  (when (not (get-flag ARG))
    :RETCODE <- '(:SKIP-NEXT-STEP)))




(define-op-key 
    (:location (make-location
                :row 7
                :col 2
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "1" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 1")
  :RETCODE <- '(:TOKEN :1))


(define-op-key 
    (:location (make-location
                :row 7
                :col 2
                :shift :F-YELLOW
                :category-1 :TRIGONOMETRY)
               :abbreviation "p>r" 
               :rational-safe nil
               :documentation "Polar to rectangular conversion")
  X <- (* X (sin (to-radians Y)))
  Y <- (* X (cos (to-radians Y))))


(define-op-key 
    (:location (make-location
                :row 7
                :col 2
                :shift :G-BLUE
                :category-1 :TRIGONOMETRY)
               :abbreviation "r>p" 
               :rational-safe nil
               :documentation "Rectangular to polar conversion")
  X <- (sqrt (+ (* X X) (* Y Y)))
  Y <- (from-radians (atan X Y)))


(define-op-key 
    (:location (make-location
                :row 7
                :col 2
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "pause"
               :rational-safe t
               :implicit-x nil
               :documentation "Pause execution, display X for 1 second")
  :RETCODE <- '(:PAUSE-1-SECOND))




(define-op-key 
    (:location (make-location
                :row 7
                :col 3
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "2" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 2")
  :RETCODE <- '(:TOKEN :2))


(define-op-key 
    (:location (make-location
                :row 7
                :col 3
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :abbreviation "r>d" 
               :rational-safe nil
               :documentation "Radians to degrees conversion")
  X <- (/ (* X 180.0d0) pi))


(define-op-key 
    (:location (make-location
                :row 7
                :col 3
                :shift :G-BLUE
                :category-1 :ARITHMETIC)
               :abbreviation "d>r" 
               :rational-safe nil
               :documentation "Degrees to radians conversion")
  X <- (/ (* X pi) 180.0d0))


(define-op-key 
    (:location (make-location
                :row 7
                :col 3
                :shift :H-BLACK
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "pi"
               :rational-safe nil
               :documentation "Enter pi")
  pi)




(define-op-key 
    (:location (make-location
                :row 7
                :col 4
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "3" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 3")
  :RETCODE <- '(:TOKEN :3))


(define-op-key 
    (:location (make-location
                :row 7
                :col 4
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :abbreviation "hms>h" 
               :rational-safe t
               :documentation "Hours.minsecs to hours")
  (multiple-value-bind (div rem)
      (floor X)
    X <- (+ div (* rem (/ 60 100)))))



(define-op-key 
    (:location (make-location
                :row 7
                :col 4
                :shift :G-BLUE
                :category-1 :ARITHMETIC)
               :abbreviation "h>hms" 
               :rational-safe t
               :documentation "Hours to hours.minsecs")
  (multiple-value-bind (div rem)
      (floor X)
    X <- (+ div (* rem (/ 60 100)))))


(define-op-key 
    (:location (make-location
                :row 7
                :col 4
                :shift :H-BLACK
                :category-1 :MEMORY
                :category-2 :MEMORY-RECALL)
               :updates-last-x nil
               :abbreviation "reg"
               :rational-safe t
               :implicit-x nil
               :documentation "Review primary registers")
  :RETCODE <- '(:REVIEW-REGISTERS))





(define-op-key 
    (:location (make-location
                :row 8
                :col 1
                :narrow-key t
                :category-1 :ARITHMETIC)
               :abbreviation "/"
               :rational-safe t
               :documentation "Divides Y by X")
  X <- (/ Y X))

(define-op-key 
    (:location (make-location
                :row 8
                :col 1
                :shift :F-YELLOW
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x>0"
               :rational-safe t
               :documentation "Skips next step unless X is greater than zero")
  (unless (> X 0)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  X <- X)


(define-op-key 
    (:location (make-location
                :row 8
                :col 1
                :shift :G-BLUE
                :narrow-key t
                :category-1 :FLOW-CONTROL)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "x>y"
               :rational-safe t
               :documentation "Skips next step unless X is > Y")
  (unless (> X Y)
    :RETCODE <- '(:SKIP-NEXT-STEP))
  Y <- Y
  X <- X)


(define-op-key
    (:location (make-location
                :row 8
                :col 1
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :abbreviation "!"
               :rational-safe t
               :implicit-x nil
               :documentation "Factorial")

  (assert (and (integerp X)
               (>= X 0)))
  (let ((result 1))
    (dotimes (i X)
      (setf result (* result (1+ i))))
    X <- result))



(define-op-key 
    (:location (make-location
                :row 8
                :col 2
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "0" 
               :rational-safe t
               :implicit-x nil
               :documentation "The digit key 0")
  :RETCODE <- '(:TOKEN :0))


(define-op-key 
    (:location (make-location
                :row 8
                :col 2
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :abbreviation "%" 
               :rational-safe t
               :documentation "X percent of Y")
  X <- (/ (* X Y) 100))


(define-op-key 
    (:location (make-location
                :row 8
                :col 2
                :shift :G-BLUE
                :category-1 :ARITHMETIC)
               :abbreviation "%chg" 
               :rational-safe t
               :documentation "Percent of Y by which X differs from Y")
  X <- (/ (* 100 (- X Y)) Y))


(define-op-key 
    (:location (make-location
                :row 8
                :col 2
                :shift :H-BLACK
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "lastx"
               :rational-safe t
               :documentation "Retrieve the last X value")
  (get-last-x))




(define-op-key 
    (:location (make-location
                :row 8
                :col 3
                :category-1 :NUMERIC)
               :updates-last-x nil
               :abbreviation "." 
               :rational-safe t
               :implicit-x nil
               :documentation "The decimal point key")
  :RETCODE <- '(:TOKEN :DOT))


(define-op-key 
    (:location (make-location
                :row 8
                :col 3
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :abbreviation "int" 
               :rational-safe t
               :documentation "Integer component of number")
  (let ((n (if (< X 0) -1 1)))
    X <- (* n (floor (abs X)))))


(define-op-key 
    (:location (make-location
                :row 8
                :col 3
                :shift :G-BLUE
                :category-1 :ARITHMETIC)
               :abbreviation "frac" 
               :rational-safe t
               :documentation "Fractional component of number")
  (let ((n (if (< X 0) -1 1)))
    (multiple-value-bind (whole frac)
        (floor (abs X))
      (declare (ignore whole))
      X <- (* n frac))))


(define-op-key 
    (:location (make-location
                :row 8
                :col 3
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :abbreviation "hms+"
               :rational-safe t
               :documentation "Hours.minsecs addition")
  (multiple-value-bind (x-whole x-frac)
      (floor X)
    (setf x-frac (* x-frac (/ 100 60)))
    (multiple-value-bind (y-whole y-frac)
        (floor Y)
      (setf y-frac (* y-frac (/ 100 60)))

      (let ((sum (+ x-whole x-frac y-whole y-frac)))
        (multiple-value-bind (sum-whole sum-frac)
            (floor sum)
          X <- (+ sum-whole (* sum-frac (/ 60 100))))))))



(define-op-key 
    (:location (make-location
                :row 8
                :col 4
                :category-1 :FLOW-CONTROL)
               :modelist (:RUN-MODE
                          :PROGRAM-EXECUTION)
               :updates-last-x nil
               :abbreviation "r/s" 
               :rational-safe t
               :implicit-x nil
               :documentation "Run/stop the program")
  :RETCODE <- '(:RUN-STOP))


(define-op-key 
    (:location (make-location
                :row 8
                :col 4
                :shift :F-YELLOW
                :category-1 :STACK-MANIPULATION)
               :modelist (:PROGRAM-EXECUTION
                          :PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "-x-" 
               :rational-safe t
               :implicit-x nil
               :documentation "Pause and display X for 5 seconds")
  :RETCODE <- '(:PAUSE-5-SECONDS))


(define-op-key 
    (:location (make-location
                :row 8
                :col 4
                :shift :G-BLUE
                :category-1 :STACK-MANIPULATION)
               :updates-last-x nil
               :abbreviation "stack" 
               :rational-safe t
               :implicit-x nil
               :documentation "Display stack")
  :RETCODE <- '(:DISPLAY-STACK))


(define-op-key 
    (:location (make-location
                :row 8
                :col 3
                :shift :H-BLACK
                :category-1 :PROGRAM-MEMORY)
               :modelist (:PROGRAMMING-MODE)
               :updates-last-x nil
               :abbreviation "space"
               :rational-safe t
               :implicit-x nil
               :documentation "NO-OP")
  :RETCODE <- '(:NO-OP))


(define-op-key
    (:location (make-location
                :compound-key '("sto" "+")
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "sto+"
               :rational-safe t
               :documentation "Adds X to a register")
  (let ((prev (recall-mem ARG)))
    (setf prev (+ prev X))
    (store-mem ARG prev)
    X <- X))

(define-op-key
    (:location (make-location
                :compound-key '("sto" "-")
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "sto-"
               :rational-safe t
               :documentation "Subtracts X from a register")
  (let ((prev (recall-mem ARG)))
    (setf prev (- prev X))
    (store-mem ARG prev)
    X <- X))


(define-op-key
    (:location (make-location
                :compound-key '("sto" "*")
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "sto*"
               :rational-safe t
               :documentation "Multiplies X into a register")
  (let ((prev (recall-mem ARG)))
    (setf prev (* prev X))
    (store-mem ARG prev)
    X <- X))

(define-op-key
    (:location (make-location
                :compound-key '("sto" "/")
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :updates-last-x nil
               :abbreviation "sto/"
               :rational-safe t
               :documentation "Divides X into a register")
  (let ((prev (recall-mem ARG)))
    (setf prev (/ prev X))
    (store-mem ARG prev)
    X <- X))

