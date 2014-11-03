;; Defining the keys in the calculator.

(define-toprow-key (1 #\A "1/x" "Reciprocal")
  X <- (/ 1.0d0 X))

(define-toprow-key (2 #\B "sqrt" "Square root")
  X <- (sqrt (to-double-fp X)))

(define-toprow-key (3 #\C "y^x" "Power")
  X <- (expt Y X))

(define-toprow-key (4 #\D "rolld" "Roll stack down"
                      :implicit-x nil
                      :updates-last-x nil)
  (roll-stack-down))

(define-toprow-key (5 #\E "x<>y" "Exchange X and Y"
                      :updates-last-x nil)
  X <- Y
  Y <- X)


(define-op-key
    (:location (make-location
                :row 2
                :col 1
                :category-1 :STATISTICS)
               :abbreviation "sigma+"
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
               :abbreviation "mean"
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
               :modelist '(:RUN-MODE)
               :takes-argument t
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "goto"
               :documentation "Branches to location specified")
  (let (ival)
    (when (string-equal ARG "(i)")
      (setf ival (floor (recall-mem "(i)"))))
    (cond
      ((and ival (<= 0 ival 9))
       :RETCODE <- '(:GOTO (format nil "~D" ival)))
      ((and ival (<= 10 ival 14))
       :RETCODE <- '(:GOTO (subseq "ABCDE" (- ival 10) (- ival 9))))
      ((and ival (<= 15 ival 19))
       :RETCODE <- '(:GOTO (subseq "abcde" (- ival 10) (- ival 9))))
      (t
       :RETCODE <- '(:GOTO ARG)))))


(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :shift :F-YELLOW
                :category-1 :FLOW-CONTROL)
               :modelist '(:RUN-MODE)
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
       :RETCODE <- '(:GOSUB (format nil "~D" ival)))
      ((and ival (<= 10 ival 14))
       :RETCODE <- '(:GOSUB (subseq "ABCDE" (- ival 10) (- ival 9))))
      ((and ival (<= 15 ival 19))
       :RETCODE <- '(:GOSUB (subseq "abcde" (- ival 10) (- ival 9))))
      (t
       :RETCODE <- '(:GOSUB ARG)))))


(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :shift :G-BLUE
                :category-1 :FLOW-CONTROL)
               :modelist '(:RUN-MODE)
               :takes-argument t
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "gosub"
               :documentation "JSRs to location specified")
  (assert (not (string-equal "(i)" ARG)))
  :RETCODE <- '(:GOSUB ARG))


(define-op-key
    (:location (make-location
                :row 2
                :col 2
                :shift :H-BLACK
                :category-1 :FLOW-CONTROL)
               :modelist '(:RUN-MODE)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "rtn"
               :documentation "Returns from subrouting")
  :RETCODE <- '(:RETURN-FROM-SUBROUTINE))


(define-op-key
    (:location (make-location
                :row 2
                :col 3
                :category-1 :MODE-SWITCH)
               :modelist '(:RUN-MODE)
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
               :modelist '(:RUN-MODE)
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
               :modelist '(:RUN-MODE)
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
               :modelist '(:RUN-MODE)
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
               :modelist '(:RUN-MODE)
               :updates-last-x nil
               :implicit-x nil
               :abbreviation "(i)"
               :documentation "Inserts the indirection-I argument")
  :RETCODE <- '(:TOKEN "(i)"))


(define-op-key
    (:location (make-location
                :row 2
                :col 4
                :shift :F-YELLOW
                :category-1 :ARITHMETIC)
               :modelist '(:RUN-MODE)
               :abbreviation "rnd"
               :documentation "Rounds X to the displayed precision")

  X <- (round-to-display-precision X))


(define-op-key
    (:location (make-location
                :row 2
                :col 4
                :shift :H-BLACK
                :category-1 :ARITHMETIC)
               :modelist '(:RUN-MODE)
               :abbreviation "x<>i"
               :documentation "Swaps registers X and I")
  (let ((tmp (recall-mem "(i)")))
    (store-mem "(i)" X)
    X <- tmp))


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

(define-op-key
    (:location (make-location
                :row 5
                :col 1
                :shift :H-BLACK
                :category-1 :FLAGS)
               :takes-argument t
               :abbreviation "SF"
               :documentation "Sets a flag")
  (set-flag ARG)
  X <- X)

(define-op-key
    (:location (make-location
                :row 6
                :col 1
                :shift :H-BLACK
                :category-1 :FLAGS)
               :takes-argument t
               :abbreviation "CF"
               :documentation "Clears a flag")
  (clear-flag ARG)
  X <- X)

(define-op-key
    (:location (make-location
                :row 7
                :col 1
                :shift :H-BLACK
                :category-1 :FLAGS)
               :takes-argument t
               :abbreviation "F?"
               :documentation "Tests a flag")
  (when (not (get-flag ARG))
    :RETCODE <- '(:SKIP-NEXT-STEP))
  X <- X)

(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :takes-argument t
               :abbreviation "STO"
               :documentation "Saves a memory register")
  (store-mem ARG X)
  X <- X)

(define-op-key
    (:location (make-location
                :row 3
                :col 4
                :category-1 :MEMORY
                :category-2 :MEMORY-RECALL)
               :takes-argument t
               :abbreviation "RCL"
               :documentation "Saves a memory register")
  (recall-mem ARG))


(define-op-key
    (:location (make-location
                :row 3
                :col 3
                :shift :H-BLACK
                :category-1 :MEMORY
                :category-2 :MEMORY-STORE)
               :abbreviation "STI"
               :documentation "Saves by indirection")
  (store-mem "(i)" X)
  X <- X)

(define-op-key
    (:location (make-location
                :row 3
                :col 4
                :shift :H-BLACK
                :category-1 :MEMORY
                :category-2 :MEMORY-RECALL)
               :abbreviation "RCI"
               :documentation "Recalls by indirection")
  (recall-mem "(i)"))




