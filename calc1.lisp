;; Defining the keys in the calculator.

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


