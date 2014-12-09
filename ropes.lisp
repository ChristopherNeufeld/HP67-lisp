;; An implementation of a rope data structure

;; This will be used for inexpensive insert/delete of indexed
;; sequences of objects.  Could be characters in a string, could
;; be something else.  In the HP-67 calculator, I'll be using it
;; to store program steps, each of which is a string.  The
;; objects will be treated as indivisible, so if each object is a
;; string, the strings are separately indivisible.

(declaim (optimize (debug 3) (safety 3)))

(defpackage :HP67-ROPES
  (:use :COMMON-LISP)
  (:export
   :MAKE-ROPE
   :ROPE-LENGTH
   :INSERT-OBJECT
   :DELETE-OBJECT
   :APPEND-OBJECT
   :ITERATE-OVER-CONTENTS
   :GET-FULL-CONTENTS
   :FLATTEN
   ))

(in-package :HP67-ROPES)

(defstruct (node)
  (weight		0)
  (parent		nil)
  (left-child		nil)
  (right-child		nil)
  (contents		(make-array 0 :adjustable t)))

(defmethod print-object ((node node) stream)
  (format stream
          "NODE:~%~Tweight= ~D~%~Tleft= ~A~%~Tright= ~A~%~Tcontents= ~A~%"
          (node-weight node)
          (node-left-child node) (node-right-child node)
          (node-contents node)))

(defstruct (rope)
  (root-node		(make-node)))


(defun rope-length (rope)
  (labels
      ((worker (node)
         (cond
           ((not node) 0)
           (t
            (+ (node-weight node)
               (worker (node-right-child node)))))))

    (worker (rope-root-node rope))))


(defun insert-object (rope index object)
  "Attempts to insert the object into the rope, so that the
   object is at offset 'index'.  The 'index' object, and all
   higher ones, all shift up by one.  If the index is more than
   one past the end of the rope, nothing happens.  Returns two
   values.  The first is the object, or nil on an error.  The
   second is non-nil only if there was a successful insertion."

  (unless (and rope (integerp index) (>= index 0))
    (return-from insert-object (values nil nil)))
  
  (when (= index (1+ (rope-length rope)))
    (append-object rope object)
    (return-from insert-object
      (values object t)))

  (multiple-value-bind
        (before after)
      (split-rope rope index)
    (append-object before object)
    (setf (rope-root-node rope)
          (rope-root-node (concatenate-ropes before after))))

  (fix-tree rope)
  (values object t))



(defun right-uncle (node)
  "Returns the node that is the first right child of a parent
   that doesn't return to myself, or nil if there isn't one."
  (when (node-parent node)
    (cond
      ((eq node (node-left-child (node-parent node)))
       (node-right-child (node-parent node)))
      (t
       (right-uncle (node-parent node))))))


(defun leftmost-leaf (node)
  "Returns the leftmost leaf under the node.  Could be self, if
   there are no children."
  (let ((nlc (node-left-child node))
        (nrc (node-right-child node)))
    (cond
      ((and (not nlc) (not nrc))
       node)
      ((not nlc)
       (leftmost-leaf nrc))
      (t
       (leftmost-leaf nlc)))))


(defun rightmost-leaf (node)
  (cond
    ((node-right-child node)
     (rightmost-leaf (node-right-child node)))
    ((node-left-child node)
     (rightmost-leaf (node-left-child node)))
    (t
     node)))


(defun fix-tree (rope)
  "Does some simple cleanups.  When a node has only one child, it
   can be clipped out."
  (labels
      ((worker (node)
         (let ((nlc (node-left-child node))
               (nrc (node-right-child node)))
           (when (node-parent node)
             (cond
               ((and nlc (not nrc))
                (setf (node-parent nlc) (node-parent node))
                (if (eq (node-left-child (node-parent node)) node)
                    (setf (node-left-child (node-parent node)) nlc)
                    (setf (node-right-child (node-parent node)) nlc)))))
           (when nlc
             (worker nlc))
           (when nrc
             (worker nrc)))))

    (worker (rope-root-node rope))))



(defun delete-object (rope index)
  "Removes the object at the specified index from the rope.
   Returns the object deleted.  If 'index' is out of range, the
   second value returned will be nil."
  (labels
      ((fix-weights-after-delete (node)
         (when (node-parent node)
           (when (eq node (node-left-child (node-parent node)))
             (decf (node-weight (node-parent node))))
           (fix-weights-after-delete (node-parent node)))))
    
    (when (and rope
               (rope-root-node rope)
               (<= 0 index (1- (rope-length rope))))
      (multiple-value-bind (rope-low rope-high)
          (split-rope rope (1+ index))

        (let* ((node (rightmost-leaf (rope-root-node rope-low)))
               (contents (node-contents node))
               (deleted-obj (aref contents (- (node-weight node) 1))))
          (setf (node-contents node)
                (adjust-array (node-contents node) (1- (node-weight node))))
          (decf (node-weight node))
          (fix-weights-after-delete node)
          (setf (rope-root-node rope)
                (rope-root-node (concatenate-ropes rope-low rope-high)))
          (fix-tree rope)
          (values deleted-obj t))))))


(defun append-object (rope object)
  "Appends the object on the end of the rope."
  (labels
      ((worker (node object)
         (cond
           ((node-right-child node)
            (worker (node-right-child node) object))
           ((node-left-child node)
            (incf (node-weight node))
            (worker (node-left-child node) object))
           (t
            (let ((clen (node-weight node)))
              (setf (node-contents node)
                    (adjust-array (node-contents node) (1+ clen)))
              (incf (node-weight node))
              (setf (aref (node-contents node) clen) object))))))

    (worker (rope-root-node rope) object))
  (fix-tree rope)
  rope)



(defun locate-node (node index)
  "Returns the node for which the index would be within, at the
   beginning, or one past end of the contents.  Only returns the
   beginning of a node for index=0, in all other cases returns
   the end of the previous leaf.  The 'index' is assumed to be in
   bounds.  In a second value, returns the offset within the
   node."
  (cond
    ((and (node-left-child node)
          (<= index (node-weight node)))
     (locate-node (node-left-child node) index))
    ((node-right-child node)
     (locate-node (node-right-child node)
                  (- index (node-weight node))))
    (t
     (values node index))))
     


(defun split-rope (rope index)
  "Returns two ropes.  The first holds all elements up to
   index-1 (could be empty), the second holds all elements from
   index onwards."
  (multiple-value-bind (node offset)
      (locate-node (rope-root-node rope) index)
    (unless (= offset (node-weight node))
      (let ((old-weight (node-weight node))
            (new-node-l (make-node :parent node))
            (new-node-r (make-node :parent node)))
        (when (> offset 0)
          (setf (node-contents new-node-l)
                (subseq (node-contents node) 0 offset)))
        (setf (node-contents new-node-r)
              (subseq (node-contents node) offset))
        (setf (node-contents node) nil)
        (setf (node-weight node) offset)
        (setf (node-left-child node) new-node-l)
        (setf (node-right-child node) new-node-r)
        (setf (node-weight new-node-l) offset)
        (setf (node-weight new-node-r) 
              (- old-weight offset))

        (setf node new-node-l
              index (node-weight new-node-l))))

    ;; Now, there is an easy split at 'node'.  That node and all
    ;; lower nodes go in the first rope, and all higher nodes
    ;; in the second.

    (let ((rope-low (make-rope))
          right-1 right-2)
      
      ;; climb up the tree, putting nodes in the right ropes
      (setf (rope-root-node rope-low) node)
      (do ()
          ((not (node-parent node)))
        
        (when (and (eq node (node-left-child (node-parent node)))
                   (node-right-child (node-parent node)))
          (cond
            ((not right-1)
             (setf right-1 (make-rope :root-node
                                      (node-right-child
                                       (node-parent node))))
             (setf (node-right-child (node-parent node)) nil))
            ((not right-2)
             (setf right-2 (make-rope :root-node
                                      (node-right-child
                                       (node-parent node))))
             (setf (node-right-child (node-parent node)) nil))
            (t
             (error "Unexpected flow"))))

        (setf node (node-parent node))
        (setf (rope-root-node rope-low) node))

      (values rope-low (concatenate-ropes right-1 right-2)))))


(defun concatenate-ropes (rope-1 rope-2)
  (cond
    ((or (not rope-1)
         (not (rope-root-node rope-1)))
     rope-2)
    ((or (not rope-2)
         (not (rope-root-node rope-2)))
     rope-1)
    (t
     (let ((node-left (rope-root-node rope-1))
           (node-right (rope-root-node rope-2))
           (new-node (make-node :weight (rope-length rope-1))))

       (setf (node-parent node-left) new-node
             (node-parent node-right) new-node
             (node-left-child new-node) node-left
             (node-right-child new-node) node-right)

       (make-rope :root-node new-node)))))


(defmacro iterate-over-contents ((rope obj) &body body)
  (let ((cur-node (gensym))
        (cur-pos (gensym)))
    `(let ((,cur-node (leftmost-leaf (rope-root-node ,rope)))
           (,cur-pos 0))
       (do ()
           (nil)
         (cond
           ((>= ,cur-pos (node-weight ,cur-node))
            (let ((uncle (right-uncle ,cur-node)))
              (unless uncle
                (return))
              (setf ,cur-node (leftmost-leaf uncle))
              (setf ,cur-pos 0)))
           (t
            (let ((,obj (aref (node-contents ,cur-node) ,cur-pos)))
              (incf ,cur-pos)
              ,@body)))))))

(defun get-full-contents (rope)
  (let ((rval (make-array (rope-length rope)))
        (pos 0))
    (iterate-over-contents (rope entry)
      (setf (aref rval pos) entry)
      (incf pos))
    rval))


(defun flatten (rope)
  (let ((contents (get-full-contents rope)))
    (setf (rope-root-node rope)
          (make-node :weight (length contents)
                     :contents contents))))


(defun sanity-check (rope)
  (labels
      ((recurse-scan (node)
         (let ((nlc (node-left-child node))
               (nrc (node-right-child node)))
           (when nlc
             (let* ((gcr (node-right-child nlc))
                    (gcw (if gcr (node-weight gcr) 0)))
               (assert (= (node-weight node)
                          (+ (node-weight nlc)
                             gcw)))))
           (when (and (not nlc)
                      (not nrc))
             (assert (= (node-weight node)
                        (length (node-contents node)))))

           (when nlc
             (recurse-scan nlc))
           (when nrc
             (recurse-scan nrc)))))

    (recurse-scan (rope-root-node rope))))


