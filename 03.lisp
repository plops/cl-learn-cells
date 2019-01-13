;; sudoku solver non-lazy

(defconstant +row-len+ 9)
(defconstant +col-len+ 9)
(defconstant +sq-size+ 3)

(defconstant +all-values+ '(1 2 3 4 5 6 7 8 9))

(defparameter *board*
  (make-array (list +row-len+ +col-len+)
	      :element-type '(unsigned-byte 8)
	      :initial-element 0))


(defmodel square-group ()
  ((constraining
    :initform (c-in nil)
    :initarg :constraining
    :accessor constraining)))

(defmodel square ()
  ((group :accessor group :initform (c-in nil))
   (exact-val :accessor exact-val :initform (c-in nil)
	      :initarg :exact-val)
   (possible-vals :accessor possible-vals
		  :initform
		  (c?
		    (when (and (group self)
			       (not (exact-val self)))
		      (let ((c (constraining (group self))))
			(remove-if-not
			 #'(lambda (v)
			     (every
			      #'(lambda (x)
				  (not (eql v (exact-val x))))
			      c))
			 +all-values+)))))))

(defun make-square (x)
  (make-instance 'square :exact-val (c-in (if (eql x 0) nil x))))

(defmodel board ()
  ((complete :accessor complete
	     :initform
	     (c-formula (:lazy :always)
	       (every #'(lambda (x)
			  (every #'exact-val x))
		      (squares self))))
   (squares :accessor squares :initarg :squares)))

(defmethod print-object ((self board) out)
  (loop for r below +row-len+ do
       (loop for c below +col-len+ do
	    (format out "~a " (exact-val (at (squares self) r c)))
	    (format out "~%"))))


(defun at (board r c)
  (elt (elt board r) c))
