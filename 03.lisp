;; sudoku solver non-lazy
(ql:quickload "cells")

(defpackage #:g
  (:use #:cl #:cells))

(in-package #:g)

(defconstant +row-len+ 9)
(defconstant +col-len+ 9)
(defconstant +sq-size+ 3)

(defconstant +all-values+ '(1 2 3 4 5 6 7 8 9))

(defparameter *board*
  (loop for j below +col-len+ collect
   (loop for i below +row-len+ collect 0))
  #+nil
  (make-array (list +row-len+ +col-len+)
	      :element-type '(unsigned-byte 8)
	      :initial-element 0))

;; similar to 2d array access i will always use sequence row, column
;; to access the board, even if it is stored as a list of lists

(defmodel square-group ()
  ((unique-squares
    :initform (c-in nil)
    :initarg :unique-squares
    :accessor unique-squares)))

(defmodel square ()
  ((group :accessor group :initform (c-in nil))
   (exact-val :accessor exact-val :initform (c-in 0)
	      :initarg :exact-val)
   (possible-vals :accessor possible-vals
		  :initform
		  (c?
		    (when (and (group self)
			       (eq 0 (exact-val self)))
		      (let ((u (unique-squares (group self))))
			(loop for e in +all-values+
			   unless
			     (member e u)
			   collect
			     e)))))))

(defun make-square (x)
  ;; exact-val is 0 when it hasn't been assigned
  (make-instance 'square :exact-val (c-in x)))

(defmodel board ()
  ;; complete-p is True when every position has been filled, i.e
  ;; no entry should be 0
  ((complete-p :accessor complete-p
	     :initform
	     (c-formula (:lazy :always)
	       (loop for e in (squares self)
		  when (eq 0 e) return nil
		    finally (return t))))
   (squares :accessor squares :initarg :squares)))

(defmethod print-object ((self board) out)
  (loop for r below +row-len+ do
       (loop for c below +col-len+ do
	    (format out "~a " (exact-val (at (squares self) r c)))
	    (format out "~%"))))


(defun at (board r c)
  (elt (elt board r) c))


(defun next-pos (pos)
  ;; search forward for next position, nil if finished
  (destructuring-bind (r c) pos
    (if (eq (- +row-len+ 1) r)
	(if (eq (- +col-len+ 1))
	    nil
	    (list (+ r 1) 0))
	(list r (+ c 1)))))


(defun next-to-try (board pos)
  ;; search forward for next empty position, nil if finished
  (let ((pos (next-pos pos)))
    (when pos
      (destructuring-bind (r c) pos
	(if (exact-val (at board r c))
	    (next-to-try board pos)
	    pos)))))

(defun nth-col (board n)
  (coerce (elt board n) 'list))

(defun nth-row (board n)
  (map 'list #'(lambda (x) (elt x n)) board))

(defun nth-block (board r c)
  (let ((rmi (* +sq-size+ (floor r +sq-size+)))
	(cmi (* +sq-size+ (floor c +sq-size+))))
    (loop for col in (subseq board rmi (+ rmi +sq-size+))
       append
	 (subseq col cmi (+ cmi +sq-size+)))))


(defun make-groups (squares)
  (loop for r below +row-len+ do
       (loop for c below +col-len+ do
	    (setf (group (at squares r c))
		  (make-instance 'square-group
				 :unique-squares
				 (c-in
				  (delete-duplicates
				   (nconc
				    (nth-col squares r)
				    (nth-row squares c)
				    (nth-block squares r c)))))))))
(defun make-board (b)
  (let ((bb (make-instance
	     'board
	     :squares
	     (c-in
	      (map 'vector
		   #'(lambda (x)
		       (map 'vector #'make-square x))
		   b)))))
    (make-groups (squares bb))
    bb))

(defun search-solution
    (b &key (next
	     (next-to-try (squares b)
			  (list 0 -1))))
  (if next
      (destructuring-bind (r c) next
	(let ((s (at (squares b) r c)))
	  ;; find first possible value
	  (loop for e in (possible-vals s)
	     do
	       (setf (exact-val s) e)
	       (when (search-solution
		      b
		      :next
		      (next-to-try (squares b) next))
		 (return t))
	     finally
	       (setf (exact-val s) 0)
	       (return nil))))
      ;; tried all positions, should be complete
      (complete-p b)
      ))

(defun sudoku (the-board)
  (let ((b (make-board the-board)))
    (search-solution b)
    (format t "solution:~%~a~%" b)))


(sudoku *board*)
