(ql:quickload "cells")

(defpackage #:g
  (:use #:cl #:cells))

(in-package #:g)

(defmodel node (family)
  ((val :initform (c-in nil) :initarg :val)))

(defun math-op-family ()
  (let ((root (make-instance 'node
			     :val (c? (apply #'+ (mapcar #'val (kids self))))
			     :kids (c? (the-kids
					(make-kid 'node :md-name :n5
						  :val (c-in 5))
					(make-kid 'node :val
						  (c? (apply #'* (mapcar #'val
									 (kids self))))
						  :kids (c? (the-kids
							     (make-kid 'node :md-name :n7 :val (c-in 7))
							     (make-kid 'node :md-name :n9 :val (c-in 9))))))))))
    (format t "value of tree=~a~%" (val root))
    (setf (val (fm-other :n7 :starting root)) 10)
    (format t "value of tree=~a~%" (val root))))


(math-op-family)


(defmodel str-model ()
  ((str :accessor str :initform (c-in "") :initarg :str
	:unchanged-if #'equal)
   (rev-str :accessor rev-str :initform (c? (reverse (str self))))))

(defobserver str ()
  (format t "changed!"))

(defun try-str-model ()
  (let ((s (make-instance 'str-model)))
    (dolist (l `("Hello!" "Bye"
			  ,(concatenate 'string "By" "e")))
      (setf (str s) l)
      (format t "str is ~a rev-str is ~a~%"
	      (str s) (rev-str s))))) 

#+nil
(try-str-model)
