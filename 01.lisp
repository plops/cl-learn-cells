;; http://stefano.dissegna.me/cells-tutorial.html
(ql:quickload "cells")

(defpackage #:g
  (:use #:cl #:cells))

(in-package #:g)
(defmodel hello-cells ()
  ((num :accessor num
	:initarg :num
	:initform (c-in 0))
   (square-num :accessor square-num
	       :initform (c? (* (num self)
				(num self))))))

(let ((h (make-instance 'hello-cells)))
  (dolist (n '(10 20 30 40))
    (setf (num h) n)
    (format t "~&~a" (list (num h) (square-num h)))))

(cells-reset)

(defobserver num ((self hello-cells))
  (format t "~&new value of num is: ~a~%" new-value))
