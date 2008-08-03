;;; -*- mode: lisp -*-

(in-package "PBUFS")

;;;; Some utilities

(defmacro defvar-for-macro (name &optional (value nil value-p) (doc nil doc-p))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar ,name 
       ,@(when value-p `(,value))
       ,@(when doc-p `(,doc)))))

(defmacro defun-for-macro (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun ,name ,args ,@body)))
