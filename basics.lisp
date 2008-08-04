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

(defun read-entire-file (pathname)
  (with-open-file (s pathname)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;;;; Some constants

(defconstant .last-reserved-field-descriptor-number. 19000)

(defconstant .first-reserved-field-descriptor-number. 19999)

(defconstant .maximum-field-descriptor-number. (1- (expt 2 29)))
