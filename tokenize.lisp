;;; -*- mode: lisp -*-

;; Copyright © 2008 Ben Hyde, Licensed under the Apache License, Version 2.0.

(in-package "PBUFS")

;;; Globals used during lexical analysis (not thread safe)

(defvar *text-to-parse*)
(defvar *start-of-token*)
(defvar *end-of-token*)
(defvar *token-kind*)
(defvar *token-value*)

(defun initialize-tokenizer (string)
  (setf *text-to-parse* string
        *start-of-token* 0))

;;;; *token-kinds* is the set of tokens, each ones has a PPCRE parse-tree-synonym.

(defvar-for-macro *token-kinds* ())

;;;; Defining Tokens for use by CL-YACC

(defmacro deftoken (kind (pattern) &body body)
  (let ((pattern (if (stringp pattern)
                     (cl-ppcre::parse-string pattern)
                     pattern))
        (handler (gensym)))
    `(progn
       (defun ,handler (pos)
         (setf *token-kind* ,kind)
         (let ((*end-of-token* pos))
           (setf *token-value* (progn ,@body)
                 *start-of-token* *end-of-token*)))
       (define-parse-tree-synonym ,kind
           (:sequence ,pattern (:filter ,handler)))
       (pushnew ,kind *token-kinds*))))

(defun tokenizer-of-string (string) ;; this is not thread safe :)
  (initialize-tokenizer string)
  (let ((scanner (create-scanner (list :sequence
                                       :start-anchor
                                       (list* :alternation
                                              (reverse *token-kinds*))))))
    #'(lambda ()
        (setf *token-kind* nil)
        (scan scanner *text-to-parse* :start *start-of-token*)
        (values *token-kind* *token-value*))))

(defun token-text ()
  (subseq *text-to-parse* *start-of-token* *end-of-token*))

(defun scan-quoted-string ()
  (loop
     for i from *end-of-token* to (length *text-to-parse*)
     as c = (char *text-to-parse* i)
     do
       (cond
         ((eq #\\ c)
          (incf i))
         ((eq #\" c)
          (setf *token-kind* :string-value)
          (setf *token-value* (subseq *text-to-parse* *end-of-token* i))
          (setf *start-of-token* *end-of-token*)
          (setf *end-of-token* i)
          (return *token-kind*)))))

(defun test-tokenizer (string)
  (loop 
     with f = (tokenizer-of-string string)
     do
       (funcall f)
       (unless *token-kind* (return))
       (format t "~&~S ~S" *token-kind* *token-value*)))
