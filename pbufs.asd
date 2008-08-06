;;; -*- mode: lisp -*-

;; Copyright © 2008 Ben Hyde, Licensed under the Apache License, Version 2.0.

(defsystem pbufs
    :author "Ben Hyde"
    :licence "Apache 2.0"
    :depends-on (cl-ppcre yacc)
    :serial t
    :components ((:file "packages")
                 (:file "basics")
                 (:file "data")
                 (:file "tokenize")
                 (:file "parse")
                 ))
