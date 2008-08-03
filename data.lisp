;;; -*- mode: lisp -*-

(in-package "PBUFS")

;;; We parse the proto file into a tree of classes, top down.

;;; The root has the useless name "%root%" and all the basic types
;;; are defined in it.  Each type we read a file, or a package; we
;;; create a package-namespace in the root to accumulate it.  Inside
;;; that one will find the messages, etc. defined in that file.

;;; This tree is built top down during parsing.  For example when we
;;; see the token "message" we create a instance of message, and
;;; as we see the field declarations create then, insert them into
;;; the message, etc.

(defvar *current-namespace* nil)

(defclass namespace-node ()
  ((parent? :type namespace-node
            :initform *current-namespace*
            :accessor parent?-of-namespace-node)))

(defclass namespace (namespace-node)
  ((elements :type list
             :accessor elements-of-namespace
             :initform nil)))

(defclass namespace-element (namespace-node)
  ((spelling :type string 
             :accessor spelling-of-namespace-element
             :initarg :spelling)))

(defmethod print-object ((x namespace-element) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (with-slots (spelling) x
      (format stream "~A" (if (slot-boundp x 'spelling) spelling '<name-unbound>)))))


(defclass unknown-namespace-element (namespace-element)
  ())

(defclass basetype-namespace-element (namespace-element)
  ())

(defclass root-namespace (namespace namespace-element)
  ())

(defclass package-namespace (namespace namespace-element)  ;; can't use "package"
  ())

(defclass root-namespace (namespace namespace-element)
  ())

(defclass namespace-of-unknown-names (namespace namespace-element)
  ())

(defclass extension-range-declaration ()
  ((lower-limit :type integer :initarg :lower-limit)
   (upper-limit :type integer :initarg :upper-limit)))
                
(defclass message (namespace namespace-element)
  ((declared-extension-range? :type extension-range-declaration
                             :accessor declared-extension-range?-of-message
                             :initform nil)))

(defclass process (namespace namespace-element)
  ())

(defclass field (namespace-element)
  ((nary :type (member :required :optional :repeated)
         :accessor nary-of-namespace-element
         :initarg :nary)
   (type :type namespace-element
         :initarg :type)
   (index :type fixnum)
   (default-value :type t 
     :accessor default-value-of-field
     :initarg :default-value)))

(defclass enumeration (namespace namespace-element)
  ())

(defclass enumeration-constant (namespace-element)
  ((value :accessor value-of-enumeration-constant
          :initarg :value)))


(defclass service (namespace)
  ())

(defclass rpc-declaration (namespace-element)
  ((query :type message
          :initarg :query  
          :accessor query-of-namespace-element)
   (reply :type message
          :initarg :reply
          :accessor reply-of-namespace-element)))

(defmethod initialize-instance :after ((name namespace-element) &rest args)
  (declare (ignore args))
  (unless (typep name 'root-namespace)
    (push name (elements-of-namespace (parent?-of-namespace-node name)))))
  
(defvar *root-namespace* (make-instance 'root-namespace :spelling "%root%"))

(defun find-name (spelling namespace)
  (labels ((recure (namespace)
             (or (find spelling (elements-of-namespace namespace) 
                       :key #'spelling-of-namespace-element
                       :test #'string=)
                 (and (parent?-of-namespace-node namespace)
                      (recure (parent?-of-namespace-node namespace))))))
    (recure namespace)))

(defun intern-name (spelling namespace)
  (or (find-name spelling namespace)
      (make-instance 'unknown-namespace-element
                     :spelling spelling
                     :parent? namespace)))

(defun intern-name-path (name-path namespace)
  (labels ((recure (path namespace)
             (let* ((spelling (first path))
                    (name? (find-name spelling namespace))) ;; note a
               (cond
                 ((rest path)
                  (assert (typep name? 'namespace))
                  (recure (rest path) namespace))
                 (t
                  name?)))))
    (recure (ppcre:split #\. name-path) namespace)))


;; note a: find-name is recursive so this semantics cool or questionable.

