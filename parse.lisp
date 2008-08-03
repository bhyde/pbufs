;;; -*- mode: lisp -*-

(in-package "PBUFS")

;;; Parsing: Dynamic State & Reduction functions

(defvar *namespace-or-unknowns*)

(defvar *current-package*)
(defvar *lastest-thing*)
(defvar *namespace-stack*)

(defmacro make-thing (&rest args) `(setf *lastest-thing* (make-instance ,@args)))

(defun-for-macro reduce-new-name (spelling)
  ;; TBD - check for duplicate name.
  (setf (spelling-of-namespace-element *lastest-thing*) spelling))

(defmacro make-thing-and-push (&rest args) 
  `(progn
     (push (shiftf *current-namespace* (make-thing ,@args)) *namespace-stack*)
     *current-namespace*))

(defun-for-macro reduce-composite (namespace name open-bracket elements close-bracket)
  (declare (ignore name open-bracket elements close-bracket))
  (assert (eq namespace *current-namespace*))
  (shiftf *current-namespace* (pop *namespace-stack*))
  namespace)

(defun-for-macro reduce-existing-name (spelling)
    (find-name spelling *current-namespace* t))

(defun-for-macro reduce-user-type (spelling)
    (let ((type? (find-name spelling *current-namespace*)))
      (assert (typep type? 'message))
      type?))

(defun-for-macro reduce-to-nil (&rest stuff)
  (declare (ignore stuff))
  nil)


(defun-for-macro reduce-enum-value (name-spelling eq the-index)
  (declare (ignore eq))
  (make-instance 'enum-name 
                 :spelling name-spelling
                 :index the-index
                 :parent? *current-namespace*))

(defun-for-macro reduce-field-declaration (field the-type name eq the-index &optional default eq2 the-default?)
  (declare (ignore name eq default eq2))
  (with-slots (type index default) field
    (setf type the-type)
    (setf index the-index)
    (when the-default?
      (setf default the-default?))))

(defconstant .maximum-field-index. 500) ;; TBD

(defun-for-macro reduce-extension (extension the-low to the-high)
  (declare (ignore to))
  (when (eq the-high :max)
    (setf the-high .maximum-field-index.))
  (with-slots (low high) extension
    (setf low the-low)
    (setf high the-low))
  extension)

(defun-for-macro reduce-rpc (rpc name op1 the-query cp1 returns op2 the-reply cp2)
    (declare (ignore name op1 cp1 returns op2 cp2))
    (with-slots (query reply) rpc
      (setf query the-query)
      (setf reply the-reply))
    rpc)

(defun-for-macro reduce-user-typename (spelling)
  (list :TBD-typename spelling))

;;;; Parsing: Define Terminals

(deftoken :|(| (#\() *token-kind*)
(deftoken :|)| (#\)) *token-kind*)
(deftoken :{ (#\{) *token-kind*)
(deftoken :} (#\}) *token-kind*)
(deftoken := (#\=) *token-kind*)
(deftoken :[ (#\[) *token-kind*)
(deftoken :] (#\]) *token-kind*)
(deftoken := (#\=) *token-kind*)
(deftoken :semicolon (#\;) *token-kind*)



(deftoken :package    ("package")    (setf *lastest-thing* *current-package*))
(deftoken :message    ("message")    (make-thing-and-push 'message))
(deftoken :extend     ("extend")     (make-thing-and-push 'extended-message))
(deftoken :enum       ("enum")       (make-thing-and-push 'enum))
(deftoken :service    ("service")    (make-thing-and-push 'service))
(deftoken :rpc        ("rpc")        (make-thing 'rpc))
(deftoken :extensions ("extensions") (make-thing 'extension-range-declaration))
(deftoken :optional   ("optional")   (make-thing 'field :nary :optional))
(deftoken :required   ("required")   (make-thing 'field :nary :required))
(deftoken :repeated   ("repeated")   (make-thing 'field :nary :repeated))
(deftoken :to         ("to")         :to)
(deftoken :max        ("max")        :max)

(defmacro define-base-type (keyword (spelling) &rest other-info)
  `(progn
     (unless (get ,keyword :basetype)
       (setf (get ,keyword :basetype)
             (let ((*current-namespace* *root-namespace*))
               (make-instance 'basetype-namespace-element
                              :spelling ,spelling
                              ,@other-info))))
     (deftoken ,keyword (,spelling) (get ,keyword :basetype))
     ,keyword))

(define-base-type :double ("double"))
(define-base-type :float ("float"))
(define-base-type :int32 ("int32"))
(define-base-type :int64 ("int64"))
(define-base-type :uint32 ("uint32"))
(define-base-type :uint64 ("uint64"))
(define-base-type :sint64 ("sint64"))
(define-base-type :fixed32 ("fixed32"))
(define-base-type :fixed64 ("fixed64"))
(define-base-type :sfixed32 ("sfixed32"))
(define-base-type :sfixed64 ("sfixed64"))
(define-base-type :bool ("bool"))
(define-base-type :string ("string"))
(define-base-type :bytes ("bytes"))

(deftoken :whitespace ((:greedy-repetition 1 nil 
                                           (:char-class #\space #\tab #\newline #\return)))
  :whitespace)
(deftoken :open-quote (#\") (scan-quoted-string))  ;; leads to :string-value
(deftoken :int-value ("([+-]|)\\\d+") (parse-integer (token-text)))
(deftoken :float-value ("TBDFLOATS") *token-kind*)
(deftoken :name ("[a-zA-Z]\\\w*")
  (token-text)
  #+nil (let ((spelling (token-text)))
          (print
           (or (find-name spelling *current-namespace*)
               (intern-name *namespace-or-unknowns*)))))
(deftoken :name-path ("[a-zA-Z][\\\w.]*")
  (intern-name-path (token-text) *current-namespace*))


;;;; Parsing: Grammar et. al.
  
(define-parser *proto-file-parse-tables*
  (:start-symbol prototype-file-contents)
  (:terminals (:|(| :|)|
                :{ :}
                :=
                :[ :]
                :semicolon
                :package
                :message
                :extend
                :enum
                :service
                :rpc
                :extensions
                :optional
                :required
                :repeated
                :to
                :max
                :double
                :float
                :int32
                :int64
                :uint32
                :uint64
                :sint64
                :fixed32
                :fixed64
                :sfixed32
                :sfixed64
                :bool
                :string
                :bytes
                :open-quote
                :int-value
                :float-value
                :name
                :name-path))

  (prototype-file-contents
   ()
   (proto-decl prototype-file-contents #'reduce-to-nil))

  (proto-decl
   (:package new-name #'reduce-to-nil)
   (:message new-name :{ field-declarations :} #'reduce-composite)
   (:extend  existing-name :{ field-declarations :})
   (service-decl #'identity))

  (new-name
   (:name #'reduce-new-name))

  (existing-name
   (:name #'reduce-existing-name))

  (field-declarations
   ()
   (field-declaration :semicolon field-declarations #'reduce-to-nil))

  (field-declaration
   (:required typename new-name := :int-value
              #'reduce-field-declaration)
   (:optional typename new-name := :int-value
              #'reduce-field-declaration)
   (:optional typename new-name := :int-value :[ :default := constant :]
              #'reduce-field-declaration)
   (:repeated typename new-name := :int-value
              #'reduce-field-declaration)
   ;; No support for repeated groups
   (type-declaration #'reduce-to-nil)
   (:extensions :integer :to :integer #'reduce-extension)
   (:extensions :integer :to :max     #'reduce-extension))

  (type-declaration
   (message #'reduce-to-nil)
   (enum-type #'reduce-to-nil))

  (enum-type
   (:enum new-name :{ enum-values :} #'reduce-composite))

  (enum-values
   ()
   (enum-value :semicolon enum-values #'reduce-to-nil))

  (enum-value
   (:name := :integer #'reduce-enum-value))

  (constant
   (:int-value #'identity)
   (:float-value #'identity)
   (:string-value #'identity)
   (enum-constant #'identity))

  (enum-constant
   (identifier #'identity))

  (service-decl
   (:service new-name :{ rpc-decls :}
             #'reduce-composite))

  (rcp-decls
   (:rpc new-name :|(| message-type :|)| :returns :|(| message-type :|)|
         #'reduce-rpc))

  (typename
   (basic-typename #'identity)
   (user-typename #'identity))

  (user-typename
   (:name #'reduce-user-typename))

  (basic-typename
   (:double #'identity)
   (:float #'identity)
   (:int32 #'identity)
   (:int64 #'identity)
   (:uint32 #'identity)
   (:uint64 #'identity)
   (:sint32 #'identity)
   (:sint64 #'identity)
   (:fixed32 #'identity)
   (:fixed64 #'identity)
   (:sfixed32 #'identity)
   (:sfixed64 #'identity)
   (:bool #'identity)
   (:string #'identity)
   (:bytes #'identity)))



;;;; Parsing: Main Entry point.

(defun parse-proto (package-name-spelling string)
  (let ((*current-namespace* *root-namespace*))
    (let ((*namespace-or-unknowns* (make-instance 'namespace-of-unknown-names
                                                  :spelling "%unknowns%"))
          (*current-package* (make-instance 'package-namespace
                                            :spelling package-name-spelling)))
      (let ((*namespace-stack* ())
            (*current-namespace* *current-package*)
            (inner-tokenizer (tokenizer-of-string string)))
        (labels ((outter-tokenizer ()
                   (funcall inner-tokenizer)
                   (when (eq *token-kind* :whitespace)
                     (outter-tokenizer))
                   (print (list :token *token-kind* *token-value*))
                   (values *token-kind* *token-value*)))
          (parse-with-lexer #'outter-tokenizer *proto-file-parse-tables*)))
      *current-package*)))

