;;; -*- mode: lisp -*-

(in-package "PBUFS")

;;; Parsing: Dynamic State & Reduction functions

(defvar *current-package*)

(defmacro define-reduction (name args &body body)
  (let ((foo-name (intern (concatenate 'string (symbol-name name) "-FOO"))))
    (flet ((scrub (args)
             (remove '&optional args)))
      `(progn
         (defun ,foo-name ,args ,@body)
         (defun-for-macro ,name ,args (,foo-name ,@(scrub args)))))))

(define-reduction reduce-ab2a (a b)
  (declare (ignore b))
  a)

(define-reduction reduce-abc2ac* (a b c)
  (declare (ignore b))
  (list* a c))

(define-reduction reduce-setting-name (namespace-element name-spelling)
  (setf (spelling-of-namespace-node namespace-element) name-spelling)
  namespace-element)

(define-reduction reduce-file-contents (decls)
  (loop for d in decls do (add-child-to-namespace *current-package* d)))

(defmethod add-child-to-namespace ((parent package-namespace) (child package-namespace))
  (declare (ignore parent))
  child)

(define-reduction reduce-adding-children (namespace os children cs)
  (declare (ignore os cs))
  (loop for child in children do (add-child-to-namespace namespace child))
  namespace)

(defmethod add-child-to-namespace ((parent message) (child extension-range-declaration))
  (setf (declared-extension-range?-of-message parent) child))

(define-reduction reduce-existing-name (spelling)
    (find-name spelling *current-namespace* t))

(define-reduction reduce-enum-constant (spelling)
  (with-slots (all-enumeration-constants) *current-package*
    (let ((constant?
           (find spelling all-enumeration-constants 
                 :key #'spelling-of-namespace-node
                 :test #'string=)))
      (assert constant? () "Unknown enumeration constant named ~A" spelling)
      constant?)))

(define-reduction reduce-user-type (spelling)
    (let ((type? (find-name spelling *current-namespace*)))
      (assert (typep type? 'message))
      type?))

(define-reduction reduce-enum-value (name-spelling eq the-index)
  (declare (ignore eq))
  (with-slots (all-enumeration-constants) *current-package*
    (let ((it (make-instance 'enumeration-constant
                             :spelling name-spelling
                             :value the-index)))
      (push it all-enumeration-constants)
      it)))


(define-reduction reduce-field-declaration (field the-type name eq the-index &optional ob default eq2 the-default? cb)
  (declare (ignore eq ob default eq2 cb))
  (with-slots (type spelling index default-value) field
    (setf spelling name)
    (setf type the-type)
    (setf index the-index)
    (when the-default?
      (setf default-value the-default?)))
  field)

(define-reduction reduce-extension (extension the-low to the-high)
  (declare (ignore to))
  (when (eq the-high :max)
    (setf the-high .maximum-field-descriptor-number.))
  (with-slots (low high) extension
    (setf low the-low)
    (setf high the-low))
  extension)

(define-reduction reduce-rpc (rpc name op1 the-query cp1 returns op2 the-reply cp2)
    (declare (ignore name op1 cp1 returns op2 cp2))
    (with-slots (query reply) rpc
      (setf query the-query)
      (setf reply the-reply))
    rpc)



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



(deftoken :package    ("package")    *current-package*)
(deftoken :message    ("message")    (make-instance 'message))
(deftoken :extend     ("extend")     (make-instance 'extended-message))
(deftoken :enum       ("enum")       (make-instance 'enumeration))
(deftoken :service    ("service")    (make-instance 'service))
(deftoken :rpc        ("rpc")        (make-instance 'rpc))
(deftoken :extensions ("extensions") (make-instance 'extension-range-declaration))
(deftoken :optional   ("optional")   (make-instance 'field :nary :optional))
(deftoken :required   ("required")   (make-instance 'field :nary :required))
(deftoken :repeated   ("repeated")   (make-instance 'field :nary :repeated))

(deftoken :default    ("default")    :default)
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
  (split #\. (token-text)))
(deftoken :comment ((:sequence "//" (:greedy-repetition 0 nil :everything) (:char-class #\newline #\return)))
  :comment)


;;;; Parsing: Grammar et. al.
  
(define-parser *proto-file-parse-tables*
  (:start-symbol file-contents)
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
                :default
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
                :string-value
                :name-path
                :name))

  (file-contents
   (decls #'reduce-file-contents))

  (decls
   ()
   (decl decls #'list*))

  (decl
   (package-header                          #'identity)
   (message-decl                            #'identity)
   (enum-decl                               #'identity)
   (extend-header  :{ field-declarations :} #'reduce-adding-children)
   (service-header :{ rpc-decls :}          #'reduce-adding-children)

   ;; Missing option
   )

  (message-decl
   (message-header :{ field-declarations :}   #'reduce-adding-children))
  (enum-decl
   (enum-header    :{ enum-values :}        #'reduce-adding-children))


  (package-header (:package :name          #'reduce-setting-name))
  (message-header (:message :name          #'reduce-setting-name))
  (enum-header    (:enum :name             #'reduce-setting-name))
  (extend-header  (:extend  existing-name  #'reduce-setting-name))
  (service-header (:service :name          #'reduce-setting-name))

  (existing-name  (:name                   #'reduce-existing-name))

  (field-declarations
   ()
   (field-declaration  field-declarations #'list*))

  (field-declaration
   (field-declaration-1 :semicolon   #'reduce-ab2a)
   (message-decl                     #'identity)
   (enum-decl                        #'identity)
   ;; No support for repeated groups
   )

  (field-declaration-1
   (:extensions :integer :to :integer          #'reduce-extension)
   (:extensions :integer :to :max              #'reduce-extension)
   (:required typename :name := :int-value
              #'reduce-field-declaration)
   (:optional typename :name := :int-value
              #'reduce-field-declaration)
   (:optional typename :name := :int-value :[ :default := constant :]
              #'reduce-field-declaration)
   (:repeated typename :name := :int-value
              #'reduce-field-declaration))

  (enum-values
   ()
   (enum-value :semicolon enum-values          #'reduce-abc2ac*))

  (enum-value
   (:name := :int-value                        #'reduce-enum-value))

  (constant
   (:int-value      #'identity)
   (:float-value    #'identity)
   (:string-value   #'identity)
   (enum-constant   #'identity))

  (enum-constant
   (:name #'reduce-enum-constant))

  (rpc-decls
   (rpc-header :|(| message-type :|)| :returns :|(| message-type :|)|
         #'reduce-rpc))

  (rpc-header 
   (:rpc :name                        #'reduce-setting-name))

  (typename
   (basic-typename #'identity)
   (user-typename #'identity))

  (user-typename
   (:name-path   #'identity)
   (:name        #'list))

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





;;;; Parsing: In pass 2 we resolve names, type names actually.

(defun find-in-namespace (spelling namespace)
  (find spelling (elements-of-namespace namespace)
        :key #'spelling-of-namespace-node
        :test #'string=))

(defun lookup-name-path (namepath namespace-stack)
  (let ((root? 
         (find (first namepath)
               namespace-stack
               :test #'find-in-namespace)))
    (labels ((check (pred)
               (unless pred
                 (error "Unable to resolve ~{~A.~}" namepath)))
             (recure (names namespace)
               (let ((next-node? 
                      (find-in-namespace (car names) namespace)))
                 (check next-node?)
                 (if (rest names)
                     (recure (rest names) next-node?)
                     next-node?))))
      (check root?)
      (recure namepath root?))))
  
(defun pass-2 (package-namespace)
  ;; Clear all-enumeration-constants for generational GC.
  (with-slots (all-enumeration-constants) *current-package*
    (setf all-enumeration-constants nil))
  ;; Sweep over the tree, resolve symbols now that we know them.
  (let ((namespace-stack ()))
    (declare (special namespace-stack))
    (labels ((recure (namespace)
               (loop for child in (elements-of-namespace namespace)
                  do (typecase child
                       (namespace
                        (let ((namespace-stack (cons child namespace-stack)))
                          (declare (special namespace-stack))
                          (recure child)))
                       (field
                        (with-slots (type) child
                          (when (consp type)
                            (setf type (lookup-name-path type namespace-stack)))))))))
      (recure package-namespace))))

;;;; Parsing: Main Entry point.

(defun parse-proto-string (package-name-spelling string)
  (let ((*current-namespace* *root-namespace*))
    (let ((*current-package* (make-instance 'package-namespace
                                            :spelling package-name-spelling)))
      (let ((inner-tokenizer (tokenizer-of-string string)))
        (labels ((outter-tokenizer ()
                   (funcall inner-tokenizer)
                   (when (or (eq *token-kind* :whitespace)
                             (eq *token-kind* :comment))
                     (outter-tokenizer))
                   (print (list :token *token-kind* *token-value*))
                   (values *token-kind* *token-value*)))
          (parse-with-lexer #'outter-tokenizer *proto-file-parse-tables*)))
      (pass-2 *current-package*))))

(defun parse-proto-file (pathname)
  (let ((name (pathname-name pathname)))
    (parse-proto-string name (read-entire-file pathname))))
