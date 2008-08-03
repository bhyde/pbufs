



(define-parser *proto-file-format*
  (:start-symbol prototype-file-contents)
  (:terminals :integer
              :identifier
              :{ :}
              :semicolon
              :=)

  (prototype-file-contents
   ()
   (proto-decl prototype-file-contents
            #'list*))

  (proto-decl
   (:package :identifier)
   (:message :identifier :{ field-declarations :})
   (:extend :identifier :{ field-declarations :})
   (service-decl #'identity))

  (field-declarations
   ()
   (field-declaration field-declarations
                      #'list*))

  (field-declaration
   (:required type :identifier := :integer :semicolon)
   (:optional type :identifier := :integer :semicolon)
   (:optional type :identifier := :integer :[:default := constant :] :semicolon)
   (:repeated type :identifier := :integer :semicolon)
   ;; No support for repeated groups
   (type-declaration #'identity)
   (:extensions :integer :to :integer #'reduce-abcd2abd)
   (:extensions :integer :to :max #'reduce-abcd2abd))

  (type-declaration
   (message #'identity)
   (enum-type #'identity))

  (enum-type
   (:enum :identifier :{ enum-values :}
          #'reduce-abcde2abd*))

  (enum-values
   ()
   (enum-value enum-values #'list*))

  (enum-value
   (:identifier := :integer
                #'reduce-abc2ab))

  (constant
   (:integer #'identity)
   (:float #'identity)
   (:string-value #'identity)
   (enum-constant #'identity))

  (enum-constant
   (identifier #'identity))

  (service-decl
   (:service :identifier :{ rpc-decls :}))

  (rcp-decls
   (:rpc :identifier :( message-type :) returns :( message-type :)))

  (scalar-type
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



