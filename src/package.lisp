;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:gql
   #:build-schema
   #:build-document
   #:generate
   #:with-context
   #:execute
   #:resolve
   #:bool
   #:true
   #:false
   #:gql-object
   #:make-name
   #:named
   #:list-type
   #:non-null-type
   #:field
   #:object
   #:interface
   #:enum
   #:enum-val
   #:set-resolver

   #:*int*
   #:*float*
   #:*string*
   #:*boolean*
   #:*id*
   #:!
   #:[]
   #:[!]
   #:[!]!

   #:object-type
   #:object-value
   #:field-definition
   #:field-name
   #:arg-values
   #:schema
   #:document
   #:variables
   #:execution-context
   #:*context*
   #:arg
   #:defobject
   #:definterface
   #:find-item
   #:find-items
   #:defenum
   #:defschema))
