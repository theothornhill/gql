;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:gql
   #:build-schema
   #:build-document
   #:make-schema
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
   #:[!]!))
