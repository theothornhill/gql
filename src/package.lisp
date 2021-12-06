;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:gql
   #:build-schema
   #:generate
   #:validate
   #:with-schema
   #:execute-request
   #:*resolvers*
   #:resolve
   #:bool
   #:true
   #:false))
