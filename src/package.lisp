;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:advance
   #:lookahead
   #:read-token
   #:make-lexer
   #:gql
   #:build-schema
   #:gql-simple-error
   #:gql-error
   #:gql-warning
   #:gql-simple-warning
   #:make-parser
   #:generate
   #:validate
   #:with-schema
   #:*schema*
   #:*all-types*
   #:*data*
   #:*errors*))
