;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:advance
   #:lookahead
   #:read-token
   #:make-lexer
   #:gql
   #:gql-simple-error
   #:gql-error
   #:gql-warning
   #:gql-simple-warning
   #:make-parser)
  (:local-nicknames (:a :alexandria)))
