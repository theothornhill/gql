;;;; package.lisp

(defpackage #:gql
  (:use #:cl)
  (:export
   #:advance
   #:lookahead
   #:read-token
   #:make-lexer
   #:gql))
