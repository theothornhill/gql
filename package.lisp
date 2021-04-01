;;;; package.lisp

(defpackage #:cl-gql
  (:use #:cl)
  (:export
   #:advance
   #:lookahead
   #:read-token
   #:make-lexer))
