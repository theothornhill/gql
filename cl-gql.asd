;;;; cl-gql.asd

(asdf:defsystem #:cl-gql
  :description "Describe cl-gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-gql")))

(asdf:defsystem :cl-gql/tests
  :description "Tests for nnin"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (:rove
               :cl-gql)
  :components ((:file "cl-gql-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run :cl-gql/tests)))
