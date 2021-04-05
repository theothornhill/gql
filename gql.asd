;;;; gql.asd

(asdf:defsystem #:gql
  :description "Describe gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "lexer")
               (:file "parser")
               (:file "conditions")
               (:file "gql")))

(asdf:defsystem :gql/tests
  :description "Tests for gql"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (:rove
               :gql)
  :components ((:file "gql-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run :gql/tests)))
