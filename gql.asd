;;;; gql.asd

(asdf:defsystem #:gql
  :description "Describe gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.4.0"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "specials")
               (:file "utils")
               (:file "lexer")
               (:file "parser")
               (:file "ast")
               (:file "introspection")
               (:file "conditions")
               (:file "gql")
               (:file "request")))

(asdf:defsystem :gql/tests
  :description "Tests for gql"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "AGPLv3"
  :version "0.4.0"
  :depends-on (:rove
               :gql)
  :components ((:file "gql-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run :gql/tests)))
