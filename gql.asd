;;;; gql.asd

(asdf:defsystem #:gql
  :description "Describe gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.3.0"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "specials")
               (:file "utils")
               (:file "ast")
               (:file "lexer")
               (:file "parser")
               (:file "introspection")
               (:file "generate")
               (:file "conditions")
               (:file "gql")))

(asdf:defsystem :gql/tests
  :description "Tests for gql"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "GPLv3"
  :version "0.3.0"
  :depends-on (:rove
               :gql)
  :components ((:file "gql-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run :gql/tests)))
