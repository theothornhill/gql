(asdf:defsystem #:gql-tests
  :description "Tests for gql"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "AGPLv3"
  :version "0.5.0"
  :depends-on (:rove :gql)
  :components ((:module "t"
                :components
                ((:file "gql-tests"))))
  :perform (test-op (o c) (symbol-call :rove '#:run :gql-tests)))
