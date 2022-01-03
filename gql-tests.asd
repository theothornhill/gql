(asdf:defsystem #:gql-tests
  :description "Tests for gql"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license "AGPLv3"
  :version "0.5.0"
  :depends-on (:rove :gql :cl-json)
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "lexer-tests")
                 (:file "parser-tests")
                 (:file "generator-tests")
                 (:file "type-system-tests")
                 (:file "regression-tests")
                 (:file "type-extension-tests")
                 (:file "validation-tests")
                 (:file "execution-tests")
                 (:file "introspection-tests")))
               (:module "t/starwars"
                :components
                ((:file "starwars-data"))))
  :perform (test-op (o c) (symbol-call :rove '#:run :gql-tests :style :dot)))
