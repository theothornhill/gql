(asdf:defsystem #:gql
  :description "Describe gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.5.0"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "parser-utils")
                 (:file "utils")
                 (:file "lexer")
                 (:file "language")
                 (:file "schema")
                 (:file "introspection")
                 (:file "conditions")
                 (:file "rules")
                 (:file "execution")
                 (:file "response")
                 (:file "gql")
                 (:file "request")
                 (:file "debugger-utils")))))
