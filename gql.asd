(asdf:defsystem #:gql
  :description "Describe gql here"
  :author "Theodor Thornhill <theo@thornhill.no>"
  :license  "AGPLv3"
  :version "0.6.0"
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
                 (:file "types")
                 (:file "mop")
                 (:file "introspection")
                 (:file "conditions")
                 (:file "rules")
                 (:file "execution")
                 (:file "gql")
                 (:file "debugger-utils")))))
