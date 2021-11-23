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
                 (:file "utils")
                 (:file "lexer")
                 (:file "parser")
                 (:file "ast")
                 (:file "introspection")
                 (:file "conditions")
                 (:file "validation")
                 (:file "execution")
                 (:file "gql")
                 (:file "request")))))
