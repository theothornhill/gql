(ql:quickload :gql/tests)

(multiple-value-bind (foo bar fails) (rove:run :gql/tests)
  (when fails
    (sb-ext:exit :code 1)))


