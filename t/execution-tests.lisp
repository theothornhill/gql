(in-package #:gql-tests)

(deftest execution
  (testing "collect-fields returns the correct fields"
    (let* ((document (build-schema "{ a { subfield1 } ...ExampleFragment }
                                   fragment ExampleFragment on Query { a { subfield2 } b }"))
           (*schema* document)
           (operation (car (gql::definitions document)))
           (operation-type (gql::operation-type operation))
           (selection-set (gql::selection-set operation))
           (result (gql::collect-fields operation-type selection-set nil nil)))
      (ok (= (hash-table-count result) 2))
      (ok (= (length (gethash "a" result)) 2))
      (ok (= (length (gethash "b" result)) 1)))))
