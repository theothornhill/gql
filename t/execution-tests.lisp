(in-package #:gql-tests)

(deftest execution
  (testing "collect-fields returns the correct fields"
    (let* ((document (build-schema "{ a { subfield1 } ...ExampleFragment }
                                   fragment ExampleFragment on Query { a { subfield2 } b }"))
           (*schema* document)
           (*all-types* (gql::all-types))
           (operation (car (gql::definitions document)))
           (operation-type (gql::operation-type operation))
           (selection-set (gql::selection-set operation))
           (result (gql::collect-fields operation-type (gql::selections selection-set) nil nil)))
      (ok (= (hash-table-count result) 2))
      (ok (= (length (gethash "a" result)) 2))
      (ok (= (length (gethash "b" result)) 1))))
  (testing "get-operation should return the correct operation"
    (let ((*schema* (build-schema "{ a { subfield1 } } ")))
      (ok (gql::get-operation *schema* "Query")))
    (let ((*schema* (build-schema "query { a { subfield1 } } ")))
      (ok (gql::get-operation *schema*)))
    (let ((*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (signals (gql::get-operation *schema* "Query") 'gql-simple-error)))
    (let ((*schema* (build-schema "mutation { a { subfield1 } }
                                   query { a { subfield1 } } ")))
      (ok (signals (gql::get-operation *schema*) 'gql-simple-error)))
    (let ((*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation *schema*)))
    (let ((*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation *schema* "Mutation"))))
  (testing "merge-selection-sets should merge multiple fields"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let* ((res (gql::execute-request (build-schema "query { dog { name } dog { owner { name } } }") nil (make-hash-table) nil))
             (data (gethash "data" res))
             (dog-res (gethash "dog" data)))
        (ok (typep res 'hash-table))
        (ok (= (hash-table-count res) 2))
        (ok (= (hash-table-count dog-res) 2))
        (ok (gethash "name" dog-res))
        (ok (gethash "owner" dog-res)))))
  (testing "A query should handle alias"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let* ((res (gql::execute-request (build-schema "query { dog { name owner { name: nameAlias } } }") nil (make-hash-table) nil))
             (data (gethash "data" res))
             (dog-res (gethash "dog" data)))
        (ok (typep res 'hash-table))
        (ok (= (hash-table-count res) 2))
        (ok (= (hash-table-count dog-res) 2))
        (ok (gethash "name" dog-res))
        (ok (gethash "owner" dog-res)))))
  (testing "A query should handle alias"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let* ((res (gql::execute-request (build-schema "query { dog { name owner { name: nameAlias } } }") nil (make-hash-table) nil))
             (data (gethash "data" res))
             (dog-res (gethash "dog" data)))
        (ok (typep res 'hash-table))
        (ok (= (hash-table-count res) 2))
        (ok (= (hash-table-count dog-res) 2))
        (ok (gethash "name" dog-res))
        (ok (gethash "owner" dog-res)))))
  (testing "A query should handle variables and arguments"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let ((variable-values (make-hash-table :test #'equal)))
        (setf (gethash "sit" variable-values) "SIT")
        (let* ((res (gql::execute-request
                     (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }")
                     nil
                     variable-values
                     nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "Boolean RESOLVED")))))))

