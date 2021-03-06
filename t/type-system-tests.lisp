(in-package #:gql-tests)

(deftest input-output-types
  (testing "scalar"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))
      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types definitions)
        ())
      (with-context ()
        (let ((dog (gethash "Dog" (gql::type-map (gql::schema gql::*context*)))))
          (ok (gql::input-type-p (gql::gql-type (gethash "name" (gql::fields dog)))))
          (ok (gql::input-type-p (gql::gql-type (gethash "nickname" (gql::fields dog)))))
          (ok (gql::output-type-p (gql::gql-type (gethash "barkVolume" (gql::fields dog)))))
          (ok (gql::output-type-p (gql::gql-type (gethash "name" (gql::fields dog)))))))))
  (testing "union"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))
      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types definitions)
        ())

      (with-context ()
        (let ((human-or-alien (gethash "HumanOrAlien" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Human" (gql::union-members human-or-alien))))
          (ng (gql::input-type-p (gethash "Alien" (gql::union-members human-or-alien))))
          (ok (gql::output-type-p (gethash "Human" (gql::union-members human-or-alien))))
          (ok (gql::output-type-p (gethash "Alien" (gql::union-members human-or-alien))))))))
  (testing "object"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))
      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types definitions)
        ())

      (with-context ()
        (let ((dog-or-human (gethash "DogOrHuman" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Dog" (gql::union-members dog-or-human))))
          (ng (gql::input-type-p (gethash "Human" (gql::union-members dog-or-human))))
          (ok (gql::output-type-p (gethash "Dog" (gql::union-members dog-or-human))))
          (ok (gql::output-type-p (gethash "Human" (gql::union-members dog-or-human))))))))
  (testing "interface"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))
      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types definitions)
        ())

      (with-context ()
        (let ((cat (gethash "Cat" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Pet" (gql::interfaces cat))))
          (ok (gql::output-type-p (gethash "Pet" (gql::interfaces cat)))))))))
