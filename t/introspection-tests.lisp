(in-package #:gql-tests)

(deftest introspection-test
  (testing "Getting __typename"
    (defclass pet (gql-object)
      ((name :initarg :name :accessor name)))

    (defclass dog (pet)
      ((owner :initarg :owner :accessor owner)
       (nickname :initarg :nickname :accessor nickname)))

    (defclass cat (pet)
      ((nickname :initarg :nickname :accessor nickname)))

    (defclass sentient (gql-object)
      ((name :initarg :name :accessor name)))

    (defclass human (sentient)
      ((pets :initarg :pets :accessor pets)))

    (let* ((doggo
             (make-instance
              'dog
              :name "Bingo-Bongo"
              :type-name "Dog"
              :nickname "Hund!"
              :owner (make-instance
                      'human
                      :name "Wingle Wangle"
                      :type-name "Human"
                      :pets `(,(make-instance
                                'dog
                                :name "Bingo-Bongo"
                                :nickname "Hund!"
                                :type-name "Dog")
                              ,(make-instance
                                'cat
                                :name "Bango-Wango"
                                :nickname "Mjausig"
                                :type-name "Cat")))))
           (query-resolvers
             (make-resolvers
               ("dog"      . (constantly doggo))))

           (dog-resolvers
             (make-resolvers
               ("name"     . 'name)
               ("nickname" . 'nickname)
               ("owner"    . 'owner)))

           (cat-resolvers
             (make-resolvers
               ("name"     . 'name)
               ("nickname" . 'nickname)
               ("owner"    . 'owner)))

           (human-resolvers
             (make-resolvers
               ("name"     . 'name)
               ("pets"     . 'pets)))

           (*resolvers*
             (make-resolvers
               ("Query"    . query-resolvers)
               ("Dog"      . dog-resolvers)
               ("Cat"      . cat-resolvers)
               ("Human"    . human-resolvers))))

      (flet ((doggo-test (query)
               (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
                 (let* ((res (gql::execute (build-schema query) nil (make-hash-table :test #'equal) nil)))
                   (format nil "~a" (cl-json:encode-json-to-string res))))))

        (ok (string=
             (doggo-test "query { dog { __typename name owner { name } } }")
             "{\"data\":{\"dog\":{\"__typename\":\"Dog\",\"name\":\"Bingo-Bongo\",\"owner\":{\"name\":\"Wingle Wangle\"}}}}"))
        (ok (string=
             (doggo-test "query { dog { name owner { __typename name } } }")
             "{\"data\":{\"dog\":{\"name\":\"Bingo-Bongo\",\"owner\":{\"__typename\":\"Human\",\"name\":\"Wingle Wangle\"}}}}"))
        (ok (string=
             (doggo-test "query { dog { __typename name owner { __typename name } } }")
             "{\"data\":{\"dog\":{\"__typename\":\"Dog\",\"name\":\"Bingo-Bongo\",\"owner\":{\"__typename\":\"Human\",\"name\":\"Wingle Wangle\"}}}}"))))))
