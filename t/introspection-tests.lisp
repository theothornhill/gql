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
           (query-type
             (gql::object :name "Query"
                          :fields `(,(gql::field :name "dog"
                                                 :type (gql::named "Dog")
                                                 :resolver (constantly doggo)))))
           (human-type
             (gql::object :name "Human"
                          :description "A Human is a human!"
                          :fields `(,(gql::field :name "name"
                                                 :type (gql::named "String")
                                                 :resolver (lambda () (name (gql::object-value gql::*execution-context*))))
                                    ,(gql::field :name "pets"
                                                 :type (gql::list-type (gql::non-null-type (gql::named "Pet")))))))
           (dog-type
             (gql::object :name "Dog"
                          :description "A Dog is a dog!"
                          :fields `(,(gql::field :name "name"
                                                 :type (gql::named "String")
                                                 :resolver (lambda () (name (gql::object-value gql::*execution-context*))))
                                    ,(gql::field :name "nickname"
                                                 :type (gql::named "String"))
                                    ,(gql::field :name "owner"
                                                 :type (gql::named "Human")
                                                 :resolver (lambda () (make-instance 'human
                                                                                :name "Wingle Wangle"
                                                                                :pets '())))))))

      (flet ((doggo-test (query)
               (with-schema (gql::make-schema :query query-type :types (list dog-type human-type))
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
