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
                                :type-name "Cat"))))))

      (defobject |Query| "Query"
        ((|dog| :gql-type (gql::named "Dog") :resolver (constantly doggo))))

      (defobject |Human| "A Human is a human!"
        ((|name|
          :gql-type (gql::named "String")
          :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
         (|pets| :gql-type (gql::list-type (gql::non-null-type (gql::named "Pet"))))))

      (defobject |Dog| "A Dog is a dog!"
        ((|name| :gql-type (gql::named "String")
                 :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
         (|nickname| :gql-type (gql::named "String"))
         (|owner| :gql-type (gql::named "Human")
                  :resolver (lambda () (make-instance 'human
                                                 :name "Wingle Wangle"
                                                 :pets '())))))

      (defschema (:query (find-item '|Query|) :types (find-items '(|Dog| |Human|)))
        ())

      (flet ((doggo-test (query)
               (with-context (:document (build-schema query))
                 (format nil "~a" (cl-json:encode-json-to-string (gql::execute nil nil))))))

        (ok (string=
             (doggo-test "query { dog { __typename name owner { name } } }")
             "{\"data\":{\"dog\":{\"__typename\":\"Dog\",\"name\":\"Bingo-Bongo\",\"owner\":{\"name\":\"Wingle Wangle\"}}}}"))
        (ok (string=
             (doggo-test "query { dog { name owner { __typename name } } }")
             "{\"data\":{\"dog\":{\"name\":\"Bingo-Bongo\",\"owner\":{\"__typename\":\"Human\",\"name\":\"Wingle Wangle\"}}}}"))
        (ok (string=
             (doggo-test "query { dog { __typename name owner { __typename name } } }")
             "{\"data\":{\"dog\":{\"__typename\":\"Dog\",\"name\":\"Bingo-Bongo\",\"owner\":{\"__typename\":\"Human\",\"name\":\"Wingle Wangle\"}}}}"))))))
