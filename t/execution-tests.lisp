(in-package #:gql-tests)

(deftest execution
  (testing "collect-fields returns the correct fields"
    (let* ((definitions (gql::definitions
                         (build-schema "{ a { subfield1 } ...ExampleFragment } fragment ExampleFragment on Query { a { subfield2 } b }"))))

      (defobject |Query| "Query" nil)
      (defschema (:query (find-item '|Query|)
                  :types (cdr definitions))
        ())

      (with-context ()
        (let* ((operation (find-if (lambda (x) (string= (gql::operation-type x) "Query")) definitions))
               (operation-type (gql::operation-type operation))
               (selection-set (gql::selection-set operation))
               (result (gql::collect-fields operation-type (gql::selections selection-set) nil nil)))
          (ok (= (hash-table-count result) 2))
          (ok (= (length (gethash "a" result)) 2))
          (ok (= (length (gethash "b" result)) 1))))))

  (testing "get-operation should return the correct operation"
    (let ((doc (gql::build-document "{ a { subfield1 } } ")))
      (ok (gql::get-operation doc "Query")))
    (let ((doc (build-schema "query { a { subfield1 } } ")))
      (ok (gql::get-operation doc)))
    (let ((doc (build-schema "mutation { a { subfield1 } } ")))
      (ok (signals (gql::get-operation doc "Query") 'gql::gql-simple-error)))
    (let ((doc (build-schema "mutation { a { subfield1 } }
                              query { a { subfield1 } } ")))
      (ok (signals (gql::get-operation doc) 'gql::gql-simple-error)))
    (let ((doc (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation doc)))
    (let ((doc (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation doc))))

  (testing "merge-selection-sets should merge multiple fields"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))

      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types (cdr definitions))
        ())

      (with-context (:document (build-schema "query { dog { name } dog { owner { name } } }"))
        (gql::set-resolver "Human" "name" (lambda () "Bingo-bongo-pappa"))
        (gql::set-resolver "Dog" "name" (lambda () "Bingo-bongo"))
        (gql::set-resolver "Dog" "owner" (lambda () t))
        (gql::set-resolver "Query" "dog" (lambda () t))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog-res (gethash "dog" data)))
          (ok (typep res 'hash-table))
          (ok (= (hash-table-count res) 1))
          (ok (= (hash-table-count dog-res) 2))
          (ok (gethash "name" dog-res))
          (ok (gethash "owner" dog-res))))))

  (testing "A query should handle alias"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))

      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types (cdr definitions))
        ())

      (with-context (:document (build-schema "query { dog { name owner { name: nameAlias } } }"))
        (gql::set-resolver "Human" "name" (lambda () "Bingo-bongo-pappa"))
        (gql::set-resolver "Dog" "name" (lambda () "Bingo-bongo"))
        (gql::set-resolver "Dog" "owner" (lambda () t))
        (gql::set-resolver "Query" "dog" (lambda () t))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog-res (gethash "dog" data)))
          (ok (typep res 'hash-table))
          (ok (= (hash-table-count res) 1))
          (ok (= (hash-table-count dog-res) 2))
          (ok (gethash "name" dog-res))
          (ok (gethash "owner" dog-res))))))

  (testing "Result coercing"
    (flet ((test (name value ty &optional return-val)
             (let* ((gql::*errors* nil)
                    (res (gql::coerce-result (named name) value)))
               (and (typep res ty) (equalp res (or return-val value)))))
           (test-error (name value error-message)
             (let* ((gql::*errors* nil)
                    (res (gql::coerce-result (named name) value)))
               (ok (string= error-message (gql::message (car res)))))))
      (ok (test "Int" 3 'integer))
      (ok (test "Int" -3 'integer))
      (test-error "String" -3 "Cannot coerce result into Int for value: -3 when value should be: String")
      (test-error "Boolean" -3 "Cannot coerce result into Int for value: -3 when value should be: Boolean")

      (ok (test "Float" -3.9 'double-float))
      (ok (test "Float" 3.9 'double-float))
      (ok (test "Float" 3342.91231236 'double-float))
      (ng (test "Int" 3342.91231236 'double-float))
      (ng (test "String" 3342.91231236 'double-float))
      (ng (test "Boolean" 3342.91231236 'double-float))

      (ok (test "String" "Look at this string!" 'string "Look at this string!"))
      (ok (test "ID" "Look at this string!" 'string))
      (test-error "Boolean" "Look at this string!" "Cannot coerce result into String or ID for value: Look at this string! when type should be: Boolean")
      (test-error "Int" "Look at this string!" "Cannot coerce result into String or ID for value: Look at this string! when type should be: Int")
      (test-error "" "Look at this string!" "Cannot coerce result into String or ID for value: Look at this string! when type should be: ")

      (ok (test "Boolean" 'true 'string "true"))
      (ok (test "Boolean" 'false 'string "false"))))

  (testing "Using resolvers that access the object from the 'db'"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))

      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types (cdr definitions))
        ())

      (with-context (:document (build-schema "query { dog { name } }"))
        (gql::defclass* dog name owner)
        (gql::set-resolver "Dog" "name"
                           (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
        (gql::set-resolver "Dog" "owner" (lambda () t))
        (gql::set-resolver "Query" "dog" (lambda () (make-instance 'dog :name "Bingo-bongo")))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (name (gethash "name" dog)))
          (ok (string= name "Bingo-bongo")))
        (setf (gql::document gql::*context*)
              (build-schema "query { dog { name: bongo } }"))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (name (gethash "bongo" dog)))
          (ok (string= name "Bingo-bongo"))))))

  (testing "A query should handle variables and arguments"
    (let*  ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))

      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types (cdr definitions))
        ())

      (with-context ()
        (gql::defclass* dog name does-know-command)
        (setf (gethash "sit" (gql::variables gql::*context*)) "SIT")

        (gql::set-resolver "Dog" "name"
                           (lambda () (name (gql::object-value gql::*execution-context*))))
        (gql::set-resolver "Dog" "doesKnowCommand"
                           (lambda ()
                             (with-slots (%does-know-command) (gql::object-value (gql::execution-context gql::*context*))
                               (if (member (gethash "dogCommand" (gql::arg-values (gql::execution-context gql::*context*)))
                                           %does-know-command
                                           :test #'equal)
                                   'true 'false))))
        (gql::set-resolver "Query" "dog" (lambda () (make-instance 'dog
                                                              :name "Bingo-bongo"
                                                              :does-know-command '("SIT" "DOWN" "HEEL"))))
        (setf (gql::document gql::*context*) (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }"))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "true")))
        (setf (gethash "sit" (gql::variables gql::*context*)) "SITT")
        (setf (gql::document gql::*context*) (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }"))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "false")))
        (setf (gql::document gql::*context*) (build-schema "query { dog { doesKnowCommand(dogCommand: \"LOL\") } }"))
        (let* ((res (gql::execute nil nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "false")))))))

(deftest abstract-type-resolvers
  (testing "Getting object-type-definition from union or interface"
    (defclass pet (gql-object)
      ((name :initarg :name :accessor name)))

    (defclass dog (pet)
      ((owner :initarg :owner :accessor owner)
       (nickname :initarg :nickname :accessor nickname)))

    (let* ((doggo (make-instance 'dog :name "Bingo-Bongo" :type-name "Dog"))
           (definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql")))))

      (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)
                  :types (cdr definitions))
        ())

      (with-context ()
        ;; We want to know if we did get the actual same reference.
        (ok (eq (gql::resolve-abstract-type (gethash "CatOrDog" (gql::type-map (gql::schema gql::*context*))) doggo)
                (gethash "Dog" (gql::type-map (gql::schema gql::*context*)))))
        (ok (eq (gql::resolve-abstract-type (gethash "Pet" (gql::type-map (gql::schema gql::*context*))) doggo)
                (gethash "Dog" (gql::type-map (gql::schema gql::*context*)))))))))

(deftest doggo-test
  (testing "Doggo-testing"
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

    (let* ((pets `(,(make-instance
                     'dog
                     :name "Bingo-Bongo"
                     :nickname "Hund!"
                     :type-name "Dog")
                   ,(make-instance
                     'cat
                     :name "Bango-Wango"
                     :nickname "Mjausig"
                     :type-name "Cat")))
           (human (make-instance 'human
                                 :name "Wingle Wangle"
                                 :pets pets
                                 :type-name "Human"))
           (doggo
             (make-instance
              'dog
              :name "Bingo-Bongo"
              :nickname "Hund!"
              :owner (make-instance
                      'human
                      :name "Wingle Wangle"
                      :pets pets))))

      (defobject |Query| "Query"
        ((|dog| :gql-type (gql::named "Dog") :resolver (constantly doggo))))

      (definterface |Pet| "A Pet is a pet!"
        ((|name| :gql-type (gql::named "String")
                 :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))))

      (defobject |Human| "A Human is a human!"
        ((|name| :gql-type (gql::named "String")
                 :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
         ("pets" :gql-type (gql::list-type (gql::non-null-type (gql::named "Pet")))
                 :resolver (lambda () (pets (gql::object-value (gql::execution-context gql::*context*)))))))

      (defobject |Dog| "A Dog is a dog!"
        ((|name| :gql-type (gql::named "String")
                 :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
         (|nickname| :gql-type (gql::named "String")
                     :resolver (lambda () (nickname (gql::object-value (gql::execution-context gql::*context*)))))
         (|owner| :gql-type (gql::named "Human")
                  :resolver (lambda () human))))

      (defobject |Cat| "A Cat is a cat!"
        ((|name| :gql-type (gql::named "String")
                 :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
         (|nickname| :gql-type (gql::named "String")
                     :resolver (lambda () (nickname (gql::object-value (gql::execution-context gql::*context*)))))))

      (defschema (:query (find-item '|Query|)
                  :types (find-items '(|Pet| |Human| |Dog| |Cat|)))
        ())

      (flet ((doggo-test (query)
               (with-context (:document (build-schema query))
                 (let* ((res (gql::execute nil nil)))
                   (format nil "~a" (cl-json:encode-json-to-string res))))))

        (ok (string=
             (doggo-test "query { dog { name owner { name pets { name nickname } } } }")
             "{\"data\":{\"dog\":{\"name\":\"Bingo-Bongo\",\"owner\":{\"name\":\"Wingle Wangle\",\"pets\":[{\"name\":\"Bingo-Bongo\",\"nickname\":\"Hund!\"},{\"name\":\"Bango-Wango\",\"nickname\":\"Mjausig\"}]}}}}"))

        (ok (string=
             (doggo-test "query { dog: doggo { name: Bingo owner { name: Wingle pets: dogs { name nickname: thisIsFun } } } }")
             "{\"data\":{\"doggo\":{\"Bingo\":\"Bingo-Bongo\",\"owner\":{\"Wingle\":\"Wingle Wangle\",\"dogs\":[{\"name\":\"Bingo-Bongo\",\"thisIsFun\":\"Hund!\"},{\"name\":\"Bango-Wango\",\"thisIsFun\":\"Mjausig\"}]}}}}"))))))
