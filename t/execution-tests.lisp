(in-package #:gql-tests)

(deftest execution
  (testing "collect-fields returns the correct fields"
    (with-schema (build-schema "{ a { subfield1 } ...ExampleFragment }
                                   fragment ExampleFragment on Query { a { subfield2 } b }")
      (let ((gql::*all-types* (make-hash-table :test #'equal)))
        ;; HACK: omg, eww!
        (setf (gethash "Query" gql::*all-types*)
              (make-instance 'gql::object-type-definition
                             :name (make-instance 'gql::named-type :name "Query")))
        (let* ((operation (car (gql::definitions gql::*schema*)))
               (operation-type (gql::operation-type operation))
               (selection-set (gql::selection-set operation))
               (result (gql::collect-fields operation-type (gql::selections selection-set) nil nil)))
          (ok (= (hash-table-count result) 2))
          (ok (= (length (gethash "a" result)) 2))
          (ok (= (length (gethash "b" result)) 1)))  )))
  (testing "get-operation should return the correct operation"
    (let ((gql::*schema* (build-schema "{ a { subfield1 } } ")))
      (ok (gql::get-operation gql::*schema* "Query")))
    (let ((gql::*schema* (build-schema "query { a { subfield1 } } ")))
      (ok (gql::get-operation gql::*schema*)))
    (let ((gql::*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (signals (gql::get-operation gql::*schema* "Query") 'gql::gql-simple-error)))
    (let ((gql::*schema* (build-schema "mutation { a { subfield1 } }
                                   query { a { subfield1 } } ")))
      (ok (signals (gql::get-operation gql::*schema*) 'gql::gql-simple-error)))
    (let ((gql::*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation gql::*schema*)))
    (let ((gql::*schema* (build-schema "mutation { a { subfield1 } } ")))
      (ok (gql::get-operation gql::*schema* "Mutation"))))
  (testing "merge-selection-sets should merge multiple fields"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let ((*resolvers* (make-hash-table :test #'equal))
            (query-resolver (make-hash-table :test #'equal))
            (dog-resolver (make-hash-table :test #'equal))
            (human-resolver (make-hash-table :test #'equal)))
        (setf (gethash "Query" *resolvers*) query-resolver)
        (setf (gethash "Human" *resolvers*) human-resolver)
        (setf (gethash "name" human-resolver) (lambda (arg) (declare (ignorable arg))
                                                "Bingo-bongo-pappa"))
        (setf (gethash "Dog" *resolvers*) dog-resolver)
        (setf (gethash "dog" query-resolver) (lambda (arg) (declare (ignorable arg)) t))
        (setf (gethash "name" dog-resolver) (lambda (arg) (declare (ignorable arg))
                                              "Bingo-bongo"))
        (setf (gethash "owner" dog-resolver) (lambda (arg) (declare (ignorable arg)) t))
        (let* ((res (gql::execute-request (build-schema "query { dog { name } dog { owner { name } } }") nil (make-hash-table) nil))
               (data (gethash "data" res))
               (dog-res (gethash "dog" data)))
          (ok (typep res 'hash-table))
          (ok (= (hash-table-count res) 2))
          (ok (= (hash-table-count dog-res) 2))
          (ok (gethash "name" dog-res))
          (ok (gethash "owner" dog-res))))))
  (testing "A query should handle alias"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let* ((*resolvers* (make-hash-table :test #'equal))
             (query-resolver (make-hash-table :test #'equal))
             (dog-resolver (make-hash-table :test #'equal))
             (human-resolver (make-hash-table :test #'equal)))
        (setf (gethash "Query" *resolvers*) query-resolver)
        (setf (gethash "Human" *resolvers*) human-resolver)
        (setf (gethash "name" human-resolver) (lambda (arg) (declare (ignorable arg))
                                                "Bingo-bongo-pappa"))
        (setf (gethash "Dog" *resolvers*) dog-resolver)
        (setf (gethash "dog" query-resolver) (lambda (arg) (declare (ignorable arg)) t))
        (setf (gethash "name" dog-resolver) (lambda (arg) (declare (ignorable arg))
                                              "Bingo-bongo"))
        (setf (gethash "owner" dog-resolver) (lambda (arg) (declare (ignorable arg)) t))
        (let* ((res (gql::execute-request (build-schema "query { dog { name owner { name: nameAlias } } }") nil (make-hash-table) nil))
               (data (gethash "data" res))
               (dog-res (gethash "dog" data)))
          (ok (typep res 'hash-table))
          (ok (= (hash-table-count res) 2))
          (ok (= (hash-table-count dog-res) 2))
          (ok (gethash "name" dog-res))
          (ok (gethash "owner" dog-res))))))
  (testing "A query should handle variables and arguments"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let ((variable-values (make-hash-table :test #'equal))
            (query-resolver (make-hash-table :test #'equal))
            (dog-resolver (make-hash-table :test #'equal))
            (*resolvers* (make-hash-table :test #'equal)))
        (setf (gethash "sit" variable-values) "SIT")
        (setf (gethash "Query" *resolvers*) query-resolver)
        (setf (gethash "Dog" *resolvers*) dog-resolver)
        (setf (gethash "dog" query-resolver)
              (lambda (arg) (declare (ignorable arg)) t))
        (setf (gethash "doesKnowCommand" dog-resolver)
              (lambda (arg args) (declare (ignorable arg))
                (if (string= (gethash "dogCommand" args) "SIT")
                    'true 'false)))
        (let* ((res (gql::execute-request
                     (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }")
                     nil
                     variable-values
                     nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "true"))))))
  (testing "Result coercing"
    (flet ((named-type (name)
             (make-instance 'gql::named-type
                            :name (make-instance 'gql::name :name name))))
      (flet ((test (name value ty &optional return-val)
               (let ((res (gql::coerce-result (named-type name) value)))
                 (and (typep res ty)
                      (equalp res (or return-val value))))))
        (ok (test "Int" 3 'integer))
        (ok (test "Int" -3 'integer))
        (ng (test "String" -3 'integer "Field error for int"))
        (ng (test "Boolean" -3 'integer "Field error for int"))

        (ok (test "Float" -3.9 'double-float))
        (ok (test "Float" 3.9 'double-float))
        (ok (test "Float" 3342.91231236 'double-float))
        (ng (test "Int" 3342.91231236 'double-float))
        (ng (test "String" 3342.91231236 'double-float))
        (ng (test "Boolean" 3342.91231236 'double-float))

        (ok (test "String" "Look at this string!" 'string "Look at this string!"))
        (ok (test "ID" "Look at this string!" 'string))
        (ok (test "Boolean" "Look at this string!" 'string "Field error for string"))
        (ok (test "Int" "Look at this string!" 'string "Field error for string"))
        (ok (test "" "Look at this string!" 'string "Field error for string"))

        (ok (test "Boolean" 'true 'string "true"))
        (ok (test "Boolean" 'false 'string "false")))))
  (testing "Using resolvers that access the object from the 'db'"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let* ((*resolvers* (make-hash-table :test #'equal))
             (query-resolver (make-hash-table :test #'equal))
             (dog-resolver (make-hash-table :test #'equal)))
        (gql::defclass* dog name owner)
        (setf (gethash "Query" *resolvers*) query-resolver)
        (setf (gethash "Dog" *resolvers*) dog-resolver)
        (setf (gethash "dog" query-resolver)
              (lambda (arg) (declare (ignorable arg))
                (make-instance 'dog :name "Bingo-bongo")))
        (setf (gethash "name" dog-resolver) (lambda (dog) (name dog)))
        (let* ((res (gql::execute-request
                     (build-schema "query { dog { name } }") nil (make-hash-table) nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (name (gethash "name" dog)))
          (ok (string= name "Bingo-bongo")))
        (let* ((res (gql::execute-request
                     (build-schema "query { dog { name: bongo } }") nil (make-hash-table) nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (name (gethash "bongo" dog)))
          (ok (string= name "Bingo-bongo"))))))
  (testing "A query should handle variables and arguments"
    (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
      (let ((variable-values (make-hash-table :test #'equal))
            (query-resolver (make-hash-table :test #'equal))
            (dog-resolver (make-hash-table :test #'equal))
            (*resolvers* (make-hash-table :test #'equal)))
        (gql::defclass* dog name does-know-command)
        (setf (gethash "sit" variable-values) "SIT")
        
        (setf (gethash "Query" *resolvers*) query-resolver)
        (setf (gethash "Dog" *resolvers*) dog-resolver)
        (setf (gethash "dog" query-resolver)
              (lambda (arg) (declare (ignorable arg))
                (make-instance 'dog
                               :name "Bingo-bongo"
                               :does-know-command '("SIT" "DOWN" "HEEL"))))
        (setf (gethash "name" dog-resolver) (lambda (dog) (name dog)))

        (setf (gethash "doesKnowCommand" dog-resolver)
              (lambda (arg args) (declare (ignorable arg))
                (with-slots (does-know-command) arg
                  (if (member (gethash "dogCommand" args) does-know-command
                              :test #'equal)
                      'true 'false))))

        (let* ((res (gql::execute-request
                     (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }")
                     nil
                     variable-values
                     nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "true")))
        (setf (gethash "sit" variable-values) "SITT")
        (let* ((res (gql::execute-request
                     (build-schema "query x($sit: String) { dog { doesKnowCommand(dogCommand: $sit) } }")
                     nil
                     variable-values
                     nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "false")))
        ;; (setf (gethash "sit" variable-values) "SIT")
        ;; (let* ((res (gql::execute-request
        ;;              (build-schema "query { dog { doesKnowCommand(dogCommand: \"SIT\") } }")
        ;;              nil
        ;;              variable-values
        ;;              nil))
        ;;        (data (gethash "data" res))
        ;;        (dog (gethash "dog" data))
        ;;        (command (gethash "doesKnowCommand" dog)))
        ;;   (ok (string= command "true")))
        (let* ((res (gql::execute-request
                     (build-schema "query { dog { doesKnowCommand(dogCommand: \"LOL\") } }")
                     nil
                     variable-values
                     nil))
               (data (gethash "data" res))
               (dog (gethash "dog" data))
               (command (gethash "doesKnowCommand" dog)))
          (ok (string= command "false")))))))

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

    (defparameter *doggo*
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

    (defparameter *query-resolvers*
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "dog" ht)
              (lambda (arg) (declare (ignorable arg)) *doggo*))
        ht))

    (defparameter *dog-resolvers*
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "name" ht) (lambda (dog) (name dog)))
        (setf (gethash "nickname" ht) (lambda (dog) (nickname dog)))
        (setf (gethash "owner" ht) (lambda (dog) (owner dog)))
        ht))

    (defparameter *cat-resolvers*
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "name" ht) (lambda (cat) (name cat)))
        (setf (gethash "nickname" ht) (lambda (cat) (nickname cat)))
        (setf (gethash "owner" ht) (lambda (cat) (owner cat)))
        ht))

    (defparameter *human-resolvers*
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "name" ht) (lambda (human) (name human)))
        (setf (gethash "pets" ht) (lambda (human) (pets human)))
        ht))

    (flet ((doggo-test (query)
             (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
               (let* ((res (gql::execute-request
                            (build-schema query) nil (make-hash-table :test #'equal) nil)))
                 (format nil "~a" (cl-json:encode-json-to-string res))))))

      (let ((*resolvers* (make-hash-table :test #'equal)))
        (setf (gethash "Query" *resolvers*) *query-resolvers*)
        (setf (gethash "Dog" *resolvers*) *dog-resolvers*)
        (setf (gethash "Cat" *resolvers*) *cat-resolvers*)
        (setf (gethash "Human" *resolvers*) *human-resolvers*)
        (let ((result (doggo-test "query { dog { name owner { name pets { name nickname } } } }")))
          (ok (string= result "{\"data\":{\"dog\":{\"name\":\"Bingo-Bongo\",\"owner\":{\"name\":\"Wingle Wangle\",\"pets\":[{\"name\":\"Bingo-Bongo\",\"nickname\":\"Hund!\"},{\"name\":\"Bango-Wango\",\"nickname\":\"Mjausig\"}]}}},\"errors\":null}"))))

      (let ((*resolvers* (make-hash-table :test #'equal)))
        (setf (gethash "Query" *resolvers*) *query-resolvers*)
        (setf (gethash "Dog" *resolvers*) *dog-resolvers*)
        (setf (gethash "Cat" *resolvers*) *cat-resolvers*)
        (setf (gethash "Human" *resolvers*) *human-resolvers*)
        (let ((result (doggo-test "query { dog: doggo { name: Bingo owner { name: Wingle pets: dogs { name nickname: thisIsFun } } } }")))
          (ok (string= result "{\"data\":{\"doggo\":{\"Bingo\":\"Bingo-Bongo\",\"owner\":{\"Wingle\":\"Wingle Wangle\",\"dogs\":[{\"name\":\"Bingo-Bongo\",\"thisIsFun\":\"Hund!\"},{\"name\":\"Bango-Wango\",\"thisIsFun\":\"Mjausig\"}]}}},\"errors\":null}")))))))
