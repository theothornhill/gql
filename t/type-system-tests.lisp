(in-package #:gql-tests)

(deftest type-system
  (testing "schema"
    (generator-test
     "schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}"
     "schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}
")
    (generator-test
     "\"\"\"Docs\"\"\"
schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}"
     "\"\"\"Docs\"\"\"
schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}
"))
  (testing "scalar"
    (generator-test
     "\"\"\"Docs\"\"\"
scalar Time
scalar Url"
     "\"\"\"Docs\"\"\"
scalar Time
scalar Url
"))
  (testing "object-type"
    (generator-test
     "type SomeType {
  \"\"\"Docs\"\"\"
  field(arg: Int @example): String @example
}"
     "type SomeType {
  \"\"\"Docs\"\"\"
  field(arg: Int @example): String @example
}
")
    (generator-test
     "type Person {
  name: String
  age: Int
  picture: Url
}"
     "type Person {
  name: String
  age: Int
  picture: Url
}
")
    (generator-test
     "type Business implements NamedEntity & ValuedEntity {
  name: String
  value: Int
  employeeCount: Int
}"
     "type Business implements NamedEntity & ValuedEntity {
  name: String
  value: Int
  employeeCount: Int
}
"))
  (testing "interface"
    (generator-test
     "interface ValuedEntity {
  value: Int
}"
     "interface ValuedEntity {
  value: Int
}
"))
  (testing "union"
    (generator-test
     "union SearchResult = Photo | Person"
     "union SearchResult =
  | Photo
  | Person
"))
  (testing "enum"
    (generator-test
     "enum Direction {
  NORTH
  EAST
  SOUTH
  WEST
}"
     "enum Direction {
  NORTH
  EAST
  SOUTH
  WEST
}
"))
  (testing "input object"
    (generator-test
     "input Point2D { x: Float y: Float }"
     "input Point2D {
  x: Float
  y: Float
}
"))
  (testing "directive-definition"
    (generator-test
     "directive @example on FIELD"
     "directive @example on
  | FIELD
")
    (generator-test
     "directive @example on
  | FIELD
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT"
     "directive @example on
  | FIELD
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
")))

(deftest input-output-types
  (testing "scalar"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))))
           (query-type (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)))
      (with-context (:schema (gql::make-schema :query query-type :types definitions))
        (let ((dog (gethash "Dog" (gql::type-map (gql::schema gql::*context*)))))
          (ok (gql::input-type-p (gql::ty (gethash "name" (gql::fields dog)))))
          (ok (gql::input-type-p (gql::ty (gethash "nickname" (gql::fields dog)))))
          (ok (gql::output-type-p (gql::ty (gethash "barkVolume" (gql::fields dog)))))
          (ok (gql::output-type-p (gql::ty (gethash "name" (gql::fields dog)))))))))
  (testing "union"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))))
           (query-type (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)))
      (with-context (:schema (gql::make-schema :query query-type :types definitions))
        (let ((human-or-alien (gethash "HumanOrAlien" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Human" (gql::union-members human-or-alien))))
          (ng (gql::input-type-p (gethash "Alien" (gql::union-members human-or-alien))))
          (ok (gql::output-type-p (gethash "Human" (gql::union-members human-or-alien))))
          (ok (gql::output-type-p (gethash "Alien" (gql::union-members human-or-alien))))))))
  (testing "object"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))))
           (query-type (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)))
      (with-context (:schema (gql::make-schema :query query-type :types definitions))
        (let ((dog-or-human (gethash "DogOrHuman" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Dog" (gql::union-members dog-or-human))))
          (ng (gql::input-type-p (gethash "Human" (gql::union-members dog-or-human))))
          (ok (gql::output-type-p (gethash "Dog" (gql::union-members dog-or-human))))
          (ok (gql::output-type-p (gethash "Human" (gql::union-members dog-or-human))))))))
  (testing "interface"
    (let* ((definitions (gql::definitions (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))))
           (query-type (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions)))
      (with-context (:schema (gql::make-schema :query query-type :types definitions))
        (let ((cat (gethash "Cat" (gql::type-map (gql::schema gql::*context*)))))
          (ng (gql::input-type-p (gethash "Pet" (gql::interfaces cat))))
          (ok (gql::output-type-p (gethash "Pet" (gql::interfaces cat)))))))))
