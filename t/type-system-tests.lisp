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
