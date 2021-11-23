(in-package #:gql-tests)

(deftest parser
  (testing "Detects EOF"
    (signals-with-check "{" gql-simple-error "Expected NAME, found EOF"))
  (testing "Returns OK"
    (ok (gql "query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
} "))
    (ok (gql "{
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}"))
    (ok (gql "query { name: string @deprecated(lol: 4) }"))
    (ok (gql "fragment friendFields on User {
  id
  name
  profilePic(size: 50)
}"))
    (ok (gql "query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}
fragment friendFields on User {
  id
  name
  profilePic(size: 50)
}"))
    (ok (gql "query inlineFragmentTyping {
  profiles(handles: 5) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}"))
    (ok (gql "query inlineFragmentNoType($expandedInfo: Boolean) {
  user(handle: $expandedInfo) {
    id
    name
    ... @include(if: $expandedInfo) {
      firstName
      lastName
      birthday
    }
  }
}"))
    (ok (gql "query getZuckProfile($devicePicSize: Int) {
  user(id: $devicePicSize) {
    id
    name
  }
}"))
    (ok (gql "query inlineFragmentTyping {
  profiles(handles: [\"foo\", \"bar\"]) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}"))
    (ok (gql "mutation {
  sendEmail(message: \"\"\"
    Hello,
      World!

    Yours,
      GraphQL.
  \"\"\")
}")))

  (testing "Locations"
    (let* ((document (nth-value 1 (gql "query { x }")))
           (definition (car (gql::definitions document)))
           (operation-location (gql::location definition))
           (selection-set (gql::selection-set definition))
           (selection-set-location (gql::location selection-set))
           (x-location (gql::location (car (gql::selections selection-set)))))
      (ok (string= (gql::operation-type definition) "Query"))
      (ok (= (gql::column (gql::start-token operation-location)) 0))
      (ok (= (gql::column (gql::end-token operation-location)) 10))
      (ok (= (gql::column (gql::end-token operation-location)) 10))

      (ok (= (gql::column (gql::start-token selection-set-location)) 6))
      (ok (= (gql::column (gql::end-token selection-set-location)) 10))

      (ok (= (gql::column (gql::start-token x-location)) 8))
      (ok (= (gql::column (gql::end-token x-location)) 8))))

  (testing "Schema"
    (ok (gql (asdf:system-relative-pathname 'gql-tests #p"t/test-files/example-schema.txt")))
    (signals-with-check (gql (asdf:system-relative-pathname 'gql-tests #p"t/test-files/empty-object.txt"))
                        gql-simple-error "Expected NAME, found BRACE-R")
    (ok (gql "
\"\"\" 
Some description at the start
\"\"\"
schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}"))
    (ok (gql "schema {
  query: MyQueryRootType
  mutation: MyMutationRootType
}"))
    (ok (gql "scalar Time"))
    (ok (gql "scalar UUID @specifiedBy(url: \"https://tools.ietf.org/html/rfc4122\")"))
    (ok (gql "
scalar Time
scalar Url"))
    (ok (gql "
type Person implements NamedEntity {
  name: String
  age: Int
}"))
    (ok (gql "
type Business implements NamedEntity & ValuedEntity {
  name: String
  value: Int
  employeeCount: Int
}"))
    (ok (gql "
type Business implements NamedEntity & ValuedEntity {
  \"\"\"with some docs\"\"\"
  name: String
  value: Int
  employeeCount: Int
}"))
    (ok (gql "
interface NamedEntity {
  name: String
}"))
    (ok (gql "
\"\"\"docs\"\"\"
interface NamedEntity {
\"\"\"docs\"\"\"
  name: String
}"))
    (ok (gql "union SearchResult = Photo | Person"))
    (ok (gql "
enum Direction {
  NORTH
  EAST
  SOUTH
  WEST
}"))
    (ok (gql "
enum Direction {
\"\"\"thing\"\"\"
  NORTH
\"\"\"thang\"\"\"
  EAST
\"\"\"thong\"\"\"
  SOUTH
  WEST
}"))
    (ok (gql "
input Point2D {
  x: Float
  y: Float
}"))
    (ok (gql "
input Point2D {
  x: Float = 4.0
  y: Float
}"))
    (ok (gql "
input Point2D {
\"\"\"docs\"\"\"
  x: Float = 4.0
  y: Float
}"))
    (ok (gql "
input Point2D {
\"\"\"docs\"\"\"
  x: String = \"Hello World\"
  y: Float
}"))
    (ok (gql "directive @example on FIELD"))
    (ok (gql "
directive @example on
  | FIELD
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT"))
    (ok (gql "
directive @example on FIELD_DEFINITION | ARGUMENT_DEFINITION

type SomeType {
  field(arg: Int @example): String @example
}"))
    (ok (gql "
extend type Story {
  isHiddenLocally: Boolean
}"))
    (ok (gql "extend type User @addedDirective"))
    (ok (gql "
extend interface NamedEntity {
  nickname: String
}"))
    (ok (gql "extend interface NamedEntity @addedDirective"))
    (ok (gql "
extend type Query {
  findDog(complex: ComplexInput): Dog
  booleanList(booleanListArg: [Boolean!]): Boolean
}"))))
