(in-package #:gql-tests)

(deftest generator
  (testing "Simple nested query"
    (generator-test
     "query lol { x { y } }"
     "query lol {
  x {
    y
  }
}
"))
  (testing "Advanced field"
    (generator-test
     "query { smallPic: profilePic(size: 64) { x } }"
     "query {
  smallPic: profilePic(size: 64) {
    x
  }
}
"))
  (testing "Advanced field with multiple args"
    (generator-test
     "query { smallPic: profilePic(size: 64, size2: 128) { x } }"
     "query {
  smallPic: profilePic(size: 64, size2: 128) {
    x
  }
}
"))
  (testing "Advanced field with directive"
    (generator-test
     "query { smallPic: profilePic(size: 64, size2: 128) @skip(if: true) { x } }"
     "query {
  smallPic: profilePic(size: 64, size2: 128) @skip(if: true) {
    x
  }
}
"))
  (testing "Advanced field without selection-set"
    (generator-test
     "query { smallPic: profilePic(size: 64, size2: 128) }"
     "query {
  smallPic: profilePic(size: 64, size2: 128)
}
"))
  (testing "Advanced field with multiple directives"
    (generator-test
     "query { smallPic: profilePic(size: $foo) @skip(if: $foo) @include(if: $bar) { x } }"
     "query {
  smallPic: profilePic(size: $foo) @skip(if: $foo) @include(if: $bar) {
    x
  }
}
"))
  (testing "Operation definition with directive"
    (generator-test
     "query myQuery @skip(if: $someTest) { field }"
     "query myQuery @skip(if: $someTest) {
  field
}
"))
  (testing "Operation definition with variable-definitions"
    (generator-test
     "query myQuery($someTest: Boolean) { field }"
     "query myQuery($someTest: Boolean) {
  field
}
"))
  (testing "Operation definition with multiple variable-definitions"
    (generator-test
     "query myQuery($someTest: Boolean, $anotherTest: String) { field }"
     "query myQuery($someTest: Boolean, $anotherTest: String) {
  field
}
"))
  (testing "Operation definition with variable-definitions and directives"
    (generator-test
     "query myQuery($someTest: Boolean) @skip(if: $someTest) { field }"
     "query myQuery($someTest: Boolean) @skip(if: $someTest) {
  field
}
"))

  (testing "Fragment spread"
    (generator-test
     "query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}"
     "query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}
"))
  (testing "Fragment definition with selection-set"
    (generator-test
     "fragment friendFields on User {
  x
}"
     "fragment friendFields on User {
  x
}
"))
  (testing "Values"
    (generator-test
     "{ user(id: 4) { x } }"
     "query {
  user(id: 4) {
    x
  }
}
")
    (generator-test
     "{ user(id: 4.1) { x } }"
     "query {
  user(id: 4.1) {
    x
  }
}
")
    (generator-test
     "{ user(id: \"hello\") { x } }"
     "query {
  user(id: \"hello\") {
    x
  }
}
")
    (generator-test
     "{ user(id: true) { x } }"
     "query {
  user(id: true) {
    x
  }
}
")
    (generator-test
     "{ user(id: null) { x } }"
     "query {
  user(id: null) {
    x
  }
}
")
    (generator-test
     "{ user(id: thing) { x } }"
     "query {
  user(id: thing) {
    x
  }
}
")
    (generator-test
     "{ user(id: [1, 2, 3]) { x } }"
     "query {
  user(id: [1, 2, 3]) {
    x
  }
}
")
    (generator-test
     "{ user(id: [1]) { x } }"
     "query {
  user(id: [1]) {
    x
  }
}
")
    (generator-test
     "{ user(id: []) { x } }"
     "query {
  user(id: []) {
    x
  }
}
")
    (generator-test
     "{ nearestThing(location: { lon: 12.43, lat: -53.211 }) }"
     "query {
  nearestThing(location: { lon: 12.43, lat: -53.211 })
}
")
    (generator-test
     "{ nearestThing(location: { lon: 12.43 }) }"
     "query {
  nearestThing(location: { lon: 12.43 })
}
")
    (generator-test
     "{ nearestThing(location: {}) }"
     "query {
  nearestThing(location: {})
}
")
    (generator-test
     "query inlineFragmentTyping {
  profiles(handles: [\"one\", \"thing\"]) {
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
}"
     "query inlineFragmentTyping {
  profiles(handles: [\"one\", \"thing\"]) {
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
}
")
    (generator-test
     "query thing($devicePicSize: [Int!]) {
  x
}"
     "query thing($devicePicSize: [Int!]) {
  x
}
")
    (generator-test
     "query thing($devicePicSize: [Int]!) {
  x
}"
     "query thing($devicePicSize: [Int]!) {
  x
}
")
    (generator-test
     "query thing($devicePicSize: [Int!]!) {
  x
}"
     "query thing($devicePicSize: [Int!]!) {
  x
}
")
    (generator-test
     "query thing($devicePicSize: [Int]) {
  x
}"
     "query thing($devicePicSize: [Int]) {
  x
}
")
    (generator-test
     "query thing($devicePicSize: [[Int]]) {
  x
}"
     "query thing($devicePicSize: [[Int]]) {
  x
}
")))
