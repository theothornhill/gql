(in-package #:gql-tests)

(deftest type-extensions
  (testing "schema extension"
    (generator-test
     "extend schema @example { query: Lol }"
     "extend schema @example {
  query: Lol
}
"))
  (testing "scalar extension"
    (generator-test
     "extend scalar Time @example"
     "extend scalar Time @example
"))
  (testing "object-type-extension"
    (generator-test
     "extend type MutationRoot { dog: Dog arguments: Arguments }"
     "extend type MutationRoot {
  dog: Dog
  arguments: Arguments
}
")
    (generator-test
     "extend type MutationRoot @addedDirective { dog: Dog arguments: Arguments }"
     "extend type MutationRoot @addedDirective {
  dog: Dog
  arguments: Arguments
}
")
    (generator-test
     "extend type MutationRoot @addedDirective"
     "extend type MutationRoot @addedDirective
"))
  (testing "interface-type-extension"
    (generator-test
     "extend interface NamedEntity"
     "extend interface NamedEntity
")
    (generator-test
     "extend interface NamedEntity { nickname: String }"
     "extend interface NamedEntity {
  nickname: String
}
"))
  (testing "union-type-extension"
    (generator-test
     "extend union SearchResult @addedDirective = Photo | Person"
     "extend union SearchResult @addedDirective =
  | Photo
  | Person
"))
  (testing "enum-type-extension"
    (generator-test
     "extend enum SearchResult @addedDirective { FOO BAR BAZ }"
     "extend enum SearchResult @addedDirective {
  FOO
  BAR
  BAZ
}
"))
  (testing "input-object-type-extension"
    (generator-test
     "extend input Point2D { x: Float y: Float }"
     "extend input Point2D {
  x: Float
  y: Float
}
")
    (generator-test
     "extend input Point2D @addedDirective { x: Float y: Float }"
     "extend input Point2D @addedDirective {
  x: Float
  y: Float
}
")))

