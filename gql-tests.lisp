(defpackage #:gql/tests
  (:use #:cl #:gql #:rove))

(in-package #:gql/tests)

(defun test-lexer-one-step (str)
  (advance (make-lexer str)))

(defun test-lexer-two-steps (str)
  (let ((lexer (make-lexer str)))
    (advance lexer)
    (advance lexer)))

(defun check-token (&key
                      str
                      (kind 'gql::name)
                      (start 0)
                      (end 1)
                      (line 1)
                      (column 1)
                      value
                      (fn #'test-lexer-one-step))
  (let ((token (funcall fn str)))
    (ok (eq     (gql::kind   token)  kind))
    (ok (eq     (gql::start  token)  start))
    (ok (eq     (gql::end    token)  end))
    (ok (eq     (gql::line   token)  line))
    (ok (eq     (gql::column token)  column))
    (ok (equalp (gql::value  token)  value))))

(defmacro signals-with-check (string condition-type expected)
  "Hack to check condition message."
  `(handler-case (gql ,string)
     (,condition-type (c) (ok (string= (format nil "~a" c) ,expected)))))

(deftest lexer
  (testing "Disallows uncommon control characters"
    (ok (signals (test-lexer-one-step (format nil "~c" #\U+0007))
            'gql-simple-error)))

  (testing "Accepts BOM headers"
    (check-token :str (format nil "~c foo" #\U+FEFF)
                 :start 2 :end 5
                 :column 3
                 :value "foo"))

  (testing "Tracks line breaks"
    (check-token :str "foo"
                 :start 0 :end 3
                 :value "foo")

    (check-token :str (format nil "~cfoo" #\Newline)
                 :start 1 :end 4
                 :line 2 :value "foo")

    (check-token :str (format nil "~cfoo" #\Return)
                 :start 1 :end 4
                 :line 2 :value "foo")

    (check-token :str (format nil "~c~cfoo" #\Return #\Linefeed)
                 :start 2 :end 5
                 :line 2 :value "foo")

    (check-token :str (format nil "~c~c~c~cfoo"
                              #\Return #\Return
                              #\Newline #\Newline)
                 :start 4 :end 7
                 :line 4 :value "foo")

    (check-token :str (format nil "~c~c~c~cfoo"
                              #\Newline #\Newline
                              #\Return #\Return)
                 :start 4 :end 7
                 :line 5 :value "foo"))

  (testing "Records line and column"
    (check-token :str (format nil "~c ~c~c ~c  foo~c"
                              #\Newline #\Return #\Newline #\Return #\Newline)
                 :start 8 :end 11
                 :line 4 :column 3
                 :value "foo"))

  (testing "Skips whitespace and comments"
    (check-token :str "

    foo


"
                 :start 6 :end 9
                 :line 3 :column 5
                 :value "foo")

    (check-token :str "
    #comment
    foo#comment
"
                 :start 18 :end 21
                 :line 3 :column 5
                 :value "foo")

    (check-token :str ",,,foo,,,"
                 :start 3 :end 6
                 :column 4 :value "foo"))

  (testing "String lexing"
    ;; TODO: Need to work without ending space
    (check-token :str "\"\" "
                 :kind 'gql::string
                 :end 2 :value "")

    (check-token :str "\"simple\""
                 :kind 'gql::string :end 8
                 :value "simple")

    (check-token :str "\" white space \""
                 :kind 'gql::string :end 15
                 :value " white space ")

    (check-token :str (format nil "\"quote ~c~c\"" #\\ #\\)
                 :kind 'gql::string :end 10
                 :value "quote \\")

    (check-token :str (format nil "\"escaped \\n\\r\\b\\t\\f\"")
                 :kind 'gql::string :end 20
                 :value (format nil "escaped ~c~c~c~c~c"
                                #\Newline #\Return #\Backspace #\Tab #\Page))

    ;; Unicode test cases
    (ok (= 0  (gql::char-to-hex (char-code #\0))))
    (ok (= 9  (gql::char-to-hex (char-code #\9))))
    (ok (= 10 (gql::char-to-hex (char-code #\A))))
    (ok (= 10 (gql::char-to-hex (char-code #\a))))
    (ok (= 15 (gql::char-to-hex (char-code #\F))))
    (ok (= 15 (gql::char-to-hex (char-code #\f))))

    (ok (= 15 (gql::unicode-char (char-code #\0)
                                 (char-code #\0)
                                 (char-code #\0)
                                 (char-code #\F))))

    (ok (= 255 (gql::unicode-char (char-code #\0)
                                  (char-code #\0)
                                  (char-code #\F)
                                  (char-code #\F))))

    (check-token :str (format nil "\"unicode \\u1234\\u5678\\u90AB\\uCDEF\"")
                 :kind 'gql::string :end 34
                 :value "unicode ሴ噸邫췯"))

  (testing "Block string lexing"
    ;; TODO: Need to work without ending space
    (check-token :str (format nil "\"\"\"\"\"\"")
                 :kind 'gql::block-string
                 :end 6 :value "")

    (check-token :str "\"\"\"simple\"\"\""
                 :kind 'gql::block-string :end 12
                 :value "simple")

    (check-token :str "\"\"\" white space \"\"\""
                 :kind 'gql::block-string :end 19
                 :value " white space ")

    (check-token :str (format nil "\"\"\"contains \" quote\"\"\"")
                 :kind 'gql::block-string :end 22
                 :value "contains \" quote")

    (check-token :str (format nil "\"\"\"contains \\\"\"\" triple quote\"\"\"")
                 :kind 'gql::block-string :end 32
                 :value "contains \"\"\" triple quote")

    (check-token :str (format nil "\"\"\"multi
line\"\"\"")
                 :kind 'gql::block-string :end 16
                 :value "multi
line")

    (check-token :str (format nil "\"\"\"multi~cline\"\"\"" #\Newline)
                 :kind 'gql::block-string :end 16
                 :value "multi
line")

    (check-token :str  (format nil "\"\"\"multi
line\"\"\"")
                 :kind 'gql::block-string :end 16
                 :value (format nil "multi~cline" #\Newline))

    (check-token :str (format nil "\"\"\"unescaped \\n\\r\\b\\t\\f\\u1234\"\"\"")
                 :kind 'gql::block-string :end 32
                 :value (format nil "unescaped \\n\\r\\b\\t\\f\\u1234"))

    (check-token :str (format nil "\"\"\"slashes \\\\ \\/\"\"\"")
                 :kind 'gql::block-string :end 19
                 :value (format nil "slashes \\\\ \\/"))

    (check-token :str "\"\"\"

        spans
          multiple
            lines

        \"\"\""
                 :kind 'gql::block-string :end 68
                 :value  "

        spans
          multiple
            lines

        ")

    (check-token :str (format nil "\"\"\"

        spans
          multiple
            lines

        ~c \"\"\" second_token" #\Newline)
                 :fn #'test-lexer-two-steps
                 :kind 'gql::name
                 :start 71 :end 83
                 :line 8 :column 6
                 :value "second_token")

    (check-token :str (concatenate
                       'string
                       (format nil "\"\"\" ~c" #\Newline)
                       (format nil "spans ~c~c" #\Return #\Newline)
                       (format nil "multiple ~c~c" #\Newline #\Return)
                       (format nil "lines ~c~c" #\Newline #\Newline)
                       (format nil "\"\"\"~c second_token" #\Newline))
                 :fn #'test-lexer-two-steps
                 :kind 'gql::name
                 :start 37 :end 49
                 :line 8 :column 2
                 :value "second_token"))

  (testing "Number lexing"
    ;; TODO: Crashes without trailing space again. Figure this out soon.
    (check-token :str "4 "           :kind 'gql::int :value "4")
    (check-token :str "4.123 "       :kind 'gql::float :end 5 :value "4.123")
    (check-token :str "-4 "          :kind 'gql::int :end 2 :value "-4")
    (check-token :str "9 "           :kind 'gql::int :value "9")
    (check-token :str "0 "           :kind 'gql::int :value "0")
    (check-token :str "-4.123 "      :kind 'gql::float :end 6 :value "-4.123")
    (check-token :str "0.123 "       :kind 'gql::float :end 5 :value "0.123")
    (check-token :str "123e4 "       :kind 'gql::float :end 5 :value "123e4")
    (check-token :str "123E4 "       :kind 'gql::float :end 5 :value "123E4")
    (check-token :str "123e-4 "      :kind 'gql::float :end 6 :value "123e-4")
    (check-token :str "123e+4 "      :kind 'gql::float :end 6 :value "123e+4")
    (check-token :str "-1.123e4 "    :kind 'gql::float :end 8 :value "-1.123e4")
    (check-token :str "-1.123E4 "    :kind 'gql::float :end 8 :value "-1.123E4")
    (check-token :str "-1.123e-4 "   :kind 'gql::float :end 9 :value "-1.123e-4")
    (check-token :str "-1.123e+4 "   :kind 'gql::float :end 9 :value "-1.123e+4")
    (check-token :str "-1.123e4567 " :kind 'gql::float :end 11 :value "-1.123e4567"))

  (testing "Punctuation lexing"
    (check-token :str "!"   :kind 'gql::bang)
    (check-token :str "$"   :kind 'gql::dollar)
    (check-token :str "("   :kind 'gql::paren-l)
    (check-token :str ")"   :kind 'gql::paren-r)
    (check-token :str "..." :kind 'gql::spread :end 3)
    (check-token :str ":"   :kind 'gql::colon)
    (check-token :str "="   :kind 'gql::equals)
    (check-token :str "@"   :kind 'gql::at)
    (check-token :str "["   :kind 'gql::bracket-l)
    (check-token :str "]"   :kind 'gql::bracket-r)
    (check-token :str "{"   :kind 'gql::brace-l)
    (check-token :str "|"   :kind 'gql::pipe)
    (check-token :str "}"   :kind 'gql::brace-r)))

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
      (ok (string= (gql::operation definition) "query"))
      (ok (= (gql::column (gql::start operation-location)) 1))
      (ok (= (gql::column (gql::end operation-location)) 11))
      (ok (= (gql::column (gql::end operation-location)) 11))

      (ok (= (gql::column (gql::start selection-set-location)) 7))
      (ok (= (gql::column (gql::end selection-set-location)) 11))

      (ok (= (gql::column (gql::start x-location)) 9))
      (ok (= (gql::column (gql::end x-location)) 9))))

  (testing "Schema"
    (ok (gql (asdf:system-relative-pathname 'gql #p"test-files/example-schema.txt")))
    (signals-with-check (gql (asdf:system-relative-pathname 'gql #p"test-files/empty-object.txt"))
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
    (ok (gql "scalar Time "))
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

(defun generator-test (input output)
  (let ((doc (nth-value 1 (gql input))))
    (ok (string-equal (generate doc 0 nil) output))))

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
")))


