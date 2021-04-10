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

(defmacro signals-with-check (string condition-type expected)
  "Hack to check condition message."
  `(handler-case (gql ,string)
     (,condition-type (c) (ok (string= (format nil "~a" c) ,expected)))))

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
    (ok (gql "query { name: string @deprecated(lol: string) }"))
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
}"))))
