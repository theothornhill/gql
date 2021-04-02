(defpackage #:cl-gql/tests
  (:use #:cl #:cl-gql #:rove))

(in-package #:cl-gql/tests)

(defun test-lexer-one-step (str)
  (advance (make-lexer str)))

(defun test-lexer-two-steps (str)
  (let ((lexer (make-lexer str)))
    (advance lexer)
    (advance lexer)))

(defun check-token (&key str
                      (kind 'cl-gql::name) (start 0) (end 1)
                      (line 1) (column 1) value (fn #'test-lexer-one-step))
  (let ((token (funcall fn str)))
    (ok (eq (cl-gql::kind token) kind))
    (ok (eq (cl-gql::start token) start))
    (ok (eq (cl-gql::end token) end))
    (ok (eq (cl-gql::line token) line))
    (ok (eq (cl-gql::column token) column))
    (ok (equalp (cl-gql::value token) value))))

(deftest lexer
  (testing "Disallows uncommon control characters"
    (ok (signals (test-lexer-one-step (format nil "~c" #\U+0007))
            'simple-error)))

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
                 :kind 'cl-gql::string
                 :end 2 :value "")

    (check-token :str "\"simple\""
                 :kind 'cl-gql::string :end 8
                 :value "simple")

    (check-token :str "\" white space \""
                 :kind 'cl-gql::string :end 15
                 :value " white space ")

    (check-token :str (format nil "\"quote ~c~c\"" #\\ #\\)
                 :kind 'cl-gql::string :end 10
                 :value "quote \\")

    (check-token :str (format nil "\"escaped \\n\\r\\b\\t\\f\"")
                 :kind 'cl-gql::string :end 20
                 :value (format nil "escaped ~c~c~c~c~c"
                                #\Newline #\Return #\Backspace #\Tab #\Page))

    ;; Unicode test https://github.com/graphql/graphql-js/blob/main/src/language/__tests__/lexer-test.js#L259
    )

  (testing "Block string lexing"
    ;; TODO: Need to work without ending space
    (check-token :str (format nil "\"\"\"\"\"\"")
                 :kind 'cl-gql::block-string
                 :end 6 :value "")

    (check-token :str "\"\"\"simple\"\"\""
                 :kind 'cl-gql::block-string :end 12
                 :value "simple")

    (check-token :str "\"\"\" white space \"\"\""
                 :kind 'cl-gql::block-string :end 19
                 :value " white space ")

    (check-token :str (format nil "\"\"\"contains \" quote\"\"\"")
                 :kind 'cl-gql::block-string :end 22
                 :value "contains \" quote")

    (check-token :str (format nil "\"\"\"contains \\\"\"\" triple quote\"\"\"")
                 :kind 'cl-gql::block-string :end 32
                 :value "contains \"\"\" triple quote")

    (check-token :str (format nil "\"\"\"multi
line\"\"\"")
                 :kind 'cl-gql::block-string :end 16
                 :value "multi
line")

    (check-token :str (format nil "\"\"\"multi~cline\"\"\"" #\Newline)
                 :kind 'cl-gql::block-string :end 16
                 :value "multi
line")

    (check-token :str  (format nil "\"\"\"multi
line\"\"\"")
                 :kind 'cl-gql::block-string :end 16
                 :value (format nil "multi~cline" #\Newline))

    (check-token :str (format nil "\"\"\"unescaped \\n\\r\\b\\t\\f\\u1234\"\"\"")
                 :kind 'cl-gql::block-string :end 32
                 :value (format nil "unescaped \\n\\r\\b\\t\\f\\u1234"))

    (check-token :str (format nil "\"\"\"slashes \\\\ \\/\"\"\"")
                 :kind 'cl-gql::block-string :end 19
                 :value (format nil "slashes \\\\ \\/"))

    (check-token :str "\"\"\"

        spans
          multiple
            lines

        \"\"\""
                 :kind 'cl-gql::block-string :end 68
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
                 :kind 'cl-gql::name
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
                 :kind 'cl-gql::name
                 :start 37 :end 49
                 :line 8 :column 2
                 :value "second_token")

    ;; Unicode test https://github.com/graphql/graphql-js/blob/main/src/language/__tests__/lexer-test.js#L259
    )




  (testing "Punctuation lexing"
    (check-token :str "!"   :kind 'cl-gql::bang)
    (check-token :str "$"   :kind 'cl-gql::dollar)
    (check-token :str "("   :kind 'cl-gql::paren-l)
    (check-token :str ")"   :kind 'cl-gql::paren-r)
    (check-token :str "..." :kind 'cl-gql::spread :end 3)
    (check-token :str ":"   :kind 'cl-gql::colon)
    (check-token :str "="   :kind 'cl-gql::equals)
    (check-token :str "@"   :kind 'cl-gql::at)
    (check-token :str "["   :kind 'cl-gql::bracket-l)
    (check-token :str "]"   :kind 'cl-gql::bracket-r)
    (check-token :str "{"   :kind 'cl-gql::brace-l)
    (check-token :str "|"   :kind 'cl-gql::pipe)
    (check-token :str "}"   :kind 'cl-gql::brace-r))

  )
