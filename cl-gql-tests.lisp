(defpackage #:cl-gql/tests
  (:use #:cl #:cl-gql #:rove))

(in-package #:cl-gql/tests)

(defun test-lexer-one-step (str)
  (advance (make-lexer str)))

(defun check-token (&key str (kind 'cl-gql::name) start end line column value)
  (let ((token (test-lexer-one-step str)))
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
                 :line 1 :column 3
                 :value "foo"))

  (testing "Tracks line breaks"
    (check-token :str "foo"
                 :start 0 :end 3
                 :line 1 :column 1
                 :value "foo")
    (check-token :str (format nil "~cfoo" #\Newline)
                 :start 1 :end 4
                 :line 2 :column 1
                 :value "foo")
    (check-token :str (format nil "~cfoo" #\Return)
                 :start 1 :end 4
                 :line 2 :column 1
                 :value "foo")
    (check-token :str (format nil "~c~cfoo" #\Return #\Linefeed)
                 :start 2 :end 5
                 :line 2 :column 1
                 :value "foo")
    (check-token :str (format nil "~c~c~c~cfoo"
                              #\Return #\Return
                              #\Newline #\Newline)
                 :start 4 :end 7
                 :line 4 :column 1
                 :value "foo")
    (check-token :str (format nil "~c~c~c~cfoo"
                              #\Newline #\Newline
                              #\Return #\Return)
                 :start 4 :end 7
                 :line 5 :column 1
                 :value "foo"))

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
                 :line 1 :column 4
                 :value "foo"))

  (testing "String lexing"
    ;; TODO: Need to work without ending space
    (check-token :str "\"\" "
                 :kind 'cl-gql::string
                 :start 0 :end 2
                 :line 1 :column 1
                 :value "")

    (check-token :str "\"simple\""
                 :kind 'cl-gql::string
                 :start 0 :end 8
                 :line 1 :column 1
                 :value "simple")

    (check-token :str "\" white space \""
                 :kind 'cl-gql::string
                 :start 0 :end 15
                 :line 1 :column 1
                 :value " white space ")

    (check-token :str (format nil "\"quote ~c~c\"" #\\ #\\)
                 :kind 'cl-gql::string
                 :start 0 :end 10
                 :line 1 :column 1
                 :value "quote \\")

    (check-token :str (format nil "\"escaped \\n\\r\\b\\t\\f\"")
                 :kind 'cl-gql::string
                 :start 0 :end 20
                 :line 1 :column 1
                 :value (format nil "escaped ~c~c~c~c~c"
                                #\Newline #\Return #\Backspace #\Tab #\Page)))

  ;; Unicode test https://github.com/graphql/graphql-js/blob/main/src/language/__tests__/lexer-test.js#L259
  )
