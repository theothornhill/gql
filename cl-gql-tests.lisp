(defpackage #:cl-gql/tests
  (:use #:cl #:cl-gql #:rove))

(in-package #:cl-gql/tests)

(defun test-lexer-one-step (str)
  (advance (make-lexer str)))

(defun check-token (&key str (sym 'cl-gql::name) start end line column value)
  (let ((token (test-lexer-one-step str)))
    (ok (eq (cl-gql::kind token) sym))
    (ok (eq (cl-gql::start token) start))
    (ok (eq (cl-gql::end token) end))
    (ok (eq (cl-gql::line token) line))
    (ok (eq (cl-gql::column token) column))
    (ok (equalp (cl-gql::value token) value))))

(deftest lexer
  (testing "Disallows uncommon control characters"
    (ok (test-lexer-one-step (format nil "~c" #\U+0007))))

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
    (check-token :str (format nil "~c~cfoo" #\Return #\Newline)
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
                 :value "foo")))
