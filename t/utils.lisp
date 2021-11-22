(in-package #:gql-tests)

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
                      (column 0)
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

(defun generator-test (input output)
  (ok (string-equal
       (generate (build-schema input) :indent-level 0 :stream nil)
       output)))

(defun validator-test (input)
  (setf *errors* nil)
  (validate (build-schema input)))

(defun validator-errors (input)
  (nth-value 1 (validator-test input)))
