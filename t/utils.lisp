(in-package #:gql-tests)

(defun test-lexer-one-step (str)
  (gql::advance (gql::make-lexer str)))

(defun test-lexer-two-steps (str)
  (let ((lexer (gql::make-lexer str)))
    (gql::advance lexer)
    (gql::advance lexer)))

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

(defun validator-test-helper (input &key no-schema)
  (let* ((definitions (gql::definitions (if no-schema
                                            (build-schema input)
                                            (build-schema (asdf:system-relative-pathname
                                                           'gql-tests
                                                           #p"t/test-files/validation-schema.graphql"))))))

    (defschema (:query (find-if (lambda (x) (string= (gql::nameof x) "Query")) definitions) 
                :types definitions)
      (:subscription (find-if (lambda (x) (string= (gql::nameof x) "Subscription")) definitions)))

    (with-context ()
      (let ((gql::*errors* nil))
          
        (gql::validate (build-schema input))
        (cl-json:encode-json-to-string gql::*errors*)))))

