(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json) :silent t))

(in-package #:gql-example)

(defclass pet (gql-object)
  ((name :initarg :name :accessor name)))

(defclass dog (pet)
  ((owner :initarg :owner :accessor owner)))

(defclass sentient (gql-object)
  ((name :initarg :name :accessor name)))

(defclass human (sentient)
  ((pets :initarg :pets :accessor pets)))

(defvar *doggo*
  (make-instance
   'dog
   :name "Bingo-Bongo"
   :type-name "Dog"
   :owner (make-instance
           'human
           :name "Wingle Wangle"
           :type-name "Human"
           :pets `(,(make-instance
                     'dog
                     :name "Bingo-Bongo"
                     :type-name "Dog")
                   ,(make-instance
                     'dog
                     :name "Bango-Wango"
                     :type-name "Dog")))))

(defvar *query-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "dog" ht)
          (lambda (arg) (declare (ignorable arg)) *doggo*))
    ht))

(defvar *dog-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (lambda (dog) (name dog)))
    (setf (gethash "owner" ht) (lambda (dog) (owner dog)))
    ht))

(defvar *human-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (lambda (human) (name human)))
    (setf (gethash "pets" ht) (lambda (human) (pets human)))
    ht))

(defun example2 (query)
  (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
    (let* ((res (gql::execute-request
                 (build-schema query) nil (make-hash-table :test #'equal) nil)))
      (format t "~a" (cl-json:encode-json-to-string res)))))

(let ((*resolvers* (make-hash-table :test #'equal)))
  (setf (gethash "Query" *resolvers*) *query-resolvers*)
  (setf (gethash "Dog" *resolvers*) *dog-resolvers*)
  (setf (gethash "Human" *resolvers*) *human-resolvers*)
  (example2 "query { dog { name owner { name pets { name } } }}"))


