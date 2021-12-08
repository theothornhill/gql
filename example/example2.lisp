(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json) :silent t))

(defpackage :gql-example2
  (:use :cl :gql))

(in-package #:gql-example2)

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
  (make-resolvers
    ("dog" . (constantly *doggo*))))

(defvar *dog-resolvers*
  (make-resolvers
    ("name"  . 'name)
    ("owner" . 'owner)))

(defvar *human-resolvers*
  (make-resolvers
    ("name" . 'name)
    ("pets" . 'pets)))

(defun example2 (query)
  (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
    (let* ((res (gql::execute (build-schema query) nil (make-hash-table :test #'equal) nil)))
      (format t "~%~a" (cl-json:encode-json-to-string res)))))

(let ((*resolvers*
        (make-resolvers
          ("Query"    . *query-resolvers*)
          ("Dog"      . *dog-resolvers*)
          ("Human"    . *human-resolvers*))))
  (example2 "query { dog { name owner { name pets { name } } } }")
  (example2 "query { dog { name owner: wingle { name pets: dogs { name } } } }"))
