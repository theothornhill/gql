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
   :owner (make-instance
           'human
           :name "Wingle Wangle"
           :pets `(,(make-instance
                     'dog
                     :name "Bingo-Bongo")
                   ,(make-instance
                     'dog
                     :name "Bango-Wango")))))
(defvar *query*
  (gql::object :name "Query"
               :fields `(,(gql::field :name "dog"
                                      :type (gql::named "Dog")
                                      :resolver (constantly *doggo*)))))

(defvar *dog*
  (gql::object :name "Dog"
               :description "A Dog is a dog!"
               :fields `(,(gql::field :name "name"
                                      :type (gql::named "String")
                                      :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
                         ,(gql::field :name "nickname"
                                      :type (gql::named "String"))
                         ,(gql::field :name "barkVolume"
                                      :type (gql::named "Int"))
                         ,(gql::field :name "owner"
                                      :type (gql::named "Human")
                                      :resolver (lambda () (make-instance 'human
                                                                     :name "Petter Smart"
                                                                     :pets '()))))))

(defvar *human*
  (gql::object :name "Human"
               :description "A Human is a human!"
               :fields `(,(gql::field :name "name"
                                      :type (gql::named "String")
                                      :resolver (lambda () (name (gql::object-value (gql::execution-context gql::*context*)))))
                         ,(gql::field :name "pets"
                                      :type (gql::list-type (gql::non-null-type (gql::named "Pet")))))))


(defun example2 (query)
  (with-context (:schema (gql::make-schema :query *query* :types (list *dog* *human*))
                 :document (build-document query))
    (let* ((res (gql::execute)))
      (format t "~%~a" (cl-json:encode-json-to-string res)))))

;; (example2 "{ __schema { types { name ofType { name } } } }")
(example2 "{ __schema { queryType { name } } }")
;; (example2 "{ __type(name: \"Dog\") { name fields { name type { name } } } }")
;; (example2 "query { dog { name owner { name pets { name } } } }")
;; (example2 "query { dog { name owner { name } } }")
;; (example2 "query { dog { name owner: wingle { name pets: dogs { name } } } }")

