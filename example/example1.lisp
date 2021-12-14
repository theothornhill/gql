(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :hunchentoot) :silent t))

(defpackage :gql-example1
  (:use :cl :gql))

(in-package :gql-example1)

(defparameter *fields*
  (list
   (gql::field :name "name"
               :type (make-instance 'gql::named-type :name (make-instance 'gql::name :name "String"))
               :resolver (constantly "Theodor Thornhill"))
   (gql::field :name "age"
               :type (make-instance 'gql::named-type :name (make-instance 'gql::name :name "Int"))
               :resolver (constantly 31))))

(defvar *example-schema*
  (build-schema `(,(gql::object :name "Query" :fields *fields*))))

(defvar *variable-values* (make-hash-table :test #'equal))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (when item
    (with-schema *example-schema*
      (let ((result (execute (build-schema (format nil "{ __type(name: Query) { name } }"))
                             nil
                             *variable-values* nil)))
        (format nil "~a~%" (cl-json:encode-json-to-string result))))))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun query (item)
  (build-schema (format nil "query { __type(name: Query) { name } }" item)))

;; Eval this when you want to run the app (hunchentoot:start *server*)
