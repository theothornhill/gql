(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :hunchentoot) :silent t))

(defpackage :gql-example1
  (:use :cl :gql))

(in-package :gql-example1)

(defparameter *fields*
  (list
   (field :name "name"
               :type *string*
               :resolver (constantly "Theodor Thornhill"))
   (field :name "age"
               :type *int*
               :resolver (constantly 31))))

(defparameter *query*
  (gql::object :name "Query" :fields *fields*))


(defvar *example-schema*
  (make-schema :query *query*))

(defvar *variable-values* (make-hash-table :test #'equal))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (when item
    (with-context (:schema *example-schema*
                   :document (build-document
                              (format nil "{ ~a }" item)))
      (format nil "~a~%" (cl-json:encode-json-to-string (execute))))))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

;; Eval this when you want to run the app (hunchentoot:start *server*)
