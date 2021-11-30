(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :drakma :hunchentoot) :silent t))

(defpackage :gql-exampleapp
  (:use :cl :gql))

(in-package :gql-exampleapp)

(defparameter *fake-db* (make-hash-table :test #'equal))
(setf (gethash "name" *fake-db*) "Theodor")
(setf (gethash "age" *fake-db*) 31)

(defvar *example-schema*
  (build-schema (asdf:system-relative-pathname 'gql "example/schema.graphql")))
(defvar *variable-values* (make-hash-table :test #'equal))

(defmethod resolve (object-type object-value field-name arg-values)
  (gethash field-name *fake-db*))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun query (item)
  (build-schema (format nil "query { ~a }" item)))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (with-schema *example-schema*
    (let ((result (execute-request (query item) nil *variable-values* nil)))
      (format nil "~a~%" (cl-json:encode-json-to-string result)))))

;; Eval this when you want to run the app (hunchentoot:start *server*)
