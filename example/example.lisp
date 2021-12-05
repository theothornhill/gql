(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :hunchentoot) :silent t))

(defpackage :gql-exampleapp
  (:use :cl :gql))

(in-package :gql-exampleapp)

(defvar *example-schema*
  (build-schema (asdf:system-relative-pathname 'gql "example/schema.graphql")))
(defvar *variable-values* (make-hash-table :test #'equal))

;; We make the hash table corresponding to the type in "example/schema.graphql"
(defparameter *Query* (make-hash-table :test #'equal))

;; The functions in here are to be used by the `resolve' internally in gql
(setf (gethash "name" *Query*) (lambda () "Theodor Thornhill"))
(setf (gethash "age" *Query*) (lambda () 31))

;; Make sure that we actually set the resolver 
(setf *resolvers* (make-hash-table :test #'equal))
(setf (gethash "Query" *resolvers*) *Query*)

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun query (item)
  (build-schema (format nil "query { ~a }" item)))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (when item
    (with-schema *example-schema*
      (let ((result (execute-request (query item) nil *variable-values* nil)))
        (format nil "~a~%" (cl-json:encode-json-to-string result))))))

;; Eval this when you want to run the app (hunchentoot:start *server*)
