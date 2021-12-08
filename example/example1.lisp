(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :hunchentoot) :silent t))

(defpackage :gql-example1
  (:use :cl :gql))

(in-package :gql-example1)

(defvar *example-schema*
  (build-schema (asdf:system-relative-pathname 'gql "example/schema.graphql")))
(defvar *variable-values* (make-hash-table :test #'equal))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (when item
    (let* ((query-resolvers
             (make-resolvers
               ("name" . (constantly "Theodor Thornhill"))
               ("age"  . (constantly 31))))

           (*resolvers*
             (make-resolvers
               ("Query" . query-resolvers))))

      (with-schema *example-schema*
        (let ((result (execute (query item) nil *variable-values* nil)))
          (format nil "~a~%" (cl-json:encode-json-to-string result)))))))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun query (item)
  (build-schema (format nil "query { ~a }" item)))

;; Eval this when you want to run the app (hunchentoot:start *server*)
