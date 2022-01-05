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
(defobject "Query"
  "Query object"
  (("dog" :type (gql::named "Dog") :resolver (constantly *doggo*))))

(defobject "Dog"
  "A Dog is a dog!"
  (("name" :type *string*
           :resolver (lambda ()
                       (with-slots (object-value) (execution-context *context*)
                         (name object-value))))
   ("nickname" :type *string*)
   ("barkVolume" :type *int*)
   ("owner" :type (gql::named "Human")
            :resolver (lambda () (make-instance 'human :name "Petter Smart" :pets '())))))

(defobject "Human"
  "A Human is a human!"
  (("name" :type *string*
           :resolver (lambda ()
                       (with-slots (object-value) (execution-context *context*)
                         (name object-value))))
   ("pets" :type ([!] "Pet"))))


(defschema
    (:query (gql::find-item "Query")
     :types (gql::find-items '("Dog" "Human")))
  ())

(defun example2 (query)
  (with-context (:document (build-document query))
    (let* ((res (gql::execute)))
      (format t "~%~a" (cl-json:encode-json-to-string res)))))

;; (example2 "{ __schema { types { name ofType { name } } } }")
;; (example2 "{ __schema { queryType { name } } }")
;; (example2 "{ __type(name: \"Dog\") { name fields { name type { name } } } }")
;; (example2 "query { dog { name owner { name pets { name } } } }")
(example2 "query { dog { name owner { name } } }")
;; (example2 "query { dog { name owner: wingle { name pets: dogs { name } } } }")

