(in-package #:gql)

(define-condition gql-condition (condition) ()
  (:documentation "Superclass for all conditions related to gql."))

(define-condition gql-error (gql-condition error) ()
  (:documentation "Superclass for all errors related to gql"))

(define-condition gql-simple-error (gql-condition simple-condition) ()
  (:documentation "like GQL-ERROR, but with formatting capabilities."))

(defun gql-error (format-control &rest format-arguments)
  (error 'gql-simple-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition gql-warning (gql-condition warning) ()
  (:documentation "Superclass for all warnings related to gql"))

(define-condition gql-simple-warning (gql-condition simple-condition) ()
  (:documentation "like GQL-WARNING, but with formatting capabilities."))

(defun gql-warning (format-control &rest format-arguments)
  (warn 'gql-simple-warning
        :format-control format-control
        :format-arguments format-arguments))
