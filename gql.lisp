(in-package #:gql)

;;; Utils

(defun slurp (file)
  (uiop:read-file-string file))

;;; api

(defgeneric gql (input &optional debug)
  (:documentation "Entry point for lexing and parsing"))

(defmethod gql :before (input &optional debug)
  (setf *debug-print* debug))

(defmethod gql ((str string) &optional debug)
  (declare (ignorable debug))
  (values str (parse (make-parser str) :document)))

(defmethod gql ((f pathname) &optional debug)
  (declare (ignorable debug))
  (let ((str (slurp f)))
    (values str (parse (make-parser str) :document))))
