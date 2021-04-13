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
  (let ((*parser* (make-parser str)))
    (values str (parse 'document))))

(defmethod gql ((f pathname) &optional debug)
  (declare (ignorable debug))
  (let* ((str (slurp f))
         (*parser* (make-parser str)))
    (values str (parse 'document))))
