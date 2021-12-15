(in-package #:gql)

(defmethod print-object ((obj operation-definition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (operation-type obj) stream)))

(defmethod print-object ((obj field-definition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (nameof obj) stream)))

(defmethod print-object ((obj object-type-definition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (nameof obj) stream)))

(defmethod print-object ((obj enum-type-definition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (nameof obj) stream)))

(defmethod print-object ((obj interface-type-definition) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (nameof obj) stream)))

(defmethod print-object ((obj named-type) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (nameof obj) stream)))

(defmethod print-object ((obj name) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (name obj) stream)))


