(in-package :gql)

;; Super simple proof of concept.
;; GQL> (gql "query { x: String }")
;; "query { x: String }"
;; #<DOCUMENT {7008E42963}>
;; GQL> (print-node #v0:1 t)
;; query { x: String }
;; NIL

(defgeneric print-node (node &optional stream)
  (:documentation "print a node as a valid GrqphQL statement."))

(defmethod print-node ((node document) &optional (stream nil))
  (let ((definitions
          (mapcar (lambda (x) (format nil "~a" (print-node x)))
                  (definitions node))))
    (format stream "~{~a~}" definitions)))

(defmethod print-node ((node operation-definition) &optional (stream nil))
  (format stream "~a ~@[~a~] ~a"
          (operation node)
          (when (name node) (print-node (name node)))
          ;; (variable-definitions node)
          ;; (directives node)
          (print-node (selection-set node))
          ;; (definitions node)
          ))

(defmethod print-node ((node name) &optional (stream nil))
  (format stream "~@[~a~]" (name node)))

(defmethod print-node ((node selection-set) &optional (stream nil))
  (let ((selections
          (mapcar (lambda (x) (format nil "~a" (print-node x)))
                  (selections node))))
    (format stream "{ ~{~a~} }" selections)))

(defmethod print-node ((node field) &optional (stream nil))
  (format stream "~a: ~a"
          (print-node (alias node))
          (print-node (name node))))

(defmethod print-node ((node argument) &optional (stream nil))
  (format stream "~a: ~a" (name node) (value node)))
