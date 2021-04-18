(in-package :gql)

;; Super simple proof of concept.
;; GQL> (gql "query { x: String }")
;; "query { x: String }"
;; #<DOCUMENT {7008E42963}>
;; GQL> (print-node #v0:1 t)
;; query { x: String }
;; NIL

(defgeneric print-node (node stream)
  (:documentation "print a node as a valid GrqphQL statement."))

(defmethod print-node ((node document) stream)
  (let ((definitions
          (mapcar (lambda (x) (format nil "~a" (print-node x nil)))
                  (definitions node))))
    (format stream "~{~a~}" definitions)))

(defmethod print-node ((node operation-definition) stream)
  (format stream "~a ~@[~a~] ~a"
          (operation node)
          (when (name node) (print-node (name node) nil))
          ;; (variable-definitions node)
          ;; (directives node)
          (print-node (selection-set node) nil)
          ;; (definitions node)
          ))

(defmethod print-node ((node name) stream)
  (format stream "~@[~a~]" (name node)))

(defmethod print-node ((node selection-set) stream)
  (let ((selections
          (mapcar (lambda (x) (format nil "~a" (print-node x nil)))
                  (selections node))))
    (format stream "{ ~{~a~} }" selections)))

(defmethod print-node ((node field) stream)
  (format stream "~a: ~a"
          (print-node (alias node) nil)
          (print-node (name node) nil)))

(defmethod print-node ((node argument) stream)
  (format stream "~a: ~a" (name node) (value node)))
