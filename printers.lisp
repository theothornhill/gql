(in-package :gql)

(defvar *gql-indentation-level* 0)

(defun add-indent (level)
  "Add indentation for pretty printing.

(add-indent 0) => \"\"
(add-indent 1) => \"  \"
(add-indent 2) => \"    \"
"
  (let ((indentation ""))
    (dotimes (_ level)
      (setf indentation (cat indentation "  ")))
    indentation))

(defun gather-nodes (node-list indent-level)
  "Collect a list of formatted subnodes.
It indents where necessary (with help from calls to PRINT-NODE, then returns a
list of strings."
  (mapcar (lambda (x) (format nil "~a" (print-node x indent-level)))
          node-list))

(defgeneric print-node (node &optional indent-level stream)
  (:documentation "print a node as a valid GrqphQL statement."))

(defmethod print-node ((node document) &optional (indent-level 0) (stream t))
  (format stream "~{~a~}" (gather-nodes (definitions node) indent-level)))

;; (defmethod print-node ((node executable-definition) &optional (indent-level 0) (stream t))
;;   (format stream "~{~a~}" (gather-nodes (definitions node) indent-level)))

;; (defmethod print-node ((node type-system-definition) &optional (indent-level 0) (stream t))
;;   (format stream "~{~a~}" (gather-nodes (definitions node) indent-level)))

;; (defmethod print-node ((node type-system-extension) &optional (indent-level 0) (stream t))
;;   (format stream "~{~a~}" (gather-nodes (definitions node) indent-level)))

(defmethod print-node ((node operation-definition) &optional (indent-level 0) (stream nil))
  (format stream "~a~@[ ~a~] ~a"
          (operation node)
          (when (name node)
            (print-node (name node)))
          ;; (variable-definitions node)
          ;; (directives node)
          (print-node (selection-set node) (1+ indent-level))
          ;; (when (definitions node) (gather-nodes (definitions node) indent-level))
          ))

(defmethod print-node ((node fragment-definition) &optional (indent-level 0) (stream t))
  (format stream "~{~a~}" (gather-nodes (definitions node) indent-level)))

(defmethod print-node ((node selection-set) &optional (indent-level 0) (stream nil))
  ;; Start by adding {
  ;; Newline
  ;; Loop the selection-set
  ;; Dedent
  ;; Newline
  (format stream "{~%~{~a~}~%~a}"
          (gather-nodes (selections node) indent-level)
          (add-indent (1- indent-level))))

(defmethod print-node ((node field) &optional (indent-level 0) (stream nil))
  ;; Fields:
  ;; 
  ;; alias
  ;; name
  ;; arguments
  ;; directives
  ;; selection-set
  ;;
  ;; First add indentation
  ;; optionally show alias, name and selection-set
  (format stream "~a~@[~a: ~]~@[~a~]~@[ ~a~]"
          (add-indent indent-level)
          (alias node)
          (print-node (name node))
          (when (selection-set node)
            (print-node (selection-set node) (1+ indent-level)))))

(defmethod print-node ((node argument) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~a: ~a" (name node) (value node)))

(defmethod print-node ((node name) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[~a~]" (name node)))
