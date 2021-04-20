(in-package :gql)

(defvar *unfold-string* "~{~a~}"
  "A format string to splice open a list of things.")

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

(defun unfold-nodes (stream nodes)
  "Splices open a list of nodes, displaying them without the enclosing ( )."
  (format stream *unfold-string* nodes))

(defun gather-nodes (node-list indent-level)
  "Collect a list of formatted subnodes.
It indents where necessary (with help from calls to GENERATE, then returns a
list of strings."
  (mapcar (lambda (x) (format nil "~a" (generate x indent-level)))
          node-list))

(defgeneric generate (node &optional indent-level stream)
  (:documentation "print a node as a valid GrqphQL statement."))

(defmethod generate ((node document) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node executable-definition) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node type-system-definition) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node type-system-extension) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node operation-definition) &optional (indent-level 0) (stream nil))
  (format stream "~a~@[ ~a~] ~a"
          (operation node)
          (when (name node)
            (generate (name node)))
          ;; (variable-definitions node)
          ;; (directives node)
          (generate (selection-set node) (1+ indent-level))
          ;; (when (definitions node) (gather-nodes (definitions node) indent-level))
          ))

(defmethod generate ((node fragment-definition) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node selection-set) &optional (indent-level 0) (stream nil))
  ;; Start by adding {
  ;; Newline
  ;; Loop the selection-set
  ;; Dedent
  ;; Newline
  (format stream "{~%~{~a~}~%~a}"
          (gather-nodes (selections node) indent-level)
          (add-indent (1- indent-level))))

(defmethod generate ((node field) &optional (indent-level 0) (stream nil))
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
  ;; TODO: Add more fields
  (format stream "~a~@[~a: ~]~@[~a~]~@[ ~a~]"
          (add-indent indent-level)
          (alias node)
          (generate (name node))
          (when (selection-set node)
            (generate (selection-set node) (1+ indent-level)))))

(defmethod generate ((node argument) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~a: ~a" (name node) (value node)))

(defmethod generate ((node name) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[~a~]" (name node)))
