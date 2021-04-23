(in-package :gql)

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
  (format stream "~{~a~}" nodes))

(defun gather-nodes (node-list indent-level)
  "Collect a list of formatted subnodes.
It indents where necessary (with help from calls to GENERATE, then returns a
list of strings."
  (mapcar (lambda (node) (format nil "~a" (generate node indent-level)))
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
  (format stream "~a~@[ ~a~]~@[ ~{~a~}~] ~a"
          (operation node)
          (when (name node) (generate (name node)))
          ;; (variable-definitions node)
          (gather-nodes (directives node) indent-level)
          (generate (selection-set node) (1+ indent-level))
          ;; (when (definitions node) (gather-nodes (definitions node) indent-level))
          ))

(defmethod generate ((node fragment-definition) &optional (indent-level 0) (stream t))
  (unfold-nodes stream (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node selection-set) &optional (indent-level 0) (stream nil))
  ;; Start by adding {
  ;; Newline
  ;; Unfold the selection-set
  ;; Dedent
  ;; Newline
  (format stream (cat "{~%~{~a~}~%~a}")
          (gather-nodes (selections node) indent-level)
          (add-indent (1- indent-level))))

(defmethod generate ((node argument) &optional (indent-level 0) (stream nil))
  (format stream "~a: ~a"
          (generate (name node) indent-level)
          (generate (value node) indent-level)))

(defmethod generate ((node field) &optional (indent-level 0) (stream nil))
  ;; Advanced example:
  ;; smallPic: profilePic(size: 64, size2: 128) @skip(if: true) { x }
  (format stream (cat "~a"                         ;; indent
                      "~@[~a: ~]"                  ;; alias
                      "~@[~a~]"                    ;; name
                      "~@[(~{~a~^, ~})~]"          ;; arguments, comma separated
                      "~@[ ~{~a~^ ~}~]"            ;; directives
                      "~@[ ~a~]")                  ;; selection-set
          (add-indent indent-level)
          (when (alias node) (generate (alias node) indent-level))
          (generate (name node))
          (when (arguments node)
            (gather-nodes (arguments node) indent-level))
          (gather-nodes (directives node) indent-level)
          (when (selection-set node)
            (generate (selection-set node) (1+ indent-level)))))

(defmethod generate ((node name) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[~a~]" (name node)))

(defmethod generate ((node directive) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream (cat "@"
                      "~@[~a~]"
                      "~@[(~{~a~^, ~})~]"          ;; arguments, comma separated
                      )
          (generate (name node))
          (when (arguments node)
            (gather-nodes (arguments node) indent-level))))

;;; Values
(defmethod generate ((node int-value) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[~a~]" (value node)))

(defmethod generate ((node boolean-value) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (let ((bool (if (value node) "true" "false")))
    (format stream "~@[~a~]" bool)))

(defmethod generate ((node var) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[$~a~]" (generate (name node))))
