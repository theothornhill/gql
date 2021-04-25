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

(defun gather-nodes (node-list indent-level)
  "Collect a list of formatted subnodes.
It indents where necessary (with help from calls to GENERATE, then returns a
list of strings."
  (mapcar (lambda (node) (format nil "~a" (generate node indent-level)))
          node-list))

(defgeneric generate (node &optional indent-level stream)
  (:documentation "Print a node NODE as a valid GrqphQL statement.
The top level definitions should default to INDENT-LEVEL 0 and STREAM T.  The
other nodes should start with whatever indentation they are passed as well as
nil as STREAM, to keep them as a string when generating.  The toplevel
nodes (that default to STREAM T) could also of course be passed other streams,
i.e. for file streams etc."))

(defmethod generate ((node name) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~@[~a~]" (name node)))

(defmethod generate ((node document) &optional (indent-level 0) (stream t))
  (format stream "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node type-system-definition) &optional (indent-level 0) (stream t))
  (format stream "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node type-system-extension) &optional (indent-level 0) (stream t))
  (format stream "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defmethod generate ((node operation-definition) &optional (indent-level 0) (stream nil))
  ;; TODO: Missing definitions
  (format stream (cat "~a"                ;; operation
                      "~@[ ~a~]"          ;; name
                      "~@[(~{~a~^, ~})~]" ;; variable definitions
                      "~@[ ~{~a~}~]"      ;; directives
                      " ~a")              ;; selection set
          (operation node)
          (when (name node) (generate (name node)))
          (gather-nodes (variable-definitions node) indent-level)
          (gather-nodes (directives node) indent-level)
          (generate (selection-set node) (1+ indent-level))))

(defmethod generate ((node selection-set) &optional (indent-level 0) (stream nil))
  ;; HMM: We assume that the nodes inside the braces know how to indent
  ;; themselves.  Not sure if it is best that this method handles indentation
  ;; per node, or if it's best handled in the child nodes.
  (format stream (cat "{~%"      ;; Brace then newline
                      "~{~a~%~}" ;; Loop over children, newline after every child
                      "~a}")     ;; Newline, then dedented brace
          (gather-nodes (selections node) indent-level)
          (add-indent (1- indent-level))))

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

(defmethod generate ((node argument) &optional (indent-level 0) (stream nil))
  (format stream "~a: ~a"
          (generate (name node) indent-level)
          (generate (value node) indent-level)))

(defmethod generate ((node fragment-spread) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream "~a...~@[~a~]~@[ ~a~]"
          (add-indent indent-level)
          (generate (name node))
          (gather-nodes (directives node) indent-level)))

(defmethod generate ((node fragment-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "fragment ~a"
                      " on ~a"
                      "~@[ ~{~a~}~]"      ;; directives
                      " ~a")              ;; selection set
          (generate (name node))
          (generate (type-condition node))
          (gather-nodes (directives node) indent-level)
          (generate (selection-set node) (1+ indent-level))))

(defmethod generate ((node inline-fragment) &optional (indent-level 0) (stream nil))
  (format stream (cat "~a..."
                      " on ~a"
                      "~@[ ~{~a~}~]"      ;; directives
                      " ~a")              ;; selection set
          (add-indent indent-level)
          (generate (type-condition node))
          (gather-nodes (directives node) indent-level)
          (generate (selection-set node) (1+ indent-level))))

(defmethod generate ((node directive) &optional (indent-level 0) (stream nil))
  (declare (ignorable indent-level))
  (format stream (cat "@"                          ;; Literal @
                      "~@[~a~]"                    ;; Name
                      "~@[(~{~a~^, ~})~]"          ;; arguments, comma separated
                      )
          (generate (name node))
          (when (arguments node)
            (gather-nodes (arguments node) indent-level))))

(defmethod generate ((node int-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[~a~]" (value node)))

(defmethod generate ((node float-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[~a~]" (value node)))

(defmethod generate ((node string-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (if (blockp node)
      (format stream "~@[\"\"\"~a\"\"\"~]" (value node))
      (format stream "~@[\"~a\"~]" (value node))))

(defmethod generate ((node boolean-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (let ((bool (if (value node) "true" "false")))
    (format stream "~@[~a~]" bool)))

(defmethod generate ((node null-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "null"))

(defmethod generate ((node enum-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[~a~]" (value node)))

(defmethod generate ((node list-value) &optional (indent-level 0) (stream nil))
  (if (list-values node)
      (format stream "~@[[~{~a~^, ~}]~]" (gather-nodes (list-values node) indent-level))
      (format stream "[]")))

(defmethod generate ((node object-value) &optional (indent-level 0) (stream nil))
  (if (fields node)
      (format stream "~@[{ ~{~a~^, ~} }~]" (gather-nodes (fields node) indent-level))
      (format stream "{}")))

(defmethod generate ((node object-field) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~a: ~a"
          (generate (name node))
          (generate (value node))))

(defmethod generate ((node var) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[$~a~]" (generate (name node))))

(defmethod generate ((node variable-definition) &optional (indent-level 0) (stream nil))
  ;; TODO: Not done yet - will probably crash things for now.
  (format stream "~@[~a~]~@[: ~a~]~@[~a~]~@[~a~]"
          (generate (var node))
          (generate (var-type node))
          (default-value node)
          (gather-nodes (directives node) indent-level)))

(defmethod generate ((node named-type) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[~a~]" (generate (name node))))

(defmethod generate ((node list-type) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[[~a]~]" (generate (ty node))))

(defmethod generate ((node non-null-type) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (format stream "~@[~a~]!" (generate (ty node))))

;; Type system
(defmethod generate ((node schema-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "schema {~%"
                      "~@[~a~]"
                      "~@[~{~a~%~}~]"
                      "~a}")
          (when (description node) (generate (description node)))
          (gather-nodes (directives node) (1+ indent-level))
          (gather-nodes (operation-types node) (1+ indent-level))
          (add-indent (1- indent-level))))

(defmethod generate ((node operation-type-definition) &optional (indent-level 0) (stream nil))
  (format stream "~a~a: ~a"
          (add-indent indent-level)
          (operation node)
          (generate (named-type node))))

(defmethod generate ((node scalar-type-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "scalar ~a"
                      "~@[~{~a~%~}~]")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (directives node) (1+ indent-level))))

(defmethod generate ((node object-type-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "type"
                      " ~a"
                      "~@[ implements~{ ~a~^ ~^&~}~]"
                      "~@[~{~a~%~}~]"
                      "~@[ {~%~{~a~%~}~]"
                      "~a}")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (interfaces node) indent-level)
          (gather-nodes (directives node) indent-level)
          (gather-nodes (fields node) (1+ indent-level))
          (add-indent (1- indent-level))))

(defmethod generate ((node field-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~]"           ;; We add indent before the optional docs as well
                      "~@[~a~%~]"
                      "~a~a"
                      "~@[(~{~a~^, ~})~]" ;; arguments, comma separated
                      "~@[: ~a~]"
                      "~@[ ~{~a~}~]")
          (when (description node)  ;; HMM: Ugly - how to fix?
            (add-indent indent-level))
          (when (description node)
            (generate (description node)))
          (add-indent indent-level)
          (generate (name node))
          (when (args node) (gather-nodes (args node) indent-level))
          (generate (ty node))
          (gather-nodes (directives node) indent-level)))

(defmethod generate ((node input-value-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "~a: "
                      "~@[~a~]"
                      "~@[~a~]"
                      "~@[ ~{~a~}~]")
          (when (description node) (generate (description node)))
          (generate (name node))
          (generate (ty node))
          (when (default-value node) (generate (default-value node)))
          (gather-nodes (directives node) indent-level)))

(defmethod generate ((node interface-type-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "interface"
                      " ~a"
                      "~@[~{~a~%~}~]"
                      "~@[ {~%~{~a~%~}~]"
                      "~a}")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (directives node) indent-level)
          (gather-nodes (fields node) (1+ indent-level))
          (add-indent (1- indent-level))))

(defmethod generate ((node union-type-definition) &optional (indent-level 0) (stream nil))
  ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
  ;; It doesn't look like we have the union member class - maybe create it?
  (format stream (cat "~@[~a~%~]"
                      "union"
                      " ~a"
                      "~@[~{~a~%~}~]"
                      "~@[ =~%~{  | ~a~^~%~}~]")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (directives node) indent-level)
          (gather-nodes (union-members node) (1+ indent-level))))

(defmethod generate ((node enum-type-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "enum"
                      " ~a"
                      "~@[~{~a~%~}~]"
                      "~@[ {~%~{~a~%~}~]"
                      "~a}")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (directives node) indent-level)
          (gather-nodes (enum-values node) (1+ indent-level))
          (add-indent (1- indent-level))))

(defmethod generate ((node enum-value-definition) &optional (indent-level 0) (stream nil))
  (format stream (cat "~@[~a~%~]"
                      "~a~a"
                      "~@[ ~{~a~}~]")
          (when (description node) (generate (description node)))
          (add-indent indent-level)
          (generate (enum-value node))
          (gather-nodes (directives node) indent-level)))

(defmethod generate ((node input-object-type-definition) &optional (indent-level 0) (stream nil))
  ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
  (format stream (cat "~@[~a~%~]"
                      "input"
                      " ~a"
                      "~@[~{~a~%~}~]"
                      "~@[ {~%~{  ~a~%~}~]"
                      "~a}")
          (when (description node) (generate (description node)))
          (generate (name node))
          (gather-nodes (directives node) indent-level)
          (gather-nodes (fields node) (1+ indent-level))
          (add-indent (1- indent-level))))
