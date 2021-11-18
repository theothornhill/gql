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
It indents where necessary (with help from calls to `generate'), then returns a
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

(defmacro defgenerator (node &optional (stream nil) &body body)
  "Convenience macro to define new generator methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod generate ((node ,node) &optional (indent-level 0) (stream ,stream))
     (declare (ignorable indent-level stream))
     (format stream ,@body)))

(defgenerator name nil
  "~@[~a~]" (name node))

(defgenerator document t
  "~{~a~%~}" (gather-nodes (definitions node) indent-level))

(defgenerator type-system-definition t
  "~{~a~%~}" (gather-nodes (definitions node) indent-level))

(defgenerator type-system-extension t
  "~{~a~%~}" (gather-nodes (definitions node) indent-level))

(defgenerator operation-definition nil
  ;; TODO: Missing definitions
  (cat "~a"                ;; operation type
       "~@[ ~a~]"          ;; name
       "~@[(~{~a~^, ~})~]" ;; variable definitions
       "~@[ ~{~a~}~]"      ;; directives
       " ~a")              ;; selection set
  (operation-type node)
  (when (name node) (generate (name node)))
  (gather-nodes (variable-definitions node) indent-level)
  (gather-nodes (directives node) indent-level)
  (generate (selection-set node) (1+ indent-level)))

(defgenerator selection-set nil
  ;; HMM: We assume that the nodes inside the braces know how to indent
  ;; themselves.  Not sure if it is best that this method handles indentation
  ;; per node, or if it's best handled in the child nodes.
  (cat "{~%"      ;; Brace then newline
       "~{~a~%~}" ;; Loop over children, newline after every child
       "~a}")     ;; Newline, then dedented brace
  (gather-nodes (selections node) indent-level)
  (add-indent (1- indent-level)))

(defgenerator field nil
  ;; Advanced example:
  ;; smallPic: profilePic(size: 64, size2: 128) @skip(if: true) { x }
  (cat "~a"                         ;; indent
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
    (generate (selection-set node) (1+ indent-level))))

(defgenerator argument nil
  "~a: ~a"
  (generate (name node) indent-level)
  (generate (value node) indent-level))

(defgenerator fragment-spread nil
  "~a...~@[~a~]~@[ ~a~]"
  (add-indent indent-level)
  (generate (fragment-name node))
  (gather-nodes (directives node) indent-level))

(defgenerator fragment-definition nil
  (cat "fragment ~a on ~a"
       "~@[ ~{~a~}~]"      ;; directives
       " ~a")              ;; selection set
  (generate (name node))
  (generate (type-condition node))
  (gather-nodes (directives node) indent-level)
  (generate (selection-set node) (1+ indent-level)))

(defgenerator inline-fragment nil
  (cat "~a... on ~a"
       "~@[ ~{~a~}~]"      ;; directives
       " ~a")              ;; selection set
  (add-indent indent-level)
  (generate (type-condition node))
  (gather-nodes (directives node) indent-level)
  (generate (selection-set node) (1+ indent-level)))

(defgenerator directive nil
  (cat "@"                          ;; Literal @
       "~@[~a~]"                    ;; Name
       "~@[(~{~a~^, ~})~]"          ;; arguments, comma separated
       )
  (generate (name node))
  (when (arguments node)
    (gather-nodes (arguments node) indent-level)))

(defgenerator int-value nil
  "~@[~a~]" (value node))

(defgenerator float-value nil
  "~@[~a~]" (value node))

(defmethod generate ((node string-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (if (blockp node)
      (format stream "~@[\"\"\"~a\"\"\"~]" (value node))
      (format stream "~@[\"~a\"~]" (value node))))

(defmethod generate ((node boolean-value) &optional (indent-level 0) (stream nil))
  (declare (ignore indent-level))
  (let ((bool (if (value node) "true" "false")))
    (format stream "~@[~a~]" bool)))

(defgenerator null-value nil
  "null")

(defgenerator enum-value nil
  "~@[~a~]" (value node))

(defmethod generate ((node list-value) &optional (indent-level 0) (stream nil))
  (if (list-values node)
      (format stream "~@[[~{~a~^, ~}]~]" (gather-nodes (list-values node) indent-level))
      (format stream "[]")))

(defmethod generate ((node object-value) &optional (indent-level 0) (stream nil))
  (if (fields node)
      (format stream "~@[{ ~{~a~^, ~} }~]" (gather-nodes (fields node) indent-level))
      (format stream "{}")))

(defgenerator object-field nil
  "~a: ~a" (generate (name node)) (generate (value node)))

(defgenerator var nil
  "~@[$~a~]" (generate (name node)))

(defgenerator variable-definition nil
  ;; TODO: Not done yet - will probably crash things for now. (why??)
  "~@[~a~]~@[: ~a~]~@[~a~]~@[~a~]"
  (generate (var node))
  (generate (var-type node))
  (default-value node)
  (gather-nodes (directives node) indent-level))

(defgenerator named-type nil
  "~@[~a~]" (generate (name node)))

(defgenerator list-type nil
  "~@[[~a]~]" (generate (ty node)))

(defgenerator non-null-type nil
  "~@[~a~]!" (generate (ty node)))

;; Type system
(defgenerator schema-definition nil
  (cat "~@[~a~%~]"
       "schema {~%"
       "~@[~a~]"
       "~@[~{~a~%~}~]"
       "~a}")
  (when (description node) (generate (description node)))
  (gather-nodes (directives node) (1+ indent-level))
  (gather-nodes (operation-types node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator operation-type-definition nil
  "~a~a: ~a"
  (add-indent indent-level)
  (operation node)
  (generate (named-type node)))

(defgenerator scalar-type-definition nil
  (cat "~@[~a~%~]"
       "scalar ~a"
       "~@[~{~a~%~}~]")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (directives node) (1+ indent-level)))

(defgenerator object-type-definition nil
  (cat "~@[~a~%~]"
       "type ~a"
       "~@[ implements~{ ~a~^ ~^&~}~]"
       "~@[~{~a~%~}~]"
       "~@[ {~%~{~a~%~}~]"
       "~a}")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (interfaces node) indent-level)
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator field-definition nil
  (cat "~@[~a~]"           ;; We add indent before the optional docs as well
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
  (gather-nodes (directives node) indent-level))

(defgenerator input-value-definition nil
  (cat "~@[~a~%~]"
       "~a: "
       "~@[~a~]"
       "~@[~a~]"
       "~@[ ~{~a~}~]")
  (when (description node) (generate (description node)))
  (generate (name node))
  (generate (ty node))
  (when (default-value node) (generate (default-value node)))
  (gather-nodes (directives node) indent-level))

(defgenerator interface-type-definition nil
  (cat "~@[~a~%~]"
       "interface ~a"
       "~@[~{~a~%~}~]"
       "~@[ {~%~{~a~%~}~]"
       "~a}")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator union-type-definition nil
  ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
  ;; It doesn't look like we have the union member class - maybe create it?
  (cat "~@[~a~%~]"
       "union ~a"
       "~@[~{~a~%~}~]"
       "~@[ =~%~{  | ~a~^~%~}~]")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (union-members node) (1+ indent-level)))

(defgenerator enum-type-definition nil
  (cat "~@[~a~%~]"
       "enum ~a"
       "~@[~{~a~%~}~]"
       "~@[ {~%~{~a~%~}~]"
       "~a}")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (enum-values node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator enum-value-definition nil
  (cat "~@[~a~%~]"
       "~a~a"
       "~@[ ~{~a~}~]")
  (when (description node) (generate (description node)))
  (add-indent indent-level)
  (generate (enum-value node))
  (gather-nodes (directives node) indent-level))

(defgenerator input-object-type-definition nil
  ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
  (cat "~@[~a~%~]"
       "input ~a"
       "~@[~{~a~%~}~]"
       "~@[ {~%~{  ~a~%~}~]"
       "~a}")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator directive-definition nil
  (cat "~@[~a~%~]"
       "directive @~a"
       "~@[~a~]"
       "~@[ on~%~{  | ~a~^~%~}~]")
  (when (description node) (generate (description node)))
  (generate (name node))
  (gather-nodes (args node) indent-level)
  (gather-nodes (locations node) indent-level))

(defgenerator schema-extension nil
  (cat "extend schema ~@[~{~a~}~]"
       "~@[ {~%~{  ~a~%~}~a}~]")
  (gather-nodes (directives node) indent-level)
  (gather-nodes (operation-types node) indent-level)
  (add-indent (1- indent-level)))

(defgenerator scalar-type-extension nil
  (cat "extend scalar ~@[~a~]"
       "~@[ ~{~a~}~]")
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (add-indent (1- indent-level)))

(defgenerator object-type-extension nil
  (cat "extend type ~a"
       "~@[ implements~{ ~a~^ ~^&~}~]"
       "~@[ ~{~a~}~]"
       "~@[ {~%~{~a~%~}~a}~]")
  (generate (name node))
  (gather-nodes (interfaces node) indent-level)
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator interface-type-extension nil
  (cat "extend interface ~a"
       "~@[~{~a~%~}~]"
       "~@[ {~%~{~a~%~}~a}~]"
       "")
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator union-type-extension nil
  (cat "extend union ~a"
       "~@[ ~{~a~}~]"
       "~@[ =~%~{  | ~a~^~%~}~]")
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (union-members node) (1+ indent-level)))

(defgenerator enum-type-extension nil
  (cat "extend enum ~a"
       "~@[ ~{~a~}~]"
       "~@[ {~%~{~a~%~}~]"
       "~a}")
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (enum-values node) (1+ indent-level))
  (add-indent (1- indent-level)))

(defgenerator input-object-type-extension nil
  ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
  (cat "extend input ~a"
       "~@[ ~{~a~}~]"
       "~@[ {~%~{  ~a~%~}~]"
       "~a}")
  (generate (name node))
  (gather-nodes (directives node) indent-level)
  (gather-nodes (fields node) (1+ indent-level))
  (add-indent (1- indent-level)))
