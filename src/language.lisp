(in-package :gql)

(defclass* parser
  lexer)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defclass* location
  start
  end
  start-token
  end-token
  source)

(defclass* ast-node
  kind
  location)

(defgql name
  :node (defnode name name)
  :parser (defparser name ()
            (with-expected-token 'name
              (make-node 'name :name (value *token*))))
  :generator (defgenerator name ()
               "~@[~a~]" (name node)))

(defgql document
  :node (defnode document definitions)
  :parser (defparser document ()
            (make-node 'document :definitions (many 'sof 'definition 'eof)))
  :validator (defvalidator document ()
               (with-slots (definitions) node
                 (every-definition-executable-p definitions)
                 (operation-name-unique-p definitions)
                 (single-anonymous-operation-definition-p definitions)
                 (subscription-operation-valid-p)))
  :generator (defgenerator document ()
               "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defgql type-system-definition
  :node (defnode type-system-definition definitions)
  :parser (defparser type-system-definition ()
            (let ((keyword-token
                    ;; The token could be a STRING or BLOCK-STRING, and if it is, we
                    ;; want to check if the next token is 'schema'.  If it isn't a
                    ;; string, just return the token we currently look at.  We still
                    ;; haven't advanced beyond the possible docstring. That happens in
                    ;; the next parse call.
                    (if (peek-description) (lookahead (lexer *parser*)) *token*)))
              (string-case (value keyword-token)
                ("schema"    (parse 'schema-definition))
                ("scalar"    (parse 'scalar-type-definition))
                ("type"      (parse 'object-type-definition))
                ("interface" (parse 'interface-type-definition))
                ("union"     (parse 'union-type-definition))
                ("enum"      (parse 'enum-type-definition))
                ("input"     (parse 'input-object-type-definition))
                ("directive" (parse 'directive-definition))
                (t           (unexpected)))))
  :generator (defgenerator type-system-definition ()
               "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defgql type-system-extension
  :node (defnode type-system-extension definitions)
  :parser (defparser type-system-extension ()
            ;; At this point our current token is "extend".  We now want to check what
            ;; type of extension this is, so we have to look ahead one token.
            (let ((keyword-token (lookahead (lexer *parser*))))
              (if (eq (kind keyword-token) 'name)
                  (string-case (value keyword-token)
                    ("schema"    (parse 'schema-extension))
                    ("scalar"    (parse 'scalar-type-extension))
                    ("type"      (parse 'object-type-extension))
                    ("interface" (parse 'interface-type-extension))
                    ("union"     (parse 'union-type-extension))
                    ("enum"      (parse 'enum-type-extension))
                    ("input"     (parse 'input-object-type-extension))
                    (t           (unexpected)))
                  (unexpected))))
  :generator (defgenerator type-system-extension ()
               "~{~a~%~}" (gather-nodes (definitions node) indent-level)))

(defgql operation-definition
  :node (defnode operation-definition operation-type name variable-definitions directives selection-set)
  :parser (defparser operation-definition ()
            (if (peek 'brace-l)
                ;; We allow for the query shorthand by first checking for the opening
                ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
                ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
                (make-node 'operation-definition
                  :operation-type "Query"
                  :selection-set (parse 'selection-set))
                (make-node 'operation-definition
                  :operation-type (parse 'operation-type)
                  :name (when (peek 'name) (parse 'name))
                  :variable-definitions (parse 'variable-definitions)
                  :directives (parse 'directives)
                  :selection-set (parse 'selection-set))))
  :generator (defgenerator operation-definition ()
               ;; TODO: Missing definitions
               (cat "~a"                ;; operation type
                    "~@[ ~a~]"          ;; name
                    "~@[(~{~a~^, ~})~]" ;; variable definitions
                    "~@[ ~{~a~}~]"      ;; directives
                    " ~a")              ;; selection set
               (operation-type node)
               (when (name node)
                 (generate (name node) :indent-level indent-level :stream stream))
               (gather-nodes (variable-definitions node) indent-level)
               (gather-nodes (directives node) indent-level)
               (generate (selection-set node) :indent-level (1+ indent-level) :stream stream)))

(defgql selection-set
  :node (defnode selection-set selections)
  :parser (defparser selection-set ()
            (make-node 'selection-set :selections (many 'brace-l 'selection 'brace-r)))
  :generator (defgenerator selection-set ()
               ;; HMM: We assume that the nodes inside the braces know how to indent
               ;; themselves.  Not sure if it is best that this method handles indentation
               ;; per node, or if it's best handled in the child nodes.
               (cat "{~%"      ;; Brace then newline
                    "~{~a~%~}" ;; Loop over children, newline after every child
                    "~a}")     ;; Newline, then dedented brace
               (gather-nodes (selections node) indent-level)
               (add-indent (1- indent-level))))

(defgql field
  :node (defnode field alias name arguments directives selection-set)
  :parser (defparser field ()
            (let ((name-or-alias (parse 'name)) alias name)
              (if (expect-optional-token 'colon)
                  (setf alias name-or-alias name (parse 'name))
                  (setf name name-or-alias))
              (make-node 'field
                :alias alias
                :name name
                :arguments (parse 'arguments)
                :directives (parse 'directives)
                :selection-set (when (peek 'brace-l) (parse 'selection-set)))))
  :generator (defgenerator field ()
               ;; Advanced example:
               ;; smallPic: profilePic(size: 64, size2: 128) @skip(if: true) { x }
               (cat "~a"                ;; indent
                    "~@[~a: ~]"         ;; alias
                    "~@[~a~]"           ;; name
                    "~@[(~{~a~^, ~})~]" ;; arguments, comma separated
                    "~@[ ~{~a~^ ~}~]"   ;; directives
                    "~@[ ~a~]")         ;; selection-set
               (add-indent indent-level)
               (when (alias node)
                 (generate (alias node) :indent-level indent-level :stream stream))
               (generate (name node))
               (when (arguments node)
                 (gather-nodes (arguments node) indent-level))
               (gather-nodes (directives node) indent-level)
               (when (selection-set node)
                 (generate
                  (selection-set node) :indent-level (1+ indent-level) :stream stream))))

(defgql argument
  :node (defnode argument name value)
  :parser (defparser argument ()
            (make-node 'argument
              :name (parse 'name)
              :value (expect-then-parse 'colon 'value)))
  :generator (defgenerator argument ()
               "~a: ~a"
               (generate (name node) :indent-level indent-level :stream stream)
               (generate (value node) :indent-level indent-level :stream stream)))

(defgql fragment-spread
  :node (defnode fragment-spread fragment-name directives)
  :parser (defparser fragment-spread ()
            (make-node 'fragment-spread
              :fragment-name (parse 'fragment-name)
              :directives (parse 'directives)))
  :generator (defgenerator fragment-spread ()
               "~a...~@[~a~]~@[ ~a~]"
               (add-indent indent-level)
               (generate (fragment-name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)))

(defgql fragment-definition
  :node (defnode fragment-definition name type-condition directives selection-set)
  :parser (defparser fragment-definition ()
            (make-node 'fragment-definition
              :name (expect-then-parse "fragment" 'fragment-name)
              :type-condition (expect-then-parse "on" 'named-type)
              :directives (parse 'directives)
              :selection-set (parse 'selection-set)))
  :generator (defgenerator fragment-definition ()
               (cat "fragment ~a on ~a"
                    "~@[ ~{~a~}~]" ;; directives
                    " ~a")         ;; selection set
               (generate (name node) :indent-level indent-level :stream stream)
               (generate (type-condition node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (generate (selection-set node) :indent-level (1+ indent-level) :stream stream)))

(defgql inline-fragment
  :node (defnode inline-fragment type-condition directives selection-set)
  :parser (defparser inline-fragment (type-condition-p)
            (make-node 'inline-fragment
              :type-condition (when type-condition-p (parse 'named-type))
              :directives (parse 'directives)
              :selection-set (parse 'selection-set)))
  :generator (defgenerator inline-fragment ()
               (cat "~a... on ~a"
                    "~@[ ~{~a~}~]" ;; directives
                    " ~a")         ;; selection set
               (add-indent indent-level)
               (generate (type-condition node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (generate (selection-set node) :indent-level  (1+ indent-level) :stream stream)))

(defgql directive
  :node (defnode directive name arguments)
  :parser (defparser directive ()
            (make-node 'directive
              :name (expect-then-parse 'at 'name)
              :arguments (parse 'arguments :constp constp)))
  :generator (defgenerator directive ()
               (cat "@"      ;; Literal @
                    "~@[~a~]" ;; Name
                    "~@[(~{~a~^, ~})~]" ;; arguments, comma separated
                    )
               (generate (name node) :indent-level indent-level :stream stream)
               (when (arguments node)
                 (gather-nodes (arguments node) indent-level))))

(defgql int-value
  :node (defnode int-value value)
  :parser (defparser int-value ()
            (make-node 'int-value
              :value (advance-then-value)))
  :generator (defgenerator int-value ()
               "~@[~a~]" (value node)))

(defgql float-value
  :node (defnode float-value value)
  :parser (defparser float-value ()
            (make-node 'float-value
              :value (advance-then-value)))
  :generator (defgenerator float-value ()
               "~@[~a~]" (value node)))

(defgql string-value
  :node (defnode string-value value blockp)
  :parser (defparser string-value ()
            (advance-one-token)
            (make-node 'string-value
              :value (value *token*)
              :blockp (when (peek 'block-string *token*) t)))
  :generator (defgenerator string-value (full)
               (if (blockp node)
                   (format stream "~@[\"\"\"~a\"\"\"~]" (value node))
                   (format stream "~@[\"~a\"~]" (value node)))))

(defgql boolean-value
  :node (defnode boolean-value value)
  :parser (defparser boolean-value (value)
            (make-node 'boolean-value :value value))
  :generator (defgenerator boolean-value (full)
               (let ((bool (if (value node) "true" "false")))
                 (format stream "~@[~a~]" bool))))

(defgql non-null-type
  :node (defnode non-null-type ty)
  :parser (defparser non-null-type (ty)
            (make-node 'non-null-type :ty ty))
  :generator (defgenerator non-null-type ()
               "~@[~a~]!" (generate (ty node) :indent-level indent-level :stream stream)))

(defgql null-value
  :node (defnode null-value value)
  :parser (defparser null-value ()
            (make-node 'null-value :value 'null))
  :generator (defgenerator null-value ()
               "null"))

(defgql enum-value
  :node (defnode enum-value value)
  :parser (defparser enum-value ()
            (string-case (value *token*)
              ("true"  (unexpected))
              ("false" (unexpected))
              ("null"  (unexpected))
              (t       (parse 'name))))
  :generator (defgenerator enum-value ()
               "~@[~a~]" (value node)))

(defgql list-value
  :node (defnode list-value list-values)
  :parser (defparser list-value ()
            (make-node 'list-value
              :list-values (any 'bracket-l 'value 'bracket-r :constp constp)))
  :generator (defgenerator list-value (full)
               (if (list-values node)
                   (format stream "~@[[~{~a~^, ~}]~]" (gather-nodes (list-values node) indent-level))
                   (format stream "[]"))))

(defgql object-value
  :node (defnode object-value fields)
  :parser (defparser object-value ()
            (make-node 'object-value
              :fields (any 'brace-l 'object-field 'brace-r :constp constp)))
  :generator (defgenerator object-value (full)
               (if (fields node)
                   (format stream "~@[{ ~{~a~^, ~} }~]" (gather-nodes (fields node) indent-level))
                   (format stream "{}"))))

(defgql object-field
  :node (defnode object-field name value)
  :parser (defparser object-field ()
            (make-node 'object-field
              :name (parse 'name)
              :value (expect-then-parse 'colon 'value :constp constp)))
  :generator (defgenerator object-field ()
               "~a: ~a"
               (generate (name node) :indent-level indent-level :stream stream)
               (generate (value node) :indent-level indent-level :stream stream)))

(defgql var
  :node (defnode var name)
  :parser (defparser var ()
            (make-node 'var
              :name (expect-then-parse 'dollar 'name)))
  :generator (defgenerator var ()
               "~@[$~a~]" (generate (name node) :indent-level indent-level :stream stream)))

(defgql variable-definition
  :node (defnode variable-definition var var-type default-value directives)
  :parser (defparser variable-definition ()
            (make-node 'variable-definition
              :var (parse 'var)
              :var-type (expect-then-parse 'colon 'type-reference)
              :default-value nil
              :directives (parse 'directives :constp t)))
  :generator (defgenerator variable-definition ()
               ;; TODO: Not done yet - will probably crash things for now. (why??)
               "~@[~a~]~@[: ~a~]~@[~a~]~@[~a~]"
               (generate (var node) :indent-level indent-level :stream stream)
               (generate (var-type node) :indent-level indent-level :stream stream)
               (default-value node)
               (gather-nodes (directives node) indent-level)))

(defgql named-type
  :node (defnode named-type name)
  :parser (defparser named-type ()
            (make-node 'named-type :name (parse 'name)))
  :generator (defgenerator named-type ()
                "~@[~a~]" (generate (name node) :indent-level indent-level :stream stream)))

(defgql list-type
  :node (defnode list-type ty)
  :parser (defparser list-type ()
            (make-node 'list-type
              :ty (prog1 (parse 'type-reference)
                    (expect-token 'bracket-r))))
  :generator (defgenerator list-type ()
               "~@[[~a]~]" (generate (ty node) :indent-level indent-level :stream stream)))

;; Type system

(defgql schema-definition
  :node (defnode schema-definition description directives operation-types)
  :parser (defparser schema-definition ()
            (make-node 'schema-definition
              :description (parse 'description)
              :directives (expect-then-parse "schema" 'directives :constp t)
              :operation-types (many 'brace-l 'operation-type-definition 'brace-r)))
  :generator (defgenerator schema-definition ()
               (cat "~@[~a~%~]"
                    "schema {~%"
                    "~@[~a~]"
                    "~@[~{~a~%~}~]"
                    "~a}")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (gather-nodes (directives node) (1+ indent-level))
               (gather-nodes (operation-types node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql operation-type-definition
  :node (defnode operation-type-definition operation named-type)
  :parser (defparser operation-type-definition ()
            (make-node 'operation-type-definition
              :operation (parse 'operation-type)
              :named-type (expect-then-parse 'colon 'named-type)))
  :generator (defgenerator operation-type-definition ()
               "~a~a: ~a"
               (add-indent indent-level)
               (operation node)
               (generate (named-type node) :indent-level indent-level :stream stream)))

(defgql scalar-type-definition
  :node (defnode scalar-type-definition description name directives)
  :parser (defparser scalar-type-definition ()
            (make-node 'scalar-type-definition
              :description (parse 'description)
              :name (expect-then-parse "scalar" 'name)
              :directives (parse 'directives :constp t)))
  :generator (defgenerator scalar-type-definition ()
               (cat "~@[~a~%~]"
                    "scalar ~a"
                    "~@[~{~a~%~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) (1+ indent-level))))

(defgql object-type-definition
  :node (defnode object-type-definition description name interfaces directives fields)
  :parser (defparser object-type-definition ()
            (make-node 'object-type-definition
              :description (parse 'description)
              :name (expect-then-parse "type" 'name)
              :interfaces (parse 'implements-interfaces)
              :directives (parse 'directives :constp t)
              :fields (parse 'fields-definition)))
  :generator (defgenerator object-type-definition ()
               (cat "~@[~a~%~]"
                    "type ~a"
                    "~@[ implements~{ ~a~^ ~^&~}~]"
                    "~@[~{~a~%~}~]"
                    "~@[ {~%~{~a~%~}~]"
                    "~a}")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (interfaces node) indent-level)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql field-definition
  :node (defnode field-definition description name args ty directives)
  :parser (defparser field-definition ()
            (make-node 'field-definition
              :description (parse 'description)
              :name (parse 'name)
              :args (parse 'argument-definitions)
              :ty (expect-then-parse 'colon 'type-reference)
              :directives (parse 'directives :constp t)))
  :generator (defgenerator field-definition ()
               (cat "~@[~a~]" ;; We add indent before the optional docs as well
                    "~@[~a~%~]"
                    "~a~a"
                    "~@[(~{~a~^, ~})~]" ;; arguments, comma separated
                    "~@[: ~a~]"
                    "~@[ ~{~a~}~]")
               (when (description node) ;; HMM: Ugly - how to fix?
                 (add-indent indent-level))
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (add-indent indent-level)
               (generate (name node) :indent-level indent-level :stream stream)
               (when (args node) (gather-nodes (args node) indent-level))
               (generate (ty node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)))

(defgql input-value-definition
  :node (defnode input-value-definition description name ty default-value directives)
  :parser (defparser input-value-definition ()
            (make-node 'input-value-definition
              :description (parse 'description)
              :name (parse 'name)
              :ty (expect-then-parse 'colon 'type-reference)
              :default-value (parse 'default-value :constp t)
              :directives (parse 'directives :constp t)))
  :generator (defgenerator input-value-definition ()
               (cat "~@[~a~%~]"
                    "~a: "
                    "~@[~a~]"
                    "~@[~a~]"
                    "~@[ ~{~a~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (generate (ty node) :indent-level indent-level :stream stream)
               (when (default-value node)
                 (generate (default-value node) :indent-level indent-level :stream stream))
               (gather-nodes (directives node) indent-level)))

(defgql interface-type-definition
  :node (defnode interface-type-definition description name directives fields)
  :parser (defparser interface-type-definition ()
            (make-node 'interface-type-definition
              :description (parse 'description)
              :name (expect-then-parse "interface" 'name)
              :directives (parse 'directives :constp t)
              :fields (parse 'fields-definition)))
  :generator (defgenerator interface-type-definition ()
               (cat "~@[~a~%~]"
                    "interface ~a"
                    "~@[~{~a~%~}~]"
                    "~@[ {~%~{~a~%~}~]"
                    "~a}")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql union-type-definition
  :node (defnode union-type-definition description name directives union-members)
  :parser (defparser union-type-definition ()
            (make-node 'union-type-definition
              :description (parse 'description)
              :name (expect-then-parse "union" 'name)
              :directives (parse 'directives :constp t)
              :union-members (parse 'union-member-types)))
  :generator (defgenerator union-type-definition ()
               ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
               ;; It doesn't look like we have the union member class - maybe create it?
               (cat "~@[~a~%~]"
                    "union ~a"
                    "~@[~{~a~%~}~]"
                    "~@[ =~%~{  | ~a~^~%~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (union-members node) (1+ indent-level))))

(defgql enum-type-definition
  :node (defnode enum-type-definition description name directives enum-values)
  :parser (defparser enum-type-definition ()
            (make-node 'enum-type-definition
              :description (parse 'description)
              :name (expect-then-parse "enum" 'name)
              :directives (parse 'directives :constp t)
              :enum-values (parse 'enum-values)))
  :generator (defgenerator enum-type-definition ()
               (cat "~@[~a~%~]"
                    "enum ~a"
                    "~@[~{~a~%~}~]"
                    "~@[ {~%~{~a~%~}~]"
                    "~a}")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (enum-values node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql enum-value-definition
  :node (defnode enum-value-definition description enum-value directives)
  :parser (defparser enum-value-definition ()
            (make-node 'enum-value-definition
              :description (parse 'description)
              :enum-value (parse 'enum-value)
              :directives (parse 'directives :constp t)))
  :generator (defgenerator enum-value-definition ()
               (cat "~@[~a~%~]"
                    "~a~a"
                    "~@[ ~{~a~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (add-indent indent-level)
               (generate (enum-value node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)))

(defgql input-object-type-definition
  :node (defnode input-object-type-definition description name directives fields)
  :parser (defparser input-object-type-definition ()
            (make-node 'input-object-type-definition
              :description (parse 'description)
              :name (expect-then-parse "input" 'name)
              :directives (parse 'directives :constp t)
              :fields (parse 'input-fields-definition)))
  :generator (defgenerator input-object-type-definition ()
               ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
               (cat "~@[~a~%~]"
                    "input ~a"
                    "~@[~{~a~%~}~]"
                    "~@[ {~%~{  ~a~%~}~]"
                    "~a}")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql directive-definition
  :node (defnode directive-definition description name args locations)
  :parser (defparser directive-definition ()
            (make-node 'directive-definition
              :description (parse 'description)
              :name (expect-then-parse '("directive" at) 'name)
              :args (parse 'argument-definitions)
              :locations (expect-then-parse "on" 'directive-locations)))
  :generator (defgenerator directive-definition ()
               (cat "~@[~a~%~]"
                    "directive @~a"
                    "~@[~a~]"
                    "~@[ on~%~{  | ~a~^~%~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (args node) indent-level)
               (gather-nodes (locations node) indent-level)))

(defgql schema-extension
  :node (defnode schema-extension directives operation-types)
  :parser (defparser schema-extension ()
            (make-node 'schema-extension
              :directives (expect-then-parse '("extend" "schema") 'directives :constp t)
              :operation-types (optional-many 'brace-l 'operation-type-definition 'brace-r)))
  :generator (defgenerator schema-extension ()
               (cat "extend schema ~@[~{~a~}~]"
                    "~@[ {~%~{  ~a~%~}~a}~]")
               (gather-nodes (directives node) indent-level)
               (gather-nodes (operation-types node) indent-level)
               (add-indent (1- indent-level))))

(defgql scalar-type-extension
  :node (defnode scalar-type-extension name directives)
  :parser (defparser scalar-type-extension ()
            (make-node 'scalar-type-extension
              :name (expect-then-parse '("extend" "scalar") 'name)
              :directives (parse 'directives :constp t)))
  :generator (defgenerator scalar-type-extension ()
               (cat "extend scalar ~@[~a~]"
                    "~@[ ~{~a~}~]")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (add-indent (1- indent-level))))

(defgql object-type-extension
  :node (defnode object-type-extension name interfaces directives fields)
  :parser (defparser object-type-extension ()
            (make-node 'object-type-extension
              :name (expect-then-parse '("extend" "type") 'name)
              :interfaces (parse 'implements-interfaces)
              :directives (parse 'directives :constp t)
              :fields (parse 'fields-definition)))
  :generator (defgenerator object-type-extension ()
               (cat "extend type ~a"
                    "~@[ implements~{ ~a~^ ~^&~}~]"
                    "~@[ ~{~a~}~]"
                    "~@[ {~%~{~a~%~}~a}~]")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (interfaces node) indent-level)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql interface-type-extension
  :node (defnode interface-type-extension name directives fields)
  :parser (defparser interface-type-extension ()
            (make-node 'interface-type-extension
              :name (expect-then-parse '("extend" "interface") 'name)
              :directives (parse 'directives :constp t)
              :fields (parse 'fields-definition)))
  :generator (defgenerator interface-type-extension ()
               (cat "extend interface ~a"
                    "~@[~{~a~%~}~]"
                    "~@[ {~%~{~a~%~}~a}~]"
                    "")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql union-type-extension
  :node (defnode union-type-extension name directives union-members)
  :parser (defparser union-type-extension ()
            (make-node 'union-type-extension
              :name (expect-then-parse '("extend" "union") 'name)
              :directives (parse 'directives :constp t)
              :union-members (parse 'union-member-types)))
  :generator (defgenerator union-type-extension ()
               (cat "extend union ~a"
                    "~@[ ~{~a~}~]"
                    "~@[ =~%~{  | ~a~^~%~}~]")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (union-members node) (1+ indent-level))))

(defgql enum-type-extension
  :node (defnode enum-type-extension name directives enum-values)
  :parser (defparser enum-type-extension ()
            (make-node 'enum-type-extension
              :name (expect-then-parse '("extend" "enum") 'name)
              :directives (parse 'directives :constp t)
              :enum-values (parse 'enum-values)))
  :generator (defgenerator enum-type-extension ()
               (cat "extend enum ~a"
                    "~@[ ~{~a~}~]"
                    "~@[ {~%~{~a~%~}~]"
                    "~a}")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (enum-values node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defgql input-object-type-extension
  :node (defnode input-object-type-extension name directives fields)
  :parser (defparser input-object-type-extension ()
            (make-node 'input-object-type-extension
              :name (expect-then-parse '("extend" "input") 'name)
              :directives (parse 'directives :constp t)
              :fields (parse 'input-fields-definition)))
  :generator (defgenerator input-object-type-extension ()
               ;; KLUDGE: This one is particularly ugly.  How to handle the indentation here?
               (cat "extend input ~a"
                    "~@[ ~{~a~}~]"
                    "~@[ {~%~{  ~a~%~}~]"
                    "~a}")
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (directives node) indent-level)
               (gather-nodes (fields node) (1+ indent-level))
               (add-indent (1- indent-level))))

(defparser definition ()
  (cond 
    ;; We only peek here so that we can read the whole thing in their
    ;; respective handlers.  This way we can still assert with
    ;; (expect-* parser 'thing) when needed.
    ((peek 'name)
     (string-case (value *token*)
       ;; ExecutableDefinition
       ("query"        (parse 'operation-definition))
       ("mutation"     (parse 'operation-definition))
       ("subscription" (parse 'operation-definition))

       ("fragment"     (parse 'fragment-definition))

       ;; TypeSystemDefinition
       ("schema"       (parse 'type-system-definition))
       ("scalar"       (parse 'type-system-definition))
       ("type"         (parse 'type-system-definition))
       ("interface"    (parse 'type-system-definition))
       ("union"        (parse 'type-system-definition))
       ("enum"         (parse 'type-system-definition))
       ("input"        (parse 'type-system-definition))
       ("directive"    (parse 'type-system-definition))

       ;; TypeSystemExtension
       ("extend"       (parse 'type-system-extension))
       (t              (unexpected))))
    ((peek 'brace-l)    (parse 'operation-definition))
    ((peek-description) (parse 'type-system-definition))
    (t                  (unexpected))))

(defparser operation-type ()
  ;; Disallow other names than query, mutation and subscription.
  (with-expected-token 'name
    (string-case (value *token*)
      ("query"        "Query")
      ("mutation"     "Mutation")
      ("subscription" "Subscription")
      (t              (unexpected *token*)))))

(defparser selection ()
  (if (peek 'spread)
      (parse 'fragment)
      (parse 'field)))

(defparser fragment ()
  (expect-token 'spread)
  (let ((type-condition-p (expect-optional-keyword "on")))
    (if (and (not type-condition-p) (peek 'name))
        (parse 'fragment-spread)
        (parse 'inline-fragment :type-condition-p type-condition-p))))

(defparser fragment-name ()
  (if (string= (value *token*) "on")
      (unexpected)
      (parse 'name)))

(defparser arguments ()
  (optional-many
   'paren-l
   (if constp 'const-argument 'argument)
   'paren-r))

(defparser const-argument ()
  (make-node 'argument
    :name (parse 'name)
    :value (expect-then-parse 'colon 'value :constp t)))

(defparser variable-definitions ()
  (optional-many 'paren-l 'variable-definition 'paren-r))

(defparser value ()
  (case (kind *token*)
    (bracket-l
     (parse 'list-value :constp constp))
    (brace-l
     (parse 'object-value :constp constp))
    (int
     (parse 'int-value))
    (float
     (parse 'float-value))
    ((string block-string)
     (parse 'string-value))
    (name
     (string-case (advance-then-value)
       ("true"  (parse 'boolean-value :value t))
       ("false" (parse 'boolean-value :value nil))
       ("null"  (parse 'null-value))
       ;; TODO: Make a better catch all here!
       (t       (make-node 'enum-value :value (value *token*)))))
    (dollar
     (unless constp (parse 'var)))
    (t (unexpected))))

(defparser directives ()
  (loop
    :with directives
    :while (peek 'at)
    :do (push (parse 'directive :constp constp) directives)
    :finally (return (nreverse directives))))

(defparser type-reference ()
  (let ((ty (if (expect-optional-token 'bracket-l)
                (parse 'list-type)
                (parse 'named-type))))
    (if (expect-optional-token 'bang)
        (parse 'non-null-type :ty ty)
        ty)))

(defparser description ()
  (when (peek-description)
    (parse 'string-value)))

(defparser implements-interfaces ()
  (when (expect-optional-keyword "implements")
    (delimited-many 'amp 'named-type)))

(defparser fields-definition ()
  (optional-many 'brace-l 'field-definition 'brace-r))

(defparser argument-definitions ()
  (optional-many 'paren-l 'input-value-definition 'paren-r))

(defparser default-value ()
  (when (expect-optional-token 'equals)
    (parse 'value :constp t)))

(defparser union-member-types ()
  (when (expect-token 'equals)
    (delimited-many 'pipe 'named-type)))

(defparser enum-values ()
  (optional-many 'brace-l 'enum-value-definition 'brace-r))

(defparser input-fields-definition ()
  (optional-many 'brace-l 'input-value-definition 'brace-r))

(defparser directive-locations ()
  (delimited-many 'pipe 'directive-location))

(defvar *executable-directive-location*
  '("query" "mutation" "subscription" "field" "fragment_definition"
    "fragment_spread" "inline_fragment"))

(defvar *type-system-directive-location*
  '("schema" "scalar" "object" "field_definition" "argument_definition"
    "interface" "union" "enum" "enum_value" "input_object" "input_field_definition"))

(defparser directive-location ()
  (let ((name (parse 'name)))
    (if (some (lambda (x) (string-equal (name name) x))
              (append
               *executable-directive-location*
               *type-system-directive-location*))
        name
        (unexpected))))
