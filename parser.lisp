(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (node-type &optional constp)
  (:method :before (node-type &optional (constp nil))
    (declare (ignorable constp))
    (when *debug-print*
      (with-token
        (with-slots (value kind) *token*
          (format t "; value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmacro defparser (node &body body)
  "Convenience macro to define new parser methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod parse ((node-type (eql ',node)) &optional (constp nil))
     (declare (ignorable constp))
     (with-token
       (declare (ignorable *token*))
       ,@body)))

(defparser document
  (make-node 'document :definitions (many 'sof 'definition 'eof)))

(defparser definition
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

(defparser operation-definition
  (if (peek 'brace-l)
      ;; We allow for the query shorthand by first checking for the opening
      ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
      ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
      (make-node 'operation-definition
        :operation "query"
        :selection-set (parse 'selection-set))
      (make-node 'operation-definition
        :operation (parse 'operation-type)
        :name (when (peek 'name) (parse 'name))
        :variable-definitions (parse 'variable-definitions)
        :directives (parse 'directives)
        :selection-set (parse 'selection-set))))

(defparser operation-type
  ;; Disallow other names than query, mutation and subscription.
  (with-expected-token 'name
    (string-case (value *token*)
      ("query"        "query")
      ("mutation"     "mutation")
      ("subscription" "subscription")
      (t              (unexpected *token*)))))

(defparser fragment-definition
  (make-node 'fragment-definition
    :name (expect-then-parse "fragment" 'fragment-name)
    :type-condition (expect-then-parse "on" 'named-type)
    :directives (parse 'directives)
    :selection-set (parse 'selection-set)))

(defparser name
  (with-expected-token 'name
    (make-node 'name :name (value *token*))))

(defparser selection-set
  (make-node 'selection-set :selections (many 'brace-l 'selection 'brace-r)))

(defparser selection
  (if (peek 'spread)
      (parse 'fragment)
      (parse 'field)))

(defparser field
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

(defparser fragment
  (expect-token 'spread)
  (let ((type-condition-p (expect-optional-keyword "on")))
    (if (and (not type-condition-p) (peek 'name))
        (make-node 'fragment-spread
          :name (parse 'fragment-name)
          :directives (parse 'directives))
        (make-node 'inline-fragment
          :type-condition (when type-condition-p (parse 'named-type))
          :directives (parse 'directives nil)
          :selection-set (parse 'selection-set)))))

(defparser fragment-name
  (if (string= (value *token*) "on")
      (unexpected)
      (parse 'name)))

(defparser arguments
  (optional-many
   'paren-l
   (if constp 'const-argument 'argument)
   'paren-r))

(defparser argument
  (make-node 'argument
    :name (parse 'name)
    :value (expect-then-parse 'colon 'value)))

(defparser const-argument
  (make-node 'argument
    :name (parse 'name)
    :value (expect-then-parse 'colon 'value)))

(defparser variable-definitions
  (optional-many 'paren-l 'variable-definition 'paren-r))

(defparser variable-definition
  (make-node 'variable-definition
    :var (parse 'var)
    :var-type (expect-then-parse 'colon 'type-reference)
    :default-value nil
    :directives (parse 'directives t)))

(defparser var
  (make-node 'var :name (expect-then-parse 'dollar 'name)))

(defparser string-value
  (advance-one-token)
  (make-node 'string-value
    :value (value *token*)
    :blockp (when (peek 'block-string) t)))

(defparser value
  (case (kind *token*)
    (bracket-l
     (parse 'list-value constp))
    (brace-l
     (parse 'object-value constp))
    (int
     (make-node 'int-value :value (advance-then-value)))
    (float
     (make-node 'float-value :value (advance-then-value)))
    ((string block-string)
     (parse 'string-value))
    (name
     (string-case (advance-then-value)
       ("true"  (make-node 'boolean-value :value t))
       ("false" (make-node 'boolean-value :value nil))
       ("null"  (make-node 'null-value))
       (t       (make-node 'enum-value :value (value *token*)))))
    (dollar
     (unless constp (parse 'var)))
    (t (unexpected))))

(defparser list-value
  (make-node 'list-value
    :list-values (any 'bracket-l 'value 'bracket-r constp)))

(defparser object-value
  (make-node 'object-value
    :fields (any 'brace-l 'object-field 'brace-r constp)))

(defparser object-value
  (make-node 'object-field
    :name (parse 'name)
    :value (expect-then-parse 'colon 'value constp)))

(defparser enum-value
  (string-case (value *token*)
    ("true"  (unexpected))
    ("false" (unexpected))
    ("null"  (unexpected))
    (t       (parse 'name))))

(defparser directives
  (loop
    with directives
    while (peek 'at)
    do (push (parse 'directive constp) directives)
    finally (return (nreverse directives))))

(defparser directive
  (make-node 'directive
    :name (expect-then-parse 'at 'name)
    :arguments (parse 'arguments constp)))

(defparser named-type
  (make-node 'named-type
    :name (parse 'name)))

(defparser type-reference
  (let ((ty (if (expect-optional-token 'bracket-l)
                (make-node 'list-type
                  :ty (prog1 (parse 'type-reference)
                        (expect-token 'bracket-r)))
                (parse 'named-type))))
    (if (expect-optional-token 'bang)
        (make-node 'non-null-type :ty ty)
        ty)))

(defun peek-description ()
  (or (peek 'string) (peek 'block-string)))

(defparser type-system-definition
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

(defparser type-system-extension
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

(defparser schema-extension
  (make-node 'schema-extension
    :directives (expect-then-parse '("extend" "schema") 'directives t)
    :operation-types (optional-many 'brace-l 'operation-type-definition 'brace-r)))

(defparser scalar-type-extension
  (make-node 'scalar-type-extension
    :name (expect-then-parse '("extend" "scalar") 'name)
    :directives (parse 'directives t)))

(defparser object-type-extension
  (make-node 'object-type-extension
    :name (expect-then-parse '("extend" "type") 'name)
    :interfaces (parse 'implements-interfaces)
    :directives (parse 'directives t)
    :fields (parse 'fields-definition)))

(defparser interface-type-extension
  (make-node 'interface-type-extension
    :name (expect-then-parse '("extend" "interface") 'name)
    :interfaces (parse 'implements-interfaces)
    :directives (parse 'directives t)
    :fields (parse 'fields-definition)))

(defparser union-type-extension
  (make-node 'union-type-extension
    :name (expect-then-parse '("extend" "union") 'name)
    :directives (parse 'directives t)
    :union-members (parse 'union-member-types)))

(defparser enum-type-extension
  (make-node 'enum-type-extension
    :name (expect-then-parse '("extend" "enum") 'name)
    :directives (parse 'directives t)
    :enum-values (parse 'enum-values)))

(defparser input-object-type-extension
  (make-node 'input-object-type-extension
    :name (expect-then-parse '("extend" "input") 'name)
    :directives (parse 'directives t)
    :enum-values (parse 'input-fields-definition)))

(defparser description
  (when (peek-description)
    (parse 'string-value)))

(defparser operation-type-definition
  (make-node 'operation-type-definition
    :operation (parse 'operation-type)
    :named-type (expect-then-parse 'colon 'named-type)))

(defparser schema-definition
  (make-node 'schema-definition
    :description (parse 'description)
    :directives (expect-then-parse "schema" 'directives t)
    :operation-types (many 'brace-l 'operation-type-definition 'brace-r)))

(defparser scalar-type-definition
  (make-node 'scalar-type-definition
    :description (parse 'description)
    :name (expect-then-parse "scalar" 'name)
    :directives (parse 'directives t)))

(defparser object-type-definition
  (make-node 'object-type-definition
    :description (parse 'description)
    :name (expect-then-parse "type" 'name)
    :interfaces (parse 'implements-interfaces)
    :directives (parse 'directives t)
    :fields (parse 'fields-definition)))

(defparser implements-interfaces
  (when (expect-optional-keyword "implements")
    (delimited-many 'amp 'named-type)))

(defparser fields-definition
  (optional-many 'brace-l 'field-definition 'brace-r))

(defparser field-definition
  (make-node 'field-definition
    :description (parse 'description)
    :name (parse 'name)
    :args (parse 'argument-definitions)
    :ty (expect-then-parse 'colon 'type-reference)
    :directives (parse 'directives t)))

(defparser argument-definitions
  (optional-many 'paren-l 'input-value-definition 'paren-r))

(defparser input-value-definition
  (make-node 'input-value-definition
    :description (parse 'description)
    :name (parse 'name)
    :ty (expect-then-parse 'colon 'type-reference)
    :default-value (parse 'default-value t)
    :directives (parse 'directives t)))

(defparser default-value
  (when (expect-optional-token 'equals)
    (parse 'value t)))

(defparser interface-type-definition
  (make-node 'interface-type-definition
    :description (parse 'description)
    :name (expect-then-parse "interface" 'name)
    :directives (parse 'directives t)
    :fields (parse 'fields-definition)))

(defparser union-type-definition
  (make-node 'union-type-definition
    :description (parse 'description)
    :name (expect-then-parse "union" 'name)
    :directives (parse 'directives t)
    :union-members (parse 'union-member-types)))

(defparser union-member-types
  (when (expect-token 'equals)
    (delimited-many 'pipe 'named-type)))

(defparser enum-type-definition
  (make-node 'enum-type-definition
    :description (parse 'description)
    :name (expect-then-parse "enum" 'name)
    :directives (parse 'directives t)
    :enum-values (parse 'enum-values)))

(defparser enum-values
  (optional-many 'brace-l 'enum-value-definition 'brace-r))

(defparser enum-value-definition
  (make-node 'enum-value-definition
    :description (parse 'description)
    :enum-value (parse 'enum-value)
    :directives (parse 'directives t)))

(defparser input-object-type-definition
  (make-node 'input-object-type-definition
    :description (parse 'description)
    :name (expect-then-parse "input" 'name)
    :directives (parse 'directives t)
    :fields (parse 'input-fields-definition)))

(defparser input-fields-definition
  (optional-many 'brace-l 'input-value-definition 'brace-r))

(defparser directive-definition
  (make-node 'directive-definition
    :description (parse 'description)
    :name (expect-then-parse '("directive" at) 'name)
    :args (parse 'argument-definitions)
    :locations (expect-then-parse "on" 'directive-locations)))

(defparser directive-locations
  (delimited-many 'pipe 'directive-location))

(defvar *executable-directive-location*
  '("query" "mutation" "subscription" "field" "fragment_definition"
    "fragment_spread" "inline_fragment"))

(defvar *type-system-directive-location*
  '("schema" "scalar" "object" "field_definition" "argument_definition"
    "interface" "union" "enum" "enum_value" "input_object" "input_field_definition"))

(defparser directive-location
  (let ((name (parse 'name)))
    (if (some (lambda (x) (string-equal (name name) x))
              (append
               *executable-directive-location*
               *type-system-directive-location*))
        name
        (unexpected))))
