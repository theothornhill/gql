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
     (let ((value (value *token*)))
       ;; ExecutableDefinition
       (cond
         ((string= value "query")        (parse 'operation-definition))
         ((string= value "mutation")     (parse 'operation-definition))
         ((string= value "subscription") (parse 'operation-definition))

         ((string= value "fragment")     (parse 'fragment-definition))

         ;; TypeSystemDefinition
         ((string= value "schema")       (parse 'type-system-definition))
         ((string= value "scalar")       (parse 'type-system-definition))
         ((string= value "type")         (parse 'type-system-definition))
         ((string= value "interface")    (parse 'type-system-definition))
         ((string= value "union")        (parse 'type-system-definition))
         ((string= value "enum")         (parse 'type-system-definition))
         ((string= value "input")        (parse 'type-system-definition))
         ((string= value "directive")    (parse 'type-system-definition))

         ;; TypeSystemExtension
         ((string= value "extend")       (parse 'type-system-extension))
         (t (unexpected)))))
    ((peek 'brace-l) (parse 'operation-definition))
    ((peek-description)  (parse 'type-system-definition))
    (t (unexpected))))

(defparser operation-definition
  (when (peek 'brace-l)
    ;; We allow for the query shorthand by first checking for the opening
    ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
    ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
    ;; We early return to avoid parsing more than we need.
    (return-from parse
      (make-node 'operation-definition
                 :directives nil
                 :variable-definitions nil
                 :name nil
                 :operation "query"
                 :selection-set (parse 'selection-set))))
  ;; Parse the OPERATION-TYPE this so that we traverse over the node if we
  ;; don't error.
  (let ((operation (parse 'operation-type))
        name)
    (when (peek 'name)
      (setf name (parse 'name)))
    (make-node 'operation-definition
               :name name
               :operation operation
               :variable-definitions (parse 'variable-definitions)
               :directives (parse 'directives)
               :selection-set (parse 'selection-set))))

(defparser operation-type
  ;; Disallow other names than query, mutation and subscription.
  (let* ((operation-token (expect-token 'name))
         (value (value operation-token)))
    (cond
      ((string= value "query") "query")
      ((string= value "mutation") "mutation")
      ((string= value "subscription") "subscription")
      (t (unexpected operation-token)))))

(defparser fragment-definition
  (make-node 'fragment-definition
             :name (expect-then-parse "fragment" 'fragment-name)
             :type-condition (expect-then-parse "on" 'named-type)
             :directives (parse 'directives)
             :selection-set (parse 'selection-set)))

(defparser name
  (with-expected-token 'name
    (make-node 'name :name (value token))))

(defparser selection-set
  ;; Selection-set : { selection+ }
  (make-node 'selection-set
             :selections (many 'brace-l 'selection 'brace-r)))

(defparser selection
  ;; Selection :
  ;;    - Field
  ;;    - FragmentSpread
  ;;    - InlineFragment
  (if (peek 'spread)
      (parse 'fragment)
      (parse 'field)))

(defparser field
  ;; Field : Alias? Name Arguments? Directives SelectionSet
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
  ;; Fragment-name : Name but not `on`
  (if (string= (value *token*) "on")
      (unexpected)
      (parse 'name)))

(defparser arguments
  (optional-many 'paren-l
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
    (bracket-l (parse 'list-value constp))
    (brace-l (parse 'object-value constp))
    (int (progn
           (advance-one-token)
           (make-node 'int-value :value (value *token*))))
    (float (progn
             (advance-one-token)
             (make-node 'float-value :value (value *token*))))
    ((or string block-string) (parse 'string-value))
    (name (progn
            (advance-one-token)
            (let ((value (value *token*)))
              (cond
                ((string= value "true")
                 (make-node 'boolean-value :value t))
                ((string= value "false")
                 (make-node 'boolean-value :value nil))
                ((string= value "null")
                 (make-node 'null-value))
                (t
                 (make-node 'enum-value :value value))))))
    (dollar (unless constp (parse 'var)))
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
  (let ((val (value *token*)))
    (if (or (string= "true" val) (string= "false" val) (string= "null" val))
        (unexpected)
        (parse 'name))))

(defparser directives
  ;; Directives[Const] : Directive[?Const]+
  (loop
    with directives
    while (peek 'at)
    do (push (parse 'directive constp) directives)
    finally (return (nreverse directives))))

(defparser directive
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (make-node 'directive
             :name (expect-then-parse 'at 'name)
             :arguments (parse 'arguments constp)))

(defparser named-type
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (make-node 'named-type :name (parse 'name)))

(defparser type-reference
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (let ((ty))
    (if (expect-optional-token 'bracket-l)
        (setf ty (make-node 'list-type
                            :ty (prog1 (parse 'type-reference)
                                  (expect-token 'bracket-r))))
        (setf ty (parse 'named-type)))
    (when (expect-optional-token 'bang)
      (return-from parse
        (make-node 'non-null-type :ty ty)))
    ty))

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
    (cond
      ((string= (value keyword-token) "schema")    (parse 'schema-definition))
      ((string= (value keyword-token) "scalar")    (parse 'scalar-type-definition))
      ((string= (value keyword-token) "type")      (parse 'object-type-definition))
      ((string= (value keyword-token) "interface") (parse 'interface-type-definition))
      ((string= (value keyword-token) "union")     (parse 'union-type-definition))
      ((string= (value keyword-token) "enum" )     (parse 'enum-type-definition))
      ((string= (value keyword-token) "input" )    (parse 'input-object-type-definition))
      ((string= (value keyword-token) "directive") (parse 'directive-definition))
      (t (unexpected)))))

(defparser type-system-extension
  (let ((keyword-token (lookahead (lexer *parser*))))
    (if (eq (kind keyword-token) 'name)
      (cond
        ((string= (value keyword-token) "schema")    (parse 'schema-extension))
        ((string= (value keyword-token) "scalar")    (parse 'scalar-type-extension))
        ((string= (value keyword-token) "type")      (parse 'object-type-extension))
        ((string= (value keyword-token) "interface") (parse 'interface-type-extension))
        ((string= (value keyword-token) "union")     (parse 'union-type-extension))
        ((string= (value keyword-token) "enum" )     (parse 'enum-type-extension))
        ((string= (value keyword-token) "input" )    (parse 'input-object-type-extension))
        (t (unexpected)))
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
